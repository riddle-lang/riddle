use super::{
    Body, BodyCtx, BodyId, CallableSignature, ClosureKind, Expr, ExprId, GenericEdge, HashMap,
    HirFunction, HirTypeRef, HirVariantKind, LabelStyle, Pattern, PendingGenericCall, ResolvedName,
    SourceLabel, Stmt, TraitId, TraitMethodCall, Type, TypeChecker, ValueUse, bound_target_param,
    builtin_callable_kind, callable_signature_type, child_exprs, collect_subst,
    generic_param_map_with_consts, grows_generic_arg, pattern_has_unresolved_param,
    record_generic_arg_spans, substitute_type, type_has_unresolved_inference,
};

impl TypeChecker<'_> {
    pub(super) fn check_call(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        callee: ExprId,
        args: &[ExprId],
        type_args: &[HirTypeRef],
        expected: Option<&Type>,
    ) -> Type {
        let span = ctx.expr_range(expr_id);
        let impl_type_args = match &ctx.body.exprs[callee] {
            Expr::Path { path, .. } => path
                .segments
                .len()
                .checked_sub(2)
                .map(|index| path.type_args_for_segment(index).to_vec())
                .unwrap_or_default(),
            _ => Vec::new(),
        };
        if matches!(&ctx.body.exprs[callee], Expr::FieldAccess { .. }) {
            return self.check_method_call(
                ctx,
                (expr_id, callee, args, type_args),
                (expected, span),
            );
        }

        if let Expr::Path {
            resolved: Some(ResolvedName::EnumVariant(enum_id, variant_index)),
            ..
        } = &ctx.body.exprs[callee]
        {
            return self.check_enum_variant_call(
                ctx,
                *enum_id,
                *variant_index,
                args,
                &impl_type_args,
                (expected, span),
            );
        }

        if let Expr::Path {
            path,
            resolved: Some(ResolvedName::Trait(trait_id)),
        } = &ctx.body.exprs[callee]
            && path.segments.len() > 1
        {
            let method = path
                .segments
                .last()
                .expect("checked path has a method")
                .0
                .clone();
            return self.check_static_trait_call(
                ctx,
                (expr_id, callee, *trait_id, &method, args),
                (expected, span),
            );
        }

        let callee_ty = self.check_expr(ctx, callee);
        let resolved_callee = self.resolve_type(&callee_ty);
        if let Type::Closure {
            id,
            generics,
            signature,
            ..
        } = &resolved_callee
            && !generics.is_empty()
        {
            if !type_args.is_empty() && type_args.len() != generics.len() {
                self.diagnostic(
                    "E0032",
                    format!(
                        "anonymous function expects {} type argument(s), got {}",
                        generics.len(),
                        type_args.len()
                    ),
                    span,
                );
            }
            let concrete = generics
                .iter()
                .enumerate()
                .map(|(index, _)| match type_args.get(index) {
                    Some(arg) => self.lower_type_ref_with_params_at(
                        arg,
                        &ctx.generic_params,
                        ctx.expr_range(callee),
                    ),
                    None => self.fresh_infer(),
                })
                .collect::<Vec<_>>();
            let subst = generics
                .iter()
                .cloned()
                .zip(concrete.iter().cloned())
                .collect::<HashMap<_, _>>();
            let instantiated = CallableSignature {
                is_unsafe: signature.is_unsafe,
                kind: signature.kind,
                params: signature
                    .params
                    .iter()
                    .map(|param| substitute_type(param, &subst))
                    .collect(),
                ret: Box::new(substitute_type(&signature.ret, &subst)),
            };
            let result = self.check_callable_value_call(
                ctx,
                callee,
                &resolved_callee,
                args,
                &instantiated,
                span,
            );
            let generic_bounds = match &self.hir.bodies[id.body].exprs[id.expr] {
                Expr::Lambda { generic_bounds, .. } => generic_bounds.clone(),
                _ => Vec::new(),
            };
            let resolved_subst = subst
                .iter()
                .map(|(name, ty)| (name.clone(), self.resolve_type(ty)))
                .collect();
            self.check_item_bounds(
                ctx,
                "anonymous function",
                &generic_bounds,
                &resolved_subst,
                span,
            );
            let concrete = concrete.iter().map(|arg| self.resolve_type(arg)).collect();
            self.result.generic_calls.insert(
                (ctx.body_id, callee),
                crate::result::GenericCall { args: concrete },
            );
            return result;
        }
        let callable_impl = self.callable_impl_for_type(&resolved_callee);
        let signature = if matches!(resolved_callee, Type::FunctionItem { .. }) {
            None
        } else {
            callable_impl
                .as_ref()
                .map(|(signature, _)| signature.clone())
                .or_else(|| self.callable_signature_for_type(&resolved_callee))
                .or_else(|| self.callable_bound_for_type(ctx, &resolved_callee))
        };
        if let Some(signature) = signature {
            if let Some((_, method)) = callable_impl {
                self.result
                    .callable_impl_calls
                    .insert((ctx.body_id, expr_id), method);
            }
            return self.check_callable_value_call(
                ctx,
                callee,
                &resolved_callee,
                args,
                &signature,
                span,
            );
        }
        let Type::FunctionItem { function: fid, .. } = callee_ty else {
            for arg in args {
                self.check_expr(ctx, *arg);
                self.record_value_use(ctx, *arg, ValueUse::Move);
            }
            if !callee_ty.is_unknown_like() {
                self.diagnostic(
                    "E0004",
                    format!("cannot call value of type {}", callee_ty.display(self.hir)),
                    ctx.expr_range(callee),
                );
            }
            return Type::Error;
        };

        self.check_function_call(
            ctx,
            callee,
            fid,
            args,
            (impl_type_args.as_slice(), type_args),
            (expected, span),
        )
    }

    pub(super) fn check_callable_value_call(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        callee: ExprId,
        callee_ty: &Type,
        args: &[ExprId],
        signature: &CallableSignature,
        span: Option<rowan::TextRange>,
    ) -> Type {
        self.record_value_use(
            ctx,
            callee,
            match signature.kind {
                ClosureKind::Fn => ValueUse::Shared,
                ClosureKind::FnMut => ValueUse::Mutable,
                ClosureKind::FnOnce => ValueUse::Move,
            },
        );
        match (signature.kind, callee_ty) {
            (ClosureKind::FnMut, Type::Ref(_, true)) => {}
            (ClosureKind::FnMut, Type::Ref(_, false)) => self.diagnostic(
                "E0031",
                "cannot call a mutable closure through an immutable reference",
                ctx.expr_range(callee),
            ),
            (ClosureKind::FnMut, _) => self.check_mutable_closure_binding(ctx, callee),
            (ClosureKind::FnOnce, Type::Ref(..)) => self.diagnostic(
                "E0035",
                "cannot call an `FnOnce` value through a reference; pass it by value",
                ctx.expr_range(callee),
            ),
            _ => {}
        }
        if signature.is_unsafe {
            self.require_unsafe(ctx, "calling an unsafe function", span);
        }
        if args.len() != signature.params.len() {
            self.diagnostic(
                "E0005",
                format!(
                    "function value expects {} argument(s), got {}",
                    signature.params.len(),
                    args.len()
                ),
                span,
            );
        }
        for (index, arg) in args.iter().enumerate() {
            if let Some(param) = signature.params.get(index) {
                let actual = self.check_expr_expected(ctx, *arg, param);
                self.expect_assignable_with_occurs_span(
                    param,
                    &actual,
                    "function argument",
                    ctx.expr_range(*arg),
                    span,
                );
                self.record_value_use(ctx, *arg, Self::parameter_value_use(param));
            } else {
                self.check_expr(ctx, *arg);
                self.record_value_use(ctx, *arg, ValueUse::Move);
            }
        }
        self.resolve_type(&signature.ret)
    }

    fn check_function_call(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        callee: ExprId,
        fid: hir::item_tree::FunctionId,
        args: &[ExprId],
        type_args: (&[HirTypeRef], &[HirTypeRef]),
        expected_span: (Option<&Type>, Option<rowan::TextRange>),
    ) -> Type {
        let (impl_type_args, type_args) = type_args;
        let (expected, span) = expected_span;

        let function = self.hir.item_tree.functions[fid].clone();
        if function.is_unsafe {
            self.require_unsafe(ctx, "calling an unsafe function", span);
        }
        let impl_generics = self.impl_generic_names(fid);
        let impl_const_generics = self.impl_const_generic_names(fid);
        let mut params = generic_param_map_with_consts(
            impl_generics
                .iter()
                .map(String::as_str)
                .chain(function.generics.iter().map(|name| name.0.as_str()))
                .chain(
                    function
                        .implicit_generics
                        .iter()
                        .map(|name| name.0.as_str()),
                ),
            impl_const_generics
                .iter()
                .map(String::as_str)
                .chain(function.const_generics.iter().map(|name| name.0.as_str())),
        );
        // A static path call (`Point::make(3)`) reaches this map without a
        // receiver, so `Self` must be bound here the same way the declaration
        // checker binds it, or `-> Self` falls through to E0034.
        if let Some(self_ty_ref) = self.impl_self_ty_ref(fid).cloned() {
            let owner = self
                .hir
                .item_tree
                .impls
                .iter()
                .find_map(|(_, imp)| imp.methods.contains(&fid).then_some(imp));
            if let Some(imp) = owner
                && imp.trait_ty.is_none()
            {
                let self_ty = self.lower_type_ref_with_params_at(
                    &self_ty_ref,
                    &params,
                    Some(imp.self_ty_range),
                );
                params.insert("Self".into(), self_ty);
            }
        }
        let mut subst = HashMap::new();
        let mut generic_arg_spans = HashMap::new();

        self.apply_impl_type_args(
            ctx,
            &function,
            &impl_generics,
            impl_type_args,
            &mut subst,
            span,
        );
        self.apply_function_type_args(ctx, &function, type_args, &mut subst, span);

        self.seed_type_inference(
            impl_generics
                .iter()
                .map(String::as_str)
                .chain(function.generics.iter().map(|name| name.0.as_str()))
                .chain(
                    function
                        .implicit_generics
                        .iter()
                        .map(|name| name.0.as_str()),
                ),
            &mut subst,
        );

        if args.len() != function.params.len() {
            self.diagnostic(
                "E0005",
                format!(
                    "function `{}` expects {} argument(s), got {}",
                    function.name.0,
                    function.params.len(),
                    args.len()
                ),
                span,
            );
        }

        self.check_function_arguments(
            ctx,
            &function,
            args,
            &params,
            (&mut subst, &mut generic_arg_spans),
            span,
        );

        // Params that appear only inside where-clause associated-type
        // bindings (`I: Iterator<Item = T>`) get bound from the concrete
        // type substituted for the target param, since no argument mentions
        // them directly.
        let impl_generic_names = impl_generics
            .iter()
            .chain(function.implicit_generics.iter().map(|name| &name.0))
            .cloned()
            .collect::<Vec<_>>();
        self.collect_bound_assoc_subst(&function, &impl_generic_names, &mut subst);

        if let (Some(expected), Some(return_ty)) = (expected, function.ret_type.as_ref()) {
            let return_pattern = self.lower_type_ref_with_params_at(
                return_ty,
                &params,
                function.ret_type_range.or(Some(function.name_range)),
            );
            collect_subst(&return_pattern, expected, &mut subst);
        }

        if !impl_generics.is_empty()
            || !impl_const_generics.is_empty()
            || !function.generics.is_empty()
            || !function.implicit_generics.is_empty()
            || !function.const_generics.is_empty()
        {
            self.record_function_generic_call(
                ctx,
                (callee, fid),
                &function,
                (&impl_generics, &impl_const_generics),
                (&subst, generic_arg_spans),
                span,
            );
        }

        let return_ty = self.function_call_return_type(&function, &params, &subst);
        if let Some(expected) = expected {
            let _ = self.unify_types(&return_ty, expected);
            self.last_occurs_error = None;
        }
        return_ty
    }

    fn function_call_return_type(
        &mut self,
        function: &HirFunction,
        params: &HashMap<String, Type>,
        subst: &HashMap<String, Type>,
    ) -> Type {
        function.ret_type.as_ref().map_or(Type::Unit, |ty| {
            substitute_type(
                &self.lower_type_ref_with_params_at(
                    ty,
                    params,
                    function.ret_type_range.or(Some(function.name_range)),
                ),
                subst,
            )
        })
    }

    fn record_function_generic_call(
        &mut self,
        ctx: &BodyCtx<'_>,
        call: (ExprId, hir::item_tree::FunctionId),
        function: &HirFunction,
        impl_generics: (&[String], &[String]),
        inference: (&HashMap<String, Type>, HashMap<String, rowan::TextRange>),
        span: Option<rowan::TextRange>,
    ) {
        let (callee, function_id) = call;
        let (impl_generics, impl_const_generics) = impl_generics;
        let (subst, generic_arg_spans) = inference;
        let inferred_names = impl_generics
            .iter()
            .map(String::as_str)
            .chain(function.generics.iter().map(|name| name.0.as_str()))
            .chain(
                function
                    .implicit_generics
                    .iter()
                    .map(|name| name.0.as_str()),
            )
            .chain(impl_const_generics.iter().map(String::as_str))
            .chain(function.const_generics.iter().map(|name| name.0.as_str()))
            .map(str::to_string)
            .collect::<Vec<_>>();
        let generic_args = inferred_names
            .iter()
            .map(|name| subst.get(name).cloned().unwrap_or(Type::Unknown))
            .collect::<Vec<_>>();
        self.result.generic_calls.insert(
            (ctx.body_id, callee),
            crate::result::GenericCall { args: generic_args },
        );
        self.pending_generic_calls.push(PendingGenericCall {
            body_id: ctx.body_id,
            callee,
            function: function_id,
            inferred_names,
            subst: subst.clone(),
            generic_arg_spans,
            callee_span: ctx.expr_range(callee),
            span,
            kind: "function",
            caller: ctx.function_id,
            check_sized: true,
        });
    }

    fn apply_impl_type_args(
        &mut self,
        ctx: &BodyCtx<'_>,
        function: &HirFunction,
        impl_generics: &[String],
        type_args: &[HirTypeRef],
        subst: &mut HashMap<String, Type>,
        span: Option<rowan::TextRange>,
    ) {
        if type_args.is_empty() {
            return;
        }
        if type_args.len() != impl_generics.len() {
            self.diagnostic(
                "E0005",
                format!(
                    "associated function `{}` expects {} type argument(s) on its type, got {}",
                    function.name.0,
                    impl_generics.len(),
                    type_args.len()
                ),
                span,
            );
        }
        for (param_name, type_arg) in impl_generics.iter().zip(type_args) {
            let lowered = self.lower_type_ref_with_params_at(type_arg, &ctx.generic_params, span);
            subst.insert(param_name.clone(), lowered);
        }
    }

    fn apply_function_type_args(
        &mut self,
        ctx: &BodyCtx<'_>,
        function: &HirFunction,
        type_args: &[HirTypeRef],
        subst: &mut HashMap<String, Type>,
        span: Option<rowan::TextRange>,
    ) {
        if type_args.is_empty() {
            return;
        }
        if type_args.len() != function.generics.len() {
            self.diagnostic(
                "E0005",
                format!(
                    "function `{}` expects {} type argument(s), got {}",
                    function.name.0,
                    function.generics.len(),
                    type_args.len()
                ),
                span,
            );
        }
        for (param_name, type_arg) in function.generics.iter().zip(type_args) {
            let lowered = self.lower_type_ref_with_params_at(type_arg, &ctx.generic_params, span);
            subst.insert(param_name.0.clone(), lowered);
        }
    }

    fn check_function_arguments(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        function: &HirFunction,
        args: &[ExprId],
        params: &HashMap<String, Type>,
        inference: (
            &mut HashMap<String, Type>,
            &mut HashMap<String, rowan::TextRange>,
        ),
        span: Option<rowan::TextRange>,
    ) {
        let (subst, generic_arg_spans) = inference;
        for (index, arg) in args.iter().enumerate() {
            let Some(param) = function.params.get(index) else {
                self.check_expr(ctx, *arg);
                self.record_value_use(ctx, *arg, ValueUse::Move);
                continue;
            };
            let pattern =
                self.lower_type_ref_with_params_at(&param.ty, params, Some(param.ty_range));
            record_generic_arg_spans(&pattern, params, ctx.expr_range(*arg), generic_arg_spans);
            let expected = substitute_type(&pattern, subst);
            let callable_expected = self
                .callable_bound_for_function_type(function, &pattern, params)
                .map(callable_signature_type)
                .map(|ty| substitute_type(&ty, subst));
            let actual = match callable_expected.as_ref() {
                Some(expected) => self.check_expr_expected(ctx, *arg, expected),
                None if pattern_has_unresolved_param(&pattern, subst) => self.check_expr(ctx, *arg),
                None => self.check_expr_expected(ctx, *arg, &expected),
            };
            if let Some(expected) = callable_expected.as_ref() {
                let _ = self.unify_types(expected, &actual);
                self.last_occurs_error = None;
                self.collect_callable_argument_subst(expected, &actual, subst);
            }
            collect_subst(&pattern, &actual, subst);
            let expected = substitute_type(&pattern, subst);
            self.expect_assignable_with_occurs_span(
                &expected,
                &actual,
                "function argument",
                ctx.expr_range(*arg),
                span,
            );
            self.record_value_use(ctx, *arg, Self::hir_parameter_value_use(&param.ty));
        }
    }

    pub(super) fn check_static_trait_call(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        call: (ExprId, ExprId, TraitId, &str, &[ExprId]),
        expected_span: (Option<&Type>, Option<rowan::TextRange>),
    ) -> Type {
        let (expr_id, callee, trait_id, method_name, args) = call;
        let (expected, span) = expected_span;
        let Some(method) = self.hir.item_tree.traits[trait_id]
            .methods
            .iter()
            .find(|method| method.name.0 == method_name)
            .cloned()
        else {
            self.diagnostic(
                "E0013",
                format!("trait has no associated function `{method_name}`"),
                span,
            );
            return Type::Error;
        };
        if method
            .params
            .first()
            .is_some_and(|param| param.name.0 == "self")
        {
            for arg in args {
                self.check_expr(ctx, *arg);
                self.record_value_use(ctx, *arg, ValueUse::Move);
            }
            self.diagnostic(
                "E0013",
                format!("`{method_name}` requires a receiver"),
                span,
            );
            return Type::Error;
        }
        if args.len() != method.params.len() {
            self.diagnostic(
                "E0005",
                format!(
                    "associated function `{method_name}` expects {} argument(s), got {}",
                    method.params.len(),
                    args.len()
                ),
                span,
            );
        };

        let mut params = ctx.generic_params.clone();
        params.insert("Self".into(), Type::Param("Self".into()));
        for name in &self.hir.item_tree.traits[trait_id].generics {
            params.entry(name.0.clone()).or_insert(Type::Unknown);
        }
        for name in &method.generics {
            params.entry(name.0.clone()).or_insert(Type::Unknown);
        }

        let mut subst = HashMap::new();
        if let (Some(expected), Some(return_ty)) = (expected, method.ret_type.as_ref()) {
            let pattern = self.lower_type_ref_with_params_at(
                return_ty,
                &params,
                method.ret_type_range.or(Some(method.name_range)),
            );
            collect_subst(&pattern, expected, &mut subst);
        }
        let receiver = subst.get("Self").cloned().unwrap_or(Type::Unknown);
        params.insert("Self".into(), receiver.clone());

        for (index, arg) in args.iter().enumerate() {
            let Some(param) = method.params.get(index) else {
                self.check_expr(ctx, *arg);
                self.record_value_use(ctx, *arg, ValueUse::Move);
                continue;
            };
            let pattern =
                self.lower_type_ref_with_params_at(&param.ty, &params, Some(param.ty_range));
            let expected_arg = substitute_type(&pattern, &subst);
            let actual = self.check_expr_expected(ctx, *arg, &expected_arg);
            collect_subst(&pattern, &actual, &mut subst);
            let expected_arg = substitute_type(&pattern, &subst);
            self.expect_assignable_with_occurs_span(
                &expected_arg,
                &actual,
                "function argument",
                ctx.expr_range(*arg),
                span,
            );
            self.record_value_use(ctx, *arg, Self::hir_parameter_value_use(&param.ty));
        }

        let receiver = subst.get("Self").cloned().unwrap_or(receiver);
        let receiver = self.resolve_type(&receiver);
        // A static `Trait::method(...)` call needs an implementer: without
        // one, MIR resolves the call to the bodiless trait declaration and
        // codegen emits a call through a null function pointer.
        let trait_name = self.hir.item_tree.traits[trait_id].name.0.clone();
        let self_satisfied = match &receiver {
            // A generic parameter relies on its bounds, checked against the
            // enclosing function's trait assumptions.
            Type::Param(_) => self.trait_method_self_satisfied(ctx, &receiver, trait_id),
            Type::Unknown | Type::Error => false,
            ty => self.result.trait_env.type_implements(ty, trait_id),
        };
        match &receiver {
            Type::Unknown | Type::Error => {
                self.diagnostic(
                    "E0005",
                    format!(
                        "cannot infer the `Self` type for `{trait_name}::{method_name}`: pass a receiver or annotate the expected type"
                    ),
                    span,
                );
            }
            ty if !self_satisfied => {
                self.diagnostic(
                    "E0026",
                    format!(
                        "no implementation of `{trait_name}` found for `{}`",
                        ty.display(self.hir)
                    ),
                    span,
                );
            }
            _ => {}
        }
        self.result
            .expr_types
            .insert((ctx.body_id, callee), receiver);
        self.result.trait_method_calls.insert(
            (ctx.body_id, expr_id),
            TraitMethodCall {
                trait_id,
                method: method_name.into(),
                dynamic: false,
            },
        );
        method.ret_type.as_ref().map_or(Type::Unit, |ty| {
            substitute_type(
                &self.lower_type_ref_with_params_at(
                    ty,
                    &params,
                    method.ret_type_range.or(Some(method.name_range)),
                ),
                &subst,
            )
        })
    }

    /// True when the enclosing function's bounds prove `ty` implements
    /// `trait_id` (used for `Trait::method` calls with a generic `Self`).
    fn trait_method_self_satisfied(
        &mut self,
        _ctx: &BodyCtx<'_>,
        ty: &Type,
        trait_id: TraitId,
    ) -> bool {
        self.active_trait_assumptions
            .iter()
            .any(|bound| bound.ty == *ty && bound.trait_id == trait_id)
    }

    pub(super) fn check_enum_variant_call(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        enum_id: hir::item_tree::EnumId,
        variant_index: usize,
        args: &[ExprId],
        type_args: &[HirTypeRef],
        expected_span: (Option<&Type>, Option<rowan::TextRange>),
    ) -> Type {
        let (expected, span) = expected_span;
        let enum_data = &self.hir.item_tree.enums[enum_id];
        let Some(variant) = enum_data.variants.get(variant_index) else {
            for arg in args {
                self.check_expr(ctx, *arg);
                self.record_value_use(ctx, *arg, ValueUse::Move);
            }
            return Type::Error;
        };

        let mut subst = match expected {
            Some(Type::Enum(expected_id, expected_args)) if *expected_id == enum_id => enum_data
                .generics
                .iter()
                .chain(enum_data.const_generics.iter())
                .zip(expected_args.iter())
                .map(|(name, ty)| (name.0.clone(), ty.clone()))
                .collect::<HashMap<_, _>>(),
            _ => HashMap::new(),
        };
        if !type_args.is_empty() {
            let expected_count = enum_data.generics.len() + enum_data.const_generics.len();
            if type_args.len() != expected_count {
                self.diagnostic(
                    "E0032",
                    format!(
                        "type `{}` expects {expected_count} type argument(s), got {}",
                        enum_data.name.0,
                        type_args.len()
                    ),
                    span,
                );
            }
            for (name, type_arg) in enum_data
                .generics
                .iter()
                .chain(enum_data.const_generics.iter())
                .zip(type_args)
            {
                let lowered =
                    self.lower_type_ref_with_params_at(type_arg, &ctx.generic_params, span);
                subst.insert(name.0.clone(), lowered);
            }
        }
        let params = generic_param_map_with_consts(
            enum_data.generics.iter().map(|name| name.0.as_str()),
            enum_data.const_generics.iter().map(|name| name.0.as_str()),
        );
        let fields = match &variant.kind {
            HirVariantKind::Tuple(fields) => fields.as_slice(),
            HirVariantKind::Unit => &[],
            HirVariantKind::Struct(_) => {
                for arg in args {
                    self.check_expr(ctx, *arg);
                    self.record_value_use(ctx, *arg, ValueUse::Move);
                }
                self.diagnostic(
                    "E0004",
                    format!(
                        "cannot call struct enum variant `{}`; use struct literal syntax",
                        variant.name.0
                    ),
                    span,
                );
                return Type::Error;
            }
        };

        if args.len() != fields.len() {
            self.diagnostic(
                "E0005",
                format!(
                    "enum variant `{}` expects {} argument(s), got {}",
                    variant.name.0,
                    fields.len(),
                    args.len()
                ),
                span,
            );
        }

        for (index, arg) in args.iter().enumerate() {
            if let Some(field) = fields.get(index) {
                let pattern = self.lower_type_ref_with_params_at(
                    field,
                    &params,
                    variant
                        .field_ranges
                        .get(index)
                        .copied()
                        .or(Some(variant.name_range)),
                );
                let expected = substitute_type(&pattern, &subst);
                let actual = if pattern_has_unresolved_param(&pattern, &subst) {
                    self.check_expr(ctx, *arg)
                } else {
                    self.check_expr_expected(ctx, *arg, &expected)
                };
                collect_subst(&pattern, &actual, &mut subst);
                let expected = substitute_type(&pattern, &subst);
                self.expect_assignable(
                    &expected,
                    &actual,
                    "enum variant argument",
                    ctx.expr_range(*arg),
                );
            } else {
                self.check_expr(ctx, *arg);
            }
            self.record_value_use(ctx, *arg, ValueUse::Move);
        }

        let args = enum_data
            .generics
            .iter()
            .chain(enum_data.const_generics.iter())
            .map(|name| subst.get(&name.0).cloned().unwrap_or(Type::Unknown))
            .collect();
        let ty = Type::Enum(enum_id, args);
        self.check_type_bounds(ctx, &ty, span);
        ty
    }

    pub(super) fn enum_variant_type(
        &mut self,
        ctx: &BodyCtx<'_>,
        enum_id: hir::item_tree::EnumId,
        expected: Option<&Type>,
        path: &hir::item_tree::HirPath,
        span: Option<rowan::TextRange>,
    ) -> Type {
        let enum_data = &self.hir.item_tree.enums[enum_id];
        let resolved_expected = expected.map(|ty| self.resolve_type(ty));
        let mut args = if let Some(Type::Enum(expected_id, args)) = resolved_expected.as_ref()
            && *expected_id == enum_id
        {
            args.clone()
        } else {
            // Fresh inference variables instead of `Unknown`: a bare
            // `Option::None` binding can later unify with a concrete
            // `Option<T>` through inference rather than erroring.
            enum_data
                .generics
                .iter()
                .chain(enum_data.const_generics.iter())
                .map(|_| self.fresh_infer())
                .collect()
        };
        let enum_segment = path.segments.len().checked_sub(2);
        let explicit = enum_segment
            .map(|index| path.type_args_for_segment(index))
            .filter(|args| !args.is_empty())
            .unwrap_or(path.type_args.as_slice());
        if !explicit.is_empty() {
            let expected_count = enum_data.generics.len() + enum_data.const_generics.len();
            if explicit.len() != expected_count {
                self.diagnostic(
                    "E0032",
                    format!(
                        "type `{}` expects {expected_count} type argument(s), got {}",
                        enum_data.name.0,
                        explicit.len()
                    ),
                    span,
                );
            }
            for (slot, arg) in args.iter_mut().zip(explicit) {
                *slot = self.lower_type_ref_with_params_at(arg, &ctx.generic_params, span);
            }
        }
        Type::Enum(enum_id, args)
    }

    pub(super) fn callable_bound_for_type(
        &mut self,
        ctx: &BodyCtx<'_>,
        ty: &Type,
    ) -> Option<CallableSignature> {
        let Type::Param(param) = self.resolve_type(ty) else {
            return None;
        };
        for bound in self.current_generic_bounds(ctx) {
            if bound_target_param(&bound) != Some(param.as_str()) {
                continue;
            }
            let Some(kind) = builtin_callable_kind(&bound.trait_ty) else {
                continue;
            };
            let signature = bound.callable.as_ref()?;
            return Some(self.lower_hir_callable_signature(
                signature,
                kind,
                &ctx.generic_params,
                Some(bound.trait_range),
            ));
        }
        let function = ctx.function?;
        for parameter in &function.params {
            let HirTypeRef::ImplTrait {
                trait_ty,
                trait_range,
                callable: Some(callable),
                hidden: Some(hidden),
            } = &parameter.ty
            else {
                continue;
            };
            if hidden.0 != param {
                continue;
            }
            let kind = builtin_callable_kind(trait_ty)?;
            return Some(self.lower_hir_callable_signature(
                callable,
                kind,
                &ctx.generic_params,
                Some(*trait_range),
            ));
        }
        None
    }

    pub(super) fn callable_bound_for_function_type(
        &mut self,
        function: &HirFunction,
        ty: &Type,
        params: &HashMap<String, Type>,
    ) -> Option<CallableSignature> {
        let Type::Param(param) = ty else {
            return None;
        };
        for bound in &function.generic_bounds {
            if bound_target_param(bound) != Some(param.as_str()) {
                continue;
            }
            let Some(kind) = builtin_callable_kind(&bound.trait_ty) else {
                continue;
            };
            let signature = bound.callable.as_ref()?;
            return Some(self.lower_hir_callable_signature(
                signature,
                kind,
                params,
                Some(bound.trait_range),
            ));
        }
        for parameter in &function.params {
            let HirTypeRef::ImplTrait {
                trait_ty,
                trait_range,
                callable: Some(callable),
                hidden: Some(hidden),
            } = &parameter.ty
            else {
                continue;
            };
            if hidden.0 != *param {
                continue;
            }
            let kind = builtin_callable_kind(trait_ty)?;
            return Some(self.lower_hir_callable_signature(
                callable,
                kind,
                params,
                Some(*trait_range),
            ));
        }
        None
    }

    pub(super) fn check_callable_requirement(
        &mut self,
        ctx: &BodyCtx<'_>,
        actual: &Type,
        required: &CallableSignature,
        target: &str,
        span: Option<rowan::TextRange>,
    ) {
        if actual.is_unknown_like() {
            return;
        }
        let actual_boundary = self
            .callable_signature_for_type(actual)
            .or_else(|| self.callable_bound_for_type(ctx, actual))
            .map_or_else(|| actual.clone(), callable_signature_type);
        let required_boundary = callable_signature_type(required.clone());
        if self.unify_types(&required_boundary, &actual_boundary) {
            return;
        }
        if self.last_occurs_error.take().is_some() {
            self.diagnostic("E0067", "cannot construct an infinite type", span);
            return;
        }
        let unsafe_mismatch = self
            .callable_signature_for_type(&actual_boundary)
            .is_some_and(|signature| signature.is_unsafe && !required.is_unsafe);
        let (code, message) = if unsafe_mismatch {
            (
                "E0001",
                format!(
                    "unsafe function does not satisfy safe `{}` bound for `{target}`",
                    required.kind.as_str()
                ),
            )
        } else {
            (
                "E0035",
                format!(
                    "type `{}` does not satisfy callable bound `{}` for `{target}`",
                    actual.display(self.hir),
                    required.kind.as_str()
                ),
            )
        };
        self.diagnostic(code, message, span);
    }

    pub(crate) fn finish_pending_generic_calls(&mut self, ctx: &BodyCtx<'_>) {
        let pending = self
            .pending_generic_calls
            .iter()
            .filter(|call| call.body_id == ctx.body_id)
            .cloned()
            .collect::<Vec<_>>();
        self.pending_generic_calls
            .retain(|call| call.body_id != ctx.body_id);

        for call in pending {
            let mut subst = call
                .subst
                .iter()
                .map(|(name, ty)| (name.clone(), self.resolve_type(ty)))
                .collect::<HashMap<_, _>>();
            let function = self.hir.item_tree.functions[call.function].clone();
            self.check_generic_bounds(
                ctx,
                &function,
                &subst,
                &call.generic_arg_spans,
                call.callee_span,
                call.span,
            );
            subst = subst
                .iter()
                .map(|(name, ty)| (name.clone(), self.resolve_type(ty)))
                .collect();
            let unresolved = call
                .inferred_names
                .iter()
                .any(|name| subst.get(name).is_none_or(type_has_unresolved_inference));
            if unresolved {
                let unresolved_names = call
                    .inferred_names
                    .iter()
                    .filter(|name| subst.get(*name).is_none_or(type_has_unresolved_inference))
                    .cloned()
                    .collect::<Vec<_>>();
                let message = match unresolved_names.as_slice() {
                    [name] => format!(
                        "cannot infer type argument `{name}` for {} `{}`",
                        call.kind, function.name.0
                    ),
                    _ => format!(
                        "cannot infer type argument(s) for {} `{}`",
                        call.kind, function.name.0
                    ),
                };
                let diagnostics_before = self.result.diagnostics.len();
                self.diagnostic("E0005", message, call.span);
                if self.result.diagnostics.len() == diagnostics_before {
                    continue;
                }
                let hint = call
                    .span
                    .and_then(|span| self.enclosing_binding_hint(ctx.body_id, span));
                let diagnostic = self
                    .result
                    .diagnostics
                    .last_mut()
                    .expect("diagnostic was pushed above");
                diagnostic.notes = vec![
                    "the unresolved type argument must be determined by a call argument or an explicit type annotation".into(),
                ];
                if let Some((name, range)) = hint {
                    diagnostic.labels.push(SourceLabel {
                        range,
                        message: format!("consider giving `{name}` an explicit type"),
                        style: LabelStyle::Secondary,
                    });
                }
                continue;
            }

            let generic_args = self
                .result
                .generic_calls
                .get(&(ctx.body_id, call.callee))
                .map(|call| {
                    call.args
                        .iter()
                        .map(|arg| self.resolve_type(arg))
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();
            if call.check_sized {
                for arg in &generic_args {
                    self.expect_sized_value(arg, call.span);
                }
            }
            if let Some(caller) = call.caller {
                self.generic_edges.push(GenericEdge {
                    caller,
                    callee: call.function,
                    grows: generic_args
                        .iter()
                        .any(|arg| grows_generic_arg(arg, &ctx.generic_params)),
                    span: call.span,
                });
            }
        }
    }

    /// Locates the outermost user-written `let` binding whose initializer
    /// contains `inner`, binds a plain name, and carries no type annotation —
    /// the natural place to add one. Standard macro expansions bind hidden
    /// `__riddle_*` temporaries (e.g. the `Vector::new` inside `vec![]`), so
    /// those are skipped in favor of the binding a user can actually annotate.
    fn enclosing_binding_hint(
        &self,
        body_id: BodyId,
        inner: rowan::TextRange,
    ) -> Option<(String, rowan::TextRange)> {
        let body = &self.hir.bodies[body_id];
        let mut hint = None;
        self.collect_binding_hint(body, body.root_block, inner, &mut hint);
        hint
    }

    fn collect_binding_hint(
        &self,
        body: &Body,
        expr: ExprId,
        inner: rowan::TextRange,
        hint: &mut Option<(String, rowan::TextRange)>,
    ) {
        if hint.is_some() {
            return;
        }
        let Some(range) = body.source_map.expr_ranges.get(&expr).copied() else {
            return;
        };
        if !range.contains(inner.start()) {
            return;
        }
        if let Expr::Block { stmts, tail } = &body.exprs[expr] {
            for stmt_id in stmts {
                let Some(stmt_range) = body.source_map.stmt_ranges.get(stmt_id).copied() else {
                    continue;
                };
                if !stmt_range.contains(inner.start()) {
                    continue;
                }
                if let Stmt::Let {
                    pat,
                    ty,
                    init: Some(init),
                    ..
                } = &body.stmts[*stmt_id]
                {
                    if matches!(ty, HirTypeRef::Unknown)
                        && let Pattern::Binding { name, .. } = &body.pats[*pat]
                        && !name.0.starts_with("__riddle_")
                        && let Some(pat_range) = body.source_map.pat_ranges.get(pat).copied()
                    {
                        *hint = Some((name.0.clone(), pat_range));
                    }
                    self.collect_binding_hint(body, *init, inner, hint);
                }
            }
            if let Some(tail) = tail {
                self.collect_binding_hint(body, *tail, inner, hint);
            }
            return;
        }
        for child in child_exprs(&body.exprs[expr]) {
            self.collect_binding_hint(body, child, inner, hint);
        }
    }

    pub(super) fn check_generic_bounds(
        &mut self,
        ctx: &BodyCtx<'_>,
        function: &HirFunction,
        subst: &HashMap<String, Type>,
        generic_arg_spans: &HashMap<String, rowan::TextRange>,
        callee_span: Option<rowan::TextRange>,
        span: Option<rowan::TextRange>,
    ) {
        for bound in &function.generic_bounds {
            let bound_span = bound_target_param(bound)
                .and_then(|name| generic_arg_spans.get(name).copied())
                .or(span);
            let actual = self.lower_type_ref_with_params_at(
                &bound.target_ty,
                subst,
                Some(bound.target_range),
            );
            if actual.is_unknown_like() {
                continue;
            }
            if let Some(kind) = builtin_callable_kind(&bound.trait_ty) {
                let Some(signature) = bound.callable.as_ref() else {
                    self.diagnostic(
                        "E0047",
                        format!("{} bound requires a callable signature", kind.as_str()),
                        Some(bound.trait_range),
                    );
                    continue;
                };
                let required = self.lower_hir_callable_signature(
                    signature,
                    kind,
                    subst,
                    Some(bound.trait_range),
                );
                self.check_callable_requirement(
                    ctx,
                    &actual,
                    &required,
                    &bound.target_ty.display(),
                    bound_span,
                );
                continue;
            }
            let Some(trait_id) = self.resolve_trait_ref(&bound.trait_ty) else {
                self.diagnostic(
                    "E0023",
                    format!(
                        "generic bound references unknown trait `{}`",
                        bound.trait_ty.display()
                    ),
                    Some(bound.trait_range),
                );
                continue;
            };
            if !self.type_satisfies_bound(
                ctx,
                &actual,
                trait_id,
                &bound.trait_ty,
                &bound.assoc_constraints,
                subst,
            ) {
                let trait_name = self.hir.item_tree.traits[trait_id].name.0.clone();
                // Nicer diagnostics for `Debug` bounds — matched by lang
                // item id, so a user trait merely named `Debug` never fires it.
                let is_debug_bound = function.name.0 == "append_debug"
                    && Some(trait_id)
                        == self
                            .result
                            .trait_env
                            .lang_items
                            .get(ty::lang_items::LangItem::Debug);
                if is_debug_bound {
                    self.report_debug_bound_failure(
                        ctx,
                        &actual,
                        &trait_name,
                        bound_span,
                        callee_span,
                    );
                    continue;
                }
                self.diagnostic(
                    "E0035",
                    format!(
                        "type `{}` does not satisfy bound `{}` for `{}`",
                        actual.display(self.hir),
                        trait_name,
                        bound.target_ty.display()
                    ),
                    bound_span,
                );
            }
        }

        self.check_impl_trait_callable_bounds(ctx, function, subst, generic_arg_spans, span);
    }

    fn check_impl_trait_callable_bounds(
        &mut self,
        ctx: &BodyCtx<'_>,
        function: &HirFunction,
        subst: &HashMap<String, Type>,
        generic_arg_spans: &HashMap<String, rowan::TextRange>,
        span: Option<rowan::TextRange>,
    ) {
        for param in &function.params {
            let HirTypeRef::ImplTrait {
                trait_ty,
                trait_range,
                callable: Some(callable),
                hidden: Some(hidden),
            } = &param.ty
            else {
                continue;
            };
            let Some(kind) = builtin_callable_kind(trait_ty) else {
                continue;
            };
            let Some(actual) = subst.get(&hidden.0) else {
                continue;
            };
            let required =
                self.lower_hir_callable_signature(callable, kind, subst, Some(*trait_range));
            let bound_span = generic_arg_spans.get(&hidden.0).copied().or(span);
            self.check_callable_requirement(ctx, actual, &required, &hidden.0, bound_span);
        }
    }

    fn report_debug_bound_failure(
        &mut self,
        ctx: &BodyCtx<'_>,
        actual: &Type,
        trait_name: &str,
        bound_span: Option<rowan::TextRange>,
        callee_span: Option<rowan::TextRange>,
    ) {
        let placeholder = concat!("{", ":?", "}");
        let type_name = actual.display(self.hir);
        let owner_package = self.hir.package_for_range(ctx.owner_range());
        let derive_target = match actual {
            Type::Struct(id, _) => {
                let item = &self.hir.item_tree.structs[*id];
                Some((item.name.0.clone(), item.name_range))
            }
            Type::Enum(id, _) => {
                let item = &self.hir.item_tree.enums[*id];
                Some((item.name.0.clone(), item.name_range))
            }
            _ => None,
        }
        .filter(|(_, range)| {
            owner_package.is_some() && self.hir.package_for_range(*range) == owner_package
        });
        let diagnostics_before = self.result.diagnostics.len();
        self.diagnostic(
            "E0035",
            format!("`{type_name}` doesn't implement `{trait_name}`"),
            bound_span,
        );
        if let Some(diagnostic) = self.result.diagnostics.get_mut(diagnostics_before) {
            diagnostic.labels[0].message = format!(
                "`{type_name}` cannot be formatted using `{placeholder}` because it doesn't implement `{trait_name}`"
            );
            if let Some(callee_span) = callee_span
                && Some(callee_span) != bound_span
            {
                diagnostic.labels.push(SourceLabel {
                    range: callee_span,
                    message: "required by this formatting parameter".into(),
                    style: LabelStyle::Secondary,
                });
            }
            diagnostic.notes = vec![format!(
                "the trait `{trait_name}` is not implemented for `{type_name}`"
            )];
            if let Some((name, range)) = derive_target {
                diagnostic.labels.push(SourceLabel {
                    range,
                    message: format!("consider annotating `{name}` with `#[derive(Debug)]`"),
                    style: LabelStyle::Secondary,
                });
                diagnostic.help = Some(format!(
                    "add `#[derive(Debug)]` to `{name}` or manually implement `Debug`"
                ));
            } else {
                diagnostic.help = Some(format!("implement `{trait_name}` for `{type_name}`"));
            }
        }
    }
}
