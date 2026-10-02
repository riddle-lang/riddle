use super::{
    BodyCtx, CallableSignature, CaptureMode, CapturePlace, CaptureSource, ClosureId, ClosureKind,
    Expr, ExprId, HirTypeRef, LabelStyle, LambdaCapture, LambdaCtx, LambdaInfo, PatId, Pattern,
    PatternBindingId, PatternBindingMode, Projection, ResolvedName, SourceLabel, Type, TypeChecker,
    UnaryOp, ValueUse, capture_mode, type_has_unresolved_inference,
};

impl TypeChecker<'_> {
    pub(super) fn declared_recursive_lambda_type(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        binding: PatternBindingId,
    ) -> Option<Type> {
        let Expr::Lambda {
            generics,
            params,
            ret_type,
            ret_type_range,
            ..
        } = &ctx.body.exprs[expr_id]
        else {
            return None;
        };
        if params
            .iter()
            .any(|param| matches!(param.ty, HirTypeRef::Unknown))
            || matches!(ret_type, HirTypeRef::Unknown)
        {
            return None;
        }
        let generics = generics.clone();
        let previous_generics = generics
            .iter()
            .map(|name| {
                (
                    name.0.clone(),
                    ctx.generic_params
                        .insert(name.0.clone(), Type::Param(name.0.clone())),
                )
            })
            .collect::<Vec<_>>();
        let referenced = self.lambda_references_binding(ctx, expr_id, binding);
        let declared = if referenced {
            let generics = generics
                .iter()
                .map(|name| name.0.clone())
                .collect::<Vec<_>>();
            let params = params
                .iter()
                .map(|param| {
                    self.lower_type_ref_with_params_at(
                        &param.ty,
                        &ctx.generic_params,
                        param.ty_range.or(param.name_range),
                    )
                })
                .collect();
            let ret =
                self.lower_type_ref_with_params_at(ret_type, &ctx.generic_params, *ret_type_range);
            Some(Type::Closure {
                id: ClosureId {
                    body: ctx.body_id,
                    expr: expr_id,
                },
                generics,
                signature: CallableSignature {
                    is_unsafe: false,
                    kind: ClosureKind::Fn,
                    params,
                    ret: Box::new(ret),
                },
            })
        } else {
            None
        };
        for (name, previous) in previous_generics {
            if let Some(previous) = previous {
                ctx.generic_params.insert(name, previous);
            } else {
                ctx.generic_params.remove(&name);
            }
        }
        declared
    }

    fn lambda_references_binding(
        &self,
        ctx: &BodyCtx<'_>,
        expr_id: ExprId,
        binding: PatternBindingId,
    ) -> bool {
        let Expr::Lambda { body, .. } = &ctx.body.exprs[expr_id] else {
            return false;
        };
        let Some(body_range) = ctx.expr_range(*body) else {
            return false;
        };
        ctx.body.exprs.iter().any(|(id, expr)| {
            matches!(
                expr,
                Expr::Path {
                    resolved: Some(ResolvedName::PatternBinding(id)),
                    ..
                } if *id == binding
            ) && ctx.expr_range(id).is_some_and(|range| {
                body_range.start() <= range.start() && range.end() <= body_range.end()
            })
        })
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn check_lambda(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        is_move: bool,
        generics: &[hir::Name],
        generic_bounds: &[hir::item_tree::HirGenericBound],
        params: &[hir::body::LambdaParam],
        ret_type: &HirTypeRef,
        ret_type_range: Option<rowan::TextRange>,
        body: ExprId,
        expected: Option<&Type>,
    ) -> Type {
        let expected = expected.map(|ty| self.callable_type(ty));
        let previous_generics = generics
            .iter()
            .map(|name| {
                (
                    name.0.clone(),
                    ctx.generic_params
                        .insert(name.0.clone(), Type::Param(name.0.clone())),
                )
            })
            .collect::<Vec<_>>();
        let expected_fn = match expected.as_ref() {
            Some(Type::CallableConstraint(signature)) => {
                Some((signature.params.as_slice(), signature.ret.as_ref()))
            }
            _ => None,
        };
        if let Some((expected_params, _)) = expected_fn
            && expected_params.len() != params.len()
        {
            self.diagnostic(
                "E0005",
                format!(
                    "anonymous function expects {} parameter(s), expected signature has {}",
                    params.len(),
                    expected_params.len()
                ),
                ctx.expr_range(expr_id),
            );
        }

        let param_types = params
            .iter()
            .enumerate()
            .map(|(index, param)| {
                if matches!(param.ty, HirTypeRef::Unknown) {
                    expected_fn
                        .and_then(|(expected, _)| expected.get(index).cloned())
                        .unwrap_or_else(|| self.fresh_infer())
                } else {
                    self.lower_type_ref_with_params_at(
                        &param.ty,
                        &ctx.generic_params,
                        param
                            .ty_range
                            .or(param.name_range)
                            .or_else(|| ctx.expr_range(expr_id)),
                    )
                }
            })
            .collect::<Vec<_>>();
        let return_ty = if matches!(ret_type, HirTypeRef::Unknown) {
            expected_fn.map_or_else(|| self.fresh_infer(), |(_, ret)| ret.clone())
        } else {
            self.lower_type_ref_with_params_at(
                ret_type,
                &ctx.generic_params,
                ret_type_range.or_else(|| ctx.expr_range(expr_id)),
            )
        };

        let old_return = std::mem::replace(&mut ctx.return_ty, return_ty.clone());
        let old_loop_stack = std::mem::take(&mut ctx.loop_stack);
        let closure_id = ClosureId {
            body: ctx.body_id,
            expr: expr_id,
        };
        ctx.lambdas.push(LambdaCtx {
            expr: expr_id,
            generic_bounds: generic_bounds.to_vec(),
            params: param_types.clone(),
            param_mutability: params.iter().map(|param| param.is_mut).collect(),
            is_move,
            outer_patterns: ctx.bindings.ids(),
            recursive_binding: ctx.bindings.closure_binding(closure_id),
            captures: Vec::new(),
        });
        for (param, ty) in params.iter().zip(&param_types) {
            if let Some(pat) = param.pat {
                self.bind_pattern(ctx, pat, ty);
                self.report_duplicate_pattern_bindings(ctx, pat);
            }
        }
        let actual = self.check_expr_expected(ctx, body, &return_ty);
        self.expect_assignable(
            &return_ty,
            &actual,
            "anonymous function return",
            ctx.expr_range(body),
        );
        self.record_value_use(ctx, body, ValueUse::Move);
        let lambda = ctx.lambdas.pop().expect("lambda context must be present");
        ctx.return_ty = old_return;
        ctx.loop_stack = old_loop_stack;

        for (name, previous) in previous_generics {
            if let Some(previous) = previous {
                ctx.generic_params.insert(name, previous);
            } else {
                ctx.generic_params.remove(&name);
            }
        }
        self.finish_lambda(
            ctx,
            expr_id,
            generics,
            params,
            param_types,
            return_ty,
            lambda,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn finish_lambda(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        generics: &[hir::Name],
        params: &[hir::body::LambdaParam],
        param_types: Vec<Type>,
        return_ty: Type,
        lambda: LambdaCtx,
    ) -> Type {
        let kind = if lambda
            .captures
            .iter()
            .any(|capture| capture.use_kind == ValueUse::Move)
        {
            ClosureKind::FnOnce
        } else if lambda
            .captures
            .iter()
            .any(|capture| capture.use_kind == ValueUse::Mutable)
        {
            ClosureKind::FnMut
        } else {
            ClosureKind::Fn
        };
        let info = LambdaInfo {
            captures: lambda.captures,
            kind,
        };
        self.result
            .lambda_infos
            .insert((ctx.body_id, expr_id), info.clone());
        self.forward_nested_captures(ctx, expr_id, &info);
        self.record_lambda(
            ctx.body_id,
            expr_id,
            params
                .iter()
                .zip(&param_types)
                .map(|(param, ty)| {
                    (
                        param.name.0.clone(),
                        param
                            .name_range
                            .or(param.ty_range)
                            .or_else(|| ctx.expr_range(expr_id)),
                        ty.clone(),
                    )
                })
                .collect(),
        );
        Type::Closure {
            id: ClosureId {
                body: ctx.body_id,
                expr: expr_id,
            },
            generics: generics.iter().map(|name| name.0.clone()).collect(),
            signature: CallableSignature {
                is_unsafe: false,
                kind,
                params: param_types,
                ret: Box::new(return_ty),
            },
        }
    }

    pub(super) fn capture_source(
        ctx: &BodyCtx<'_>,
        resolved: Option<&ResolvedName>,
    ) -> Option<CaptureSource> {
        let lambda = ctx.lambdas.last()?;
        match resolved? {
            ResolvedName::PatternBinding(id)
                if lambda.outer_patterns.contains(id) && lambda.recursive_binding != Some(*id) =>
            {
                Some(CaptureSource::Pattern(*id))
            }
            ResolvedName::Param(index) => Some(CaptureSource::Param(*index)),
            ResolvedName::LambdaParam {
                lambda: owner,
                index,
            } if *owner != lambda.expr => Some(CaptureSource::LambdaParam {
                lambda: *owner,
                index: *index,
            }),
            _ => None,
        }
    }

    pub(super) fn record_capture(
        ctx: &mut BodyCtx<'_>,
        place: CapturePlace,
        name: String,
        ty: Type,
        use_kind: ValueUse,
    ) {
        let is_move = ctx
            .lambdas
            .last()
            .expect("captures are only recorded inside lambdas")
            .is_move;
        let lambda = ctx
            .lambdas
            .last_mut()
            .expect("captures are only recorded inside lambdas");
        if let Some(capture) = lambda
            .captures
            .iter_mut()
            .find(|capture| capture.place.is_prefix_of(&place))
        {
            capture.use_kind = capture.use_kind.merge(use_kind);
            capture.mode = capture_mode(is_move, capture.use_kind);
            return;
        }
        let mut use_kind = use_kind;
        let mut index = 0;
        while index < lambda.captures.len() {
            if place.is_prefix_of(&lambda.captures[index].place) {
                use_kind = use_kind.merge(lambda.captures.remove(index).use_kind);
            } else {
                index += 1;
            }
        }
        lambda.captures.push(LambdaCapture {
            place,
            name,
            ty,
            mode: capture_mode(is_move, use_kind),
            use_kind,
        });
    }

    pub(crate) fn record_value_use(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        use_kind: ValueUse,
    ) {
        let requested_use = use_kind;
        if requested_use == ValueUse::Move
            && self
                .result
                .expr_types
                .get(&(ctx.body_id, expr_id))
                .is_some_and(type_has_unresolved_inference)
        {
            self.pending_move_uses.insert((ctx.body_id, expr_id));
        }
        let use_kind = if use_kind == ValueUse::Move
            && self
                .result
                .expr_types
                .get(&(ctx.body_id, expr_id))
                .is_some_and(|ty| self.result.trait_env.type_is_copy(ty))
        {
            ValueUse::Copy
        } else {
            use_kind
        };
        self.result
            .value_uses
            .entry((ctx.body_id, expr_id))
            .and_modify(|current| *current = current.merge(use_kind))
            .or_insert(use_kind);
        match ctx.body.exprs[expr_id].clone() {
            Expr::Block {
                tail: Some(tail), ..
            } => self.record_value_use(ctx, tail, requested_use),
            Expr::If {
                then_branch,
                else_branch,
                ..
            } => {
                self.record_value_use(ctx, then_branch, requested_use);
                if let Some(else_branch) = else_branch {
                    self.record_value_use(ctx, else_branch, requested_use);
                }
            }
            Expr::Match { arms, .. } => {
                for arm in arms {
                    self.record_value_use(ctx, arm.body, requested_use);
                }
            }
            Expr::Unsafe { body } => self.record_value_use(ctx, body, requested_use),
            Expr::Cast { base, .. } => {
                // A cast reads the pointer out of a reference operand instead
                // of consuming it, so `let p = r as *const i32;` must leave a
                // `&mut r` usable afterwards. Every other supported operand
                // type is `Copy`, which the downgrade above already covers.
                let base_use = if requested_use == ValueUse::Move
                    && self
                        .result
                        .expr_types
                        .get(&(ctx.body_id, base))
                        .is_some_and(|ty| matches!(self.resolve_type(ty), Type::Ref(..)))
                {
                    ValueUse::Copy
                } else {
                    requested_use
                };
                self.record_value_use(ctx, base, base_use);
            }
            _ => self.record_capture_use(ctx, expr_id, use_kind),
        }
    }

    pub(super) const fn parameter_value_use(ty: &Type) -> ValueUse {
        match ty {
            Type::Ref(_, false) => ValueUse::Shared,
            Type::Ref(_, true) => ValueUse::Mutable,
            _ => ValueUse::Move,
        }
    }

    pub(super) const fn hir_parameter_value_use(ty: &HirTypeRef) -> ValueUse {
        match ty {
            HirTypeRef::Ref(_, false) => ValueUse::Shared,
            HirTypeRef::Ref(_, true) => ValueUse::Mutable,
            _ => ValueUse::Move,
        }
    }

    pub(super) fn record_capture_use(
        &self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        use_kind: ValueUse,
    ) {
        let Some((place, name, ty)) = self.capture_place(ctx, expr_id) else {
            return;
        };
        Self::record_capture(ctx, place, name, ty, use_kind);
    }

    pub(super) fn capture_place(
        &self,
        ctx: &BodyCtx<'_>,
        expr_id: ExprId,
    ) -> Option<(CapturePlace, String, Type)> {
        match &ctx.body.exprs[expr_id] {
            Expr::Path { path, resolved } => {
                let source = Self::capture_source(ctx, resolved.as_ref())?;
                let ty = self
                    .result
                    .expr_types
                    .get(&(ctx.body_id, expr_id))
                    .cloned()
                    .unwrap_or(Type::Unknown);
                Some((CapturePlace::root(source), path.display(), ty))
            }
            Expr::FieldAccess { base, field } => {
                let (mut place, name, mut ty) = self.capture_place(ctx, *base)?;
                let base_ty = self
                    .result
                    .expr_types
                    .get(&(ctx.body_id, *base))
                    .map_or(Type::Unknown, |ty| self.resolve_type(ty));
                let index = match base_ty {
                    Type::Struct(struct_id, _) => self.hir.item_tree.structs[struct_id]
                        .fields
                        .iter()
                        .position(|candidate| candidate.name == *field),
                    Type::Tuple(elements) => field
                        .0
                        .parse::<usize>()
                        .ok()
                        .filter(|index| *index < elements.len()),
                    _ => None,
                };
                if let Some(index) = index {
                    place.projections.push(Projection::Field(index));
                    ty = self
                        .result
                        .expr_types
                        .get(&(ctx.body_id, expr_id))
                        .cloned()
                        .unwrap_or(Type::Unknown);
                    return Some((place, name, ty));
                }
                Some((place, name, ty))
            }
            Expr::IndexAccess { base, index } => {
                let (mut place, name, mut ty) = self.capture_place(ctx, *base)?;
                let constant = match &ctx.body.exprs[*index] {
                    Expr::IntLiteral { value, .. } => usize::try_from(*value).ok(),
                    _ => None,
                };
                if let Some(index) = constant {
                    place.projections.push(Projection::Index(Some(index)));
                    ty = self
                        .result
                        .expr_types
                        .get(&(ctx.body_id, expr_id))
                        .cloned()
                        .unwrap_or(Type::Unknown);
                }
                Some((place, name, ty))
            }
            Expr::Unary {
                operand,
                op: UnaryOp::Deref,
            } => self.capture_place(ctx, *operand),
            _ => None,
        }
    }

    pub(super) fn forward_nested_captures(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        nested_expr: ExprId,
        nested: &LambdaInfo,
    ) {
        let Some(outer) = ctx.lambdas.last() else {
            return;
        };
        let outer_expr = outer.expr;
        let outer_patterns = outer.outer_patterns.clone();
        for capture in &nested.captures {
            let comes_from_outside = match &capture.place.source {
                CaptureSource::Pattern(id) => outer_patterns.contains(id),
                CaptureSource::Param(_) => true,
                CaptureSource::LambdaParam { lambda, .. } => *lambda != outer_expr,
            };
            if !comes_from_outside {
                continue;
            }
            let use_kind = match capture.mode {
                CaptureMode::Shared => ValueUse::Shared,
                CaptureMode::Mutable => ValueUse::Mutable,
                CaptureMode::Value if self.result.trait_env.type_is_copy(&capture.ty) => {
                    ValueUse::Copy
                }
                CaptureMode::Value => ValueUse::Move,
            };
            Self::record_capture(
                ctx,
                capture.place.clone(),
                capture.name.clone(),
                capture.ty.clone(),
                use_kind,
            );
        }
        self.result
            .value_uses
            .entry((ctx.body_id, nested_expr))
            .or_insert(ValueUse::Shared);
    }

    pub(super) fn pattern_value_use(&self, ctx: &BodyCtx<'_>, pat: PatId) -> ValueUse {
        let binding_use = |id| match self
            .result
            .pattern_binding_modes
            .get(&(ctx.body_id, id))
            .copied()
            .unwrap_or(PatternBindingMode::Move)
        {
            PatternBindingMode::Ref => ValueUse::Shared,
            PatternBindingMode::RefMut => ValueUse::Mutable,
            PatternBindingMode::Move => {
                if self
                    .result
                    .pattern_binding_types
                    .get(&(ctx.body_id, id))
                    .is_some_and(|ty| {
                        !type_has_unresolved_inference(ty) && self.result.trait_env.type_is_copy(ty)
                    })
                {
                    ValueUse::Copy
                } else {
                    ValueUse::Move
                }
            }
        };
        match &ctx.body.pats[pat] {
            Pattern::Binding { .. } => binding_use(PatternBindingId {
                pattern: pat,
                field: None,
            }),
            Pattern::Tuple { elements } | Pattern::TupleStruct { elements, .. } => elements
                .iter()
                .map(|element| self.pattern_value_use(ctx, *element))
                .fold(ValueUse::Shared, ValueUse::merge),
            Pattern::Struct { fields, .. } => fields
                .iter()
                .enumerate()
                .map(|(index, field)| {
                    field.pat.map_or_else(
                        || {
                            binding_use(PatternBindingId {
                                pattern: pat,
                                field: Some(index),
                            })
                        },
                        |pat| self.pattern_value_use(ctx, pat),
                    )
                })
                .fold(ValueUse::Shared, ValueUse::merge),
            Pattern::Reference { mutable, pattern } => {
                self.pattern_value_use(ctx, *pattern).merge(if *mutable {
                    ValueUse::Mutable
                } else {
                    ValueUse::Shared
                })
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Path { .. } | Pattern::Or { .. } => {
                ValueUse::Shared
            }
        }
    }

    pub(super) fn check_mutable_closure_binding(&mut self, ctx: &BodyCtx<'_>, callee: ExprId) {
        let Expr::Path { resolved, .. } = &ctx.body.exprs[callee] else {
            return;
        };
        let is_parameter = matches!(
            resolved,
            Some(ResolvedName::Param(_) | ResolvedName::LambdaParam { .. })
        );
        let (immutable, binding_range) = match resolved {
            Some(ResolvedName::PatternBinding(id)) => {
                (!ctx.bindings.is_mut(*id), ctx.pat_range(id.pattern))
            }
            Some(ResolvedName::Param(index)) => ctx
                .function
                .and_then(|function| function.params.get(*index))
                .map_or((true, None), |param| {
                    (
                        !ctx.resolved_param_is_mut(&ResolvedName::Param(*index)),
                        Some(param.name_range),
                    )
                }),
            Some(ResolvedName::LambdaParam { lambda, index }) => {
                let Expr::Lambda { params, .. } = &ctx.body.exprs[*lambda] else {
                    return;
                };
                params.get(*index).map_or((true, None), |param| {
                    (
                        !ctx.resolved_param_is_mut(&ResolvedName::LambdaParam {
                            lambda: *lambda,
                            index: *index,
                        }),
                        param.name_range,
                    )
                })
            }
            _ => (false, None),
        };
        if immutable {
            let call_range = ctx.expr_range(callee);
            self.diagnostic(
                "E0031",
                if is_parameter {
                    "cannot call a mutable closure through an immutable parameter"
                } else {
                    "cannot call a mutable closure through an immutable binding"
                },
                binding_range.or(call_range),
            );
            if let (Some(_), Some(call_range)) = (binding_range, call_range) {
                let diagnostic = self.result.diagnostics.last_mut().unwrap();
                diagnostic.labels[0].message = if is_parameter {
                    "immutable parameter".into()
                } else {
                    "immutable closure binding".into()
                };
                diagnostic.labels.push(SourceLabel {
                    range: call_range,
                    message: "mutable closure called here".into(),
                    style: LabelStyle::Secondary,
                });
            }
        }
    }
}
