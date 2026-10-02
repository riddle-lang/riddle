use super::{
    Body, Builder, BuiltinOperator, Expr, ExprId, FuncRef, HashMap, HirBinOp, HirTypeRef, HirUnOp,
    Inst, InstKind, IntTy, LowerCtx, PanicSite, PathAnchor, ResolvedName, Type, UnOp, Value,
    builtin_comparison_types, comparison_trait, convert_cmp_op, convert_unop,
};
use crate::source_map::line_column;

impl LowerCtx<'_> {
    pub(super) fn is_std_range_expr(&self, expr: ExprId) -> bool {
        self.current_body
            .and_then(|bid| self.type_result.expr_types.get(&(bid, expr)))
            .and_then(|ty| match ty {
                type_checker::Type::Struct(sid, _) => Some(*sid),
                _ => None,
            })
            .is_some_and(|sid| {
                let s = &self.hir.item_tree.structs[sid];
                if s.name.0 != "Range" || s.fields.len() != 2 {
                    return false;
                }
                let is_i32 = |ty: &HirTypeRef| {
                    matches!(ty, HirTypeRef::Named(p)
                        if p.anchor == PathAnchor::Plain
                            && p.segments.len() == 1
                            && p.segments[0].0 == "i32"
                            && p.type_args.is_empty())
                };
                s.fields[0].name.0 == "start"
                    && s.fields[1].name.0 == "end"
                    && is_i32(&s.fields[0].ty)
                    && is_i32(&s.fields[1].ty)
            })
    }

    pub(super) fn array_iter_info(&self, expr: ExprId) -> Option<(Type, usize)> {
        self.current_body
            .and_then(|bid| self.type_result.expr_types.get(&(bid, expr)))
            .and_then(|ty| match ty {
                type_checker::Type::Array(inner, len) => {
                    Some((self.convert_type(inner), len.as_usize()?))
                }
                _ => None,
            })
    }

    pub(super) fn callee_function_id(&self, callee: ExprId) -> Option<hir::item_tree::FunctionId> {
        self.current_body
            .and_then(|bid| self.type_result.expr_types.get(&(bid, callee)))
            .and_then(|ty| match ty {
                type_checker::Type::FunctionItem { function: fid, .. } => Some(*fid),
                _ => None,
            })
    }

    pub(super) fn lower_builtin_call(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        callee: ExprId,
        args: &[ExprId],
        result_ty: Type,
    ) -> Option<Value> {
        let fid = self.callee_function_id(callee)?;
        let function = &self.hir.item_tree.functions[fid];
        if !self.hir.std_loaded || self.hir.package_for_range(function.name_range).is_some() {
            return None;
        }
        let builtin = function
            .attrs
            .iter()
            .find(|attr| attr.name.0 == "builtin")
            .and_then(|attr| attr.value.clone())?;
        let body_id = self.current_body?;
        let generic_call = self
            .type_result
            .generic_calls
            .get(&(body_id, callee))
            .cloned();
        match builtin.as_str() {
            "panic" => {
                let message = self.lower_expr(builder, param_values, body, *args.first()?);
                let range = body.source_map.expr_ranges.get(&callee)?;
                let offset: usize = range.start().into();
                let (line, column) = line_column(self.source, offset);
                let offset = u32::try_from(offset).ok()?;
                Some(builder.panic(
                    message,
                    PanicSite {
                        offset,
                        line,
                        column,
                    },
                ))
            }
            "panic_at" => {
                let message = self.lower_expr(builder, param_values, body, *args.first()?);
                let Expr::IntLiteral { value: line, .. } = body.exprs[*args.get(1)?] else {
                    return None;
                };
                let Expr::IntLiteral { value: column, .. } = body.exprs[*args.get(2)?] else {
                    return None;
                };
                // The expansion attributes the call back to the original
                // `panic!` position, so record the call-site offset for
                // module-accurate file resolution in the backend.
                let range = body.source_map.expr_ranges.get(&callee)?;
                let offset = u32::try_from(usize::from(range.start())).ok()?;
                Some(builder.panic(
                    message,
                    PanicSite {
                        offset,
                        line: u32::try_from(line).ok()?,
                        column: u32::try_from(column).ok()?,
                    },
                ))
            }
            "size_of" => {
                let ty = self.convert_type(generic_call.as_ref()?.args.first()?);
                Some(builder.size_of(ty))
            }
            "replace" => {
                let destination = self.lower_expr(builder, param_values, body, *args.first()?);
                let source = self.lower_expr(builder, param_values, body, *args.get(1)?);
                let previous = builder.load(destination, result_ty);
                builder.store(source, destination);
                Some(previous)
            }
            "swap" => {
                let element_ty = self.convert_type(generic_call.as_ref()?.args.first()?);
                let a = self.lower_expr(builder, param_values, body, *args.first()?);
                let b = self.lower_expr(builder, param_values, body, *args.get(1)?);
                let left = builder.load(a, element_ty.clone());
                let right = builder.load(b, element_ty);
                builder.store(right, a);
                builder.store(left, b);
                Some(builder.unit_const())
            }
            _ => None,
        }
    }

    pub(super) fn lower_static_trait_call(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        call: (ExprId, ExprId, &[ExprId], Type),
    ) -> Option<Value> {
        let (expr_id, callee, args, result_ty) = call;
        let body_id = self.current_body?;
        let trait_call = self
            .type_result
            .trait_method_calls
            .get(&(body_id, expr_id))
            .cloned()?;

        if let hir::body::Expr::FieldAccess { base, .. } = &body.exprs[callee] {
            if trait_call.dynamic {
                let object = self.lower_expr(builder, param_values, body, *base);
                let base_tc_ty = self.type_result.expr_types.get(&(body_id, *base))?;
                let base_tc_ty = match base_tc_ty {
                    type_checker::Type::Ref(inner, _) => inner.as_ref(),
                    ty => ty,
                };
                let (root_trait_id, root_args) = match base_tc_ty {
                    type_checker::Type::DynTrait { trait_id, args, .. }
                    | type_checker::Type::OwnedDynTrait { trait_id, args, .. } => (*trait_id, args),
                    _ => return None,
                };
                let root_subst = self.hir.item_tree.traits[root_trait_id]
                    .generics
                    .iter()
                    .map(|name| name.0.clone())
                    .zip(root_args.iter().map(|arg| self.convert_type(arg)))
                    .collect::<HashMap<_, _>>();
                let methods = self.dyn_trait_methods(root_trait_id, &root_subst);
                let object_ty =
                    self.convert_type(self.type_result.expr_types.get(&(body_id, *base))?);
                let Type::Struct(struct_ty) = object_ty else {
                    return None;
                };
                let field_name = self.dyn_trait_method_field_name(
                    &methods,
                    trait_call.trait_id,
                    &trait_call.method,
                );
                let def = struct_ty.def();
                let (slot, method_ty) =
                    def.fields
                        .iter()
                        .enumerate()
                        .find_map(|(index, (name, ty))| {
                            (name == &field_name).then(|| match ty {
                                Type::FnPtr(signature) => (index, Type::FnPtr(signature.clone())),
                                _ => (index, ty.clone()),
                            })
                        })?;
                let Type::FnPtr(_) = method_ty else {
                    return None;
                };
                let data = builder.extract_value(object, 0, Type::Ptr(Box::new(Type::Unit)));
                let mut values =
                    self.lower_expr_sequence(builder, param_values, body, expr_id, 1, args);
                values.insert(0, data);
                let method = builder.extract_value(object, slot, method_ty);
                return Some(builder.call_indirect(method, values, result_ty));
            }
            let receiver_ty = self
                .type_result
                .expr_types
                .get(&(body_id, *base))
                .cloned()
                .map(|ty| self.specialize_expr_type(body, *base, &ty))?;
            let rhs_ty = args
                .first()
                .and_then(|arg| self.type_result.expr_types.get(&(body_id, *arg)))
                .cloned();
            let callee_ty = self
                .type_result
                .expr_types
                .get(&(body_id, callee))
                .cloned()
                .unwrap_or_else(|| receiver_ty.clone());
            let candidates = [
                (&receiver_ty, rhs_ty.as_ref()),
                (&receiver_ty, None),
                (&callee_ty, rhs_ty.as_ref()),
                (&callee_ty, None),
            ];
            let (fid, dispatch_ty, dispatch_rhs) =
                candidates.into_iter().find_map(|(ty, rhs)| {
                    self.find_trait_impl_method(trait_call.trait_id, &trait_call.method, ty, rhs)
                        .map(|fid| (fid, ty, rhs))
                })?;
            if let Some(op) = self.builtin_operator_for_method(fid) {
                return Some(self.lower_builtin_operator_method_call(
                    builder,
                    param_values,
                    body,
                    expr_id,
                    *base,
                    args,
                    op,
                ));
            }
            let receiver_param = self.hir.item_tree.functions[fid].params.first()?.ty.clone();
            let receiver = self.lower_receiver_arg(
                builder,
                param_values,
                body,
                *base,
                &receiver_param,
                self.mono_method_param_type(fid, 0, dispatch_ty, dispatch_rhs)
                    .as_ref(),
            );
            let mut values = Vec::with_capacity(args.len() + 1);
            for (index, arg) in args.iter().enumerate() {
                let raw = self.lower_expr(builder, param_values, body, *arg);
                let expected =
                    self.mono_method_param_type(fid, index + 1, dispatch_ty, dispatch_rhs);
                let adjusted = match self.type_result.expr_types.get(&(body_id, *arg)) {
                    Some(arg_ty) => self.adjust_arg_for_reference_self_impl(
                        builder,
                        raw,
                        arg_ty,
                        expected.as_ref(),
                    ),
                    None => raw,
                };
                values.push(adjusted);
            }
            values.insert(0, receiver);
            let name = self
                .mono_method_name_for_receiver(fid, dispatch_ty, dispatch_rhs)
                .unwrap_or_else(|| self.function_name(fid));
            let result_ty = self.instance_result_type(&name, result_ty);
            return Some(builder.call(FuncRef::Local(name), values, result_ty));
        }
        if !matches!(
            body.exprs[callee],
            hir::body::Expr::Path {
                resolved: Some(ResolvedName::Trait(_)),
                ..
            }
        ) {
            return None;
        }
        let receiver_ty = self.type_result.expr_types.get(&(body_id, callee))?;
        let fid = self.find_trait_impl_method(
            trait_call.trait_id,
            &trait_call.method,
            receiver_ty,
            None,
        )?;
        let name = self
            .mono_method_name_for_receiver(fid, receiver_ty, None)
            .unwrap_or_else(|| self.function_name(fid));
        let values = self.lower_expr_sequence(builder, param_values, body, expr_id, 0, args);
        let result_ty = self.instance_result_type(&name, result_ty);
        Some(builder.call(FuncRef::Local(name), values, result_ty))
    }

    pub(super) fn lower_operator_call(
        &mut self,
        builder: &mut Builder,
        lhs: ExprId,
        rhs: Option<ExprId>,
        fid: hir::item_tree::FunctionId,
        args: Vec<Value>,
        ret_ty: Type,
    ) -> Value {
        let name = self
            .mono_method_name(fid, lhs, rhs)
            .unwrap_or_else(|| self.function_name(fid));
        let ret_ty = self.instance_result_type(&name, ret_ty);
        builder.call(FuncRef::Local(name), args, ret_ty)
    }

    /// Result type for a call to a locally generated function instance. A
    /// generic caller's recorded expression type can lose an associated type
    /// (`Option<Self::Item>` in a generic body lowers to a Unit placeholder),
    /// while the instance's own signature — whose associated types were
    /// seeded concretely at monomorphization — is the ABI truth.
    pub(super) fn instance_result_type(&self, name: &str, fallback: Type) -> Type {
        self.module
            .function_order
            .iter()
            .map(|fid| &self.module.functions[*fid])
            .find(|function| function.name == name)
            .map_or(fallback, |function| function.ret_type.clone())
    }

    pub(super) fn lower_comparison(
        &mut self,
        builder: &mut Builder,
        op: HirBinOp,
        lhs: Value,
        rhs: Value,
        lhs_ty: &type_checker::Type,
        rhs_ty: &type_checker::Type,
    ) -> Value {
        let lhs_ty = self.substitute_tc_type(lhs_ty);
        let rhs_ty = self.substitute_tc_type(rhs_ty);

        match (&lhs_ty, &rhs_ty) {
            (type_checker::Type::Tuple(lhs_elements), type_checker::Type::Tuple(rhs_elements))
                if lhs_elements.len() == rhs_elements.len() =>
            {
                let elements = lhs_elements
                    .iter()
                    .zip(rhs_elements)
                    .enumerate()
                    .map(|(index, (lhs_ty, rhs_ty))| {
                        (
                            builder.extract_value(lhs, index, self.convert_type(lhs_ty)),
                            builder.extract_value(rhs, index, self.convert_type(rhs_ty)),
                            lhs_ty.clone(),
                            rhs_ty.clone(),
                        )
                    })
                    .collect();
                return self.lower_aggregate_comparison(builder, op, elements);
            }
            (
                type_checker::Type::Array(lhs_inner, lhs_len),
                type_checker::Type::Array(rhs_inner, rhs_len),
            ) if lhs_len == rhs_len => {
                let Some(len) = lhs_len.as_usize() else {
                    return self.lower_comparison_leaf(builder, op, lhs, rhs, &lhs_ty, &rhs_ty);
                };
                let lhs_mir_ty = self.convert_type(lhs_inner);
                let rhs_mir_ty = self.convert_type(rhs_inner);
                let elements = (0..len)
                    .map(|index| {
                        let index_value = builder.iconst(index as u64, IntTy::Usize);
                        let lhs_ptr = builder.index_ptr(lhs, index_value, lhs_mir_ty.clone());
                        let rhs_ptr = builder.index_ptr(rhs, index_value, rhs_mir_ty.clone());
                        (
                            builder.load(lhs_ptr, lhs_mir_ty.clone()),
                            builder.load(rhs_ptr, rhs_mir_ty.clone()),
                            lhs_inner.as_ref().clone(),
                            rhs_inner.as_ref().clone(),
                        )
                    })
                    .collect();
                return self.lower_aggregate_comparison(builder, op, elements);
            }
            _ => {}
        }

        self.lower_comparison_leaf(builder, op, lhs, rhs, &lhs_ty, &rhs_ty)
    }

    pub(super) fn lower_aggregate_comparison(
        &mut self,
        builder: &mut Builder,
        op: HirBinOp,
        elements: Vec<(Value, Value, type_checker::Type, type_checker::Type)>,
    ) -> Value {
        match op {
            HirBinOp::Eq => self.lower_aggregate_equality(builder, elements, false),
            HirBinOp::Neq => self.lower_aggregate_equality(builder, elements, true),
            HirBinOp::Lt | HirBinOp::Gt | HirBinOp::LtEq | HirBinOp::GtEq => {
                self.lower_aggregate_ordering(builder, op, elements)
            }
            _ => unreachable!("aggregate comparison called with non-comparison op"),
        }
    }

    pub(super) fn lower_aggregate_equality(
        &mut self,
        builder: &mut Builder,
        elements: Vec<(Value, Value, type_checker::Type, type_checker::Type)>,
        negate: bool,
    ) -> Value {
        if elements.is_empty() {
            return builder.bconst(!negate);
        }

        let merge_block = builder.func.new_block_labeled("cmp_merge");
        let mut phi_args = Vec::with_capacity(elements.len() + 1);
        for (lhs, rhs, lhs_ty, rhs_ty) in elements {
            let equal = self.lower_comparison(builder, HirBinOp::Eq, lhs, rhs, &lhs_ty, &rhs_ty);
            let next_block = builder.func.new_block_labeled("cmp_next");
            let result_block = builder.func.new_block_labeled("cmp_result");
            builder.set_cond_branch(equal, next_block, result_block);

            builder.switch_to_block(result_block);
            let result = builder.bconst(negate);
            let result_exit = builder.current_block;
            builder.set_branch(merge_block);
            phi_args.push((result, result_exit));
            builder.switch_to_block(next_block);
        }

        let result = builder.bconst(!negate);
        let result_exit = builder.current_block;
        builder.set_branch(merge_block);
        phi_args.push((result, result_exit));

        builder.switch_to_block(merge_block);
        builder
            .func
            .push_inst(merge_block, Inst::new(InstKind::Phi(phi_args), Type::Bool))
    }

    pub(super) fn lower_aggregate_ordering(
        &mut self,
        builder: &mut Builder,
        op: HirBinOp,
        elements: Vec<(Value, Value, type_checker::Type, type_checker::Type)>,
    ) -> Value {
        if elements.is_empty() {
            return builder.bconst(matches!(op, HirBinOp::LtEq | HirBinOp::GtEq));
        }

        let merge_block = builder.func.new_block_labeled("cmp_merge");
        let mut phi_args = Vec::with_capacity(elements.len() + 1);
        for (lhs, rhs, lhs_ty, rhs_ty) in elements {
            let equal = self.lower_comparison(builder, HirBinOp::Eq, lhs, rhs, &lhs_ty, &rhs_ty);
            let next_block = builder.func.new_block_labeled("cmp_next");
            let result_block = builder.func.new_block_labeled("cmp_result");
            builder.set_cond_branch(equal, next_block, result_block);

            builder.switch_to_block(result_block);
            let decision_op = match op {
                HirBinOp::Lt | HirBinOp::LtEq => HirBinOp::Lt,
                HirBinOp::Gt | HirBinOp::GtEq => HirBinOp::Gt,
                _ => unreachable!("non-ordering op in aggregate ordering"),
            };
            let result = self.lower_comparison(builder, decision_op, lhs, rhs, &lhs_ty, &rhs_ty);
            let result_exit = builder.current_block;
            if builder.needs_return() {
                builder.set_branch(merge_block);
                phi_args.push((result, result_exit));
            }
            builder.switch_to_block(next_block);
        }

        let result = builder.bconst(matches!(op, HirBinOp::LtEq | HirBinOp::GtEq));
        let result_exit = builder.current_block;
        builder.set_branch(merge_block);
        phi_args.push((result, result_exit));

        builder.switch_to_block(merge_block);
        builder
            .func
            .push_inst(merge_block, Inst::new(InstKind::Phi(phi_args), Type::Bool))
    }

    pub(super) fn lower_comparison_leaf(
        &mut self,
        builder: &mut Builder,
        op: HirBinOp,
        lhs: Value,
        rhs: Value,
        lhs_ty: &type_checker::Type,
        rhs_ty: &type_checker::Type,
    ) -> Value {
        if matches!(
            (lhs_ty, rhs_ty),
            (type_checker::Type::Unit, type_checker::Type::Unit)
        ) {
            return builder.bconst(matches!(op, HirBinOp::Eq | HirBinOp::LtEq | HirBinOp::GtEq));
        }
        if builtin_comparison_types(op, lhs_ty, rhs_ty) {
            return builder.cmp(convert_cmp_op(op), lhs, rhs);
        }

        if let Some((lang, method)) = comparison_trait(op)
            && let Some(lang_item) = type_checker::lang_items::LangItem::from_name(lang)
            && let Some(trait_id) = self.type_result.trait_env.lang_items.get(lang_item)
            && let Some(fid) = self.find_trait_impl_method(trait_id, method, lhs_ty, Some(rhs_ty))
        {
            let Some(receiver_ty) = self.hir.item_tree.functions[fid]
                .params
                .first()
                .map(|param| param.ty.clone())
            else {
                return builder.cmp(convert_cmp_op(op), lhs, rhs);
            };
            let Some(rhs_param_ty) = self.hir.item_tree.functions[fid]
                .params
                .get(1)
                .map(|param| param.ty.clone())
            else {
                return builder.cmp(convert_cmp_op(op), lhs, rhs);
            };
            // Both parameters belong to the instance keyed by the receiver,
            // so the rhs parameter is substituted through `lhs_ty` too.
            let lhs_arg = self.lower_comparison_arg(
                builder,
                lhs,
                lhs_ty,
                &receiver_ty,
                self.mono_method_param_type(fid, 0, lhs_ty, Some(rhs_ty))
                    .as_ref(),
            );
            let rhs_arg = self.lower_comparison_arg(
                builder,
                rhs,
                rhs_ty,
                &rhs_param_ty,
                self.mono_method_param_type(fid, 1, lhs_ty, Some(rhs_ty))
                    .as_ref(),
            );
            let name = self
                .mono_method_name_for_receiver(fid, lhs_ty, Some(rhs_ty))
                .unwrap_or_else(|| self.function_name(fid));
            return builder.call(FuncRef::Local(name), vec![lhs_arg, rhs_arg], Type::Bool);
        }

        builder.cmp(convert_cmp_op(op), lhs, rhs)
    }

    pub(super) fn lower_comparison_arg(
        &self,
        builder: &mut Builder,
        value: Value,
        actual_ty: &type_checker::Type,
        expected: &hir::item_tree::HirTypeRef,
        expected_substituted: Option<&Type>,
    ) -> Value {
        let actual_mir_ty = self.convert_type(actual_ty);
        // An impl whose self type is itself a reference (`impl PartialEq for
        // &T`) takes `&&T` parameters: when the operand carries exactly the
        // inner reference, materialize the extra level. Every other shape
        // keeps the historical declared-type behavior below.
        if let Some(Type::Ref(inner, mutable)) = expected_substituted
            && matches!(**inner, Type::Ref(_, _))
            && **inner == actual_mir_ty
        {
            let place = builder.alloca(actual_mir_ty.clone());
            builder.store(value, place);
            return builder.unop(
                if *mutable { UnOp::MutRef } else { UnOp::Ref },
                place,
                Type::Ref(inner.clone(), *mutable),
            );
        }
        match expected {
            hir::item_tree::HirTypeRef::Ref(_, _) if matches!(actual_mir_ty, Type::Ref(_, _)) => {
                value
            }
            hir::item_tree::HirTypeRef::Ref(_, mutable) => {
                let place = builder.alloca(actual_mir_ty.clone());
                builder.store(value, place);
                builder.unop(
                    if *mutable { UnOp::MutRef } else { UnOp::Ref },
                    place,
                    Type::Ref(Box::new(actual_mir_ty), *mutable),
                )
            }
            _ => value,
        }
    }

    /// Adjusts a plain call argument for an impl whose self type is itself a
    /// reference (`impl PartialEq for &T` taking `&&T` parameters): when the
    /// substituted parameter is reference-of-reference and the argument
    /// carries exactly the inner reference, materialize the extra level.
    /// Every other argument passes through untouched, matching the
    /// historical behavior.
    pub(super) fn adjust_arg_for_reference_self_impl(
        &self,
        builder: &mut Builder,
        raw: Value,
        arg_ty: &type_checker::Type,
        expected: Option<&Type>,
    ) -> Value {
        let actual = self.convert_type(arg_ty);
        if let Some(Type::Ref(inner, mutable)) = expected
            && matches!(**inner, Type::Ref(_, _))
            && **inner == actual
        {
            let place = builder.alloca(actual.clone());
            builder.store(raw, place);
            return builder.unop(
                if *mutable { UnOp::MutRef } else { UnOp::Ref },
                place,
                Type::Ref(inner.clone(), *mutable),
            );
        }
        raw
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn lower_builtin_operator_method_call(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        owner: ExprId,
        base: ExprId,
        args: &[ExprId],
        op: BuiltinOperator,
    ) -> Value {
        let value_ty = self
            .current_body
            .and_then(|bid| self.type_result.expr_types.get(&(bid, base)))
            .map_or(Type::Unit, |ty| self.convert_type(ty));
        match op {
            BuiltinOperator::Binary(op) => {
                let expressions = [
                    base,
                    *args.first().expect("checked binary operator missing rhs"),
                ];
                let values =
                    self.lower_expr_sequence(builder, param_values, body, owner, 0, &expressions);
                let [lhs, rhs] = values.as_slice() else {
                    unreachable!();
                };
                builder.binop(op, *lhs, *rhs, value_ty)
            }
            BuiltinOperator::Unary(op) => {
                let operand = self.lower_expr(builder, param_values, body, base);
                builder.unop(op, operand, value_ty)
            }
            BuiltinOperator::Assign(op) => {
                let rhs = self.lower_expr(
                    builder,
                    param_values,
                    body,
                    *args
                        .first()
                        .expect("checked assignment operator missing rhs"),
                );
                let place = self.lower_lvalue(builder, param_values, body, base);
                let lhs = builder.load(place, value_ty.clone());
                let value = builder.binop(op, lhs, rhs, value_ty);
                builder.store(value, place);
                builder.unit_const()
            }
        }
    }

    pub(super) fn actual_method_fid(
        &self,
        callee: ExprId,
        fid: hir::item_tree::FunctionId,
        base: ExprId,
    ) -> hir::item_tree::FunctionId {
        let Some(body_id) = self.current_body else {
            return fid;
        };
        let Some(receiver_ty) = self.type_result.expr_types.get(&(body_id, base)) else {
            return fid;
        };
        if let Some(call) = self.type_result.trait_method_calls.get(&(body_id, callee)) {
            return self
                .find_trait_impl_method(call.trait_id, &call.method, receiver_ty, None)
                .unwrap_or(fid);
        }
        let Some(imp) = self.impl_for_method(fid) else {
            return fid;
        };
        if self.impl_type_matches(imp, receiver_ty) {
            return fid;
        }
        let Some(trait_ty) = &imp.trait_ty else {
            return fid;
        };
        let Some(trait_id) = self.resolve_trait_ref(trait_ty) else {
            return fid;
        };
        let method_name = &self.hir.item_tree.functions[fid].name;
        self.find_trait_impl_method(trait_id, &method_name.0, receiver_ty, None)
            .unwrap_or(fid)
    }

    pub(super) fn find_trait_impl_method(
        &self,
        trait_id: hir::item_tree::TraitId,
        method_name: &str,
        receiver_ty: &type_checker::Type,
        rhs_ty: Option<&type_checker::Type>,
    ) -> Option<hir::item_tree::FunctionId> {
        self.find_trait_impl_method_with_mir_args(trait_id, method_name, receiver_ty, None, rhs_ty)
    }

    pub(super) fn find_trait_impl_method_with_mir_args(
        &self,
        trait_id: hir::item_tree::TraitId,
        method_name: &str,
        receiver_ty: &type_checker::Type,
        trait_args: Option<&[Type]>,
        rhs_ty: Option<&type_checker::Type>,
    ) -> Option<hir::item_tree::FunctionId> {
        let receiver_ty = self.substitute_tc_type(receiver_ty);
        let opaque_hidden = match &receiver_ty {
            type_checker::Type::OpaqueTrait { id, .. } => self
                .type_result
                .opaque_hidden_types
                .get(id)
                .map(|hidden| self.substitute_tc_type(hidden)),
            _ => None,
        };
        let receiver_ty = opaque_hidden.as_ref().unwrap_or(&receiver_ty);
        let dereferenced = match &receiver_ty {
            type_checker::Type::Ref(inner, _) => Some(inner.as_ref()),
            _ => None,
        };
        for receiver_ty in std::iter::once(receiver_ty).chain(dereferenced) {
            for (_, candidate) in self.hir.item_tree.impls.iter() {
                let Some(candidate_trait) = candidate.trait_ty.as_ref() else {
                    continue;
                };
                if self.resolve_trait_ref(candidate_trait) != Some(trait_id)
                    || !self.impl_type_matches(candidate, receiver_ty)
                    || trait_args.is_some_and(|args| {
                        !self.impl_trait_args_match_mir(candidate, receiver_ty, args)
                    })
                    || !self.impl_trait_args_match(candidate, receiver_ty, rhs_ty)
                {
                    continue;
                }
                return candidate
                    .methods
                    .iter()
                    .copied()
                    .find(|candidate_fid| {
                        self.hir.item_tree.functions[*candidate_fid].name.0 == method_name
                    })
                    .or_else(|| self.default_method(trait_id, method_name));
            }
        }
        None
    }

    pub(super) fn default_method(
        &self,
        trait_id: hir::item_tree::TraitId,
        method_name: &str,
    ) -> Option<hir::item_tree::FunctionId> {
        self.hir.item_tree.traits[trait_id]
            .default_methods
            .iter()
            .copied()
            .find(|fid| self.hir.item_tree.functions[*fid].name.0 == method_name)
    }

    pub(super) fn lower_receiver_arg(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        base: ExprId,
        expected: &hir::item_tree::HirTypeRef,
        expected_substituted: Option<&Type>,
    ) -> Value {
        let base_ty = self
            .current_body
            .and_then(|bid| self.type_result.expr_types.get(&(bid, base)))
            .map_or(Type::Unit, |t| self.convert_expr_type(body, base, t));

        // An impl whose self type is itself a reference (`impl PartialEq for
        // &T`) takes a `&&T` receiver: when the operand carries exactly the
        // inner reference, materialize the extra level. Every other shape
        // keeps the historical declared-type behavior below.
        if let Some(Type::Ref(inner, mutable)) = expected_substituted
            && matches!(**inner, Type::Ref(_, _))
            && **inner == base_ty
        {
            if *mutable {
                let place = self.lower_lvalue(builder, param_values, body, base);
                return builder.unop(
                    convert_unop(HirUnOp::MutRef),
                    place,
                    Type::Ref(inner.clone(), *mutable),
                );
            }
            // Shared borrows of a value-typed operand (a parameter, say) need
            // their own slot: `Ref` of a placeless value would point at the
            // pointee instead of at storage holding the reference.
            let value = self.lower_expr(builder, param_values, body, base);
            let slot = builder.alloca(base_ty.clone());
            builder.store(value, slot);
            return builder.unop(
                convert_unop(HirUnOp::Ref),
                slot,
                Type::Ref(inner.clone(), *mutable),
            );
        }

        match expected {
            hir::item_tree::HirTypeRef::Ref(_, _) if matches!(base_ty, Type::Ref(_, _)) => {
                self.lower_expr(builder, param_values, body, base)
            }
            hir::item_tree::HirTypeRef::Ref(_, true) => {
                let place = self.lower_lvalue(builder, param_values, body, base);
                builder.unop(
                    convert_unop(HirUnOp::MutRef),
                    place,
                    Type::Ref(Box::new(base_ty), true),
                )
            }
            hir::item_tree::HirTypeRef::Ref(_, mutable) => {
                let base_val = self.lower_lvalue(builder, param_values, body, base);
                let expected_ty = Type::Ref(Box::new(base_ty), *mutable);
                builder.unop(convert_unop(HirUnOp::Ref), base_val, expected_ty)
            }
            _ => self.lower_expr(builder, param_values, body, base),
        }
    }

    pub(super) fn lower_trait_index_place(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        expr_id: ExprId,
        base: ExprId,
        index: ExprId,
    ) -> Option<Value> {
        let body_id = self.current_body?;
        let call = self
            .type_result
            .trait_method_calls
            .get(&(body_id, expr_id))?
            .clone();
        if call.method != "index" && call.method != "index_mut" {
            return None;
        }
        let base_ty = self
            .type_result
            .expr_types
            .get(&(body_id, base))
            .cloned()
            .map(|ty| self.substitute_tc_type(&ty))?;
        let receiver_ty = match &base_ty {
            type_checker::Type::Ref(inner, _) => inner.as_ref().clone(),
            _ => base_ty.clone(),
        };
        let index_ty = self
            .type_result
            .expr_types
            .get(&(body_id, index))
            .cloned()
            .map(|ty| self.substitute_tc_type(&ty))?;
        let fid = self.find_trait_impl_method(
            call.trait_id,
            &call.method,
            &receiver_ty,
            Some(&index_ty),
        )?;
        let function = &self.hir.item_tree.functions[fid];
        let receiver_param = function.params.first()?.ty.clone();
        let index_param = function.params.get(1)?.ty.clone();
        let name = self
            .mono_method_name_for_receiver(fid, &receiver_ty, Some(&index_ty))
            .unwrap_or_else(|| self.function_name(fid));
        let receiver =
            self.lower_receiver_arg(builder, param_values, body, base, &receiver_param, None);
        let index = self.lower_receiver_arg(builder, param_values, body, index, &index_param, None);
        let output = self
            .type_result
            .expr_types
            .get(&(body_id, expr_id))
            .cloned()
            .map(|ty| self.convert_type(&ty))?;
        let result = Type::Ref(Box::new(output), call.method == "index_mut");
        Some(builder.call(FuncRef::Local(name), vec![receiver, index], result))
    }
}
