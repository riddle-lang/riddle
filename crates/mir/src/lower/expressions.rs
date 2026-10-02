use super::{
    BinOp, Body, Builder, CapturePlace, CaptureSource, CmpOp, EnumVariantKind, Expr, ExprId,
    ExprLoweringInput, FnPtrType, ForExprInput, FuncRef, HashMap, HirBinOp, HirUnOp, Inst,
    InstKind, IntTy, LambdaExprInput, LambdaFunctionInput, LambdaInfo, LoopTargets, LowerCtx,
    OperatorCall, PatId, ResolvedName, Type, Value, closure_call_signature, closure_env_type,
    convert_binop, convert_unop, determine_cast_op, is_byte_str_layout_cast,
    is_dyn_object_safe_method, is_raw_parts_to_slice_cast, is_slice_to_raw_parts_cast,
    parse_float_suffix, parse_int_suffix,
};

impl LowerCtx<'_> {
    // 表达式降级
    pub(super) fn lower_expr(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        expr_id: ExprId,
    ) -> Value {
        // 命中缓存直接返回
        if let Some(&v) = self.expr_cache.get(&expr_id) {
            return v;
        }

        let expr = &body.exprs[expr_id];

        // 从类型检查结果中查表达式类型
        let tc_type = self
            .current_body
            .and_then(|bid| self.type_result.expr_types.get(&(bid, expr_id)));
        let mir_type = tc_type.map_or(Type::Unit, |t| self.convert_expr_type(body, expr_id, t));
        let diverges = matches!(mir_type, Type::Never);
        let input = ExprLoweringInput {
            param_values,
            body,
            expr_id,
            tc_type,
            mir_type: &mir_type,
            diverges,
        };

        let value = match expr {
            Expr::Missing => builder.unit_const(),

            Expr::IntLiteral { value, suffix } => {
                let ty = parse_int_suffix(suffix.as_deref());
                builder.iconst(*value, ty)
            }

            Expr::FloatLiteral { value, suffix } => {
                // HIR 中 value 已经是 f64，直接使用
                let ty = parse_float_suffix(suffix.as_deref());
                builder.fconst(*value, ty)
            }

            Expr::StringLiteral { value } => builder.sconst(value.clone()),

            Expr::CharLiteral { value } => builder.char_const(value.chars().next().unwrap_or('\0')),

            Expr::BoolLiteral { value } => builder.bconst(*value),

            Expr::Path { path, resolved } => {
                self.lower_path_expr(builder, &input, path, resolved.as_ref())
            }

            Expr::Binary { lhs, rhs, op } => {
                return self.lower_binary_expr(builder, &input, *lhs, *rhs, *op);
            }

            Expr::Unary { operand, op } => {
                return self.lower_unary_expr(builder, &input, *operand, *op);
            }

            Expr::Block { stmts, tail } => self.lower_block_expr(builder, &input, stmts, *tail),

            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => self.lower_if_expr(builder, &input, *cond, *then_branch, *else_branch),

            Expr::While {
                condition,
                body: while_body,
            } => self.lower_while_expr(builder, &input, *condition, *while_body),

            Expr::Loop { body: loop_body } => self.lower_loop_expr(builder, &input, *loop_body),

            Expr::For {
                pat,
                iterable,
                body: for_body,
            } => self.lower_for_expression(builder, &input, *pat, *iterable, *for_body),

            Expr::Match { scrutinee, arms } => self.lower_match_expr(
                builder,
                input.param_values,
                input.body,
                input.expr_id,
                *scrutinee,
                arms,
                self.coerced_expr_type(input.expr_id, input.mir_type),
            ),

            Expr::Array { elements } => self.lower_array_expr(builder, &input, elements),

            Expr::Tuple { elements } => self.lower_tuple_expr(builder, &input, elements),

            Expr::ArrayRepeat { value, .. } => {
                self.lower_array_repeat_expr(builder, &input, *value)
            }

            Expr::Struct {
                fields, resolved, ..
            } => self.lower_struct_expr(builder, &input, fields, resolved.as_ref()),

            Expr::Call { callee, args, .. } => {
                return self.lower_call_expr(builder, &input, *callee, args);
            }

            Expr::Lambda {
                params,
                body: lambda_body,
                ..
            } => self.lower_lambda_expr(builder, &input, params, *lambda_body),

            Expr::FieldAccess { base, field } => {
                self.lower_field_expr(builder, &input, *base, field)
            }

            Expr::IndexAccess { base, index } => {
                return self.lower_index_expr(builder, &input, *base, *index);
            }

            Expr::Unsafe { body: body_expr } => {
                self.lower_expr(builder, input.param_values, input.body, *body_expr)
            }

            Expr::Cast { base, target: _ } => self.lower_cast_expr(builder, &input, *base),

            Expr::Try { operand } => self.lower_try_expr(
                builder,
                input.param_values,
                input.body,
                input.expr_id,
                *operand,
                input.mir_type.clone(),
            ),
        };

        self.finish_expr(builder, &input, value)
    }

    fn finish_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        value: Value,
    ) -> Value {
        let value = self.apply_expr_coercion(builder, input.expr_id, value);
        if input.diverges && builder.needs_return() {
            builder.set_unreachable();
        }
        self.expr_cache.insert(input.expr_id, value);
        value
    }

    fn lower_path_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        path: &hir::item_tree::HirPath,
        resolved: Option<&ResolvedName>,
    ) -> Value {
        match resolved {
            Some(ResolvedName::Param(index)) => {
                let capture = CapturePlace::root(CaptureSource::Param(*index));
                let value = if let Some(access) = self.capture_access_for_place(builder, &capture) {
                    builder.load(access.place, access.ty)
                } else if let Some(storage) =
                    self.parameter_place(builder, &CaptureSource::Param(*index))
                {
                    builder.load(storage, input.mir_type.clone())
                } else {
                    input
                        .param_values
                        .get(*index)
                        .copied()
                        .unwrap_or_else(|| builder.unit_const())
                };
                self.clear_drop_flags_if_moved(builder, input.body, input.expr_id);
                value
            }
            Some(ResolvedName::LambdaParam { lambda, index }) => {
                let source = CaptureSource::LambdaParam {
                    lambda: *lambda,
                    index: *index,
                };
                let value = if self.current_lambda == Some(*lambda) {
                    self.parameter_place(builder, &source)
                        .map(|place| builder.load(place, input.mir_type.clone()))
                        .or_else(|| input.param_values.get(*index).copied())
                        .unwrap_or_else(|| builder.unit_const())
                } else if let Some(access) =
                    self.capture_access_for_place(builder, &CapturePlace::root(source.clone()))
                {
                    builder.load(access.place, access.ty)
                } else {
                    builder.unit_const()
                };
                self.clear_drop_flags_if_moved(builder, input.body, input.expr_id);
                value
            }
            Some(ResolvedName::PatternBinding(id)) => {
                let source = CaptureSource::Pattern(*id);
                let value = if let Some(access) =
                    self.capture_access_for_place(builder, &CapturePlace::root(source))
                {
                    builder.load(access.place, access.ty)
                } else {
                    self.binding_value(builder, *id, input.mir_type)
                        .unwrap_or_else(|| builder.unit_const())
                };
                self.clear_drop_flags_if_moved(builder, input.body, input.expr_id);
                value
            }
            Some(ResolvedName::Function(fid)) => {
                let args = match input.tc_type {
                    Some(type_checker::Type::FunctionItem { args, .. }) => args.clone(),
                    _ => Vec::new(),
                };
                self.lower_function_value(builder, *fid, &args, input.mir_type)
            }
            Some(ResolvedName::Const(id)) => self.lower_const_value(builder, *id),
            Some(ResolvedName::EnumVariant(enum_id, variant)) => self.lower_enum_variant_value(
                builder,
                *enum_id,
                *variant,
                Vec::new(),
                input.mir_type.clone(),
            ),
            _ => path
                .as_single_name()
                .and_then(|name| {
                    self.generic_const_subst
                        .get(&name.0)
                        .map(|value| builder.iconst(*value as u64, IntTy::Usize))
                })
                .unwrap_or_else(|| builder.unit_const()),
        }
    }

    fn lower_binary_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        lhs: ExprId,
        rhs: ExprId,
        op: HirBinOp,
    ) -> Value {
        if matches!(op, HirBinOp::And | HirBinOp::Or) {
            let value = self.lower_short_circuit_expr(
                builder,
                input.param_values,
                input.body,
                lhs,
                rhs,
                op,
            );
            let value = self.apply_expr_coercion(builder, input.expr_id, value);
            self.expr_cache.insert(input.expr_id, value);
            return value;
        }
        if let Some(call) = self
            .current_body
            .and_then(|body| self.type_result.operator_calls.get(&(body, input.expr_id)))
            .cloned()
        {
            return self.lower_binary_operator_call(builder, input, lhs, rhs, call);
        }
        if op.is_assignment() {
            return self.lower_assignment_expr(builder, input, lhs, rhs, op);
        }

        let values = self.lower_expr_sequence(
            builder,
            input.param_values,
            input.body,
            input.expr_id,
            0,
            &[lhs, rhs],
        );
        let [lhs_value, rhs_value] = values.as_slice() else {
            unreachable!();
        };
        let value = if matches!(
            op,
            HirBinOp::Eq
                | HirBinOp::Neq
                | HirBinOp::Lt
                | HirBinOp::Gt
                | HirBinOp::LtEq
                | HirBinOp::GtEq
        ) {
            let lhs_ty = self.expr_type(lhs);
            let rhs_ty = self.expr_type(rhs);
            self.lower_comparison(builder, op, *lhs_value, *rhs_value, &lhs_ty, &rhs_ty)
        } else {
            builder.binop(
                convert_binop(op),
                *lhs_value,
                *rhs_value,
                input.mir_type.clone(),
            )
        };
        self.finish_expr(builder, input, value)
    }

    fn expr_type(&self, expr: ExprId) -> type_checker::Type {
        self.current_body
            .and_then(|body| self.type_result.expr_types.get(&(body, expr)))
            .cloned()
            .unwrap_or(type_checker::Type::Error)
    }

    pub(super) fn convert_expr_type(
        &self,
        body: &Body,
        expr_id: ExprId,
        tc_type: &type_checker::Type,
    ) -> Type {
        self.opaque_hidden_for_expr(body, expr_id, tc_type)
            .map_or_else(
                || self.convert_type(tc_type),
                |hidden| self.convert_type(&hidden),
            )
    }

    pub(super) fn specialize_expr_type(
        &self,
        body: &Body,
        expr_id: ExprId,
        tc_type: &type_checker::Type,
    ) -> type_checker::Type {
        self.opaque_hidden_for_expr(body, expr_id, tc_type)
            .unwrap_or_else(|| tc_type.clone())
    }

    fn opaque_hidden_for_expr(
        &self,
        body: &Body,
        expr_id: ExprId,
        tc_type: &type_checker::Type,
    ) -> Option<type_checker::Type> {
        let type_checker::Type::OpaqueTrait { id, .. } = tc_type else {
            return None;
        };
        let body_id = self.current_body?;
        let callee = match &body.exprs[expr_id] {
            hir::body::Expr::Call { callee, .. } => *callee,
            hir::body::Expr::Path {
                resolved: Some(ResolvedName::PatternBinding(binding)),
                ..
            } => {
                let init = body.stmts.iter().find_map(|(_, stmt)| match stmt {
                    hir::body::Stmt::Let { pat, init, .. } if *pat == binding.pattern => *init,
                    _ => None,
                })?;
                return self.opaque_hidden_for_expr(body, init, tc_type);
            }
            hir::body::Expr::FieldAccess { base, .. } => {
                return self.opaque_hidden_for_expr(body, *base, tc_type);
            }
            _ => return None,
        };
        let generic_call = self.type_result.generic_calls.get(&(body_id, callee))?;
        let hir::body::Expr::Path {
            resolved: Some(ResolvedName::Function(fid)),
            ..
        } = &body.exprs[callee]
        else {
            return None;
        };
        let mut names = Vec::new();
        if let Some(impl_id) = self.method_impls.get(fid) {
            let imp = &self.hir.item_tree.impls[*impl_id];
            names.extend(imp.generics.iter().map(|name| name.0.clone()));
            names.extend(imp.const_generics.iter().map(|name| name.0.clone()));
        }
        let function = &self.hir.item_tree.functions[*fid];
        names.extend(function.generics.iter().map(|name| name.0.clone()));
        names.extend(function.implicit_generics.iter().map(|name| name.0.clone()));
        names.extend(function.const_generics.iter().map(|name| name.0.clone()));
        let subst = names
            .into_iter()
            .zip(generic_call.args.iter().cloned())
            .collect::<HashMap<_, _>>();
        let hidden = self.type_result.opaque_hidden_types.get(id)?;
        Some(type_checker::substitute_type(hidden, &subst))
    }

    fn coerced_expr_type(&self, expr_id: ExprId, fallback: &Type) -> Type {
        self.current_body
            .and_then(|body_id| self.type_result.expr_coercions.get(&(body_id, expr_id)))
            .map_or_else(|| fallback.clone(), |target| self.convert_type(target))
    }

    fn lower_binary_operator_call(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        lhs: ExprId,
        rhs: ExprId,
        call: OperatorCall,
    ) -> Value {
        let function = match call {
            OperatorCall::Function(function) => Some(function),
            OperatorCall::Trait(call) => {
                let lhs_ty = self
                    .current_body
                    .and_then(|body| self.type_result.expr_types.get(&(body, lhs)));
                let rhs_ty = self
                    .current_body
                    .and_then(|body| self.type_result.expr_types.get(&(body, rhs)));
                lhs_ty.and_then(|lhs_ty| {
                    self.find_trait_impl_method(call.trait_id, &call.method, lhs_ty, rhs_ty)
                })
            }
        };
        let Some(function) = function else {
            return builder.unit_const();
        };
        if let Some(op) = self.builtin_operator_for_method(function) {
            return self.lower_builtin_operator_method_call(
                builder,
                input.param_values,
                input.body,
                input.expr_id,
                lhs,
                &[rhs],
                op,
            );
        }
        let params = &self.hir.item_tree.functions[function].params;
        let receiver_ty = params.first().map(|param| param.ty.clone());
        let rhs_ty = params.get(1).map(|param| param.ty.clone());
        let lhs_tc_ty = self
            .current_body
            .and_then(|body| self.type_result.expr_types.get(&(body, lhs)));
        let rhs_tc_ty = self
            .current_body
            .and_then(|body| self.type_result.expr_types.get(&(body, rhs)));
        let lhs_value = if let Some(receiver_ty) = &receiver_ty {
            self.lower_receiver_arg(
                builder,
                input.param_values,
                input.body,
                lhs,
                receiver_ty,
                lhs_tc_ty
                    .and_then(|ty| self.mono_method_param_type(function, 0, ty, rhs_tc_ty))
                    .as_ref(),
            )
        } else {
            self.lower_expr(builder, input.param_values, input.body, lhs)
        };
        let rhs_value = if let Some(rhs_ty) = rhs_ty {
            self.lower_receiver_arg(
                builder,
                input.param_values,
                input.body,
                rhs,
                &rhs_ty,
                lhs_tc_ty
                    .and_then(|ty| self.mono_method_param_type(function, 1, ty, rhs_tc_ty))
                    .as_ref(),
            )
        } else {
            self.lower_expr(builder, input.param_values, input.body, rhs)
        };
        self.lower_operator_call(
            builder,
            lhs,
            Some(rhs),
            function,
            vec![lhs_value, rhs_value],
            input.mir_type.clone(),
        )
    }

    fn lower_assignment_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        lhs: ExprId,
        rhs: ExprId,
        op: HirBinOp,
    ) -> Value {
        let rhs_value = self.lower_expr(builder, input.param_values, input.body, rhs);
        let lhs_place = self.lower_lvalue(builder, input.param_values, input.body, lhs);
        if op != HirBinOp::Assign {
            let base_op = op.compound_base().unwrap();
            let value_ty = self
                .current_body
                .and_then(|body| self.type_result.expr_types.get(&(body, lhs)))
                .map_or_else(|| input.mir_type.clone(), |ty| self.convert_type(ty));
            let current = builder.load(lhs_place, value_ty.clone());
            let updated = builder.binop(convert_binop(base_op), current, rhs_value, value_ty);
            builder.store(updated, lhs_place);
            return builder.unit_const();
        }

        let lhs_ty = self
            .current_body
            .and_then(|body| self.type_result.expr_types.get(&(body, lhs)))
            .cloned();
        let assignment_slots = self
            .drop_place_from_expr(input.body, lhs)
            .and_then(|(source, projection)| {
                self.drop_slots.get(&source).map(|slots| {
                    slots
                        .iter()
                        .filter(|slot| {
                            projection.is_empty()
                                || slot.projection.starts_with(projection.as_slice())
                        })
                        .cloned()
                        .collect::<Vec<_>>()
                })
            })
            .unwrap_or_default();
        let raw_pointer_index = matches!(
            &input.body.exprs[lhs],
            Expr::IndexAccess { base, .. }
                if self
                    .current_body
                    .and_then(|body| self.type_result.expr_types.get(&(body, *base)))
                    .is_some_and(|ty| matches!(ty, type_checker::Type::Ptr { .. }))
        );
        if !raw_pointer_index
            && let Some(ty) = lhs_ty.as_ref()
            && self.type_needs_drop(ty, 0)
        {
            if assignment_slots.is_empty() {
                self.emit_drop_glue(builder, lhs_place, ty);
            } else {
                for slot in &assignment_slots {
                    self.emit_drop_slot(builder, slot);
                }
            }
        }
        builder.store(rhs_value, lhs_place);
        for slot in &assignment_slots {
            let active = builder.bconst(true);
            let flag = Self::drop_slot_flag_place(builder, slot);
            builder.store(active, flag);
        }
        builder.unit_const()
    }

    fn lower_unary_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        operand: ExprId,
        op: HirUnOp,
    ) -> Value {
        if let Some(call) = self
            .current_body
            .and_then(|body| self.type_result.operator_calls.get(&(body, input.expr_id)))
            .cloned()
        {
            let function = match call {
                OperatorCall::Function(function) => Some(function),
                OperatorCall::Trait(call) => self
                    .current_body
                    .and_then(|body| self.type_result.expr_types.get(&(body, operand)))
                    .and_then(|operand_ty| {
                        self.find_trait_impl_method(call.trait_id, &call.method, operand_ty, None)
                    }),
            };
            let Some(function) = function else {
                return builder.unit_const();
            };
            if let Some(op) = self.builtin_operator_for_method(function) {
                return self.lower_builtin_operator_method_call(
                    builder,
                    input.param_values,
                    input.body,
                    input.expr_id,
                    operand,
                    &[],
                    op,
                );
            }
            let receiver_ty = self.hir.item_tree.functions[function]
                .params
                .first()
                .map(|param| param.ty.clone());
            let operand_tc_ty = self
                .current_body
                .and_then(|body| self.type_result.expr_types.get(&(body, operand)));
            let value = if let Some(receiver_ty) = receiver_ty {
                self.lower_receiver_arg(
                    builder,
                    input.param_values,
                    input.body,
                    operand,
                    &receiver_ty,
                    operand_tc_ty
                        .and_then(|ty| self.mono_method_param_type(function, 0, ty, None))
                        .as_ref(),
                )
            } else {
                self.lower_expr(builder, input.param_values, input.body, operand)
            };
            return self.lower_operator_call(
                builder,
                operand,
                None,
                function,
                vec![value],
                input.mir_type.clone(),
            );
        }
        if matches!(op, HirUnOp::Neg)
            && let Expr::IntLiteral { value, .. } = &input.body.exprs[operand]
            && let Type::Int(ty) = input.mir_type
        {
            return builder.negative_iconst(*value, *ty);
        }
        if matches!(op, HirUnOp::Ref | HirUnOp::MutRef)
            && let Some(type_checker::Type::Ref(inner, _)) = input.tc_type
            && let type_checker::Type::DynTrait {
                trait_id,
                args,
                assoc_bindings,
            } = inner.as_ref()
            && let Some(value) = self.lower_dyn_trait_ref(
                builder,
                input.param_values,
                input.body,
                operand,
                *trait_id,
                args.iter()
                    .map(|arg| self.convert_type(arg))
                    .collect::<Vec<_>>(),
                self.convert_dyn_assoc_bindings(assoc_bindings),
                input.mir_type,
            )
        {
            return self.finish_expr(builder, input, value);
        }
        let operand_value = if matches!(op, HirUnOp::Ref | HirUnOp::MutRef) {
            self.lower_lvalue(builder, input.param_values, input.body, operand)
        } else {
            self.lower_expr(builder, input.param_values, input.body, operand)
        };
        if matches!(op, HirUnOp::Pos) {
            return operand_value;
        }
        let value = builder.unop(convert_unop(op), operand_value, input.mir_type.clone());
        self.finish_expr(builder, input, value)
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_dyn_trait_ref(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        operand: ExprId,
        trait_id: hir::item_tree::TraitId,
        trait_args: Vec<Type>,
        trait_assoc: Vec<(String, Type)>,
        object_ty: &Type,
    ) -> Option<Value> {
        let Type::Struct(_) = object_ty else {
            return None;
        };
        let concrete_ty = self.expr_type(operand);
        if matches!(
            concrete_ty,
            type_checker::Type::DynTrait { .. } | type_checker::Type::OwnedDynTrait { .. }
        ) {
            let object = self.lower_expr(builder, param_values, body, operand);
            return self.lower_dyn_trait_from_object(
                builder,
                object,
                &concrete_ty,
                trait_id,
                &trait_args,
                &trait_assoc,
                object_ty,
            );
        }
        let place = self.lower_lvalue(builder, param_values, body, operand);
        let data = builder.cast(
            crate::instr::CastOp::PtrToPtr,
            place,
            Type::Ptr(Box::new(Type::Unit)),
        );
        self.lower_dyn_trait_from_data(
            builder,
            data,
            &concrete_ty,
            trait_id,
            &trait_args,
            &trait_assoc,
            object_ty,
            false,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_dyn_trait_from_data(
        &mut self,
        builder: &mut Builder,
        data: Value,
        concrete_ty: &type_checker::Type,
        trait_id: hir::item_tree::TraitId,
        trait_args: &[Type],
        trait_assoc: &[(String, Type)],
        object_ty: &Type,
        owned: bool,
    ) -> Option<Value> {
        let Type::Struct(struct_ty) = object_ty else {
            return None;
        };
        let mut trait_subst = self.hir.item_tree.traits[trait_id]
            .generics
            .iter()
            .map(|name| name.0.clone())
            .zip(trait_args.iter().cloned())
            .collect::<HashMap<_, _>>();
        for (name, ty) in trait_assoc {
            trait_subst.insert(format!("Self::{name}"), ty.clone());
        }
        let methods = self.dyn_trait_methods(trait_id, &trait_subst);
        let mut values = vec![data];
        for (field_name, field_ty) in struct_ty.def().fields.iter().skip(1) {
            let Some((method_trait_id, method_name, method_subst)) = methods
                .iter()
                .find(|(method_trait_id, method, _)| {
                    is_dyn_object_safe_method(method)
                        && self.dyn_trait_method_field_name(
                            &methods,
                            *method_trait_id,
                            &method.name.0,
                        ) == *field_name
                })
                .map(|(trait_id, method, subst)| (*trait_id, method.name.0.clone(), subst.clone()))
            else {
                continue;
            };
            let method_args = self.hir.item_tree.traits[method_trait_id]
                .generics
                .iter()
                .map(|name| method_subst.get(&name.0).cloned().unwrap_or(Type::Unit))
                .collect::<Vec<_>>();
            let fid = self.find_trait_impl_method_with_mir_args(
                method_trait_id,
                &method_name,
                concrete_ty,
                Some(&method_args),
                None,
            )?;
            let Type::FnPtr(signature) = field_ty else {
                return None;
            };
            let adapter = self.ensure_dyn_trait_adapter(trait_id, fid, concrete_ty, signature);
            values.push(
                builder.function_ref(FuncRef::Local(adapter), Type::FnPtr(signature.clone())),
            );
        }
        if owned {
            let drop_name = self.ensure_dyn_trait_drop_adapter(concrete_ty);
            values.push(builder.function_ref(
                FuncRef::Local(drop_name),
                super::types::dyn_trait_drop_function_type(),
            ));
        }
        let value = builder.struct_value(values, object_ty.clone());
        self.coerced_values.insert((value, object_ty.clone()));
        Some(value)
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_dyn_trait_from_object(
        &mut self,
        builder: &mut Builder,
        source: Value,
        source_tc_ty: &type_checker::Type,
        target_trait_id: hir::item_tree::TraitId,
        target_args: &[Type],
        target_assoc: &[(String, Type)],
        target_object_ty: &Type,
    ) -> Option<Value> {
        let (source_trait_id, source_args, source_assoc, source_owned) = match source_tc_ty {
            type_checker::Type::DynTrait {
                trait_id,
                args,
                assoc_bindings,
            } => (*trait_id, args, assoc_bindings, false),
            type_checker::Type::OwnedDynTrait {
                trait_id,
                args,
                assoc_bindings,
            } => (*trait_id, args, assoc_bindings, true),
            _ => return None,
        };
        let Type::Struct(source_struct) = self.convert_type(source_tc_ty) else {
            return None;
        };
        let Type::Struct(target_struct) = target_object_ty else {
            return None;
        };
        let source_mir_args = source_args
            .iter()
            .map(|arg| self.convert_type(arg))
            .collect::<Vec<_>>();
        let mut source_subst = self.hir.item_tree.traits[source_trait_id]
            .generics
            .iter()
            .map(|name| name.0.clone())
            .zip(source_mir_args)
            .collect::<HashMap<_, _>>();
        for (name, ty) in self.convert_dyn_assoc_bindings(source_assoc) {
            source_subst.insert(format!("Self::{name}"), ty);
        }
        let mut target_subst = self.hir.item_tree.traits[target_trait_id]
            .generics
            .iter()
            .map(|name| name.0.clone())
            .zip(target_args.iter().cloned())
            .collect::<HashMap<_, _>>();
        for (name, ty) in target_assoc {
            target_subst.insert(format!("Self::{name}"), ty.clone());
        }
        let source_methods = self.dyn_trait_methods(source_trait_id, &source_subst);
        let target_methods = self.dyn_trait_methods(target_trait_id, &target_subst);
        let mut values = vec![builder.extract_value(source, 0, Type::Ptr(Box::new(Type::Unit)))];
        let target_def = target_struct.def();
        let source_def = source_struct.def();
        for (field_name, field_ty) in target_def.fields.iter().skip(1) {
            if field_name == "drop" {
                if !source_owned {
                    return None;
                }
                let index = source_def.fields.len().checked_sub(1)?;
                values.push(builder.extract_value(source, index, field_ty.clone()));
                continue;
            }
            let (method_trait_id, method_name) = target_methods
                .iter()
                .find(|(method_trait_id, method, _)| {
                    is_dyn_object_safe_method(method)
                        && self.dyn_trait_method_field_name(
                            &target_methods,
                            *method_trait_id,
                            &method.name.0,
                        ) == *field_name
                })
                .map(|(trait_id, method, _)| (*trait_id, method.name.0.clone()))?;
            let source_field_name =
                self.dyn_trait_method_field_name(&source_methods, method_trait_id, &method_name);
            let source_index = source_def
                .fields
                .iter()
                .position(|(name, _)| name == &source_field_name)?;
            values.push(builder.extract_value(source, source_index, field_ty.clone()));
        }
        let value = builder.struct_value(values, target_object_ty.clone());
        self.coerced_values
            .insert((value, target_object_ty.clone()));
        Some(value)
    }

    fn lower_block_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        statements: &[hir::body::StmtId],
        tail: Option<ExprId>,
    ) -> Value {
        self.drop_scopes.push(Vec::new());
        for &statement in statements {
            self.lower_stmt(builder, input.param_values, input.body, statement);
            if !builder.needs_return() {
                break;
            }
        }
        let result = if builder.needs_return() {
            match tail {
                Some(tail) => self.lower_expr(builder, input.param_values, input.body, tail),
                None => builder.unit_const(),
            }
        } else {
            builder.unit_const()
        };
        if builder.needs_return() {
            self.emit_current_drop_scope(builder);
        }
        self.drop_scopes.pop();
        result
    }

    fn lower_if_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        condition: ExprId,
        then_expr: ExprId,
        else_expr: Option<ExprId>,
    ) -> Value {
        self.temporary_drop_scopes.push(Vec::new());
        let condition = self.lower_expr(builder, input.param_values, input.body, condition);
        if builder.needs_return() {
            self.emit_current_temporary_drop_scope(builder);
        }
        self.temporary_drop_scopes.pop();
        let then_block = builder.func.new_block_labeled("then");
        let else_block = builder.func.new_block_labeled("else");
        let merge_block = builder.func.new_block_labeled("merge");
        builder.set_cond_branch(condition, then_block, else_block);

        builder.switch_to_block(then_block);
        self.temporary_drop_scopes.push(Vec::new());
        let then_value = self.lower_expr(builder, input.param_values, input.body, then_expr);
        if builder.needs_return() {
            self.emit_current_temporary_drop_scope(builder);
        }
        self.temporary_drop_scopes.pop();
        let then_exit = builder.current_block;
        let mut phi_args = Vec::new();
        if builder.needs_return() {
            builder.set_branch(merge_block);
            phi_args.push((then_value, then_exit));
        }

        builder.switch_to_block(else_block);
        self.temporary_drop_scopes.push(Vec::new());
        let else_value = match else_expr {
            Some(expr) => self.lower_expr(builder, input.param_values, input.body, expr),
            None => builder.unit_const(),
        };
        if builder.needs_return() {
            self.emit_current_temporary_drop_scope(builder);
        }
        self.temporary_drop_scopes.pop();
        let else_exit = builder.current_block;
        if builder.needs_return() {
            builder.set_branch(merge_block);
            phi_args.push((else_value, else_exit));
        }

        builder.switch_to_block(merge_block);
        if phi_args.is_empty() {
            builder.unit_const()
        } else {
            let result_ty = self.coerced_expr_type(input.expr_id, input.mir_type);
            let phi = builder.func.push_inst(
                merge_block,
                Inst::new(InstKind::Phi(phi_args), result_ty.clone()),
            );
            if self.current_body.is_some_and(|body_id| {
                self.type_result
                    .expr_coercions
                    .contains_key(&(body_id, input.expr_id))
            }) {
                self.coerced_values.insert((phi, result_ty));
            }
            phi
        }
    }

    fn lower_while_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        condition: ExprId,
        body: ExprId,
    ) -> Value {
        let cond_block = builder.func.new_block_labeled("while_cond");
        let body_block = builder.func.new_block_labeled("while_body");
        let exit_block = builder.func.new_block_labeled("while_exit");
        builder.set_branch(cond_block);

        builder.switch_to_block(cond_block);
        self.temporary_drop_scopes.push(Vec::new());
        let condition = self.lower_expr(builder, input.param_values, input.body, condition);
        if builder.needs_return() {
            self.emit_current_temporary_drop_scope(builder);
        }
        self.temporary_drop_scopes.pop();
        builder.set_cond_branch(condition, body_block, exit_block);

        builder.switch_to_block(body_block);
        self.loop_targets.push(LoopTargets {
            break_block: exit_block,
            continue_block: cond_block,
            break_slot: None,
            drop_depth: self.drop_scopes.len(),
            temporary_drop_depth: self.temporary_drop_scopes.len(),
        });
        self.lower_expr(builder, input.param_values, input.body, body);
        self.loop_targets.pop();
        if builder.needs_return() {
            builder.set_branch(cond_block);
        }

        builder.switch_to_block(exit_block);
        builder.unit_const()
    }

    fn lower_loop_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        body: ExprId,
    ) -> Value {
        let result_ty = self.coerced_expr_type(input.expr_id, input.mir_type);
        let body_block = builder.func.new_block_labeled("loop_body");
        let exit_block = builder.func.new_block_labeled("loop_exit");

        // break 值经 alloca 槽传出：break 可在任意嵌套深度、数量不定，
        // 且跳转前穿插 drop 代码，不适合 phi。
        let break_slot = if matches!(&result_ty, Type::Unit | Type::Never) {
            None
        } else {
            Some(builder.alloca(result_ty.clone()))
        };

        builder.set_branch(body_block);

        builder.switch_to_block(body_block);
        self.loop_targets.push(LoopTargets {
            break_block: exit_block,
            continue_block: body_block,
            break_slot,
            drop_depth: self.drop_scopes.len(),
            temporary_drop_depth: self.temporary_drop_scopes.len(),
        });
        self.lower_expr(builder, input.param_values, input.body, body);
        self.loop_targets.pop();
        if builder.needs_return() {
            builder.set_branch(body_block);
        }

        builder.switch_to_block(exit_block);
        match break_slot {
            Some(slot) => {
                let value = builder.load(slot, result_ty.clone());
                if self.current_body.is_some_and(|body_id| {
                    self.type_result
                        .expr_coercions
                        .contains_key(&(body_id, input.expr_id))
                }) {
                    self.coerced_values.insert((value, result_ty));
                }
                value
            }
            None => builder.unit_const(),
        }
    }

    fn lower_for_expression(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        pat: PatId,
        iterable: ExprId,
        body: ExprId,
    ) -> Value {
        self.lower_for_expr(
            builder,
            ForExprInput {
                param_values: input.param_values,
                body: input.body,
                expr_id: input.expr_id,
                pat,
                iterable,
                loop_body: body,
            },
        )
    }

    fn lower_array_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        elements: &[ExprId],
    ) -> Value {
        let values = self.lower_expr_sequence(
            builder,
            input.param_values,
            input.body,
            input.expr_id,
            0,
            elements,
        );
        builder.array_value(values, input.mir_type.clone())
    }

    fn lower_tuple_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        elements: &[ExprId],
    ) -> Value {
        let values = self.lower_expr_sequence(
            builder,
            input.param_values,
            input.body,
            input.expr_id,
            0,
            elements,
        );
        builder.tuple_value(values, input.mir_type.clone())
    }

    fn lower_array_repeat_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        element: ExprId,
    ) -> Value {
        let len = match input.tc_type {
            Some(type_checker::Type::Array(_, len)) => len.as_usize().unwrap_or(0),
            _ => 0,
        };
        let value = self.lower_expr(builder, input.param_values, input.body, element);
        builder.array_value(vec![value; len], input.mir_type.clone())
    }

    fn lower_struct_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        fields: &[hir::body::StructExprField],
        resolved: Option<&ResolvedName>,
    ) -> Value {
        if let Some(ResolvedName::EnumVariant(enum_id, variant)) = resolved {
            let expressions = match &self.hir.item_tree.enums[*enum_id].variants[*variant].kind {
                hir::item_tree::HirVariantKind::Struct(expected_fields) => expected_fields
                    .iter()
                    .filter_map(|expected| {
                        fields
                            .iter()
                            .find(|field| field.name == expected.name)
                            .map(|field| field.value)
                    })
                    .collect(),
                _ => Vec::new(),
            };
            let values = self.lower_expr_sequence(
                builder,
                input.param_values,
                input.body,
                input.expr_id,
                0,
                &expressions,
            );
            self.lower_enum_variant_value(
                builder,
                *enum_id,
                *variant,
                values,
                input.mir_type.clone(),
            )
        } else {
            let expressions = fields.iter().map(|field| field.value).collect::<Vec<_>>();
            let values = self.lower_expr_sequence(
                builder,
                input.param_values,
                input.body,
                input.expr_id,
                0,
                &expressions,
            );
            builder.struct_value(values, input.mir_type.clone())
        }
    }

    fn lower_call_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        callee: ExprId,
        args: &[ExprId],
    ) -> Value {
        if let Some(value) = self.lower_recursive_lambda_call(builder, input, callee, args) {
            return self.finish_expr(builder, input, value);
        }
        if let Some(value) = self.lower_generic_lambda_call(builder, input, callee, args) {
            return self.finish_expr(builder, input, value);
        }
        if let Some(value) = self.lower_callable_impl_call(builder, input, callee, args) {
            return self.finish_expr(builder, input, value);
        }
        if let Some(value) = self.lower_static_trait_call(
            builder,
            input.param_values,
            input.body,
            (input.expr_id, callee, args, input.mir_type.clone()),
        ) {
            return self.finish_expr(builder, input, value);
        }
        if let Expr::Path {
            resolved: Some(ResolvedName::EnumVariant(enum_id, variant)),
            ..
        } = &input.body.exprs[callee]
        {
            let values = self.lower_expr_sequence(
                builder,
                input.param_values,
                input.body,
                input.expr_id,
                1,
                args,
            );
            let value = self.lower_enum_variant_value(
                builder,
                *enum_id,
                *variant,
                values,
                input.mir_type.clone(),
            );
            return self.finish_expr(builder, input, value);
        }
        if let Some(value) = self.lower_builtin_call(
            builder,
            input.param_values,
            input.body,
            callee,
            args,
            input.mir_type.clone(),
        ) {
            return self.finish_expr(builder, input, value);
        }
        if let Some(value) = self.lower_bound_trait_call(builder, input, callee, args) {
            return self.finish_expr(builder, input, value);
        }
        let Some(target) = self.callee_function_id(callee) else {
            let value = self.lower_indirect_call_expr(builder, input, callee, args);
            return self.finish_expr(builder, input, value);
        };
        self.lower_named_call_expr(builder, input, callee, args, target)
    }

    fn lower_recursive_lambda_call(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        callee: ExprId,
        args: &[ExprId],
    ) -> Option<Value> {
        let body_id = self.current_body?;
        let type_checker::Type::Closure { id, generics, .. } =
            self.type_result.expr_types.get(&(body_id, callee))?
        else {
            return None;
        };
        if !generics.is_empty() || self.current_lambda != Some(id.expr) || body_id != id.body {
            return None;
        }
        let name = self.lambda_functions.get(&(id.body, id.expr))?.clone();
        let mut values = Vec::with_capacity(args.len() + 1);
        values.push(self.current_lambda_env?);
        values.extend(self.lower_expr_sequence(
            builder,
            input.param_values,
            input.body,
            input.expr_id,
            1,
            args,
        ));
        Some(builder.call(FuncRef::Local(name), values, input.mir_type.clone()))
    }

    fn lower_generic_lambda_call(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        callee: ExprId,
        args: &[ExprId],
    ) -> Option<Value> {
        let caller_body = self.current_body?;
        let type_checker::Type::Closure {
            id,
            generics,
            signature,
        } = self.type_result.expr_types.get(&(caller_body, callee))?
        else {
            return None;
        };
        if generics.is_empty() {
            return None;
        }
        let concrete = &self
            .type_result
            .generic_calls
            .get(&(caller_body, callee))?
            .args;
        let info = self
            .type_result
            .lambda_infos
            .get(&(id.body, id.expr))
            .cloned()
            .unwrap_or(LambdaInfo {
                captures: Vec::new(),
                kind: signature.kind,
            });
        let tc_subst = generics
            .iter()
            .cloned()
            .zip(concrete.iter().cloned())
            .collect::<HashMap<_, _>>();
        let instantiated = type_checker::CallableSignature {
            is_unsafe: signature.is_unsafe,
            kind: signature.kind,
            params: signature
                .params
                .iter()
                .map(|param| type_checker::substitute_type(param, &tc_subst))
                .collect(),
            ret: Box::new(type_checker::substitute_type(&signature.ret, &tc_subst)),
        };
        let (capture_types, _) = self.lambda_environment_type("generic_lambda", &info);
        let suffix = concrete
            .iter()
            .map(|ty| super::mono_type_name(&self.convert_type(ty)))
            .chain(capture_types.iter().map(super::mono_type_name))
            .collect::<Vec<_>>()
            .join("_");
        let key = (id.body, id.expr, suffix.clone());
        let recursive = self.current_body == Some(id.body)
            && self.current_lambda == Some(id.expr)
            && self.current_lambda_env.is_some();
        let name = if let Some(name) = self.generic_lambda_functions.get(&key) {
            name.clone()
        } else {
            let offset = self.hir.bodies[id.body]
                .source_map
                .expr_ranges
                .get(&id.expr)
                .map_or(0, |range| u32::from(range.start()));
            let name = self.qualify_current_symbol(format!(
                "__riddle_lambda_{offset}_{}",
                if suffix.is_empty() { "unit" } else { &suffix }
            ));
            self.generic_lambda_functions.insert(key, name.clone());
            let (params, lambda_body) = match &self.hir.bodies[id.body].exprs[id.expr] {
                Expr::Lambda { params, body, .. } => (params.clone(), *body),
                _ => return None,
            };
            let mut call_params = Vec::with_capacity(instantiated.params.len() + 1);
            call_params.push(closure_env_type());
            call_params.extend(instantiated.params.iter().map(|ty| self.convert_type(ty)));
            let call_signature = FnPtrType {
                params: call_params,
                ret: Box::new(self.convert_type(&instantiated.ret)),
            };
            let mut merged_tc_subst = self.generic_tc_subst.clone();
            merged_tc_subst.extend(tc_subst.clone());
            let mut mir_subst: HashMap<String, Type> = tc_subst
                .iter()
                .map(|(name, ty)| (name.clone(), self.convert_type(ty)))
                .collect();
            mir_subst.extend(self.generic_subst.clone());
            let old_tc_subst = std::mem::replace(&mut self.generic_tc_subst, merged_tc_subst);
            let old_subst = std::mem::replace(&mut self.generic_subst, mir_subst);
            let (_, env_struct) = self.lambda_environment_type(&name, &info);
            self.lower_lambda_function(&LambdaFunctionInput {
                body_id: id.body,
                expr_id: id.expr,
                params: &params,
                body: lambda_body,
                name: &name,
                call_signature: &call_signature,
                info: &info,
                capture_types: &capture_types,
                env_struct: &env_struct,
            });
            self.lower_lambda_drop_function(
                &format!("{name}_drop"),
                &info,
                &capture_types,
                &env_struct,
                !info.captures.is_empty() && self.analysis.lambda_escapes(id.body, id.expr),
            );
            self.generic_subst = old_subst;
            self.generic_tc_subst = old_tc_subst;
            name
        };
        let (_, env_struct) = self.lambda_environment_type(&name, &info);
        let mut values = Vec::with_capacity(args.len() + 1);
        let heap_env = !info.captures.is_empty() && self.analysis.lambda_escapes(id.body, id.expr);
        let env = if recursive {
            self.current_lambda_env?
        } else {
            self.lower_lambda_environment(
                builder,
                input.param_values,
                &info,
                &capture_types,
                &env_struct,
                heap_env,
            )
        };
        values.push(env);
        values.extend(self.lower_expr_sequence(
            builder,
            input.param_values,
            input.body,
            input.expr_id,
            1,
            args,
        ));
        Some(builder.call(FuncRef::Local(name), values, input.mir_type.clone()))
    }

    fn lower_callable_impl_call(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        callee: ExprId,
        args: &[ExprId],
    ) -> Option<Value> {
        let body_id = self.current_body?;
        let fid = *self
            .type_result
            .callable_impl_calls
            .get(&(body_id, input.expr_id))?;
        let receiver_ty = self.type_result.expr_types.get(&(body_id, callee))?;
        let receiver_param = self.hir.item_tree.functions[fid].params.first()?.ty.clone();
        let name = self
            .mono_method_name_for_receiver(fid, receiver_ty, None)
            .unwrap_or_else(|| self.function_name(fid));
        let receiver = self.lower_receiver_arg(
            builder,
            input.param_values,
            input.body,
            callee,
            &receiver_param,
            None,
        );
        let args = self.lower_expr_sequence(
            builder,
            input.param_values,
            input.body,
            input.expr_id,
            1,
            args,
        );
        let mut values = Vec::with_capacity(args.len() + 1);
        values.push(receiver);
        values.extend(args);
        Some(builder.call(FuncRef::Local(name), values, input.mir_type.clone()))
    }

    // Lowers a method call whose receiver satisfies a where-clause trait
    // bound (`I: Iterator`): the checker records these as non-dynamic trait
    // method calls, and the concrete impl resolves per receiver at MIR time.
    fn lower_bound_trait_call(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        callee: ExprId,
        args: &[ExprId],
    ) -> Option<Value> {
        let body_id = self.current_body?;
        let call = match self.type_result.trait_method_calls.get(&(body_id, callee)) {
            Some(call) => call.clone(),
            None => {
                return None;
            }
        };
        if call.dynamic {
            return None;
        }
        let base = match &input.body.exprs[callee] {
            Expr::FieldAccess { base, .. } => *base,
            _ => {
                return None;
            }
        };
        let receiver_ty = match self.type_result.expr_types.get(&(body_id, base)) {
            Some(ty) => ty,
            None => {
                return None;
            }
        };
        let fid = match self.find_trait_impl_method(call.trait_id, &call.method, receiver_ty, None)
        {
            Some(fid) => fid,
            None => {
                return None;
            }
        };
        Some(self.lower_named_call_expr(builder, input, callee, args, fid))
    }

    fn lower_indirect_call_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        callee: ExprId,
        args: &[ExprId],
    ) -> Value {
        let expressions = std::iter::once(callee)
            .chain(args.iter().copied())
            .collect::<Vec<_>>();
        let mut values = self.lower_expr_sequence(
            builder,
            input.param_values,
            input.body,
            input.expr_id,
            0,
            &expressions,
        );
        let callee_value = values.remove(0);
        let mut args = values;
        let callee_ty = self
            .current_body
            .and_then(|body| self.type_result.expr_types.get(&(body, callee)))
            .map_or(Type::Unit, |ty| self.convert_type(ty));
        let (callee_value, callee_ty) = match &callee_ty {
            Type::Ref(inner, _) if closure_call_signature(inner).is_some() => (
                builder.load(callee_value, inner.as_ref().clone()),
                inner.as_ref().clone(),
            ),
            _ => (callee_value, callee_ty),
        };
        if let Some(signature) = closure_call_signature(&callee_ty) {
            let call = builder.extract_value(callee_value, 0, Type::FnPtr(signature));
            let env = builder.extract_value(callee_value, 1, closure_env_type());
            args.insert(0, env);
            builder.call_indirect(call, args, input.mir_type.clone())
        } else {
            builder.call_indirect(callee_value, args, input.mir_type.clone())
        }
    }

    fn lower_named_call_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        callee: ExprId,
        args: &[ExprId],
        target: hir::item_tree::FunctionId,
    ) -> Value {
        let method_target = match &input.body.exprs[callee] {
            Expr::FieldAccess { base, .. } => {
                Some((self.actual_method_fid(callee, target, *base), *base))
            }
            _ => None,
        };
        if let Some((function, base)) = method_target
            && let Some(op) = self.builtin_operator_for_method(function)
        {
            return self.lower_builtin_operator_method_call(
                builder,
                input.param_values,
                input.body,
                input.expr_id,
                base,
                args,
                op,
            );
        }

        let name = if let Some((function, base)) = method_target {
            self.mono_function_name(function, callee)
                .or_else(|| self.mono_method_name(function, base, args.first().copied()))
                .unwrap_or_else(|| self.function_name(function))
        } else {
            self.mono_function_name(target, callee)
                .unwrap_or_else(|| self.function_name(target))
        };
        let method_dispatch_types = method_target.map(|(function, base)| {
            let base_tc_ty = self
                .current_body
                .and_then(|body| self.type_result.expr_types.get(&(body, base)));
            let rhs_tc_ty = args.first().and_then(|arg| {
                self.current_body
                    .and_then(|body| self.type_result.expr_types.get(&(body, *arg)))
            });
            (function, base, base_tc_ty, rhs_tc_ty)
        });
        let receiver = if let Some((function, base, base_tc_ty, rhs_tc_ty)) = method_dispatch_types
            && let Some(receiver) = self.hir.item_tree.functions[function].params.first()
        {
            let receiver_ty = receiver.ty.clone();
            Some(
                self.lower_receiver_arg(
                    builder,
                    input.param_values,
                    input.body,
                    base,
                    &receiver_ty,
                    base_tc_ty
                        .and_then(|ty| self.mono_method_param_type(function, 0, ty, rhs_tc_ty))
                        .as_ref(),
                ),
            )
        } else {
            None
        };
        let args = if let Some((function, _, base_tc_ty, rhs_tc_ty)) = method_dispatch_types {
            let body_id = self.current_body;
            let mut values = Vec::with_capacity(args.len());
            for (index, arg) in args.iter().enumerate() {
                let raw = self.lower_expr(builder, input.param_values, input.body, *arg);
                let expected = base_tc_ty
                    .and_then(|ty| self.mono_method_param_type(function, index + 1, ty, rhs_tc_ty));
                let adjusted =
                    match body_id.and_then(|bid| self.type_result.expr_types.get(&(bid, *arg))) {
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
            values
        } else {
            self.lower_expr_sequence(
                builder,
                input.param_values,
                input.body,
                input.expr_id,
                1,
                args,
            )
        };
        let mut values = Vec::with_capacity(args.len() + usize::from(receiver.is_some()));
        if let Some(receiver) = receiver {
            values.push(receiver);
        }
        values.extend(args);
        let is_extern = self.hir.item_tree.extern_function_ids.contains(&target)
            && !self.hir.function_bodies.contains_key(&target);
        let function = if is_extern {
            FuncRef::Extern(name)
        } else {
            FuncRef::Local(name)
        };
        let call_ty = match &function {
            FuncRef::Local(name) => self.instance_result_type(name, input.mir_type.clone()),
            _ => input.mir_type.clone(),
        };
        let value = builder.call(function, values, call_ty);
        self.finish_expr(builder, input, value)
    }

    fn lower_lambda_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        params: &[hir::body::LambdaParam],
        body: ExprId,
    ) -> Value {
        let body_id = self.current_body.expect("lambda outside of a body");
        self.lower_lambda(
            builder,
            input.param_values,
            &LambdaExprInput {
                body_id,
                expr_id: input.expr_id,
                params,
                body,
                ty: input.mir_type,
            },
        )
    }

    fn lower_field_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        base: ExprId,
        field: &hir::Name,
    ) -> Value {
        let captured = self
            .capture_place_from_expr(input.body, input.expr_id)
            .and_then(|place| self.capture_access_for_place(builder, &place))
            .filter(|access| &access.ty == input.mir_type);
        let value = if self.expression_requires_temporary_place(input.body, base) {
            let base_place =
                self.materialize_temporary_place(builder, input.param_values, input.body, base);
            let field = builder.field_ptr(
                base_place,
                self.resolve_field_index(base, field),
                input.mir_type.clone(),
            );
            builder.load(field, input.mir_type.clone())
        } else if let Some(access) = captured {
            builder.load(access.place, access.ty)
        } else {
            let base_value = self.lower_expr(builder, input.param_values, input.body, base);
            let field = self.resolve_field_index(base, field);
            builder.extract_value(base_value, field, input.mir_type.clone())
        };
        self.clear_drop_flags_if_moved(builder, input.body, input.expr_id);
        value
    }

    fn lower_index_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        base: ExprId,
        index: ExprId,
    ) -> Value {
        if let Some(place) = self.lower_trait_index_place(
            builder,
            input.param_values,
            input.body,
            input.expr_id,
            base,
            index,
        ) {
            let value = builder.load(place, input.mir_type.clone());
            self.clear_drop_flags_if_moved(builder, input.body, input.expr_id);
            let value = self.apply_expr_coercion(builder, input.expr_id, value);
            if !input.diverges {
                self.expr_cache.insert(input.expr_id, value);
            }
            return value;
        }
        let captured = self
            .capture_place_from_expr(input.body, input.expr_id)
            .and_then(|place| self.capture_access_for_place(builder, &place))
            .filter(|access| &access.ty == input.mir_type);
        let value = if let Some(access) = captured {
            let value = builder.load(access.place, access.ty);
            self.clear_drop_flags_if_moved(builder, input.body, input.expr_id);
            value
        } else {
            let (base_value, index_value) = if self
                .expression_requires_temporary_place(input.body, base)
            {
                (
                    self.materialize_temporary_place(builder, input.param_values, input.body, base),
                    self.lower_expr(builder, input.param_values, input.body, index),
                )
            } else {
                let values = self.lower_expr_sequence(
                    builder,
                    input.param_values,
                    input.body,
                    input.expr_id,
                    0,
                    &[base, index],
                );
                let [base, index] = values.as_slice() else {
                    unreachable!();
                };
                (*base, *index)
            };
            let ptr = if let Some(len) = self.index_len(builder, base_value, base) {
                builder.checked_index_ptr(base_value, index_value, len, input.mir_type.clone())
            } else {
                builder.index_ptr(base_value, index_value, input.mir_type.clone())
            };
            let value = builder.load(ptr, input.mir_type.clone());
            self.clear_drop_flags_if_moved(builder, input.body, input.expr_id);
            self.clear_dynamic_index_drop_flags_if_moved(
                builder,
                input.body,
                input.expr_id,
                index,
                index_value,
            );
            value
        };
        self.finish_expr(builder, input, value)
    }

    fn lower_cast_expr(
        &mut self,
        builder: &mut Builder,
        input: &ExprLoweringInput<'_>,
        base: ExprId,
    ) -> Value {
        let base_value = self.lower_expr(builder, input.param_values, input.body, base);
        let base_ty = self
            .current_body
            .and_then(|body| self.type_result.expr_types.get(&(body, base)))
            .map_or(Type::Unit, |ty| self.convert_type(ty));
        if is_raw_parts_to_slice_cast(&base_ty, input.mir_type) {
            let Type::Tuple(parts) = &base_ty else {
                unreachable!();
            };
            let data = builder.extract_value(base_value, 0, parts[0].clone());
            let len = builder.extract_value(base_value, 1, Type::Int(IntTy::Usize));
            builder.struct_value(vec![data, len], input.mir_type.clone())
        } else if is_slice_to_raw_parts_cast(&base_ty, input.mir_type) {
            let Type::Tuple(parts) = input.mir_type else {
                unreachable!();
            };
            let data = builder.extract_value(base_value, 0, parts[0].clone());
            let len = builder.extract_value(base_value, 1, Type::Int(IntTy::Usize));
            builder.struct_value(vec![data, len], input.mir_type.clone())
        } else if is_byte_str_layout_cast(&base_ty, input.mir_type) {
            let data =
                builder.extract_value(base_value, 0, Type::Ptr(Box::new(Type::Int(IntTy::U8))));
            let len = builder.extract_value(base_value, 1, Type::Int(IntTy::Usize));
            builder.struct_value(vec![data, len], input.mir_type.clone())
        } else {
            let Some(op) = determine_cast_op(&base_ty, input.mir_type) else {
                let range = input
                    .body
                    .source_map
                    .expr_ranges
                    .get(&input.expr_id)
                    .copied()
                    .unwrap_or_default();
                self.module.diagnostics.push(type_checker::Diagnostic {
                    code: "E0999",
                    severity: type_checker::Severity::Error,
                    message: format!("unsupported cast from {base_ty:?} to {:?}", input.mir_type),
                    labels: vec![type_checker::SourceLabel {
                        range,
                        message: "this cast cannot be lowered".into(),
                        style: type_checker::LabelStyle::Primary,
                    }],
                    help: None,
                    notes: Vec::new(),
                });
                return builder.unit_const();
            };
            builder.cast(op, base_value, input.mir_type.clone())
        }
    }

    pub(super) fn lower_short_circuit_expr(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        lhs: ExprId,
        rhs: ExprId,
        op: HirBinOp,
    ) -> Value {
        let lhs = self.lower_expr(builder, param_values, body, lhs);
        if !builder.needs_return() {
            return lhs;
        }

        let rhs_block = builder.func.new_block_labeled("logical_rhs");
        let short_block = builder.func.new_block_labeled("logical_short");
        let merge_block = builder.func.new_block_labeled("logical_merge");
        if matches!(op, HirBinOp::And) {
            builder.set_cond_branch(lhs, rhs_block, short_block);
        } else {
            builder.set_cond_branch(lhs, short_block, rhs_block);
        }

        let mut phi_args = Vec::with_capacity(2);
        builder.switch_to_block(rhs_block);
        self.temporary_drop_scopes.push(Vec::new());
        let rhs = self.lower_expr(builder, param_values, body, rhs);
        if builder.needs_return() {
            self.emit_current_temporary_drop_scope(builder);
            let rhs_exit = builder.current_block;
            builder.set_branch(merge_block);
            phi_args.push((rhs, rhs_exit));
        }
        self.temporary_drop_scopes.pop();

        builder.switch_to_block(short_block);
        let short = builder.bconst(matches!(op, HirBinOp::Or));
        let short_exit = builder.current_block;
        builder.set_branch(merge_block);
        phi_args.push((short, short_exit));

        builder.switch_to_block(merge_block);
        let phi = Inst::new(InstKind::Phi(phi_args), Type::Bool);
        builder.func.push_inst(merge_block, phi)
    }

    pub(super) fn lower_try_expr(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        expr_id: ExprId,
        operand: ExprId,
        result_ty: Type,
    ) -> Value {
        let Some(body_id) = self.current_body else {
            return builder.unit_const();
        };
        let operand_tc_ty = self
            .type_result
            .expr_types
            .get(&(body_id, operand))
            .cloned()
            .unwrap_or(type_checker::Type::Unknown);
        let type_checker::Type::Enum(result_id, result_args) = &operand_tc_ty else {
            return self.lower_expr(builder, param_values, body, operand);
        };
        let Some(return_ty) = self.current_function_return_type() else {
            return self.lower_expr(builder, param_values, body, operand);
        };
        let type_checker::Type::Enum(return_id, _) = &return_ty else {
            return self.lower_expr(builder, param_values, body, operand);
        };
        let enum_data = &self.hir.item_tree.enums[*result_id];
        let lang_items = &self.type_result.trait_env.lang_items;
        // Mirror the type checker's decision: the registered lang enum when
        // std is loaded, otherwise the name-based fallback for no-std files.
        let is_result = lang_items
            .get_enum(type_checker::lang_items::LangItem::Result)
            .is_some_and(|registered| registered == *result_id)
            || (lang_items
                .get_enum(type_checker::lang_items::LangItem::Result)
                .is_none()
                && enum_data.name.0 == "Result");
        let is_option = lang_items
            .get_enum(type_checker::lang_items::LangItem::Option)
            .is_some_and(|registered| registered == *result_id)
            || (lang_items
                .get_enum(type_checker::lang_items::LangItem::Option)
                .is_none()
                && enum_data.name.0 == "Option");
        let expected_args = match (is_result, is_option) {
            (true, _) => 2,
            (_, true) => 1,
            _ => 0,
        };
        if result_id != return_id || expected_args == 0 || result_args.len() != expected_args {
            return self.lower_expr(builder, param_values, body, operand);
        }

        let operand_value = self.lower_expr(builder, param_values, body, operand);
        let operand_mir_ty = self.convert_type(&operand_tc_ty);
        let tag = builder.extract_value(operand_value, 0, Type::Int(IntTy::U32));
        let (ok_variant_name, err_variant_name) = if is_result {
            ("Ok", "Err")
        } else {
            ("Some", "None")
        };
        let Some(ok_variant) = enum_data
            .variants
            .iter()
            .position(|variant| variant.name.0 == ok_variant_name)
        else {
            return operand_value;
        };
        let Some(err_variant) = enum_data
            .variants
            .iter()
            .position(|variant| variant.name.0 == err_variant_name)
        else {
            return operand_value;
        };
        let ok_block = builder.func.new_block_labeled("try_ok");
        let err_block = builder.func.new_block_labeled("try_err");
        let merge_block = builder.func.new_block_labeled("try_merge");
        let expected_tag = builder.iconst(ok_variant as u64, IntTy::U32);
        let is_ok = builder.cmp(CmpOp::Eq, tag, expected_tag);
        builder.set_cond_branch(is_ok, ok_block, err_block);

        builder.switch_to_block(err_block);
        let err_offset = 1 + Self::enum_payload_offset(enum_data, err_variant);
        let error_mir_ty = match &operand_mir_ty {
            Type::Enum(enum_ty) => enum_ty
                .variants
                .get(err_variant)
                .and_then(|variant| match &variant.kind {
                    EnumVariantKind::Tuple(fields) if fields.len() == 1 => fields.first().cloned(),
                    EnumVariantKind::Struct(fields) if fields.len() == 1 => {
                        fields.first().map(|(_, ty)| ty.clone())
                    }
                    _ => None,
                })
                .unwrap_or(Type::Unit),
            _ => Type::Unit,
        };
        let error_value = builder.extract_value(operand_value, err_offset, error_mir_ty);
        let converted_error = if is_result {
            let error_tc_ty = result_args[1].clone();
            self.convert_try_error(builder, expr_id, error_value, &error_tc_ty, &return_ty)
        } else {
            error_value
        };
        let return_mir_ty = self.current_function_return_mir_type();
        let error_payloads = if is_result {
            vec![converted_error]
        } else {
            Vec::new()
        };
        let error_result = self.lower_enum_variant_value(
            builder,
            *return_id,
            err_variant,
            error_payloads,
            return_mir_ty,
        );
        self.emit_current_drop_scope(builder);
        builder.set_return(Some(error_result));

        builder.switch_to_block(ok_block);
        let ok_offset = 1 + Self::enum_payload_offset(enum_data, ok_variant);
        let ok_mir_ty = match &operand_mir_ty {
            Type::Enum(enum_ty) => enum_ty
                .variants
                .get(ok_variant)
                .and_then(|variant| match &variant.kind {
                    EnumVariantKind::Tuple(fields) if fields.len() == 1 => fields.first().cloned(),
                    EnumVariantKind::Struct(fields) if fields.len() == 1 => {
                        fields.first().map(|(_, ty)| ty.clone())
                    }
                    _ => None,
                })
                .unwrap_or_else(|| result_ty.clone()),
            _ => result_ty.clone(),
        };
        let ok_value = builder.extract_value(operand_value, ok_offset, ok_mir_ty);
        builder.set_branch(merge_block);

        builder.switch_to_block(merge_block);
        let phi = Inst::new(InstKind::Phi(vec![(ok_value, ok_block)]), result_ty);
        builder.func.push_inst(merge_block, phi)
    }

    fn convert_try_error(
        &mut self,
        builder: &mut Builder,
        expr_id: ExprId,
        error_value: Value,
        error_ty: &type_checker::Type,
        return_ty: &type_checker::Type,
    ) -> Value {
        let Some(body_id) = self.current_body else {
            return error_value;
        };
        let Some(call) = self
            .type_result
            .trait_method_calls
            .get(&(body_id, expr_id))
            .cloned()
        else {
            return error_value;
        };
        let target_error = match return_ty {
            type_checker::Type::Enum(_, return_args) => return_args.get(1).unwrap_or(error_ty),
            _ => error_ty,
        };
        // `Into` impls have the source error as their self type, while `From`
        // impls receive the target error as self and take the source as trait
        // argument, so the lookup direction depends on the recorded trait.
        let is_from = self.hir.item_tree.traits[call.trait_id].name.0 == "From";
        let (receiver_error, trait_arg_error) = if is_from {
            (target_error, error_ty)
        } else {
            (error_ty, target_error)
        };
        let Some(fid) = self.find_trait_impl_method(
            call.trait_id,
            &call.method,
            receiver_error,
            Some(trait_arg_error),
        ) else {
            return error_value;
        };
        let name = self
            .mono_method_name_for_receiver(fid, receiver_error, Some(trait_arg_error))
            .unwrap_or_else(|| self.function_name(fid));
        let return_ty = self.hir.item_tree.functions[fid]
            .ret_type
            .as_ref()
            .map_or(Type::Unit, |ty| self.convert_hir_type(ty));
        builder.call(FuncRef::Local(name), vec![error_value], return_ty)
    }

    pub(super) fn current_function_return_type(&self) -> Option<type_checker::Type> {
        let fid = self.current_function?;
        let function = &self.hir.item_tree.functions[fid];
        Some(
            function
                .ret_type
                .as_ref()
                .map_or(type_checker::Type::Unit, |ty| {
                    self.lower_hir_type_for_pattern(ty, &self.generic_tc_subst)
                }),
        )
    }

    pub(super) fn current_function_return_mir_type(&self) -> Type {
        let Some(fid) = self.current_function else {
            return Type::Unit;
        };
        self.hir.item_tree.functions[fid]
            .ret_type
            .as_ref()
            .map_or(Type::Unit, |ty| self.convert_hir_type(ty))
    }

    pub(super) fn apply_expr_coercion(
        &mut self,
        builder: &mut Builder,
        expr_id: ExprId,
        value: Value,
    ) -> Value {
        let Some(body_id) = self.current_body else {
            return value;
        };
        let Some(target) = self.type_result.expr_coercions.get(&(body_id, expr_id)) else {
            return value;
        };
        let Some(actual) = self.type_result.expr_types.get(&(body_id, expr_id)) else {
            return value;
        };
        let target_mir_type = self.convert_type(target);
        if self
            .coerced_values
            .contains(&(value, target_mir_type.clone()))
        {
            return value;
        }
        if let type_checker::Type::OwnedDynTrait {
            trait_id,
            args,
            assoc_bindings: target_assoc,
        } = target
        {
            if let type_checker::Type::OwnedDynTrait {
                trait_id: actual_trait_id,
                args: actual_args,
                assoc_bindings: actual_assoc,
                ..
            } = actual
            {
                if actual_trait_id == trait_id
                    && actual_args == args
                    && actual_assoc == target_assoc
                {
                    return value;
                }
                let target_assoc = self.convert_dyn_assoc_bindings(target_assoc);
                return self
                    .lower_dyn_trait_from_object(
                        builder,
                        value,
                        actual,
                        *trait_id,
                        &args
                            .iter()
                            .map(|arg| self.convert_type(arg))
                            .collect::<Vec<_>>(),
                        &target_assoc,
                        &target_mir_type,
                    )
                    .unwrap_or(value);
            }
            let object_ty = target_mir_type;
            let storage = builder.heap_alloc(self.convert_type(actual));
            builder.store(value, storage);
            let data = builder.cast(
                crate::instr::CastOp::PtrToPtr,
                storage,
                Type::Ptr(Box::new(Type::Unit)),
            );
            if let Some(object) = self.lower_dyn_trait_from_data(
                builder,
                data,
                actual,
                *trait_id,
                &args
                    .iter()
                    .map(|arg| self.convert_type(arg))
                    .collect::<Vec<_>>(),
                &self.convert_dyn_assoc_bindings(target_assoc),
                &object_ty,
                true,
            ) {
                return object;
            }
            return value;
        }
        let (type_checker::Type::Ref(actual, _), type_checker::Type::Ref(target, target_mut)) =
            (actual, target)
        else {
            return value;
        };
        if let type_checker::Type::DynTrait {
            trait_id,
            args,
            assoc_bindings: target_assoc,
        } = target.as_ref()
        {
            match actual.as_ref() {
                type_checker::Type::DynTrait {
                    trait_id: actual_trait_id,
                    args: actual_args,
                    assoc_bindings: actual_assoc,
                    ..
                } => {
                    if actual_trait_id == trait_id
                        && actual_args == args
                        && actual_assoc == target_assoc
                    {
                        return value;
                    }
                    let target_assoc = self.convert_dyn_assoc_bindings(target_assoc);
                    return self
                        .lower_dyn_trait_from_object(
                            builder,
                            value,
                            actual.as_ref(),
                            *trait_id,
                            &args
                                .iter()
                                .map(|arg| self.convert_type(arg))
                                .collect::<Vec<_>>(),
                            &target_assoc,
                            &target_mir_type,
                        )
                        .unwrap_or(value);
                }
                type_checker::Type::OwnedDynTrait {
                    trait_id: actual_trait_id,
                    args: actual_args,
                    assoc_bindings: actual_assoc,
                    ..
                } => {
                    if actual_trait_id == trait_id
                        && actual_args == args
                        && actual_assoc == target_assoc
                    {
                        return value;
                    }
                    let source = type_checker::Type::DynTrait {
                        trait_id: *actual_trait_id,
                        args: actual_args.clone(),
                        assoc_bindings: actual_assoc.clone(),
                    };
                    let target_assoc = self.convert_dyn_assoc_bindings(target_assoc);
                    return self
                        .lower_dyn_trait_from_object(
                            builder,
                            value,
                            &source,
                            *trait_id,
                            &args
                                .iter()
                                .map(|arg| self.convert_type(arg))
                                .collect::<Vec<_>>(),
                            &target_assoc,
                            &target_mir_type,
                        )
                        .unwrap_or(value);
                }
                _ => {
                    let data = builder.cast(
                        crate::instr::CastOp::PtrToPtr,
                        value,
                        Type::Ptr(Box::new(Type::Unit)),
                    );
                    return self
                        .lower_dyn_trait_from_data(
                            builder,
                            data,
                            actual,
                            *trait_id,
                            &args
                                .iter()
                                .map(|arg| self.convert_type(arg))
                                .collect::<Vec<_>>(),
                            &self.convert_dyn_assoc_bindings(target_assoc),
                            &target_mir_type,
                            false,
                        )
                        .unwrap_or(value);
                }
            }
        }
        let (
            type_checker::Type::Array(_, type_checker::ConstArg::Value(len)),
            type_checker::Type::Slice(_),
        ) = (actual.as_ref(), target.as_ref())
        else {
            return value;
        };
        let len = builder.iconst(*len as u64, IntTy::Usize);
        let value = builder.struct_value(
            vec![value, len],
            Type::Ref(Box::new(self.convert_type(target.as_ref())), *target_mut),
        );
        self.coerced_values
            .insert((value, self.convert_type(target)));
        value
    }

    pub(super) fn lower_for_expr(
        &mut self,
        builder: &mut Builder,
        input: ForExprInput<'_>,
    ) -> Value {
        if let Some(info) = self
            .current_body
            .and_then(|bid| self.type_result.for_loops.get(&(bid, input.expr_id)))
            .cloned()
        {
            return self.lower_iterator_for_expr(builder, input, &info);
        }

        if let Some((item_ty, len)) = self.array_iter_info(input.iterable) {
            return self.lower_array_for_expr(builder, input, &item_ty, len);
        }

        let ForExprInput {
            param_values,
            body,
            pat,
            iterable,
            loop_body: for_body,
            ..
        } = input;

        let iterable_value = self.lower_expr(builder, param_values, body, iterable);
        if !self.is_std_range_expr(iterable) {
            return builder.unit_const();
        }

        let i32_ty = Type::Int(IntTy::I32);
        let start = builder.extract_value(iterable_value, 0, i32_ty.clone());
        let end = builder.extract_value(iterable_value, 1, i32_ty.clone());
        let cursor = builder.alloca(i32_ty.clone());
        builder.store(start, cursor);

        let cond_block = builder.func.new_block_labeled("for_cond");
        let body_block = builder.func.new_block_labeled("for_body");
        let step_block = builder.func.new_block_labeled("for_step");
        let exit_block = builder.func.new_block_labeled("for_exit");

        builder.set_branch(cond_block);

        builder.switch_to_block(cond_block);
        let current = builder.load(cursor, i32_ty.clone());
        let current_end = end;
        let keep_going = builder.cmp(CmpOp::Lt, current, current_end);
        builder.set_cond_branch(keep_going, body_block, exit_block);

        builder.switch_to_block(body_block);
        self.push_match_pattern_bindings(
            builder,
            body,
            pat,
            current,
            None,
            &type_checker::Type::Int(type_checker::IntTy::I32),
        );
        self.loop_targets.push(LoopTargets {
            break_block: exit_block,
            continue_block: step_block,
            break_slot: None,
            drop_depth: self.drop_scopes.len(),
            temporary_drop_depth: self.temporary_drop_scopes.len(),
        });
        self.lower_expr(builder, param_values, body, for_body);
        self.loop_targets.pop();
        self.pattern_bindings.pop();
        if builder.needs_return() {
            builder.set_branch(step_block);
        }

        builder.switch_to_block(step_block);
        let current = builder.load(cursor, i32_ty.clone());
        let one = builder.iconst(1, IntTy::I32);
        let next = builder.binop(BinOp::Add, current, one, i32_ty);
        builder.store(next, cursor);
        builder.set_branch(cond_block);

        builder.switch_to_block(exit_block);
        builder.unit_const()
    }

    pub(super) fn lower_array_for_expr(
        &mut self,
        builder: &mut Builder,
        input: ForExprInput<'_>,
        item_ty: &Type,
        len: usize,
    ) -> Value {
        let ForExprInput {
            param_values,
            body,
            pat,
            iterable,
            loop_body: for_body,
            ..
        } = input;
        let body_id = self.current_body.expect("for loop outside a function body");
        let item_tc_ty = match self
            .type_result
            .expr_types
            .get(&(body_id, iterable))
            .map(|ty| self.substitute_tc_type(ty))
        {
            Some(type_checker::Type::Array(item, _)) => *item,
            _ => type_checker::Type::Unknown,
        };
        let array_tc_ty = type_checker::Type::Array(
            Box::new(item_tc_ty.clone()),
            type_checker::ConstArg::Value(len),
        );
        let array_ty = Type::Array(Box::new(item_ty.clone()), len);
        let iterable_value = self.lower_expr(builder, param_values, body, iterable);
        let iterable_place = builder.alloca(array_ty);
        builder.store(iterable_value, iterable_place);
        let owner_slots = self.create_drop_slots(builder, iterable_place, &array_tc_ty, Vec::new());
        self.drop_scopes
            .push(owner_slots.iter().cloned().rev().collect());

        let index_ty = Type::Int(IntTy::I32);
        let zero = builder.iconst(0, IntTy::I32);
        let end = builder.iconst(len as u64, IntTy::I32);
        let cursor = builder.alloca(index_ty.clone());
        builder.store(zero, cursor);

        let cond_block = builder.func.new_block_labeled("for_array_cond");
        let body_block = builder.func.new_block_labeled("for_array_body");
        let step_block = builder.func.new_block_labeled("for_array_step");
        let exit_block = builder.func.new_block_labeled("for_array_exit");

        builder.set_branch(cond_block);

        builder.switch_to_block(cond_block);
        let current = builder.load(cursor, index_ty.clone());
        let keep_going = builder.cmp(CmpOp::Lt, current, end);
        builder.set_cond_branch(keep_going, body_block, exit_block);

        builder.switch_to_block(body_block);
        let item_ptr = builder.index_ptr(iterable_place, current, item_ty.clone());
        let item = builder.load(item_ptr, item_ty.clone());
        Self::clear_indexed_drop_slots(builder, &owner_slots, current, IntTy::I32);
        let item_place = self.type_needs_drop(&item_tc_ty, 0).then(|| {
            let place = builder.alloca(item_ty.clone());
            builder.store(item, place);
            place
        });
        self.push_match_pattern_bindings(builder, body, pat, item, item_place, &item_tc_ty);
        let pattern_sources =
            self.push_pattern_drop_scope(builder, body, pat, item_place, &item_tc_ty, true);
        let item_drop_depth = self.drop_scopes.len() - 1;
        self.loop_targets.push(LoopTargets {
            break_block: exit_block,
            continue_block: step_block,
            break_slot: None,
            drop_depth: item_drop_depth,
            temporary_drop_depth: self.temporary_drop_scopes.len(),
        });
        self.lower_expr(builder, param_values, body, for_body);
        self.loop_targets.pop();
        if builder.needs_return() {
            self.emit_current_drop_scope(builder);
            builder.set_branch(step_block);
        }
        self.pop_pattern_drop_scope(pattern_sources);
        self.pattern_bindings.pop();

        builder.switch_to_block(step_block);
        let current = builder.load(cursor, index_ty.clone());
        let one = builder.iconst(1, IntTy::I32);
        let next = builder.binop(BinOp::Add, current, one, index_ty);
        builder.store(next, cursor);
        builder.set_branch(cond_block);

        builder.switch_to_block(exit_block);
        self.emit_current_drop_scope(builder);
        self.drop_scopes.pop();
        builder.unit_const()
    }

    pub(super) fn lower_iterator_for_expr(
        &mut self,
        builder: &mut Builder,
        input: ForExprInput<'_>,
        info: &type_checker::ForLoopInfo,
    ) -> Value {
        let ForExprInput {
            param_values,
            body,
            pat,
            iterable,
            loop_body: for_body,
            ..
        } = input;
        let body_id = self.current_body.expect("for loop outside a function body");
        let iterable_ty = self
            .type_result
            .expr_types
            .get(&(body_id, iterable))
            .cloned()
            .map(|ty| self.substitute_tc_type(&ty))
            .expect("missing iterable type for checked for loop");
        let iter_tc_ty = self.substitute_tc_type(&info.iter_ty);
        let next_tc_ty = self.substitute_tc_type(&info.next_ty);

        let iterable_value = self.lower_expr(builder, param_values, body, iterable);
        let iter_ty = self.convert_type(&iter_tc_ty);
        let item_ty = self.convert_type(&info.item_ty);
        let option_ty = self.convert_type(&next_tc_ty);
        let into_iter_fid = self
            .find_trait_impl_method(
                info.into_iter.trait_id,
                &info.into_iter.method,
                &iterable_ty,
                None,
            )
            .expect("missing IntoIterator impl method for checked for loop");
        let next_fid = self
            .find_trait_impl_method(info.next.trait_id, &info.next.method, &iter_tc_ty, None)
            .expect("missing Iterator impl method for checked for loop");
        let into_iter_name = self
            .mono_method_name_for_receiver(into_iter_fid, &iterable_ty, None)
            .unwrap_or_else(|| self.function_name(into_iter_fid));
        let next_name = self
            .mono_method_name_for_receiver(next_fid, &iter_tc_ty, None)
            .unwrap_or_else(|| self.function_name(next_fid));

        let iter_value = builder.call(
            FuncRef::Local(into_iter_name),
            vec![iterable_value],
            iter_ty.clone(),
        );
        // `IntoIterator::into_iter(self)` consumes the owner. Its destructor
        // must not run again after the iterator takes over the allocation.
        if !matches!(iterable_ty, type_checker::Type::Ref(..))
            && let Some((source, _)) = self.drop_place_from_expr(body, iterable)
        {
            self.clear_drop_slots_for_source(builder, &source);
        }
        let iter_slot = builder.alloca(iter_ty.clone());
        builder.store(iter_value, iter_slot);
        let iter_owner_slots = self.create_drop_slots(builder, iter_slot, &iter_tc_ty, Vec::new());
        self.drop_scopes
            .push(iter_owner_slots.iter().cloned().rev().collect());
        // ponytail: array IntoIterator is sequential; add ManuallyDrop-like storage before
        // permitting custom array iterators that yield elements out of order.
        let tracks_array = matches!(iterable_ty, type_checker::Type::Array(..));
        let array_cursor = Self::iterator_array_cursor(builder, tracks_array);

        let cond_block = builder.func.new_block_labeled("for_iter_cond");
        let body_block = builder.func.new_block_labeled("for_iter_body");
        let exit_block = builder.func.new_block_labeled("for_iter_exit");

        builder.set_branch(cond_block);

        builder.switch_to_block(cond_block);
        let next_value = self.lower_iterator_next(
            builder, next_fid, &next_name, iter_slot, &iter_ty, option_ty,
        );
        let tag = builder.extract_value(next_value, 0, Type::Int(IntTy::U32));
        let some_tag = builder.iconst(info.some_variant as u64, IntTy::U32);
        let has_item = builder.cmp(CmpOp::Eq, tag, some_tag);
        builder.set_cond_branch(has_item, body_block, exit_block);

        builder.switch_to_block(body_block);
        let item = self.iterator_item_value(builder, next_value, &next_tc_ty, info, &item_ty);
        Self::advance_array_iterator_drop_slots(builder, array_cursor, &iter_owner_slots);
        let item_place = self.type_needs_drop(&info.item_ty, 0).then(|| {
            let place = builder.alloca(item_ty.clone());
            builder.store(item, place);
            place
        });
        self.push_match_pattern_bindings(builder, body, pat, item, item_place, &info.item_ty);
        let pattern_sources =
            self.push_pattern_drop_scope(builder, body, pat, item_place, &info.item_ty, true);
        let item_drop_depth = self.drop_scopes.len() - 1;
        self.loop_targets.push(LoopTargets {
            break_block: exit_block,
            continue_block: cond_block,
            break_slot: None,
            drop_depth: item_drop_depth,
            temporary_drop_depth: self.temporary_drop_scopes.len(),
        });
        self.lower_expr(builder, param_values, body, for_body);
        self.loop_targets.pop();
        if builder.needs_return() {
            self.emit_current_drop_scope(builder);
            builder.set_branch(cond_block);
        }
        self.pop_pattern_drop_scope(pattern_sources);
        self.pattern_bindings.pop();

        builder.switch_to_block(exit_block);
        self.emit_current_drop_scope(builder);
        self.drop_scopes.pop();
        builder.unit_const()
    }

    fn iterator_array_cursor(builder: &mut Builder, tracks_array: bool) -> Option<Value> {
        tracks_array.then(|| {
            let zero = builder.iconst(0, IntTy::Usize);
            let cursor = builder.alloca(Type::Int(IntTy::Usize));
            builder.store(zero, cursor);
            cursor
        })
    }

    fn advance_array_iterator_drop_slots(
        builder: &mut Builder,
        cursor: Option<Value>,
        owner_slots: &[super::DropSlot],
    ) {
        let Some(cursor) = cursor else {
            return;
        };
        let current = builder.load(cursor, Type::Int(IntTy::Usize));
        Self::clear_indexed_drop_slots(builder, owner_slots, current, IntTy::Usize);
        let one = builder.iconst(1, IntTy::Usize);
        let next = builder.binop(BinOp::Add, current, one, Type::Int(IntTy::Usize));
        builder.store(next, cursor);
    }

    fn lower_iterator_next(
        &self,
        builder: &mut Builder,
        next_fid: hir::item_tree::FunctionId,
        next_name: &str,
        iter_slot: Value,
        iter_ty: &Type,
        option_ty: Type,
    ) -> Value {
        let receiver = match self.hir.item_tree.functions[next_fid]
            .params
            .first()
            .map(|param| &param.ty)
        {
            Some(hir::item_tree::HirTypeRef::Ref(_, mutable)) => {
                let op = if *mutable {
                    HirUnOp::MutRef
                } else {
                    HirUnOp::Ref
                };
                builder.unop(
                    convert_unop(op),
                    iter_slot,
                    Type::Ref(Box::new(iter_ty.clone()), *mutable),
                )
            }
            _ => iter_slot,
        };
        builder.call(
            FuncRef::Local(next_name.to_owned()),
            vec![receiver],
            option_ty,
        )
    }

    fn iterator_item_value(
        &self,
        builder: &mut Builder,
        next_value: Value,
        next_ty: &type_checker::Type,
        info: &type_checker::ForLoopInfo,
        item_ty: &Type,
    ) -> Value {
        let type_checker::Type::Enum(option_id, _) = next_ty else {
            unreachable!("checked Iterator::next result is not an enum");
        };
        let payload_index =
            1 + Self::enum_payload_offset(&self.hir.item_tree.enums[*option_id], info.some_variant);
        builder.extract_value(next_value, payload_index, item_ty.clone())
    }
}
