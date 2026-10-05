use super::{
    BinaryOp, BodyCtx, Expr, ExprId, HashSet, IntTy, LangItem, OperatorCall, ResolvedName, TraitId,
    TraitMethodCall, Type, TypeChecker, UnaryOp, ValueUse, assign_operator_trait,
    binary_operator_trait, bound_target_param, type_has_unresolved_inference, unary_operator_trait,
};

impl TypeChecker<'_> {
    #[allow(clippy::too_many_arguments)]
    pub(super) fn check_overloaded_binary(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        lhs: ExprId,
        rhs: ExprId,
        op: BinaryOp,
        lhs_ty: &Type,
        rhs_ty: &Type,
        expected: Option<&Type>,
        span: Option<rowan::TextRange>,
    ) -> Option<Type> {
        let (item, method_name) = binary_operator_trait(op)?;
        let trait_id = self.result.trait_env.lang_items.get(item)?;
        if let Some(ty) = self.check_trait_bound_operator(
            ctx,
            expr_id,
            lhs,
            Some((rhs, rhs_ty)),
            lhs_ty,
            trait_id,
            method_name,
        ) {
            return Some(ty);
        }
        let receiver_ty = Self::default_inferred_numeric_type(lhs_ty);
        let method = self.find_trait_impl_method_with_output(
            &receiver_ty,
            Some(rhs_ty),
            Some(rhs_ty),
            expected,
            trait_id,
            method_name,
        )?;
        if method.function.is_unsafe {
            self.require_unsafe(ctx, "calling an unsafe function", span);
        }

        let receiver = method.function.params.first()?;
        let expected_receiver = self.lower_type_ref_with_params_at(
            &receiver.ty,
            &method.subst,
            Some(receiver.ty_range),
        );
        let actual_receiver = Self::receiver_argument_type(&receiver_ty, &expected_receiver);
        self.expect_assignable(
            &expected_receiver,
            &actual_receiver,
            "operator receiver",
            ctx.expr_range(lhs),
        );
        self.record_value_use(ctx, lhs, Self::hir_parameter_value_use(&receiver.ty));

        let Some(rhs_param) = method.function.params.get(1) else {
            self.diagnostic(
                "E0005",
                format!(
                    "operator method `{}` needs a rhs parameter",
                    method.function.name.0
                ),
                span,
            );
            return Some(Type::Error);
        };
        let expected_rhs = self.lower_type_ref_with_params_at(
            &rhs_param.ty,
            &method.subst,
            Some(rhs_param.ty_range),
        );
        let actual_rhs = Self::receiver_argument_type(rhs_ty, &expected_rhs);
        self.expect_assignable(
            &expected_rhs,
            &actual_rhs,
            "right operand",
            ctx.expr_range(rhs),
        );
        self.record_value_use(ctx, rhs, Self::hir_parameter_value_use(&rhs_param.ty));

        self.result
            .operator_calls
            .insert((ctx.body_id, expr_id), OperatorCall::Function(method.fid));

        Some(method.function.ret_type.as_ref().map_or(Type::Unit, |ty| {
            self.lower_type_ref_with_params_at(
                ty,
                &method.subst,
                method
                    .function
                    .ret_type_range
                    .or(Some(method.function.name_range)),
            )
        }))
    }

    pub(super) fn check_index_access(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        base: ExprId,
        index: ExprId,
        expected: Option<&Type>,
        span: Option<rowan::TextRange>,
    ) -> Type {
        let base_ty = self.check_expr(ctx, base);
        let index_ty = self.check_expr(ctx, index);
        let receiver_ty = match &base_ty {
            Type::Ref(inner, _) => inner.as_ref(),
            _ => &base_ty,
        };

        if let Some(output) =
            self.check_builtin_index(ctx, index, &index_ty, receiver_ty, expected, span)
        {
            return output;
        }

        let Some(trait_id) = self.result.trait_env.lang_items.get(LangItem::Index) else {
            self.diagnostic("E0036", "missing `Index` trait", span);
            return Type::Error;
        };
        if let Some(output) = self.check_trait_bound_index(
            ctx,
            expr_id,
            base,
            index,
            receiver_ty,
            &index_ty,
            trait_id,
            "index",
            expected,
        ) {
            return output;
        }
        let Some(method) = self.find_trait_impl_method(
            receiver_ty,
            Some(&index_ty),
            Some(&index_ty),
            trait_id,
            "index",
        ) else {
            if !base_ty.is_unknown_like() && !index_ty.is_unknown_like() {
                self.diagnostic(
                    "E0036",
                    format!(
                        "type `{}` cannot be indexed by `{}`",
                        base_ty.display(self.hir),
                        index_ty.display(self.hir)
                    ),
                    span,
                );
            }
            return Type::Error;
        };

        let receiver = &method.function.params[0];
        let expected_receiver = self.lower_type_ref_with_params_at(
            &receiver.ty,
            &method.subst,
            Some(receiver.ty_range),
        );
        let actual_receiver = Self::receiver_argument_type(&base_ty, &expected_receiver);
        self.expect_assignable(
            &expected_receiver,
            &actual_receiver,
            "index receiver",
            ctx.expr_range(base),
        );
        self.record_value_use(ctx, base, Self::hir_parameter_value_use(&receiver.ty));

        let index_param = &method.function.params[1];
        let expected_index = self.lower_type_ref_with_params_at(
            &index_param.ty,
            &method.subst,
            Some(index_param.ty_range),
        );
        self.expect_assignable(&expected_index, &index_ty, "index", ctx.expr_range(index));
        self.constrain_index_type(ctx, index, &index_ty, &expected_index);
        self.record_value_use(ctx, index, Self::hir_parameter_value_use(&index_param.ty));
        self.result.trait_method_calls.insert(
            (ctx.body_id, expr_id),
            TraitMethodCall {
                trait_id,
                method: "index".into(),
                dynamic: false,
            },
        );

        let output = method
            .function
            .ret_type
            .as_ref()
            .map(|ret| {
                self.lower_type_ref_with_params_at(
                    ret,
                    &method.subst,
                    method
                        .function
                        .ret_type_range
                        .or(Some(method.function.name_range)),
                )
            })
            .and_then(|ty| match ty {
                Type::Ref(output, _) => Some(*output),
                _ => None,
            })
            .unwrap_or(Type::Error);
        self.constrain_index_output(&output, expected);
        output
    }

    fn check_builtin_index(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        index: ExprId,
        index_ty: &Type,
        receiver_ty: &Type,
        expected: Option<&Type>,
        span: Option<rowan::TextRange>,
    ) -> Option<Type> {
        let output = match receiver_ty {
            Type::Slice(inner) | Type::Array(inner, _) => *inner.clone(),
            Type::Ptr { inner, .. } => {
                self.require_unsafe(ctx, "indexing a raw pointer", span);
                *inner.clone()
            }
            _ => return None,
        };
        if !index_ty.is_unknown_like() && !index_ty.is_integer() {
            self.expect_assignable(
                &Type::Int(IntTy::I32),
                index_ty,
                "index",
                ctx.expr_range(index),
            );
        }
        self.record_value_use(ctx, index, ValueUse::Move);
        self.constrain_index_output(&output, expected);
        Some(output)
    }

    fn constrain_index_output(&mut self, output: &Type, expected: Option<&Type>) {
        if let Some(expected) = expected
            && type_has_unresolved_inference(output)
        {
            let _ = self.unify_types(output, expected);
            self.last_occurs_error = None;
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn check_trait_bound_index(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        base: ExprId,
        index: ExprId,
        receiver_ty: &Type,
        index_ty: &Type,
        trait_id: TraitId,
        method_name: &str,
        expected: Option<&Type>,
    ) -> Option<Type> {
        let Type::Param(param) = receiver_ty else {
            return None;
        };
        let bound = self.current_generic_bounds(ctx).into_iter().find(|bound| {
            bound_target_param(bound).is_some_and(|name| name == *param)
                && self
                    .resolve_trait_ref(&bound.trait_ty)
                    .is_some_and(|bound_trait| self.trait_implies(bound_trait, trait_id))
        })?;
        let bound_trait = self.resolve_trait_ref(&bound.trait_ty)?;
        let method = self.hir.item_tree.traits[trait_id]
            .methods
            .iter()
            .find(|method| method.name.0 == method_name)
            .cloned()?;
        let bound_subst = self.trait_ref_subst(
            bound_trait,
            &bound.trait_ty,
            receiver_ty,
            &ctx.generic_params,
            Some(bound.trait_range),
        );
        let subst = self.supertrait_subst(
            bound_trait,
            trait_id,
            receiver_ty,
            &bound_subst,
            &mut HashSet::new(),
        )?;

        let receiver = method.params.first()?;
        let expected_receiver =
            self.lower_type_ref_with_params_at(&receiver.ty, &subst, Some(receiver.ty_range));
        let base_ty = self
            .result
            .expr_types
            .get(&(ctx.body_id, base))
            .cloned()
            .unwrap_or(Type::Error);
        let actual_receiver = Self::receiver_argument_type(&base_ty, &expected_receiver);
        self.expect_assignable(
            &expected_receiver,
            &actual_receiver,
            "index receiver",
            ctx.expr_range(base),
        );
        self.record_value_use(ctx, base, Self::hir_parameter_value_use(&receiver.ty));
        let index_param = method.params.get(1)?;
        let expected_index =
            self.lower_type_ref_with_params_at(&index_param.ty, &subst, Some(index_param.ty_range));
        self.expect_assignable(&expected_index, index_ty, "index", ctx.expr_range(index));
        self.constrain_index_type(ctx, index, index_ty, &expected_index);
        self.record_value_use(ctx, index, Self::hir_parameter_value_use(&index_param.ty));
        self.result.trait_method_calls.insert(
            (ctx.body_id, expr_id),
            TraitMethodCall {
                trait_id,
                method: method_name.into(),
                dynamic: false,
            },
        );

        let output = self.bound_assoc_type(ctx, &bound, "Output").or_else(|| {
            method.ret_type.as_ref().and_then(|ret| {
                match self.lower_type_ref_with_params_at(
                    ret,
                    &subst,
                    method.ret_type_range.or(Some(method.name_range)),
                ) {
                    Type::Ref(output, _) => Some(*output),
                    _ => None,
                }
            })
        });
        if let Some(expected) = expected
            && output.as_ref().is_some_and(type_has_unresolved_inference)
        {
            let output = output.as_ref().unwrap();
            let _ = self.unify_types(output, expected);
            self.last_occurs_error = None;
        }
        output
    }

    pub(super) fn constrain_index_type(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        index: ExprId,
        actual: &Type,
        expected: &Type,
    ) {
        if matches!(actual, Type::InferInt) && matches!(expected, Type::Int(_)) {
            self.result
                .expr_types
                .insert((ctx.body_id, index), expected.clone());
            if let Expr::Path {
                resolved: Some(ResolvedName::PatternBinding(id)),
                ..
            } = &ctx.body.exprs[index]
                && self
                    .result
                    .pattern_binding_types
                    .get(&(ctx.body_id, *id))
                    .is_some_and(|ty| matches!(ty, Type::InferInt))
            {
                self.result
                    .pattern_binding_types
                    .insert((ctx.body_id, *id), expected.clone());
                ctx.bindings.set_type(*id, expected.clone());
            }
        }
    }

    pub(super) fn check_overloaded_unary(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        operand: ExprId,
        op: UnaryOp,
        operand_ty: &Type,
        span: Option<rowan::TextRange>,
    ) -> Option<Type> {
        let (item, method_name) = unary_operator_trait(op)?;
        let trait_id = self.result.trait_env.lang_items.get(item)?;
        if let Some(ty) = self.check_trait_bound_operator(
            ctx,
            expr_id,
            operand,
            None,
            operand_ty,
            trait_id,
            method_name,
        ) {
            return Some(ty);
        }
        let method = self.find_trait_impl_method(operand_ty, None, None, trait_id, method_name)?;
        if method.function.is_unsafe {
            self.require_unsafe(ctx, "calling an unsafe function", span);
        }
        let receiver = method.function.params.first()?;
        let expected_receiver = self.lower_type_ref_with_params_at(
            &receiver.ty,
            &method.subst,
            Some(receiver.ty_range),
        );
        let actual_receiver = Self::receiver_argument_type(operand_ty, &expected_receiver);
        self.expect_assignable(
            &expected_receiver,
            &actual_receiver,
            "operator receiver",
            ctx.expr_range(operand),
        );
        self.record_value_use(ctx, operand, Self::hir_parameter_value_use(&receiver.ty));
        self.result
            .operator_calls
            .insert((ctx.body_id, expr_id), OperatorCall::Function(method.fid));
        Some(method.function.ret_type.as_ref().map_or(Type::Unit, |ty| {
            self.lower_type_ref_with_params_at(
                ty,
                &method.subst,
                method
                    .function
                    .ret_type_range
                    .or(Some(method.function.name_range)),
            )
        }))
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn check_overloaded_assign(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        lhs: ExprId,
        rhs: ExprId,
        op: BinaryOp,
        lhs_ty: &Type,
        rhs_ty: &Type,
        span: Option<rowan::TextRange>,
    ) -> Option<()> {
        let (item, method_name) = assign_operator_trait(op)?;
        let trait_id = self.result.trait_env.lang_items.get(item)?;
        if self
            .check_trait_bound_operator(
                ctx,
                expr_id,
                lhs,
                Some((rhs, rhs_ty)),
                lhs_ty,
                trait_id,
                method_name,
            )
            .is_some()
        {
            return Some(());
        }
        let receiver_ty = Self::default_inferred_numeric_type(lhs_ty);
        let method = self.find_trait_impl_method(
            &receiver_ty,
            Some(rhs_ty),
            Some(rhs_ty),
            trait_id,
            method_name,
        )?;
        if method.function.is_unsafe {
            self.require_unsafe(ctx, "calling an unsafe function", span);
        }
        let receiver = method.function.params.first()?;
        let expected_receiver = self.lower_type_ref_with_params_at(
            &receiver.ty,
            &method.subst,
            Some(receiver.ty_range),
        );
        let actual_receiver = Self::receiver_argument_type(&receiver_ty, &expected_receiver);
        self.expect_assignable(
            &expected_receiver,
            &actual_receiver,
            "operator receiver",
            ctx.expr_range(lhs),
        );
        self.record_value_use(ctx, lhs, Self::hir_parameter_value_use(&receiver.ty));
        let rhs_param = method.function.params.get(1)?;
        let expected_rhs = self.lower_type_ref_with_params_at(
            &rhs_param.ty,
            &method.subst,
            Some(rhs_param.ty_range),
        );
        let actual_rhs = Self::receiver_argument_type(rhs_ty, &expected_rhs);
        self.expect_assignable(
            &expected_rhs,
            &actual_rhs,
            "right operand",
            ctx.expr_range(rhs),
        );
        self.record_value_use(ctx, rhs, Self::hir_parameter_value_use(&rhs_param.ty));
        self.result
            .operator_calls
            .insert((ctx.body_id, expr_id), OperatorCall::Function(method.fid));
        Some(())
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn check_trait_bound_operator(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        lhs: ExprId,
        rhs: Option<(ExprId, &Type)>,
        lhs_ty: &Type,
        trait_id: TraitId,
        method_name: &str,
    ) -> Option<Type> {
        let Type::Param(param) = lhs_ty else {
            return None;
        };
        let bounds = self.current_generic_bounds(ctx);
        let bound = bounds
            .iter()
            .find(|bound| {
                bound_target_param(bound).is_some_and(|name| name == *param)
                    && self
                        .resolve_trait_ref(&bound.trait_ty)
                        .is_some_and(|bound_trait| self.trait_implies(bound_trait, trait_id))
            })?
            .clone();
        let bound_trait = self.resolve_trait_ref(&bound.trait_ty)?;

        let method = self.hir.item_tree.traits[trait_id]
            .methods
            .iter()
            .find(|method| method.name.0 == method_name)
            .cloned()?;
        if method.is_unsafe {
            self.require_unsafe(
                ctx,
                "calling an unsafe function",
                rhs.and_then(|(rhs, _)| ctx.expr_range(rhs)),
            );
        }

        let bound_subst = self.trait_ref_subst(
            bound_trait,
            &bound.trait_ty,
            lhs_ty,
            &ctx.generic_params,
            Some(bound.trait_range),
        );
        let subst = self.supertrait_subst(
            bound_trait,
            trait_id,
            lhs_ty,
            &bound_subst,
            &mut HashSet::new(),
        )?;
        let receiver = method.params.first()?;
        self.record_value_use(ctx, lhs, Self::hir_parameter_value_use(&receiver.ty));
        if let Some((rhs, rhs_ty)) = rhs {
            let rhs_param = method.params.get(1)?;
            let expected_rhs =
                self.lower_type_ref_with_params_at(&rhs_param.ty, &subst, Some(rhs_param.ty_range));
            let actual_rhs = Self::receiver_argument_type(rhs_ty, &expected_rhs);
            self.expect_assignable(
                &expected_rhs,
                &actual_rhs,
                "right operand",
                ctx.expr_range(rhs),
            );
            self.record_value_use(ctx, rhs, Self::hir_parameter_value_use(&rhs_param.ty));
        }
        self.result.operator_calls.insert(
            (ctx.body_id, expr_id),
            OperatorCall::Trait(TraitMethodCall {
                trait_id,
                method: method_name.into(),
                dynamic: false,
            }),
        );
        let output = self
            .bound_assoc_type(ctx, &bound, "Output")
            .or_else(|| {
                method.ret_type.as_ref().map(|ret| {
                    self.lower_type_ref_with_params_at(
                        ret,
                        &subst,
                        method.ret_type_range.or(Some(method.name_range)),
                    )
                })
            })
            .unwrap_or(Type::Unit);
        Some(output)
    }

    pub(super) fn check_binary_types(
        &mut self,
        ctx: &BodyCtx<'_>,
        operands: (ExprId, ExprId),
        op: BinaryOp,
        types: (&Type, &Type),
        span: Option<rowan::TextRange>,
    ) -> Type {
        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                self.check_arithmetic_types(ctx, operands, types, span)
            }
            BinaryOp::Mod => self.check_remainder_types(types, span),
            BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor => {
                self.check_bitwise_types(types, span)
            }
            BinaryOp::Shl | BinaryOp::Shr => self.check_shift_types(types, span),
            BinaryOp::Eq | BinaryOp::Neq => self.check_equality_types(ctx, types, span),
            BinaryOp::Lt | BinaryOp::Gt | BinaryOp::LtEq | BinaryOp::GtEq => {
                self.check_ordering_types(ctx, types, span)
            }
            BinaryOp::And | BinaryOp::Or => self.check_logical_types(ctx, operands, types),
            BinaryOp::Assign
            | BinaryOp::AddAssign
            | BinaryOp::SubAssign
            | BinaryOp::MulAssign
            | BinaryOp::DivAssign
            | BinaryOp::ModAssign
            | BinaryOp::BitAndAssign
            | BinaryOp::BitOrAssign
            | BinaryOp::BitXorAssign
            | BinaryOp::ShlAssign
            | BinaryOp::ShrAssign => unreachable!(),
        }
    }

    fn check_arithmetic_types(
        &mut self,
        ctx: &BodyCtx<'_>,
        operands: (ExprId, ExprId),
        types: (&Type, &Type),
        span: Option<rowan::TextRange>,
    ) -> Type {
        let (lhs, rhs) = operands;
        let (lhs_ty, rhs_ty) = types;
        self.expect_numeric(lhs_ty, "left operand", ctx.expr_range(lhs));
        self.expect_numeric(rhs_ty, "right operand", ctx.expr_range(rhs));
        if !lhs_ty.is_numeric() || !rhs_ty.is_numeric() {
            return Type::Error;
        }
        if lhs_ty.is_unknown_like() || rhs_ty.is_unknown_like() {
            Type::Unknown
        } else if let Some(result) = Self::join_numeric_types(lhs_ty, rhs_ty) {
            result
        } else {
            self.diagnostic(
                "E0001",
                format!(
                    "binary operands have different types: {} and {}",
                    lhs_ty.display(self.hir),
                    rhs_ty.display(self.hir)
                ),
                span,
            );
            Type::Error
        }
    }

    fn check_remainder_types(
        &mut self,
        types: (&Type, &Type),
        span: Option<rowan::TextRange>,
    ) -> Type {
        let (lhs_ty, rhs_ty) = types;
        if !lhs_ty.is_unknown_like()
            && !rhs_ty.is_unknown_like()
            && (!lhs_ty.is_integer() || !rhs_ty.is_integer())
        {
            self.diagnostic(
                "E0003",
                format!(
                    "remainder requires integer operands, got {} and {}",
                    lhs_ty.display(self.hir),
                    rhs_ty.display(self.hir)
                ),
                span,
            );
            return Type::Error;
        }
        Self::join_numeric_types(lhs_ty, rhs_ty).unwrap_or_else(|| lhs_ty.clone())
    }

    fn check_bitwise_types(
        &mut self,
        types: (&Type, &Type),
        span: Option<rowan::TextRange>,
    ) -> Type {
        let (lhs_ty, rhs_ty) = types;
        if !lhs_ty.is_unknown_like()
            && !rhs_ty.is_unknown_like()
            && (!lhs_ty.is_bitwise_scalar() || !rhs_ty.is_bitwise_scalar())
        {
            self.diagnostic(
                "E0003",
                format!(
                    "bitwise operation requires integer or bool operands, got {} and {}",
                    lhs_ty.display(self.hir),
                    rhs_ty.display(self.hir)
                ),
                span,
            );
            return Type::Error;
        }
        if lhs_ty == &Type::Bool && rhs_ty == &Type::Bool {
            Type::Bool
        } else if let Some(result) = Self::join_numeric_types(lhs_ty, rhs_ty) {
            result
        } else {
            self.diagnostic(
                "E0001",
                format!(
                    "bitwise operands have different types: {} and {}",
                    lhs_ty.display(self.hir),
                    rhs_ty.display(self.hir)
                ),
                span,
            );
            Type::Error
        }
    }

    fn check_shift_types(&mut self, types: (&Type, &Type), span: Option<rowan::TextRange>) -> Type {
        let (lhs_ty, rhs_ty) = types;
        if !lhs_ty.is_unknown_like()
            && !rhs_ty.is_unknown_like()
            && (!lhs_ty.is_integer() || !rhs_ty.is_integer())
        {
            self.diagnostic(
                "E0003",
                format!(
                    "shift operation requires integer operands, got {} and {}",
                    lhs_ty.display(self.hir),
                    rhs_ty.display(self.hir)
                ),
                span,
            );
            return Type::Error;
        }
        lhs_ty.clone()
    }

    fn check_equality_types(
        &mut self,
        ctx: &BodyCtx<'_>,
        types: (&Type, &Type),
        span: Option<rowan::TextRange>,
    ) -> Type {
        let (lhs_ty, rhs_ty) = types;
        if Self::is_builtin_equality(lhs_ty, rhs_ty)
            && Self::join_numeric_types(lhs_ty, rhs_ty).is_none()
        {
            self.expect_assignable(lhs_ty, rhs_ty, "comparison", span);
        }
        if !Self::is_builtin_equality(lhs_ty, rhs_ty)
            && !lhs_ty.is_unknown_like()
            && !rhs_ty.is_unknown_like()
            && !self.type_has_lang_trait_with_args(
                ctx,
                lhs_ty,
                std::slice::from_ref(rhs_ty),
                LangItem::PartialEq,
            )
        {
            self.diagnostic(
                "E0036",
                format!(
                    "type `{}` must implement `PartialEq` for equality comparison",
                    lhs_ty.display(self.hir)
                ),
                span,
            );
        }
        Type::Bool
    }

    fn check_ordering_types(
        &mut self,
        ctx: &BodyCtx<'_>,
        types: (&Type, &Type),
        span: Option<rowan::TextRange>,
    ) -> Type {
        let (lhs_ty, rhs_ty) = types;
        if Self::is_builtin_ordering(lhs_ty, rhs_ty) {
            if *lhs_ty != Type::Char && Self::join_numeric_types(lhs_ty, rhs_ty).is_none() {
                self.expect_assignable(lhs_ty, rhs_ty, "comparison", span);
            }
        } else if !lhs_ty.is_unknown_like()
            && !rhs_ty.is_unknown_like()
            && !self.type_has_lang_trait_with_args(
                ctx,
                lhs_ty,
                std::slice::from_ref(rhs_ty),
                LangItem::PartialOrd,
            )
        {
            self.diagnostic(
                "E0003",
                format!(
                    "ordered comparison requires compatible numeric or char operands or `PartialOrd`, got {} and {}",
                    lhs_ty.display(self.hir),
                    rhs_ty.display(self.hir)
                ),
                span,
            );
            return Type::Error;
        }
        Type::Bool
    }

    fn check_logical_types(
        &mut self,
        ctx: &BodyCtx<'_>,
        operands: (ExprId, ExprId),
        types: (&Type, &Type),
    ) -> Type {
        let (lhs, rhs) = operands;
        let (lhs_ty, rhs_ty) = types;
        self.expect_assignable(&Type::Bool, lhs_ty, "left operand", ctx.expr_range(lhs));
        self.expect_assignable(&Type::Bool, rhs_ty, "right operand", ctx.expr_range(rhs));
        Type::Bool
    }

    pub(super) fn check_unary(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        operand: ExprId,
        op: UnaryOp,
        expected: Option<&Type>,
        span: Option<rowan::TextRange>,
    ) -> Type {
        let negated_literal = match &ctx.body.exprs[operand] {
            Expr::IntLiteral { value, suffix } if op == UnaryOp::Neg => {
                Some((*value, suffix.clone()))
            }
            _ => None,
        };
        let operand_ty = if let Some((value, suffix)) = negated_literal {
            let ty = self.check_integer_literal(
                value,
                suffix.as_deref(),
                expected,
                ctx.expr_range(operand),
                true,
            );
            self.result
                .expr_types
                .insert((ctx.body_id, operand), ty.clone());
            ty
        } else {
            match (op, expected) {
                (UnaryOp::Ref, Some(Type::Ref(inner, false)))
                | (UnaryOp::MutRef, Some(Type::Ref(inner, true))) => {
                    self.check_expr_inner(ctx, operand, Some(inner))
                }
                (UnaryOp::Ref | UnaryOp::MutRef, _) => self.check_place_expr(ctx, operand),
                (UnaryOp::Neg | UnaryOp::Pos, Some(expected)) if expected.is_numeric() => {
                    self.check_expr_expected(ctx, operand, expected)
                }
                _ => self.check_expr(ctx, operand),
            }
        };
        if matches!(op, UnaryOp::Neg | UnaryOp::Not)
            && !operand_ty.is_numeric()
            && !operand_ty.is_bitwise_scalar()
            && !operand_ty.is_unknown_like()
            && let Some(ty) =
                self.check_overloaded_unary(ctx, expr_id, operand, op, &operand_ty, span)
        {
            return ty;
        }
        self.record_value_use(
            ctx,
            operand,
            match op {
                UnaryOp::Ref | UnaryOp::Deref => ValueUse::Shared,
                UnaryOp::MutRef => ValueUse::Mutable,
                UnaryOp::Neg | UnaryOp::Pos | UnaryOp::Not => ValueUse::Copy,
            },
        );
        match op {
            UnaryOp::Neg | UnaryOp::Pos => {
                self.expect_numeric(&operand_ty, "unary operand", ctx.expr_range(operand));
                operand_ty
            }
            UnaryOp::Not => {
                if operand_ty.is_unknown_like() || operand_ty.is_bitwise_scalar() {
                    operand_ty
                } else {
                    self.diagnostic(
                        "E0003",
                        format!(
                            "unary `!` requires a bool or integer operand, got {}",
                            operand_ty.display(self.hir)
                        ),
                        ctx.expr_range(operand),
                    );
                    Type::Error
                }
            }
            UnaryOp::Ref => Type::Ref(
                Box::new(match operand_ty {
                    Type::OwnedDynTrait {
                        trait_id,
                        args,
                        assoc_bindings,
                    } => Type::DynTrait {
                        trait_id,
                        args,
                        assoc_bindings,
                    },
                    ty => ty,
                }),
                false,
            ),
            UnaryOp::MutRef => {
                self.check_assign_mut(ctx, operand, ctx.expr_range(operand));
                Type::Ref(
                    Box::new(match operand_ty {
                        Type::OwnedDynTrait {
                            trait_id,
                            args,
                            assoc_bindings,
                        } => Type::DynTrait {
                            trait_id,
                            args,
                            assoc_bindings,
                        },
                        ty => ty,
                    }),
                    true,
                )
            }
            UnaryOp::Deref => match &operand_ty {
                Type::Ref(inner, _) => *inner.clone(),
                Type::Ptr { inner, .. } => {
                    self.require_unsafe(ctx, "dereferencing a raw pointer", span);
                    *inner.clone()
                }
                Type::Unknown | Type::Error => operand_ty,
                other => {
                    self.diagnostic(
                        "E0008",
                        format!(
                            "cannot dereference value of type {}",
                            other.display(self.hir)
                        ),
                        ctx.expr_range(operand),
                    );
                    Type::Error
                }
            },
        }
    }

    pub(super) fn check_integer_literal(
        &mut self,
        value: u64,
        suffix: Option<&str>,
        expected: Option<&Type>,
        span: Option<rowan::TextRange>,
        negative: bool,
    ) -> Type {
        let ty = self.int_literal_type(suffix, expected, span);
        let int_ty = match ty {
            Type::Int(ty) => ty,
            Type::InferInt => IntTy::I32,
            _ => return ty,
        };
        let valid = if negative {
            int_ty.contains_negative_magnitude(value, self.hir.pointer_width_bits)
        } else {
            int_ty.contains_u64(value, self.hir.pointer_width_bits)
        };
        if !valid {
            let value = if negative {
                format!("-{value}")
            } else {
                value.to_string()
            };
            self.diagnostic(
                "E0011",
                format!(
                    "integer literal `{value}` is out of range for `{}`",
                    int_ty.as_str()
                ),
                span,
            );
        }
        ty
    }
}
