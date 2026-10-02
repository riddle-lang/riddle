use super::{
    BinaryOp, BodyCtx, Expr, ExprId, HirStructField, HirTypeRef, IntTy, LoopCtx, Pattern,
    PatternBindingId, ResolvedName, Stmt, StmtId, StructId, TraitMethodCall, Type, TypeChecker,
    UnaryOp, ValueUse, Visibility, is_supported_cast, is_unsafe_dst_layout_cast,
    struct_field_is_visible_for_owner, substitute_type, type_contains_unresolved_const_param,
    type_ref_contains_error,
};
use ty::types::FloatTy;

impl TypeChecker<'_> {
    pub(super) fn struct_field_is_visible(
        &self,
        ctx: &BodyCtx<'_>,
        struct_id: StructId,
        visibility: &Visibility,
    ) -> bool {
        struct_field_is_visible_for_owner(
            self.hir,
            ctx.owner_range(),
            ctx.function_id,
            ctx.const_id,
            struct_id,
            visibility,
        )
    }

    pub(super) fn check_struct_field_visibility(
        &mut self,
        ctx: &BodyCtx<'_>,
        struct_id: StructId,
        field: &HirStructField,
        span: Option<rowan::TextRange>,
    ) {
        if self.struct_field_is_visible(ctx, struct_id, &field.visibility) {
            return;
        }
        self.diagnostic(
            "E0054",
            format!(
                "field `{}` of struct `{}` is private",
                field.name.0, self.hir.item_tree.structs[struct_id].name.0
            ),
            span,
        );
    }

    pub(crate) fn expr_always_returns(&self, ctx: &BodyCtx<'_>, expr_id: ExprId) -> bool {
        match &ctx.body.exprs[expr_id] {
            Expr::Block { stmts, tail } => {
                stmts
                    .iter()
                    .any(|stmt| self.stmt_always_returns(ctx, *stmt))
                    || tail.is_some_and(|tail| self.expr_always_returns(ctx, tail))
            }
            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.expr_always_returns(ctx, *cond)
                    || else_branch.is_some_and(|else_branch| {
                        self.expr_always_returns(ctx, *then_branch)
                            && self.expr_always_returns(ctx, else_branch)
                    })
            }
            Expr::While { condition, .. } => self.expr_always_returns(ctx, *condition),
            Expr::Unsafe { body } => self.expr_always_returns(ctx, *body),
            _ => self
                .result
                .expr_types
                .get(&(ctx.body_id, expr_id))
                .is_some_and(Type::is_never),
        }
    }

    pub(super) fn stmt_always_returns(&self, ctx: &BodyCtx<'_>, stmt_id: StmtId) -> bool {
        match &ctx.body.stmts[stmt_id] {
            Stmt::Return { .. } | Stmt::Break { .. } | Stmt::Continue => true,
            Stmt::Expr { expr } => self.expr_always_returns(ctx, *expr),
            Stmt::Let { init, .. } => init.is_some_and(|expr| self.expr_always_returns(ctx, expr)),
            Stmt::Item { .. } => false,
        }
    }

    pub(crate) fn check_stmt(&mut self, ctx: &mut BodyCtx<'_>, stmt_id: StmtId) {
        match &ctx.body.stmts[stmt_id] {
            Stmt::Let {
                pat,
                ty,
                ty_range,
                init,
                else_,
            } => {
                let pat = *pat;
                let else_ = *else_;
                let recursive_lambda = init.and_then(|expr| {
                    matches!(ctx.body.pats[pat], Pattern::Binding { .. })
                        .then(|| {
                            self.declared_recursive_lambda_type(
                                ctx,
                                expr,
                                PatternBindingId {
                                    pattern: pat,
                                    field: None,
                                },
                            )
                        })
                        .flatten()
                });
                let declared = if let Some(recursive) = recursive_lambda.clone() {
                    recursive
                } else if init.is_none() && matches!(ty, HirTypeRef::Unknown) {
                    // A delayed binding can infer its type from a later
                    // assignment, just like Rust's `let value; value = ...`.
                    self.fresh_infer()
                } else {
                    self.lower_type_ref_with_params_at(ty, &ctx.generic_params, *ty_range)
                };
                let explicit_error = type_ref_contains_error(ty)
                    || type_contains_unresolved_const_param(&declared, &ctx.generic_params);
                if explicit_error {
                    self.diagnostic("E0034", "invalid type annotation", ctx.stmt_range(stmt_id));
                } else {
                    self.check_type_bounds(ctx, &declared, ctx.stmt_range(stmt_id));
                }
                if let Some(recursive) = recursive_lambda {
                    self.bind_pattern(ctx, pat, &recursive);
                }
                let init_ty = init.map(|expr| {
                    if explicit_error || declared.is_unknown_like() {
                        self.check_expr(ctx, expr)
                    } else {
                        self.check_expr_expected(ctx, expr, &declared)
                    }
                });

                if let Some(else_expr) = else_ {
                    // Checked before the bindings are bound: the else block
                    // runs when the pattern did not match, so it cannot see
                    // them, and it must diverge for the bindings to be
                    // initialized on every path past the statement.
                    let else_ty = self.check_expr(ctx, else_expr);
                    if !matches!(else_ty, Type::Never | Type::Error) {
                        self.diagnostic(
                            "E0066",
                            "`else` block of `let`-`else` must diverge",
                            ctx.expr_range(else_expr),
                        );
                    }
                }
                if let Some(init_ty) = init_ty {
                    if !explicit_error && !declared.is_unknown_like() {
                        self.expect_assignable(
                            &declared,
                            &init_ty,
                            "let initializer",
                            ctx.stmt_range(stmt_id),
                        );
                    }
                    let inferred = declared.is_unknown_like() && !explicit_error;
                    let local_ty = if explicit_error {
                        declared
                    } else {
                        // An unannotated binding defaults its numeric
                        // inference immediately (integers to `i32`, floats
                        // to `f64`): letting `InferInt` linger on the
                        // binding would let any later assignment through
                        // `numeric_assignable` silently truncate the value
                        // into a narrower integer type.
                        match declared.or(init_ty) {
                            Type::InferInt => Type::Int(IntTy::I32),
                            Type::InferFloat => Type::Float(FloatTy::F64),
                            other => other,
                        }
                    };
                    if inferred {
                        self.expect_sized_value(&local_ty, ctx.stmt_range(stmt_id));
                    }
                    self.bind_let_pattern(ctx, pat, &local_ty, stmt_id, true, else_.is_some());
                } else {
                    let delayed_bindings = Self::mark_delayed_pattern(ctx, pat);
                    if matches!(ty, HirTypeRef::Unknown) {
                        for binding in delayed_bindings {
                            self.pending_delayed_bindings.push((
                                ctx.body_id,
                                binding,
                                ctx.pat_range(binding.pattern)
                                    .or_else(|| ctx.stmt_range(stmt_id)),
                            ));
                        }
                    }
                    self.bind_let_pattern(ctx, pat, &declared, stmt_id, false, else_.is_some());
                }
                if let Some(init) = *init {
                    let use_kind = self.pattern_value_use(ctx, pat);
                    self.record_value_use(ctx, init, use_kind);
                }
            }
            Stmt::Expr { expr } => {
                self.check_expr(ctx, *expr);
                self.record_value_use(ctx, *expr, ValueUse::Move);
            }
            Stmt::Return { value } => {
                let expected = ctx.return_ty.clone();
                let actual = value.map_or(Type::Unit, |expr| {
                    self.check_expr_expected(ctx, expr, &expected)
                });
                self.expect_assignable(&expected, &actual, "return value", ctx.stmt_range(stmt_id));
                if let Some(value) = *value {
                    self.record_value_use(ctx, value, ValueUse::Move);
                }
            }
            Stmt::Break { value } => {
                let value = *value;
                if ctx.loop_stack.is_empty() {
                    self.diagnostic(
                        "E0042",
                        "`break` outside of a loop",
                        ctx.stmt_range(stmt_id),
                    );
                    if let Some(value) = value {
                        self.check_expr(ctx, value);
                    }
                } else if !ctx.loop_stack.last().is_some_and(|ctx| ctx.allows_value)
                    && value.is_some()
                {
                    self.diagnostic(
                        "E0065",
                        "`break` with a value is only allowed inside `loop`",
                        ctx.stmt_range(stmt_id),
                    );
                    self.check_expr(ctx, value.expect("checked above"));
                } else {
                    let (ty, range) = match value {
                        Some(value) => {
                            let expected = ctx
                                .loop_stack
                                .last()
                                .and_then(|loop_ctx| loop_ctx.expected_type.clone());
                            let ty = match expected.as_ref() {
                                Some(expected) => self.check_expr_expected(ctx, value, expected),
                                None => self.check_expr(ctx, value),
                            };
                            self.record_value_use(ctx, value, ValueUse::Move);
                            (ty, ctx.expr_range(value))
                        }
                        None => (Type::Unit, ctx.stmt_range(stmt_id)),
                    };
                    if let Some(loop_ctx) = ctx.loop_stack.last_mut() {
                        loop_ctx.break_types.push((ty, range));
                    }
                }
            }
            Stmt::Continue => {
                if ctx.loop_stack.is_empty() {
                    self.diagnostic(
                        "E0042",
                        "`continue` outside of a loop",
                        ctx.stmt_range(stmt_id),
                    );
                }
            }
            Stmt::Item { .. } => {}
        }
    }

    pub(crate) fn check_expr(&mut self, ctx: &mut BodyCtx<'_>, expr_id: ExprId) -> Type {
        let ty = self.check_expr_inner(ctx, expr_id, None);
        self.finish_value_expr(ctx, expr_id, ty)
    }

    pub(crate) fn check_expr_expected(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        expected: &Type,
    ) -> Type {
        let ty = self.check_expr_inner(ctx, expr_id, Some(expected));
        let ty = self.finish_value_expr(ctx, expr_id, ty);
        if Self::is_slice_coercion(expected, &ty)
            || Self::is_dyn_trait_coercion(expected, &ty)
            || Self::is_owned_dyn_trait_coercion(expected, &ty)
        {
            self.result
                .expr_coercions
                .insert((ctx.body_id, expr_id), expected.clone());
        }
        ty
    }

    pub(super) fn check_place_expr(&mut self, ctx: &mut BodyCtx<'_>, expr_id: ExprId) -> Type {
        if matches!(
            &ctx.body.exprs[expr_id],
            Expr::Unary {
                op: UnaryOp::Deref,
                ..
            }
        ) {
            self.check_expr_inner(ctx, expr_id, None)
        } else {
            self.check_expr(ctx, expr_id)
        }
    }

    pub(super) fn finish_value_expr(
        &mut self,
        ctx: &BodyCtx<'_>,
        expr_id: ExprId,
        ty: Type,
    ) -> Type {
        if ty.is_valid_value_type() {
            return ty;
        }
        self.expect_sized_value(&ty, ctx.expr_range(expr_id));
        self.result
            .expr_types
            .insert((ctx.body_id, expr_id), Type::Error);
        Type::Error
    }

    pub(super) fn check_expr_inner(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        expected: Option<&Type>,
    ) -> Type {
        let span = ctx.expr_range(expr_id);
        let ty = match &ctx.body.exprs[expr_id] {
            Expr::Missing => Type::Error,
            Expr::IntLiteral { value, suffix } => {
                self.check_integer_literal(*value, suffix.as_deref(), expected, span, false)
            }
            Expr::FloatLiteral { suffix, .. } => {
                self.float_literal_type(suffix.as_deref(), expected, span)
            }
            Expr::StringLiteral { .. } => Type::Ref(Box::new(Type::Str), false),
            Expr::CharLiteral { .. } => Type::Char,
            Expr::BoolLiteral { .. } => Type::Bool,
            Expr::Path { path, resolved } => path
                .as_single_name()
                .and_then(|name| ctx.bindings.get(&name.0))
                .cloned()
                .unwrap_or_else(|| {
                    if path
                        .as_single_name()
                        .and_then(|name| ctx.generic_params.get(&name.0))
                        .is_some_and(|ty| matches!(ty, Type::Const(_)))
                    {
                        Type::Int(IntTy::Usize)
                    } else if let Some(ResolvedName::EnumVariant(enum_id, _)) = resolved {
                        self.enum_variant_type(ctx, *enum_id, expected, path, span)
                    } else {
                        self.type_of_resolved_name(ctx, resolved.as_ref())
                    }
                }),
            Expr::Struct {
                resolved,
                fields,
                path,
                ..
            } => self.check_struct_expr(
                ctx,
                resolved.as_ref(),
                fields,
                &path.type_args,
                expected,
                span,
            ),
            Expr::Binary { lhs, rhs, op } => {
                self.check_binary(ctx, expr_id, *lhs, *rhs, *op, expected, span)
            }
            Expr::Unary { operand, op } => {
                self.check_unary(ctx, expr_id, *operand, *op, expected, span)
            }
            Expr::Block { stmts, tail } => {
                ctx.push_scope();
                for stmt in stmts {
                    self.check_stmt(ctx, *stmt);
                }
                let ty = tail.map_or(Type::Unit, |expr| match expected {
                    Some(expected) => self.check_expr_expected(ctx, expr, expected),
                    None => self.check_expr(ctx, expr),
                });
                ctx.pop_scope();
                ty
            }
            Expr::If { .. }
            | Expr::While { .. }
            | Expr::Loop { .. }
            | Expr::For { .. }
            | Expr::Match { .. }
            | Expr::Array { .. }
            | Expr::Tuple { .. }
            | Expr::ArrayRepeat { .. } => self.check_control_expr(ctx, expr_id, expected, span),
            Expr::Call { .. }
            | Expr::Lambda { .. }
            | Expr::FieldAccess { .. }
            | Expr::Unsafe { .. }
            | Expr::IndexAccess { .. }
            | Expr::Cast { .. }
            | Expr::Try { .. } => self.check_operation_expr(ctx, expr_id, expected, span),
        };

        let ty = if ty.is_never() || self.expr_always_returns(ctx, expr_id) {
            Type::Never
        } else {
            ty
        };

        self.result
            .expr_types
            .insert((ctx.body_id, expr_id), ty.clone());
        ty
    }

    fn check_control_expr(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        expected: Option<&Type>,
        span: Option<rowan::TextRange>,
    ) -> Type {
        match &ctx.body.exprs[expr_id] {
            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let cond_ty = self.check_expr(ctx, *cond);
                self.expect_assignable(
                    &Type::Bool,
                    &cond_ty,
                    "if condition",
                    ctx.expr_range(*cond),
                );
                self.record_value_use(ctx, *cond, ValueUse::Move);

                let then_ty = match expected {
                    Some(expected) => self.check_expr_expected(ctx, *then_branch, expected),
                    None => self.check_expr(ctx, *then_branch),
                };
                let else_ty = else_branch.map_or(Type::Unit, |expr| match expected {
                    Some(expected) => self.check_expr_expected(ctx, expr, expected),
                    None => self.check_expr(ctx, expr),
                });
                if let Some(expected @ Type::OpaqueCallable { .. }) = expected {
                    self.expect_assignable(expected, &then_ty, "opaque callable return", span);
                    self.expect_assignable(expected, &else_ty, "opaque callable return", span);
                    expected.clone()
                } else {
                    self.join_branch_types(then_ty, else_ty, "if branches", span)
                }
            }
            Expr::While { condition, body } => {
                let condition_ty = self.check_expr(ctx, *condition);
                self.expect_assignable(
                    &Type::Bool,
                    &condition_ty,
                    "while condition",
                    ctx.expr_range(*condition),
                );
                self.record_value_use(ctx, *condition, ValueUse::Move);
                ctx.loop_stack.push(LoopCtx {
                    allows_value: false,
                    expected_type: None,
                    break_types: Vec::new(),
                });
                self.check_expr(ctx, *body);
                ctx.loop_stack.pop();
                self.record_value_use(ctx, *body, ValueUse::Move);
                Type::Unit
            }
            Expr::Loop { body } => {
                ctx.loop_stack.push(LoopCtx {
                    allows_value: true,
                    expected_type: expected.cloned(),
                    break_types: Vec::new(),
                });
                self.check_expr(ctx, *body);
                let loop_ctx = ctx.loop_stack.pop().expect("loop context must be present");
                self.record_value_use(ctx, *body, ValueUse::Move);
                let mut result = None;
                for (ty, range) in loop_ctx.break_types {
                    result = Some(match result {
                        None => ty,
                        Some(acc) => self.join_branch_types(acc, ty, "break values", range),
                    });
                }
                result.unwrap_or(Type::Never)
            }
            Expr::For {
                pat,
                iterable,
                body,
            } => self.check_for(ctx, expr_id, *pat, *iterable, *body, span),
            Expr::Match { scrutinee, arms } => {
                self.check_match(ctx, *scrutinee, arms, expected, span)
            }
            Expr::Array { elements } => self.check_array(ctx, elements, expected, span),
            Expr::Tuple { elements } => {
                let expected = match expected {
                    Some(Type::Tuple(elements)) => Some(elements.as_slice()),
                    _ => None,
                };
                let types = elements
                    .iter()
                    .enumerate()
                    .map(
                        |(index, expr)| match expected.and_then(|types| types.get(index)) {
                            Some(expected) => self.check_expr_expected(ctx, *expr, expected),
                            None => self.check_expr(ctx, *expr),
                        },
                    )
                    .collect();
                for element in elements {
                    self.record_value_use(ctx, *element, ValueUse::Move);
                }
                Type::Tuple(types)
            }
            Expr::ArrayRepeat { value, len } => {
                self.check_array_repeat(ctx, *value, *len, expected, span)
            }
            _ => unreachable!("control expression expected"),
        }
    }

    fn check_operation_expr(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        expected: Option<&Type>,
        span: Option<rowan::TextRange>,
    ) -> Type {
        match &ctx.body.exprs[expr_id] {
            Expr::Call {
                callee,
                args,
                type_args,
            } => self.check_call(ctx, expr_id, *callee, args, type_args, expected),
            Expr::Lambda {
                is_move,
                generics,
                generic_bounds,
                params,
                ret_type,
                ret_type_range,
                body,
            } => self.check_lambda(
                ctx,
                expr_id,
                *is_move,
                generics,
                generic_bounds,
                params,
                ret_type,
                *ret_type_range,
                *body,
                expected,
            ),
            Expr::FieldAccess { base, field } => {
                self.check_field_access(ctx, *base, field, expected, span)
            }
            Expr::Unsafe { body } => {
                ctx.unsafe_depth += 1;
                let ty = match expected {
                    Some(expected) => self.check_expr_expected(ctx, *body, expected),
                    None => self.check_expr(ctx, *body),
                };
                ctx.unsafe_depth -= 1;
                ty
            }
            Expr::IndexAccess { base, index } => {
                self.check_index_access(ctx, expr_id, *base, *index, expected, span)
            }
            Expr::Cast { base, target } => {
                let source_ty = self.check_expr(ctx, *base);
                // A cast reads its operand's value; it never consumes it.
                // Every other supported cast works on scalars and raw
                // pointers, which are `Copy` — so only a reference operand can
                // make this distinction observable, and recording a move for
                // one ended the reference's life: `let p = r as *const i32;`
                // followed by `*r = 5` reported `E0100: use of moved value`.
                let resolved_source = self.resolve_type(&source_ty);
                self.record_value_use(
                    ctx,
                    *base,
                    if matches!(resolved_source, Type::Ref(..)) {
                        ValueUse::Copy
                    } else {
                        ValueUse::Move
                    },
                );
                let target_ty =
                    self.lower_type_ref_with_params_at(target, &ctx.generic_params, span);
                if is_unsafe_dst_layout_cast(&source_ty, &target_ty) {
                    self.require_unsafe(ctx, "performing a DST layout cast", span);
                }
                if let (
                    Type::Ptr {
                        mutable: source_mutable,
                        ..
                    },
                    Type::Ptr {
                        mutable: target_mutable,
                        ..
                    },
                ) = (&source_ty, &target_ty)
                    && *target_mutable
                    && !*source_mutable
                {
                    // Widening `*const T` into `*mut T` grants write access;
                    // the cast itself must sit in `unsafe`, like Rust.
                    self.require_unsafe(ctx, "casting `*const T` to `*mut T`", span);
                }
                if !source_ty.is_unknown_like()
                    && !matches!(target_ty, Type::Error)
                    && !is_supported_cast(&source_ty, &target_ty)
                {
                    self.diagnostic(
                        "E0012",
                        format!(
                            "cannot cast `{}` to `{}`",
                            source_ty.display(self.hir),
                            target_ty.display(self.hir)
                        ),
                        span,
                    );
                }
                target_ty
            }
            Expr::Try { operand } => self.check_try(ctx, expr_id, *operand, expected, span),
            _ => unreachable!("operation expression expected"),
        }
    }

    pub(super) fn check_try(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        operand: ExprId,
        expected: Option<&Type>,
        span: Option<rowan::TextRange>,
    ) -> Type {
        let return_ty = self.resolve_type(&ctx.return_ty);
        let lang_items = &self.result.trait_env.lang_items;
        let result_enum = lang_items.get_enum(ty::lang_items::LangItem::Result);
        let option_enum = lang_items.get_enum(ty::lang_items::LangItem::Option);
        // The registered lang enum decides when std is loaded (a user enum
        // named `Result` cannot hijack `?`); without std, fall back to
        // name-based recognition so local `Result`/`Option` still work.
        let (ret_is_result, ret_is_option) = match &return_ty {
            Type::Enum(enum_id, args) => (
                args.len() == 2 && self.is_lang_enum_or_fallback(result_enum, *enum_id, "Result"),
                args.len() == 1 && self.is_lang_enum_or_fallback(option_enum, *enum_id, "Option"),
            ),
            _ => (false, false),
        };
        let operand_expected = match (expected, &return_ty) {
            (Some(success), Type::Enum(enum_id, _)) if ret_is_result => {
                Some(Type::Enum(*enum_id, vec![success.clone(), Type::Unknown]))
            }
            (Some(success), Type::Enum(enum_id, _)) if ret_is_option => {
                Some(Type::Enum(*enum_id, vec![success.clone()]))
            }
            _ => None,
        };
        let operand_ty = if let Some(expected) = &operand_expected {
            self.check_expr_expected(ctx, operand, expected)
        } else {
            self.check_expr(ctx, operand)
        };
        let operand_ty = self.resolve_type(&operand_ty);
        self.record_value_use(ctx, operand, ValueUse::Move);

        let (operand_is_result, operand_is_option) = match &operand_ty {
            Type::Enum(enum_id, _args) => (
                self.is_lang_enum_or_fallback(result_enum, *enum_id, "Result"),
                self.is_lang_enum_or_fallback(option_enum, *enum_id, "Option"),
            ),
            _ => (false, false),
        };
        let Type::Enum(result_id, result_args) = &operand_ty else {
            self.diagnostic(
                "E0061",
                "`?` requires a Result or Option value as its operand",
                span,
            );
            return Type::Error;
        };
        if !operand_is_option && !operand_is_result {
            self.diagnostic(
                "E0061",
                "`?` operates on the standard `Option`/`Result` enums",
                span,
            );
            return Type::Error;
        }
        if operand_is_option && result_args.len() == 1 {
            let Type::Enum(return_id, return_args) = &return_ty else {
                self.diagnostic(
                    "E0062",
                    "the `?` operator can only be used in a function returning Option",
                    span,
                );
                return result_args[0].clone();
            };
            if *return_id != *result_id || return_args.len() != 1 {
                self.diagnostic(
                    "E0062",
                    "the `?` operator can only be used in a function returning Option",
                    span,
                );
                return result_args[0].clone();
            }
            return result_args[0].clone();
        }
        if !operand_is_result || result_args.len() != 2 {
            self.diagnostic(
                "E0061",
                "`?` requires a Result or Option value as its operand",
                span,
            );
            return Type::Error;
        }

        let return_ty = self.resolve_type(&ctx.return_ty);
        let Type::Enum(return_id, return_args) = &return_ty else {
            self.diagnostic(
                "E0062",
                "the `?` operator can only be used in a function returning Result",
                span,
            );
            return result_args[0].clone();
        };
        if *return_id != *result_id || return_args.len() != 2 {
            self.diagnostic(
                "E0062",
                "the `?` operator can only be used in a function returning Result",
                span,
            );
            return result_args[0].clone();
        }

        let source_error = &result_args[1];
        let target_error = &return_args[1];
        if !Self::bound_types_match(target_error, source_error) {
            self.check_try_error_conversion(ctx, expr_id, source_error, target_error, span);
        }

        result_args[0].clone()
    }

    /// The registered lang enum decides; when that lang item was never
    /// registered (no std), fall back to the enum's name.
    fn is_lang_enum_or_fallback(
        &self,
        registered: Option<hir::item_tree::EnumId>,
        enum_id: hir::item_tree::EnumId,
        name: &str,
    ) -> bool {
        match registered {
            Some(registered) => registered == enum_id,
            None => self.hir.item_tree.enums[enum_id].name.0 == name,
        }
    }

    fn check_try_error_conversion(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        source_error: &Type,
        target_error: &Type,
        span: Option<rowan::TextRange>,
    ) {
        if self.check_try_into_conversion(ctx, expr_id, source_error, target_error, span) {
            return;
        }
        if self.check_try_from_conversion(ctx, expr_id, source_error, target_error, span) {
            return;
        }
        self.report_try_conversion_error(source_error, target_error, span);
    }

    fn check_try_into_conversion(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        source_error: &Type,
        target_error: &Type,
        span: Option<rowan::TextRange>,
    ) -> bool {
        let Some(into_trait) = self.find_trait_by_name("Into") else {
            return false;
        };
        let Some(method) =
            self.find_trait_impl_method(source_error, Some(target_error), None, into_trait, "into")
        else {
            return false;
        };
        let converted = method.function.ret_type.as_ref().map_or(Type::Unit, |ty| {
            substitute_type(
                &self.lower_type_ref_with_params_at(
                    ty,
                    &method.subst,
                    method
                        .function
                        .ret_type_range
                        .or(Some(method.function.name_range)),
                ),
                &method.subst,
            )
        });
        if !Self::bound_types_match(target_error, &converted) {
            self.diagnostic(
                "E0063",
                format!(
                    "`Into::into` returns `{}`, not the enclosing Result error type `{}`",
                    converted.display(self.hir),
                    target_error.display(self.hir)
                ),
                span,
            );
            return true;
        }
        self.result.trait_method_calls.insert(
            (ctx.body_id, expr_id),
            TraitMethodCall {
                trait_id: into_trait,
                method: "into".into(),
                dynamic: false,
            },
        );
        true
    }

    fn check_try_from_conversion(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        source_error: &Type,
        target_error: &Type,
        span: Option<rowan::TextRange>,
    ) -> bool {
        let Some(from_trait) = self.find_trait_by_name("From") else {
            return false;
        };
        let Some(method) =
            self.find_trait_impl_method(target_error, Some(source_error), None, from_trait, "from")
        else {
            return false;
        };
        let converted = method.function.ret_type.as_ref().map_or(Type::Unit, |ty| {
            substitute_type(
                &self.lower_type_ref_with_params_at(
                    ty,
                    &method.subst,
                    method
                        .function
                        .ret_type_range
                        .or(Some(method.function.name_range)),
                ),
                &method.subst,
            )
        });
        if !Self::bound_types_match(target_error, &converted) {
            self.diagnostic(
                "E0063",
                format!(
                    "`From::from` returns `{}`, not the enclosing Result error type `{}`",
                    converted.display(self.hir),
                    target_error.display(self.hir)
                ),
                span,
            );
            return true;
        }
        self.result.trait_method_calls.insert(
            (ctx.body_id, expr_id),
            TraitMethodCall {
                trait_id: from_trait,
                method: "from".into(),
                dynamic: false,
            },
        );
        true
    }

    fn report_try_conversion_error(
        &mut self,
        source_error: &Type,
        target_error: &Type,
        span: Option<rowan::TextRange>,
    ) {
        self.diagnostic(
            "E0063",
            format!(
                "cannot convert `{}` into the enclosing Result error type `{}`",
                source_error.display(self.hir),
                target_error.display(self.hir)
            ),
            span,
        );
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn check_binary(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        lhs: ExprId,
        rhs: ExprId,
        op: BinaryOp,
        expected: Option<&Type>,
        span: Option<rowan::TextRange>,
    ) -> Type {
        if op == BinaryOp::Assign {
            let lhs_ty = self.check_place_expr(ctx, lhs);
            self.expect_sized_value(&lhs_ty, span);
            let rhs_ty = self.check_expr_expected(ctx, rhs, &lhs_ty);
            self.expect_assignable(&lhs_ty, &rhs_ty, "assignment", span);
            self.check_assign_mut(ctx, lhs, span);
            self.record_value_use(ctx, rhs, ValueUse::Move);
            return Type::Unit;
        }

        if let Some(base_op) = op.compound_base() {
            let lhs_ty = self.check_expr(ctx, lhs);
            let rhs_ty = self.check_expr(ctx, rhs);
            if !Self::is_builtin_binary_operator(base_op, &lhs_ty, &rhs_ty)
                && !lhs_ty.is_unknown_like()
                && self
                    .check_overloaded_assign(
                        ctx, expr_id, lhs, rhs, base_op, &lhs_ty, &rhs_ty, span,
                    )
                    .is_some()
            {
                self.check_assign_mut(ctx, lhs, span);
                return Type::Unit;
            }
            let result_ty =
                self.check_binary_types(ctx, (lhs, rhs), base_op, (&lhs_ty, &rhs_ty), span);
            self.expect_assignable(&lhs_ty, &result_ty, "assignment", span);
            self.check_assign_mut(ctx, lhs, span);
            self.record_value_use(ctx, rhs, ValueUse::Move);
            return Type::Unit;
        }

        let lhs_ty = match (op, expected) {
            (BinaryOp::Eq | BinaryOp::Neq, _) => self.check_place_expr(ctx, lhs),
            (
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod,
                Some(expected),
            ) if expected.is_numeric() => self.check_expr_expected(ctx, lhs, expected),
            _ => self.check_expr(ctx, lhs),
        };
        let rhs_ty = match op {
            BinaryOp::Eq | BinaryOp::Neq => self.check_place_expr(ctx, rhs),
            _ => self.check_expr(ctx, rhs),
        };
        let is_overload_candidate = !Self::is_builtin_binary_operator(op, &lhs_ty, &rhs_ty);
        if is_overload_candidate
            && !lhs_ty.is_unknown_like()
            && let Some(ty) = self.check_overloaded_binary(
                ctx, expr_id, lhs, rhs, op, &lhs_ty, &rhs_ty, expected, span,
            )
        {
            return ty;
        }

        if matches!(
            op,
            BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div
                | BinaryOp::Mod
                | BinaryOp::BitAnd
                | BinaryOp::BitOr
                | BinaryOp::BitXor
                | BinaryOp::Eq
                | BinaryOp::Neq
                | BinaryOp::Lt
                | BinaryOp::Gt
                | BinaryOp::LtEq
                | BinaryOp::GtEq
        ) {
            self.unify_types(&lhs_ty, &rhs_ty);
        }
        let lhs_ty = self.resolve_type(&lhs_ty);
        let rhs_ty = self.resolve_type(&rhs_ty);
        let result = self.check_binary_types(ctx, (lhs, rhs), op, (&lhs_ty, &rhs_ty), span);
        self.record_value_use(ctx, lhs, ValueUse::Copy);
        self.record_value_use(ctx, rhs, ValueUse::Copy);
        result
    }
}
