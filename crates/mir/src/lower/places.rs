use super::{
    Body, Builder, CapturePlace, CaptureSource, DropProjection, DropSlot, Expr, ExprId, HirUnOp,
    IntTy, LetPatternInput, LetSource, LetStorage, LowerCtx, PatId, Pattern, PatternBindingId,
    PatternBindingMode, Projection, ResolvedName, Stmt, StmtId, Type, TypePattern, UnOp, Value,
    let_pattern_bindings, resolve_field_index,
};

impl LowerCtx<'_> {
    pub(super) fn resolve_field_index(&self, base: ExprId, field_name: &hir::Name) -> usize {
        let Some(body_id) = self.current_body else {
            return 0;
        };
        resolve_field_index(self.hir, self.type_result, body_id, base, field_name)
    }

    pub(super) fn capture_place_from_expr(
        &self,
        body: &Body,
        expr_id: ExprId,
    ) -> Option<CapturePlace> {
        match &body.exprs[expr_id] {
            Expr::Path {
                resolved: Some(resolved),
                ..
            } => {
                let source = match resolved {
                    ResolvedName::PatternBinding(id) => CaptureSource::Pattern(*id),
                    ResolvedName::Param(index) => CaptureSource::Param(*index),
                    ResolvedName::LambdaParam { lambda, index } => CaptureSource::LambdaParam {
                        lambda: *lambda,
                        index: *index,
                    },
                    _ => return None,
                };
                Some(CapturePlace::root(source))
            }
            Expr::FieldAccess { base, field } => {
                let mut place = self.capture_place_from_expr(body, *base)?;
                let base_ty = self
                    .current_body
                    .and_then(|body_id| self.type_result.expr_types.get(&(body_id, *base)));
                if matches!(
                    base_ty,
                    Some(type_checker::Type::Struct(..) | type_checker::Type::Tuple(_))
                ) {
                    place
                        .projections
                        .push(Projection::Field(self.resolve_field_index(*base, field)));
                }
                Some(place)
            }
            Expr::IndexAccess { base, index } => {
                let mut place = self.capture_place_from_expr(body, *base)?;
                if let Expr::IntLiteral { value, .. } = body.exprs[*index]
                    && let Ok(index) = usize::try_from(value)
                {
                    place.projections.push(Projection::Index(Some(index)));
                }
                Some(place)
            }
            Expr::Unary {
                operand,
                op: HirUnOp::Deref,
            } => self.capture_place_from_expr(body, *operand),
            _ => None,
        }
    }

    /// Resolve a path as an lvalue (storage location) without loading.
    /// For mut bindings: returns the alloca pointer directly.
    /// For non-mut bindings / params: returns the value as-is (SSA values are
    /// immutable, so treating them as both value and location is safe).
    pub(super) fn lower_lvalue(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        expr_id: ExprId,
    ) -> Value {
        if let Expr::IndexAccess { base, index } = &body.exprs[expr_id]
            && let Some(place) =
                self.lower_trait_index_place(builder, param_values, body, expr_id, *base, *index)
        {
            return place;
        }
        if let Some(access) = self
            .capture_place_from_expr(body, expr_id)
            .and_then(|place| self.capture_access_for_place(builder, &place))
            && self
                .current_body
                .and_then(|body_id| self.type_result.expr_types.get(&(body_id, expr_id)))
                .map(|ty| self.convert_type(ty))
                .is_some_and(|ty| ty == access.ty)
        {
            return access.place;
        }
        let expr = &body.exprs[expr_id];
        match expr {
            Expr::Path { resolved, .. } => match resolved {
                Some(ResolvedName::Param(idx)) => self
                    .parameter_place(builder, &CaptureSource::Param(*idx))
                    .or_else(|| param_values.get(*idx).copied())
                    .unwrap_or_else(|| builder.unit_const()),
                Some(ResolvedName::LambdaParam { lambda, index }) => {
                    let source = CaptureSource::LambdaParam {
                        lambda: *lambda,
                        index: *index,
                    };
                    self.parameter_place(builder, &source)
                        .or_else(|| {
                            (self.current_lambda == Some(*lambda))
                                .then(|| param_values.get(*index).copied())
                                .flatten()
                        })
                        .unwrap_or_else(|| builder.unit_const())
                }
                Some(ResolvedName::PatternBinding(id)) => self
                    .binding_place(builder, *id)
                    .or_else(|| self.scope_map.get(id).copied())
                    .unwrap_or_else(|| builder.unit_const()),
                _ => self.materialize_temporary_place(builder, param_values, body, expr_id),
            },
            Expr::IndexAccess { base, index } => {
                let base_val = self.lower_place_base(builder, param_values, body, *base);
                let index_val = self.lower_expr(builder, param_values, body, *index);
                let mir_type = self
                    .current_body
                    .and_then(|bid| self.type_result.expr_types.get(&(bid, expr_id)))
                    .map_or(Type::Unit, |t| self.convert_type(t));
                if let Some(len) = self.index_len(builder, base_val, *base) {
                    builder.checked_index_ptr(base_val, index_val, len, mir_type)
                } else {
                    builder.index_ptr(base_val, index_val, mir_type)
                }
            }
            Expr::FieldAccess { base, field } => {
                let base_val = self.lower_place_base(builder, param_values, body, *base);
                let field_idx = self.resolve_field_index(*base, field);
                let field_ty = self
                    .current_body
                    .and_then(|bid| self.type_result.expr_types.get(&(bid, expr_id)))
                    .map_or(Type::Unit, |t| self.convert_type(t));
                builder.field_ptr(base_val, field_idx, field_ty)
            }
            Expr::Unary {
                operand,
                op: HirUnOp::Deref,
            } => self.lower_expr(builder, param_values, body, *operand),
            _ => self.materialize_temporary_place(builder, param_values, body, expr_id),
        }
    }

    pub(super) fn materialize_temporary_place(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        expr_id: ExprId,
    ) -> Value {
        let tc_ty = self
            .current_body
            .and_then(|body_id| self.type_result.expr_types.get(&(body_id, expr_id)))
            .cloned()
            .unwrap_or(type_checker::Type::Unknown);
        let ty = self.convert_expr_type(body, expr_id, &tc_ty);
        let value = self.lower_expr(builder, param_values, body, expr_id);
        let extends_to_block = self
            .current_body
            .is_some_and(|body_id| self.analysis.temporary_escapes(body_id, expr_id));
        let place = if extends_to_block {
            self.reference_storage(builder, ty)
        } else {
            builder.alloca(ty)
        };
        builder.store(value, place);
        self.register_temporary_drop_place(builder, expr_id, place, &tc_ty, extends_to_block);
        place
    }

    pub(super) fn expression_requires_temporary_place(&self, body: &Body, expr_id: ExprId) -> bool {
        self.capture_place_from_expr(body, expr_id).is_none()
            && self.temporary_drop_place_from_expr(body, expr_id).is_none()
            && self
                .current_body
                .and_then(|body_id| self.type_result.expr_types.get(&(body_id, expr_id)))
                .is_some_and(|ty| self.type_needs_drop(ty, 0))
    }

    pub(super) fn register_discarded_temporary(
        &mut self,
        builder: &mut Builder,
        expr_id: ExprId,
        value: Value,
    ) {
        let Some(tc_ty) = self
            .current_body
            .and_then(|body_id| self.type_result.expr_types.get(&(body_id, expr_id)))
            .cloned()
        else {
            return;
        };
        if !self.type_needs_drop(&tc_ty, 0) {
            return;
        }
        let place = builder.alloca(self.convert_type(&tc_ty));
        builder.store(value, place);
        self.register_temporary_drop_place(builder, expr_id, place, &tc_ty, false);
    }

    pub(super) fn register_temporary_drop_place(
        &mut self,
        builder: &mut Builder,
        expr_id: ExprId,
        place: Value,
        ty: &type_checker::Type,
        extends_to_block: bool,
    ) {
        if !self.type_needs_drop(ty, 0) {
            return;
        }
        let slots = self.create_drop_slots(builder, place, ty, Vec::new());
        self.temporary_drop_slots.insert(expr_id, slots.clone());
        let target = if extends_to_block {
            self.drop_scopes.last_mut()
        } else {
            self.temporary_drop_scopes
                .last_mut()
                .or_else(|| self.drop_scopes.last_mut())
        };
        if let Some(scope) = target {
            scope.extend(slots.into_iter().rev());
        }
    }

    pub(super) fn lower_place_base(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        base: ExprId,
    ) -> Value {
        let indirect = self
            .current_body
            .and_then(|body_id| self.type_result.expr_types.get(&(body_id, base)))
            .is_some_and(|ty| {
                matches!(
                    ty,
                    type_checker::Type::Ref(..) | type_checker::Type::Ptr { .. }
                )
            });
        if indirect {
            self.lower_expr(builder, param_values, body, base)
        } else {
            self.lower_lvalue(builder, param_values, body, base)
        }
    }

    // 语句降级

    pub(super) fn lower_stmt(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        stmt_id: StmtId,
    ) {
        let stmt = &body.stmts[stmt_id];
        match stmt {
            Stmt::Let { .. } => self.lower_let_stmt(builder, param_values, body, stmt),
            Stmt::Expr { expr } => {
                self.temporary_drop_scopes.push(Vec::new());
                let value = self.lower_expr(builder, param_values, body, *expr);
                if builder.needs_return() {
                    self.register_discarded_temporary(builder, *expr, value);
                    self.emit_current_temporary_drop_scope(builder);
                }
                self.temporary_drop_scopes.pop();
            }
            Stmt::Return { value } => {
                self.temporary_drop_scopes.push(Vec::new());
                let rv = value.map(|v| self.lower_expr(builder, param_values, body, v));
                self.emit_temporary_drop_scopes_since(builder, 0);
                self.emit_drop_scopes_since(builder, 0);
                builder.set_return(rv);
                self.temporary_drop_scopes.pop();
            }
            Stmt::Break { value } => {
                let target = *self
                    .loop_targets
                    .last()
                    .expect("break statement outside a checked loop");
                // 先存 break 值再 drop：值本身可能是待 drop 作用域里的局部量
                if let Some(value) = value {
                    let v = self.lower_expr(builder, param_values, body, *value);
                    if let Some(slot) = target.break_slot {
                        builder.store(v, slot);
                    }
                }
                self.emit_temporary_drop_scopes_since(builder, target.temporary_drop_depth);
                self.emit_drop_scopes_since(builder, target.drop_depth);
                builder.set_branch(target.break_block);
            }
            Stmt::Continue => {
                let target = *self
                    .loop_targets
                    .last()
                    .expect("continue statement outside a checked loop");
                self.emit_temporary_drop_scopes_since(builder, target.temporary_drop_depth);
                self.emit_drop_scopes_since(builder, target.drop_depth);
                builder.set_branch(target.continue_block);
            }
            Stmt::Item { .. } => {}
        }
    }

    fn lower_let_stmt(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        stmt: &Stmt,
    ) {
        let Stmt::Let {
            pat,
            init,
            ty,
            else_,
            ..
        } = stmt
        else {
            unreachable!("lower_let_stmt called for a non-let statement");
        };
        self.temporary_drop_scopes.push(Vec::new());
        let storage = self.lower_let_storage(builder, param_values, body, *pat, *init, ty);
        self.scope_map.insert(storage.root, storage.value);

        // let-else 只在模式匹配成功后绑定,先求出匹配条件;不可反驳模式
        // 没有条件,走普通绑定路径(else 块永不执行,不再降级)。
        let condition = else_.and_then(|else_expr| {
            let value = if self.storage_bindings.contains(&storage.root) {
                let ty = self.convert_type(&storage.value_ty);
                builder.load(storage.value, ty)
            } else {
                storage.value
            };
            self.lower_pattern_condition(builder, body, *pat, value, &storage.value_ty)
                .map(|condition| (condition, else_expr))
        });

        let slots = if storage.needs_drop {
            let slots =
                self.create_drop_slots(builder, storage.value, &storage.value_ty, Vec::new());
            // 与延迟绑定同理:匹配块之外值可能未绑定,drop 标记先置
            // inactive,匹配成功(或延迟赋值)后再激活。
            if storage.delayed || condition.is_some() {
                for slot in &slots {
                    let inactive = builder.bconst(false);
                    let flag = Self::drop_slot_flag_place(builder, slot);
                    builder.store(inactive, flag);
                }
            }
            slots
        } else {
            Vec::new()
        };

        let Some((condition, else_expr)) = condition else {
            self.bind_let_storage(builder, body, *pat, *init, &storage, slots);
            if builder.needs_return() {
                self.emit_current_temporary_drop_scope(builder);
            }
            self.temporary_drop_scopes.pop();
            return;
        };

        let match_block = builder.func.new_block_labeled("let_else_match");
        let diverge_block = builder.func.new_block_labeled("let_else_diverge");
        let merge_block = builder.func.new_block_labeled("let_else_merge");
        builder.set_cond_branch(condition, match_block, diverge_block);

        builder.switch_to_block(diverge_block);
        self.lower_expr(builder, param_values, body, else_expr);
        // E0066 已保证 else 块发散,这里只为未终止的情况兜底。
        if builder.needs_return() {
            self.emit_current_temporary_drop_scope(builder);
            builder.set_unreachable();
        }

        builder.switch_to_block(match_block);
        self.bind_let_storage(builder, body, *pat, *init, &storage, slots.clone());
        for slot in &slots {
            let active = builder.bconst(true);
            let flag = Self::drop_slot_flag_place(builder, slot);
            builder.store(active, flag);
        }
        if builder.needs_return() {
            builder.set_branch(merge_block);
        }

        builder.switch_to_block(merge_block);
        if builder.needs_return() {
            self.emit_current_temporary_drop_scope(builder);
        }
        self.temporary_drop_scopes.pop();
    }

    /// 把 let 的存储按模式拆成各个绑定,并为每个绑定登记拥有的 drop
    /// 槽位。let-else 在匹配块里调用它,普通 let 则无条件调用。
    fn bind_let_storage(
        &mut self,
        builder: &mut Builder,
        body: &Body,
        pat: PatId,
        init: Option<ExprId>,
        storage: &LetStorage,
        slots: Vec<DropSlot>,
    ) {
        let mut bound = vec![(storage.root, Vec::new())];
        if !matches!(body.pats[pat], Pattern::Binding { .. }) {
            let source = if self.storage_bindings.contains(&storage.root) {
                LetSource::Place(storage.value)
            } else {
                LetSource::Value(storage.value)
            };
            bound.clear();
            self.bind_let_pattern(
                builder,
                body,
                LetPatternInput {
                    pat,
                    source,
                    value_ty: &storage.value_ty,
                    projection: Vec::new(),
                },
                &mut bound,
            );
        }
        self.disarm_moved_let_source(builder, body, init, &bound);
        for (id, projection) in bound {
            let owned = slots
                .iter()
                .filter(|slot| slot.projection.starts_with(&projection))
                .cloned()
                .collect::<Vec<_>>();
            if !owned.is_empty() {
                self.register_drop_slots(CaptureSource::Pattern(id), &owned);
            }
        }
        if let Some(scope) = self.drop_scopes.last_mut() {
            scope.extend(slots.into_iter().rev());
        }
    }

    /// The tracked place an initializer reads from, with the projection into
    /// that binding's storage: `p` → (`p`, []), `p.a` → (`p`, [Field(0)]).
    fn let_move_source(
        &self,
        body: &Body,
        expr: ExprId,
    ) -> Option<(CaptureSource, Vec<DropProjection>)> {
        let mut projection = Vec::new();
        let mut current = expr;
        loop {
            match &body.exprs[current] {
                Expr::Path {
                    resolved: Some(ResolvedName::PatternBinding(id)),
                    ..
                } => {
                    let id = *id;
                    return self
                        .storage_bindings
                        .contains(&id)
                        .then(|| (CaptureSource::Pattern(id), projection.clone()));
                }
                Expr::FieldAccess { base, field } => {
                    let index = self.resolve_field_index(*base, field);
                    projection.push(DropProjection::Field(index));
                    current = *base;
                }
                _ => return None,
            }
        }
    }

    /// A `let` that moves out of a tracked place takes ownership with it, so
    /// the source's drop slots covering the moved projections must stop running
    /// glue. Without this both owners drop the same buffer, which the C
    /// runtime silently absorbs and a strict allocator reports as a
    /// use-after-free. `match` arms do the same through
    /// `transfer_pattern_drop_flags`.
    fn disarm_moved_let_source(
        &mut self,
        builder: &mut Builder,
        body: &Body,
        init: Option<ExprId>,
        bound: &[(PatternBindingId, Vec<DropProjection>)],
    ) {
        if bound.is_empty() {
            return;
        }
        let Some((source, base)) = init.and_then(|init| self.let_move_source(body, init)) else {
            return;
        };
        let Some(slots) = self.drop_slots.get(&source).cloned() else {
            return;
        };
        let mut flags = Vec::new();
        for (_, projection) in bound {
            let mut moved = base.clone();
            moved.extend(projection.iter().cloned());
            for slot in &slots {
                if slot.projection.starts_with(&moved) || moved.starts_with(&slot.projection) {
                    flags.push(Self::drop_slot_flag_place(builder, slot));
                }
            }
        }
        let inactive = builder.bconst(false);
        for flag in flags {
            builder.store(inactive, flag);
        }
    }

    fn lower_let_storage(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        pat: PatId,
        init: Option<ExprId>,
        ty: &hir::item_tree::HirTypeRef,
    ) -> LetStorage {
        let value_ty = init
            .and_then(|init_expr| {
                self.adjusted_expr_type(init_expr)
                    .cloned()
                    .map(|ty| self.specialize_expr_type(body, init_expr, &ty))
            })
            .or_else(|| {
                self.current_body
                    .and_then(|body_id| self.type_result.pattern_types.get(&(body_id, pat)))
                    .cloned()
            })
            .unwrap_or_else(|| self.lower_hir_type_for_pattern(ty, &self.generic_tc_subst));
        let needs_drop = self.type_needs_drop(&value_ty, 0);
        let delayed = init.is_none();
        let bindings = let_pattern_bindings(body, pat);
        let is_mut = bindings.iter().any(|(_, is_mut)| *is_mut);
        let escapes = matches!(body.pats[pat], Pattern::Binding { .. })
            && self.current_body.is_some_and(|bid| {
                bindings
                    .iter()
                    .any(|(id, _)| id.field.is_none() && self.analysis.escapes(bid, *id))
            });
        let needs_address = self.current_body.is_some_and(|bid| {
            bindings
                .iter()
                .any(|(id, _)| self.analysis.local_needs_address(bid, *id))
        });
        let root = PatternBindingId {
            pattern: pat,
            field: None,
        };
        let value = if escapes {
            let ptr = self.reference_storage(builder, self.let_storage_type(body, init, &value_ty));
            self.initialize_let_storage(builder, param_values, body, init, ptr);
            self.storage_bindings.insert(root);
            ptr
        } else if delayed || is_mut || needs_address || needs_drop {
            let ptr = builder.alloca(self.let_storage_type(body, init, &value_ty));
            self.initialize_let_storage(builder, param_values, body, init, ptr);
            self.storage_bindings.insert(root);
            ptr
        } else if let Some(expr) = init {
            self.lower_expr(builder, param_values, body, expr)
        } else {
            builder.unit_const()
        };
        LetStorage {
            root,
            value,
            value_ty,
            needs_drop,
            delayed,
        }
    }

    fn let_storage_type(
        &self,
        body: &Body,
        init: Option<ExprId>,
        value_ty: &type_checker::Type,
    ) -> Type {
        let Some(expr) = init else {
            return self.convert_type(value_ty);
        };
        let Some(ty) = self.adjusted_expr_type(expr) else {
            return self.convert_type(value_ty);
        };
        self.convert_expr_type(body, expr, ty)
    }

    fn initialize_let_storage(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        init: Option<ExprId>,
        place: Value,
    ) {
        if let Some(expr) = init {
            let value = self.lower_expr(builder, param_values, body, expr);
            builder.store(value, place);
        }
    }

    /// Bind the elements of a destructuring `let`. Non-escaping bindings stay
    /// as projections; an escaping binding gets its own stable storage.
    pub(super) fn bind_let_pattern(
        &mut self,
        builder: &mut Builder,
        body: &Body,
        input: LetPatternInput<'_>,
        bound: &mut Vec<(PatternBindingId, Vec<DropProjection>)>,
    ) {
        let LetPatternInput {
            pat,
            source,
            value_ty,
            projection,
        } = input;
        let (source, value_ty) = self.adjust_let_pattern_source(builder, pat, source, value_ty);
        let value_ty = &value_ty;
        match body.pats[pat].clone() {
            Pattern::Binding { .. } => self.bind_let_element(
                builder,
                body,
                PatternBindingId {
                    pattern: pat,
                    field: None,
                },
                source,
                value_ty,
                projection,
                bound,
            ),
            Pattern::Reference { pattern, .. } => {
                let type_checker::Type::Ref(inner, _) = value_ty else {
                    return;
                };
                let reference = match source {
                    LetSource::Place(place) => builder.load(place, self.convert_type(value_ty)),
                    LetSource::Value(value) => value,
                };
                let inner_value = builder.load(reference, self.convert_type(inner));
                self.bind_let_pattern(
                    builder,
                    body,
                    LetPatternInput {
                        pat: pattern,
                        source: LetSource::Value(inner_value),
                        value_ty: inner,
                        projection,
                    },
                    bound,
                );
            }
            Pattern::Tuple { elements } => {
                let type_checker::Type::Tuple(element_types) = value_ty else {
                    return;
                };
                let element_types = element_types.clone();
                for (index, child) in elements.into_iter().enumerate() {
                    let Some(child_ty) = element_types.get(index) else {
                        break;
                    };
                    let child_source = self.project(builder, source, index, child_ty);
                    let mut child_projection = projection.clone();
                    child_projection.push(DropProjection::Field(index));
                    self.bind_let_pattern(
                        builder,
                        body,
                        LetPatternInput {
                            pat: child,
                            source: child_source,
                            value_ty: child_ty,
                            projection: child_projection,
                        },
                        bound,
                    );
                }
            }
            Pattern::Struct { path, fields } => {
                self.bind_let_struct_pattern(
                    builder,
                    body,
                    &LetPatternInput {
                        pat,
                        source,
                        value_ty,
                        projection,
                    },
                    &path,
                    fields,
                    bound,
                );
            }
            Pattern::TupleStruct { path, elements } => {
                // let-else admits refutable patterns, so enum variants do
                // reach `let` even though irrefutable `let` rejects them.
                let name = path.segments.last().map(|name| name.0.as_str());
                let TypePattern::EnumVariant {
                    enum_id,
                    variant_index,
                    args,
                } = self.classify_type_pattern(value_ty, name)
                else {
                    return;
                };
                let payloads = self.enum_variant_payload_types(enum_id, &args, variant_index);
                let offset =
                    Self::enum_payload_offset(&self.hir.item_tree.enums[enum_id], variant_index);
                for (index, child) in elements.into_iter().enumerate() {
                    let Some((_, child_ty)) = payloads.get(index) else {
                        break;
                    };
                    let field_index = 1 + offset + index;
                    let child_source = self.project(builder, source, field_index, child_ty);
                    let mut child_projection = projection.clone();
                    child_projection.push(DropProjection::Field(field_index));
                    self.bind_let_pattern(
                        builder,
                        body,
                        LetPatternInput {
                            pat: child,
                            source: child_source,
                            value_ty: child_ty,
                            projection: child_projection,
                        },
                        bound,
                    );
                }
            }
            // Unit variants and or-patterns introduce no bindings.
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Path { .. } | Pattern::Or { .. } => {
            }
        }
    }

    fn bind_let_struct_pattern(
        &mut self,
        builder: &mut Builder,
        body: &Body,
        input: &LetPatternInput<'_>,
        path: &hir::item_tree::HirPath,
        fields: Vec<hir::body::FieldPat>,
        bound: &mut Vec<(PatternBindingId, Vec<DropProjection>)>,
    ) {
        let (payloads, offset) = if let type_checker::Type::Struct(struct_id, args) = input.value_ty
        {
            (
                self.struct_pattern_field_types(*struct_id, args)
                    .into_iter()
                    .map(|(name, ty)| (Some(name), ty))
                    .collect::<Vec<_>>(),
                0,
            )
        } else {
            let name = path.segments.last().map(|name| name.0.as_str());
            let TypePattern::EnumVariant {
                enum_id,
                variant_index,
                args,
            } = self.classify_type_pattern(input.value_ty, name)
            else {
                return;
            };
            (
                self.enum_variant_payload_types(enum_id, &args, variant_index),
                1 + Self::enum_payload_offset(&self.hir.item_tree.enums[enum_id], variant_index),
            )
        };
        for (binding_index, field) in fields.into_iter().enumerate() {
            let Some((index, (_, child_ty))) = payloads
                .iter()
                .enumerate()
                .find(|(_, (name, _))| name.as_deref() == Some(field.name.0.as_str()))
            else {
                continue;
            };
            let field_index = offset + index;
            let child_ty = child_ty.clone();
            let child_source = self.project(builder, input.source, field_index, &child_ty);
            let mut projection = input.projection.clone();
            projection.push(DropProjection::Field(field_index));
            match field.pat {
                Some(child) => self.bind_let_pattern(
                    builder,
                    body,
                    LetPatternInput {
                        pat: child,
                        source: child_source,
                        value_ty: &child_ty,
                        projection,
                    },
                    bound,
                ),
                None => self.bind_let_element(
                    builder,
                    body,
                    PatternBindingId {
                        pattern: input.pat,
                        field: Some(binding_index),
                    },
                    child_source,
                    &child_ty,
                    projection,
                    bound,
                ),
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn bind_let_element(
        &mut self,
        builder: &mut Builder,
        body: &Body,
        id: PatternBindingId,
        source: LetSource,
        value_ty: &type_checker::Type,
        projection: Vec<DropProjection>,
        bound: &mut Vec<(PatternBindingId, Vec<DropProjection>)>,
    ) {
        let mode = self.pattern_binding_mode(id);
        let binding_ty = self.pattern_binding_type(id, value_ty);
        let escapes = self
            .current_body
            .is_some_and(|body_id| self.analysis.escapes(body_id, id));
        match mode {
            PatternBindingMode::Move => match source {
                LetSource::Place(place) if escapes => {
                    let value = builder.load(place, self.convert_type(&binding_ty));
                    self.bind_let_value(builder, body, id, value, &binding_ty);
                }
                LetSource::Place(place) => {
                    self.storage_bindings.insert(id);
                    self.scope_map.insert(id, place);
                }
                LetSource::Value(value) => {
                    self.bind_let_value(builder, body, id, value, &binding_ty);
                }
            },
            PatternBindingMode::Ref | PatternBindingMode::RefMut => {
                let place = match source {
                    LetSource::Place(place) => place,
                    LetSource::Value(value) => {
                        self.materialize_pattern_place(builder, value, None, value_ty)
                    }
                };
                let op = if mode == PatternBindingMode::RefMut {
                    UnOp::MutRef
                } else {
                    UnOp::Ref
                };
                let value = builder.unop(op, place, self.convert_type(&binding_ty));
                self.bind_let_value(builder, body, id, value, &binding_ty);
            }
        }
        if mode == PatternBindingMode::Move {
            bound.push((id, projection));
        }
    }

    pub(super) fn bind_let_value(
        &mut self,
        builder: &mut Builder,
        body: &Body,
        id: PatternBindingId,
        value: Value,
        value_ty: &type_checker::Type,
    ) {
        let is_mut = id.field.is_none()
            && matches!(body.pats[id.pattern], Pattern::Binding { is_mut: true, .. });
        let escapes = self
            .current_body
            .is_some_and(|body_id| self.analysis.escapes(body_id, id));
        let needs_address = self
            .current_body
            .is_some_and(|body_id| self.analysis.local_needs_address(body_id, id));
        let place = if escapes {
            Some(self.reference_storage(builder, self.convert_type(value_ty)))
        } else if is_mut || needs_address || self.type_needs_drop(value_ty, 0) {
            Some(builder.alloca(self.convert_type(value_ty)))
        } else {
            None
        };
        if let Some(place) = place {
            builder.store(value, place);
            self.storage_bindings.insert(id);
            self.scope_map.insert(id, place);
        } else {
            self.scope_map.insert(id, value);
        }
    }

    pub(super) fn adjust_let_pattern_source(
        &self,
        builder: &mut Builder,
        pat: PatId,
        mut source: LetSource,
        value_ty: &type_checker::Type,
    ) -> (LetSource, type_checker::Type) {
        let target = self.pattern_type(pat).unwrap_or_else(|| value_ty.clone());
        let mut current = value_ty.clone();
        while current != target {
            let type_checker::Type::Ref(inner, _) = &current else {
                break;
            };
            let reference = match source {
                LetSource::Place(place) => builder.load(place, self.convert_type(&current)),
                LetSource::Value(value) => value,
            };
            current = inner.as_ref().clone();
            source = LetSource::Place(reference);
        }
        (source, current)
    }

    pub(super) fn project(
        &self,
        builder: &mut Builder,
        source: LetSource,
        index: usize,
        field_ty: &type_checker::Type,
    ) -> LetSource {
        let mir_ty = self.convert_type(field_ty);
        match source {
            LetSource::Place(place) => LetSource::Place(builder.field_ptr(place, index, mir_ty)),
            LetSource::Value(value) => {
                LetSource::Value(builder.extract_value(value, index, mir_ty))
            }
        }
    }

    pub(super) fn adjusted_expr_type(&self, expr: ExprId) -> Option<&type_checker::Type> {
        let body = self.current_body?;
        self.type_result
            .expr_coercions
            .get(&(body, expr))
            .or_else(|| self.type_result.expr_types.get(&(body, expr)))
    }

    pub(super) fn lower_const_value(
        &mut self,
        builder: &mut Builder,
        const_id: hir::item_tree::ConstId,
    ) -> Value {
        let Some(body_id) = self.hir.const_bodies.get(&const_id).copied() else {
            return builder.unit_const();
        };
        if !self.active_consts.insert(const_id) {
            return builder.unit_const();
        }

        let body = &self.hir.bodies[body_id];
        let old_expr_cache = std::mem::take(&mut self.expr_cache);
        let old_body = self.current_body.replace(body_id);
        let value = self.lower_expr(builder, &[], body, body.root_block);
        self.current_body = old_body;
        self.expr_cache = old_expr_cache;
        self.active_consts.remove(&const_id);
        value
    }

    pub(super) fn index_len(
        &self,
        builder: &mut Builder,
        base: Value,
        expr: ExprId,
    ) -> Option<Value> {
        let ty = self.substitute_tc_type(self.adjusted_expr_type(expr)?);
        let mut ty = &ty;
        while let type_checker::Type::Ref(inner, _) = ty {
            ty = inner;
        }
        match ty {
            type_checker::Type::Array(_, type_checker::ConstArg::Value(len)) => {
                Some(builder.iconst(*len as u64, IntTy::Usize))
            }
            type_checker::Type::Slice(_) => {
                Some(builder.extract_value(base, 1, Type::Int(IntTy::Usize)))
            }
            _ => None,
        }
    }

    // 类型转换

    pub(super) fn substitute_tc_type(&self, ty: &type_checker::Type) -> type_checker::Type {
        use type_checker::{ConstArg as TcConstArg, Type as TcType};

        match ty {
            TcType::Param(name) => self
                .generic_tc_subst
                .get(name)
                .map_or_else(|| ty.clone(), |ty| self.substitute_tc_type(ty)),
            TcType::Ref(inner, mutable) => {
                TcType::Ref(Box::new(self.substitute_tc_type(inner)), *mutable)
            }
            TcType::Ptr { mutable, inner } => TcType::Ptr {
                mutable: *mutable,
                inner: Box::new(self.substitute_tc_type(inner)),
            },
            TcType::Tuple(elements) => TcType::Tuple(
                elements
                    .iter()
                    .map(|element| self.substitute_tc_type(element))
                    .collect(),
            ),
            TcType::Slice(inner) => TcType::Slice(Box::new(self.substitute_tc_type(inner))),
            TcType::Array(inner, len) => {
                let len = match len {
                    TcConstArg::Param(name) => self
                        .generic_tc_subst
                        .get(name)
                        .and_then(|ty| match ty {
                            TcType::Const(value) => Some(value.clone()),
                            _ => None,
                        })
                        .unwrap_or_else(|| len.clone()),
                    _ => len.clone(),
                };
                TcType::Array(Box::new(self.substitute_tc_type(inner)), len)
            }
            TcType::Struct(id, args) => TcType::Struct(
                *id,
                args.iter()
                    .map(|arg| self.substitute_tc_type(arg))
                    .collect(),
            ),
            TcType::Enum(id, args) => TcType::Enum(
                *id,
                args.iter()
                    .map(|arg| self.substitute_tc_type(arg))
                    .collect(),
            ),
            TcType::FunctionItem { function, args } => TcType::FunctionItem {
                function: *function,
                args: args
                    .iter()
                    .map(|arg| self.substitute_tc_type(arg))
                    .collect(),
            },
            TcType::Closure {
                id,
                generics,
                signature,
            } => TcType::Closure {
                id: *id,
                generics: generics.clone(),
                signature: type_checker::CallableSignature {
                    is_unsafe: signature.is_unsafe,
                    kind: signature.kind,
                    params: signature
                        .params
                        .iter()
                        .map(|param| self.substitute_tc_type(param))
                        .collect(),
                    ret: Box::new(self.substitute_tc_type(&signature.ret)),
                },
            },
            TcType::OpaqueCallable { id, signature } => TcType::OpaqueCallable {
                id: *id,
                signature: type_checker::CallableSignature {
                    is_unsafe: signature.is_unsafe,
                    kind: signature.kind,
                    params: signature
                        .params
                        .iter()
                        .map(|param| self.substitute_tc_type(param))
                        .collect(),
                    ret: Box::new(self.substitute_tc_type(&signature.ret)),
                },
            },
            TcType::OpaqueTrait { id, trait_id, args } => TcType::OpaqueTrait {
                id: *id,
                trait_id: *trait_id,
                args: args
                    .iter()
                    .map(|arg| self.substitute_tc_type(arg))
                    .collect(),
            },
            TcType::CallableConstraint(signature) => {
                TcType::CallableConstraint(type_checker::CallableSignature {
                    is_unsafe: signature.is_unsafe,
                    kind: signature.kind,
                    params: signature
                        .params
                        .iter()
                        .map(|param| self.substitute_tc_type(param))
                        .collect(),
                    ret: Box::new(self.substitute_tc_type(&signature.ret)),
                })
            }
            TcType::Const(TcConstArg::Param(name)) => self
                .generic_tc_subst
                .get(name)
                .cloned()
                .unwrap_or_else(|| ty.clone()),
            _ => ty.clone(),
        }
    }
}
