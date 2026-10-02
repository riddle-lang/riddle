use super::{
    BinOp, Body, Builder, CaptureSource, CmpOp, DropProjection, DropSlot, ExprId, HashMap, HashSet,
    Inst, InstKind, IntTy, LiteralPattern, LowerCtx, MatchArm, MatchBindingInput, PatId, Pattern,
    PatternBindingId, PatternBindingMode, PatternBindingValue, Type, TypePattern, UnOp, Value,
    parse_float_suffix, parse_int_suffix,
};

impl LowerCtx<'_> {
    #[allow(clippy::too_many_arguments)]
    pub(super) fn lower_match_expr(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        expr_id: ExprId,
        scrutinee: ExprId,
        arms: &[MatchArm],
        result_ty: Type,
    ) -> Value {
        let scrutinee_value = self.lower_expr(builder, param_values, body, scrutinee);
        let scrutinee_ty = self
            .current_body
            .and_then(|body_id| self.type_result.expr_types.get(&(body_id, scrutinee)))
            .cloned()
            .unwrap_or(type_checker::Type::Unknown);
        let scrutinee_place = self.type_needs_drop(&scrutinee_ty, 0).then(|| {
            let place = builder.alloca(self.convert_type(&scrutinee_ty));
            builder.store(scrutinee_value, place);
            place
        });
        let scrutinee_source = self.drop_place_from_expr(body, scrutinee);
        let merge_block = builder.func.new_block_labeled("match_merge");
        let mut next_test = builder.current_block;
        let mut phi_args = Vec::new();

        for arm in arms {
            builder.switch_to_block(next_test);
            let arm_block = builder.func.new_block_labeled("match_arm");
            let miss_block = builder.func.new_block_labeled("match_next");
            match self.lower_pattern_condition(
                builder,
                body,
                arm.pat,
                scrutinee_value,
                &scrutinee_ty,
            ) {
                Some(condition) => builder.set_cond_branch(condition, arm_block, miss_block),
                None => builder.set_branch(arm_block),
            }

            builder.switch_to_block(arm_block);
            self.push_match_pattern_bindings(
                builder,
                body,
                arm.pat,
                scrutinee_value,
                scrutinee_place,
                &scrutinee_ty,
            );

            if let Some(guard) = arm.guard {
                let guarded_body = builder.func.new_block_labeled("match_guarded_arm");
                self.temporary_drop_scopes.push(Vec::new());
                let guard_value = self.lower_expr(builder, param_values, body, guard);
                if builder.needs_return() {
                    self.emit_current_temporary_drop_scope(builder);
                }
                self.temporary_drop_scopes.pop();
                builder.set_cond_branch(guard_value, guarded_body, miss_block);
                builder.switch_to_block(guarded_body);
            }

            if let Some((source, projection)) = &scrutinee_source {
                self.transfer_pattern_drop_flags(builder, source, projection);
            }

            let pattern_sources = self.push_pattern_drop_scope(
                builder,
                body,
                arm.pat,
                scrutinee_place,
                &scrutinee_ty,
                scrutinee_source.is_none(),
            );
            self.temporary_drop_scopes.push(Vec::new());
            let arm_value = self.lower_expr(builder, param_values, body, arm.body);
            if builder.needs_return() {
                self.emit_current_temporary_drop_scope(builder);
                self.emit_current_drop_scope(builder);
                let arm_exit = builder.current_block;
                builder.set_branch(merge_block);
                phi_args.push((arm_value, arm_exit));
            }
            self.temporary_drop_scopes.pop();
            self.pop_pattern_drop_scope(pattern_sources);
            self.pattern_bindings.pop();
            next_test = miss_block;
        }

        builder.switch_to_block(next_test);
        builder.set_unreachable();
        builder.switch_to_block(merge_block);
        if phi_args.is_empty() {
            builder.unit_const()
        } else {
            let phi = builder.func.push_inst(
                merge_block,
                Inst::new(InstKind::Phi(phi_args), result_ty.clone()),
            );
            if self.current_body.is_some_and(|body_id| {
                self.type_result
                    .expr_coercions
                    .contains_key(&(body_id, expr_id))
            }) {
                self.coerced_values.insert((phi, result_ty));
            }
            phi
        }
    }

    pub(super) fn lower_pattern_condition(
        &mut self,
        builder: &mut Builder,
        body: &Body,
        pat: PatId,
        value: Value,
        value_ty: &type_checker::Type,
    ) -> Option<Value> {
        let (value, _, value_ty) = self.adjust_pattern_value(builder, pat, value, None, value_ty);
        let value_ty = &value_ty;
        let pattern = body.pats[pat].clone();
        match pattern {
            Pattern::Wildcard => None,
            Pattern::Reference { pattern, .. } => {
                let type_checker::Type::Ref(inner, _) = value_ty else {
                    return Some(builder.bconst(false));
                };
                let inner_value = builder.load(value, self.convert_type(inner));
                self.lower_pattern_condition(builder, body, pattern, inner_value, inner)
            }
            Pattern::Binding { name, .. } => {
                let TypePattern::EnumVariant {
                    enum_id,
                    variant_index,
                    args,
                } = self.classify_type_pattern(value_ty, Some(&name.0))
                else {
                    return None;
                };
                Some(Self::lower_variant_tag_condition(
                    builder,
                    value,
                    enum_id,
                    variant_index,
                    &args,
                ))
            }
            Pattern::Struct { ref fields, .. }
                if matches!(value_ty, type_checker::Type::Struct(_, _)) =>
            {
                self.lower_struct_pattern_condition(builder, body, value, value_ty, fields)
            }
            Pattern::Path { .. } | Pattern::TupleStruct { .. } | Pattern::Struct { .. } => {
                self.lower_enum_pattern_condition(builder, body, value, value_ty, pattern)
            }
            Pattern::Literal(literal) => {
                let literal_value = self.lower_literal_pattern(builder, &literal, value_ty);
                Some(builder.cmp(CmpOp::Eq, value, literal_value))
            }
            Pattern::Tuple { ref elements }
                if elements.is_empty() && value_ty == &type_checker::Type::Unit =>
            {
                None
            }
            Pattern::Tuple { elements } => {
                self.lower_tuple_pattern_condition(builder, body, value, value_ty, elements)
            }
            Pattern::Or { alternatives } => {
                let mut condition = builder.bconst(false);
                for alternative in alternatives {
                    let alternative_ty = value_ty.clone();
                    let alt_condition = self.lower_pattern_condition(
                        builder,
                        body,
                        alternative,
                        value,
                        &alternative_ty,
                    )?;
                    condition = builder.binop(BinOp::BitOr, condition, alt_condition, Type::Bool);
                }
                Some(condition)
            }
        }
    }

    fn lower_struct_pattern_condition(
        &mut self,
        builder: &mut Builder,
        body: &Body,
        value: Value,
        value_ty: &type_checker::Type,
        fields: &[hir::body::FieldPat],
    ) -> Option<Value> {
        let type_checker::Type::Struct(struct_id, args) = value_ty else {
            unreachable!();
        };
        let field_types = self.struct_pattern_field_types(*struct_id, args);
        let mut condition = None;
        for field in fields {
            let Some(child) = field.pat else {
                continue;
            };
            let Some((index, (_, child_ty))) = field_types
                .iter()
                .enumerate()
                .find(|(_, (name, _))| *name == field.name.0)
            else {
                continue;
            };
            let child_value = builder.extract_value(value, index, self.convert_type(child_ty));
            let child_condition =
                self.lower_pattern_condition(builder, body, child, child_value, child_ty);
            condition = Self::and_pattern_conditions(builder, condition, child_condition);
        }
        condition
    }

    fn lower_enum_pattern_condition(
        &mut self,
        builder: &mut Builder,
        body: &Body,
        value: Value,
        value_ty: &type_checker::Type,
        pattern: Pattern,
    ) -> Option<Value> {
        let (Pattern::Path { path }
        | Pattern::TupleStruct { path, .. }
        | Pattern::Struct { path, .. }) = &pattern
        else {
            unreachable!();
        };
        let name = path.segments.last().map(|name| name.0.as_str());
        let TypePattern::EnumVariant {
            enum_id,
            variant_index,
            args,
        } = self.classify_type_pattern(value_ty, name)
        else {
            return Some(builder.bconst(false));
        };
        let mut condition = Some(Self::lower_variant_tag_condition(
            builder,
            value,
            enum_id,
            variant_index,
            &args,
        ));
        let payloads = self.enum_variant_payload_types(enum_id, &args, variant_index);
        let offset = Self::enum_payload_offset(&self.hir.item_tree.enums[enum_id], variant_index);

        match pattern {
            Pattern::TupleStruct { elements, .. } => {
                for (index, child) in elements.into_iter().enumerate() {
                    let Some((_, child_ty)) = payloads.get(index) else {
                        break;
                    };
                    condition = self.lower_guarded_enum_payload_condition(
                        builder,
                        body,
                        (condition, value, 1 + offset + index, child, child_ty),
                    );
                }
            }
            Pattern::Struct { fields, .. } => {
                for field in fields {
                    let Some(child) = field.pat else {
                        continue;
                    };
                    let Some((index, (_, child_ty))) = payloads
                        .iter()
                        .enumerate()
                        .find(|(_, (name, _))| name.as_deref() == Some(&field.name.0))
                    else {
                        continue;
                    };
                    condition = self.lower_guarded_enum_payload_condition(
                        builder,
                        body,
                        (condition, value, 1 + offset + index, child, child_ty),
                    );
                }
            }
            Pattern::Path { .. } => {}
            _ => unreachable!(),
        }
        condition
    }

    fn lower_guarded_enum_payload_condition(
        &mut self,
        builder: &mut Builder,
        body: &Body,
        payload: (Option<Value>, Value, usize, PatId, &type_checker::Type),
    ) -> Option<Value> {
        let (condition, value, index, child, child_ty) = payload;
        let Some(condition) = condition else {
            let child_value = builder.extract_value(value, index, self.convert_type(child_ty));
            return self.lower_pattern_condition(builder, body, child, child_value, child_ty);
        };

        let payload_block = builder.func.new_block_labeled("match_pattern_payload");
        let miss_block = builder.func.new_block_labeled("match_pattern_miss");
        let merge_block = builder.func.new_block_labeled("match_pattern_merge");
        builder.set_cond_branch(condition, payload_block, miss_block);

        builder.switch_to_block(payload_block);
        let child_value = builder.extract_value(value, index, self.convert_type(child_ty));
        let matched = self
            .lower_pattern_condition(builder, body, child, child_value, child_ty)
            .unwrap_or_else(|| builder.bconst(true));
        let payload_exit = builder.current_block;
        builder.set_branch(merge_block);

        builder.switch_to_block(miss_block);
        let missed = builder.bconst(false);
        builder.set_branch(merge_block);

        builder.switch_to_block(merge_block);
        Some(builder.func.push_inst(
            merge_block,
            Inst::new(
                InstKind::Phi(vec![(matched, payload_exit), (missed, miss_block)]),
                Type::Bool,
            ),
        ))
    }

    fn lower_tuple_pattern_condition(
        &mut self,
        builder: &mut Builder,
        body: &Body,
        value: Value,
        value_ty: &type_checker::Type,
        elements: Vec<PatId>,
    ) -> Option<Value> {
        let type_checker::Type::Tuple(element_types) = value_ty else {
            return Some(builder.bconst(false));
        };
        let mut condition = None;
        for (index, child) in elements.into_iter().enumerate() {
            let Some(child_ty) = element_types.get(index) else {
                break;
            };
            let child_value = builder.extract_value(value, index, self.convert_type(child_ty));
            let child_condition =
                self.lower_pattern_condition(builder, body, child, child_value, child_ty);
            condition = Self::and_pattern_conditions(builder, condition, child_condition);
        }
        condition
    }

    pub(super) fn lower_variant_tag_condition(
        builder: &mut Builder,
        value: Value,
        _enum_id: hir::item_tree::EnumId,
        variant_index: usize,
        _args: &[type_checker::Type],
    ) -> Value {
        let tag = builder.extract_value(value, 0, Type::Int(IntTy::U32));
        let expected = builder.iconst(variant_index as u64, IntTy::U32);
        builder.cmp(CmpOp::Eq, tag, expected)
    }

    pub(super) fn and_pattern_conditions(
        builder: &mut Builder,
        lhs: Option<Value>,
        rhs: Option<Value>,
    ) -> Option<Value> {
        match (lhs, rhs) {
            (Some(lhs), Some(rhs)) => Some(builder.binop(BinOp::BitAnd, lhs, rhs, Type::Bool)),
            (Some(value), None) | (None, Some(value)) => Some(value),
            (None, None) => None,
        }
    }

    pub(super) fn lower_literal_pattern(
        &self,
        builder: &mut Builder,
        literal: &LiteralPattern,
        expected: &type_checker::Type,
    ) -> Value {
        match literal {
            LiteralPattern::Int { value, suffix, .. } => {
                let ty = match self.convert_type(expected) {
                    Type::Int(ty) => ty,
                    _ => parse_int_suffix(suffix.as_deref()),
                };
                builder.iconst(*value, ty)
            }
            LiteralPattern::Float { value, suffix, .. } => {
                let ty = match self.convert_type(expected) {
                    Type::Float(ty) => ty,
                    _ => parse_float_suffix(suffix.as_deref()),
                };
                builder.fconst(*value, ty)
            }
            LiteralPattern::String(value) => builder.sconst(value.clone()),
            LiteralPattern::Char(value) => builder.char_const(value.chars().next().unwrap_or('\0')),
            LiteralPattern::Bool(value) => builder.bconst(*value),
        }
    }

    pub(super) fn push_match_pattern_bindings(
        &mut self,
        builder: &mut Builder,
        body: &Body,
        pat: PatId,
        value: Value,
        place: Option<Value>,
        value_ty: &type_checker::Type,
    ) {
        let mut scope = HashMap::new();
        self.collect_match_pattern_bindings(
            builder,
            body,
            MatchBindingInput {
                pat,
                value,
                place,
                value_ty,
                projection: Vec::new(),
            },
            &mut scope,
        );
        self.pattern_bindings.push(scope);
    }

    fn collect_match_pattern_bindings(
        &mut self,
        builder: &mut Builder,
        body: &Body,
        input: MatchBindingInput<'_>,
        scope: &mut HashMap<PatternBindingId, PatternBindingValue>,
    ) {
        let MatchBindingInput {
            pat,
            value,
            place,
            value_ty,
            projection,
        } = input;
        let (value, place, value_ty) =
            self.adjust_pattern_value(builder, pat, value, place, value_ty);
        let input = MatchBindingInput {
            pat,
            value,
            place,
            value_ty: &value_ty,
            projection,
        };
        match body.pats[pat].clone() {
            Pattern::Binding { name, .. } => {
                if !matches!(
                    self.classify_type_pattern(input.value_ty, Some(&name.0)),
                    TypePattern::EnumVariant { .. }
                ) {
                    self.insert_match_pattern_binding(
                        builder,
                        PatternBindingId {
                            pattern: pat,
                            field: None,
                        },
                        input,
                        scope,
                    );
                }
            }
            Pattern::Reference { pattern, .. } => {
                let type_checker::Type::Ref(inner, _) = input.value_ty else {
                    return;
                };
                let inner_value = builder.load(input.value, self.convert_type(inner));
                self.collect_match_pattern_bindings(
                    builder,
                    body,
                    MatchBindingInput {
                        pat: pattern,
                        value: inner_value,
                        place: None,
                        value_ty: inner,
                        projection: input.projection,
                    },
                    scope,
                );
            }
            Pattern::Tuple { elements } => {
                self.collect_tuple_pattern_bindings(builder, body, &input, elements, scope);
            }
            Pattern::TupleStruct { path, elements } => self.collect_tuple_struct_pattern_bindings(
                builder, body, &input, &path, elements, scope,
            ),
            Pattern::Struct { path, fields } => {
                self.collect_struct_pattern_bindings(builder, body, &input, &path, fields, scope);
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Path { .. } | Pattern::Or { .. } => {
            }
        }
    }

    fn collect_tuple_pattern_bindings(
        &mut self,
        builder: &mut Builder,
        body: &Body,
        input: &MatchBindingInput<'_>,
        elements: Vec<PatId>,
        scope: &mut HashMap<PatternBindingId, PatternBindingValue>,
    ) {
        let type_checker::Type::Tuple(element_types) = input.value_ty else {
            return;
        };
        for (index, child) in elements.into_iter().enumerate() {
            let Some(child_ty) = element_types.get(index) else {
                break;
            };
            let child_value =
                builder.extract_value(input.value, index, self.convert_type(child_ty));
            let child_place = input
                .place
                .map(|place| builder.field_ptr(place, index, self.convert_type(child_ty)));
            let mut projection = input.projection.clone();
            projection.push(DropProjection::Field(index));
            self.collect_match_pattern_bindings(
                builder,
                body,
                MatchBindingInput {
                    pat: child,
                    value: child_value,
                    place: child_place,
                    value_ty: child_ty,
                    projection,
                },
                scope,
            );
        }
    }

    fn collect_tuple_struct_pattern_bindings(
        &mut self,
        builder: &mut Builder,
        body: &Body,
        input: &MatchBindingInput<'_>,
        path: &hir::item_tree::HirPath,
        elements: Vec<PatId>,
        scope: &mut HashMap<PatternBindingId, PatternBindingValue>,
    ) {
        let name = path.segments.last().map(|name| name.0.as_str());
        let TypePattern::EnumVariant {
            enum_id,
            variant_index,
            args,
        } = self.classify_type_pattern(input.value_ty, name)
        else {
            return;
        };
        let payloads = self.enum_variant_payload_types(enum_id, &args, variant_index);
        let offset = Self::enum_payload_offset(&self.hir.item_tree.enums[enum_id], variant_index);
        for (index, child) in elements.into_iter().enumerate() {
            let Some((_, child_ty)) = payloads.get(index) else {
                break;
            };
            let field_index = 1 + offset + index;
            let child_value =
                builder.extract_value(input.value, field_index, self.convert_type(child_ty));
            let child_place = input
                .place
                .map(|place| builder.field_ptr(place, field_index, self.convert_type(child_ty)));
            let mut projection = input.projection.clone();
            projection.push(DropProjection::Field(field_index));
            self.collect_match_pattern_bindings(
                builder,
                body,
                MatchBindingInput {
                    pat: child,
                    value: child_value,
                    place: child_place,
                    value_ty: child_ty,
                    projection,
                },
                scope,
            );
        }
    }

    fn collect_struct_pattern_bindings(
        &mut self,
        builder: &mut Builder,
        body: &Body,
        input: &MatchBindingInput<'_>,
        path: &hir::item_tree::HirPath,
        fields: Vec<hir::body::FieldPat>,
        scope: &mut HashMap<PatternBindingId, PatternBindingValue>,
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
                .find(|(_, (name, _))| name.as_deref() == Some(&field.name.0))
            else {
                continue;
            };
            let field_index = offset + index;
            let child_value =
                builder.extract_value(input.value, field_index, self.convert_type(child_ty));
            let child_place = input
                .place
                .map(|place| builder.field_ptr(place, field_index, self.convert_type(child_ty)));
            let mut projection = input.projection.clone();
            projection.push(DropProjection::Field(field_index));
            let child_input = MatchBindingInput {
                pat: field.pat.unwrap_or(input.pat),
                value: child_value,
                place: child_place,
                value_ty: child_ty,
                projection,
            };
            if field.pat.is_some() {
                self.collect_match_pattern_bindings(builder, body, child_input, scope);
            } else {
                self.insert_match_pattern_binding(
                    builder,
                    PatternBindingId {
                        pattern: input.pat,
                        field: Some(binding_index),
                    },
                    child_input,
                    scope,
                );
            }
        }
    }

    fn insert_match_pattern_binding(
        &mut self,
        builder: &mut Builder,
        id: PatternBindingId,
        input: MatchBindingInput<'_>,
        scope: &mut HashMap<PatternBindingId, PatternBindingValue>,
    ) {
        let mode = self.pattern_binding_mode(id);
        let binding_ty = self.pattern_binding_type(id, input.value_ty);
        let mir_ty = self.convert_type(&binding_ty);
        let (value, place) = match mode {
            PatternBindingMode::Move => {
                let (value, place) = (input.value, input.place);
                let storage = self.current_body.map(|bid| {
                    let escapes = self.analysis.escapes(bid, id);
                    let needs_address = self.analysis.local_needs_address(bid, id);
                    (escapes, needs_address)
                });
                if place.is_none()
                    && storage.is_some_and(|(escapes, needs_address)| escapes || needs_address)
                {
                    // Address-taken match payload: materialize the storage
                    // at the binding point, which dominates every later use
                    // in the arm. Lazy materialization inside a nested arm
                    // would define the place in a block that sibling arms
                    // never execute, leaving their reads indeterminate.
                    // Like `let` storage, only genuinely escaping values go
                    // to the GC heap; address-taken values stay on the stack.
                    let (escapes, _) = storage.unwrap_or((false, false));
                    let place = if escapes {
                        self.reference_storage(builder, mir_ty.clone())
                    } else {
                        builder.alloca(mir_ty.clone())
                    };
                    builder.store(value, place);
                    self.storage_bindings.insert(id);
                    self.scope_map.insert(id, place);
                    (value, Some(place))
                } else {
                    (value, place)
                }
            }
            PatternBindingMode::Ref | PatternBindingMode::RefMut => {
                let place = self.materialize_pattern_place(
                    builder,
                    input.value,
                    input.place,
                    input.value_ty,
                );
                let op = if mode == PatternBindingMode::RefMut {
                    UnOp::MutRef
                } else {
                    UnOp::Ref
                };
                (builder.unop(op, place, mir_ty.clone()), None)
            }
        };
        scope.insert(
            id,
            PatternBindingValue::direct(value, mir_ty, binding_ty, place, input.projection),
        );
    }

    pub(super) fn adjust_pattern_value(
        &self,
        builder: &mut Builder,
        pat: PatId,
        mut value: Value,
        mut place: Option<Value>,
        value_ty: &type_checker::Type,
    ) -> (Value, Option<Value>, type_checker::Type) {
        let target = self.pattern_type(pat).unwrap_or_else(|| value_ty.clone());
        let mut current = value_ty.clone();
        while current != target {
            let type_checker::Type::Ref(inner, _) = current else {
                break;
            };
            let inner = *inner;
            let reference = value;
            value = builder.load(reference, self.convert_type(&inner));
            place = Some(reference);
            current = inner;
        }
        (value, place, current)
    }

    pub(super) fn materialize_pattern_place(
        &self,
        builder: &mut Builder,
        value: Value,
        place: Option<Value>,
        value_ty: &type_checker::Type,
    ) -> Value {
        place.unwrap_or_else(|| {
            let place = self.reference_storage(builder, self.convert_type(value_ty));
            builder.store(value, place);
            place
        })
    }

    pub(super) fn pattern_type(&self, pat: PatId) -> Option<type_checker::Type> {
        self.current_body
            .and_then(|body_id| self.type_result.pattern_types.get(&(body_id, pat)))
            .cloned()
    }

    pub(super) fn pattern_binding_mode(&self, id: PatternBindingId) -> PatternBindingMode {
        self.current_body
            .and_then(|body_id| self.type_result.pattern_binding_modes.get(&(body_id, id)))
            .copied()
            .unwrap_or(PatternBindingMode::Move)
    }

    pub(super) fn pattern_binding_type(
        &self,
        id: PatternBindingId,
        fallback: &type_checker::Type,
    ) -> type_checker::Type {
        self.current_body
            .and_then(|body_id| self.type_result.pattern_binding_types.get(&(body_id, id)))
            .cloned()
            .unwrap_or_else(|| fallback.clone())
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn push_pattern_drop_scope(
        &mut self,
        builder: &mut Builder,
        body: &Body,
        pat: PatId,
        place: Option<Value>,
        value_ty: &type_checker::Type,
        owns_whole_value: bool,
    ) -> Vec<CaptureSource> {
        let mut slots = place
            .map(|place| self.create_pattern_owner_slots(builder, body, pat, place, value_ty))
            .unwrap_or_default();
        if !owns_whole_value {
            let moved = self.moved_pattern_projections();
            slots.retain(|slot| {
                moved.iter().any(|projection| {
                    slot.projection.starts_with(projection)
                        || projection.starts_with(&slot.projection)
                })
            });
        }
        let mut sources = Vec::new();
        if let Some(bindings) = self.pattern_bindings.last() {
            for (id, binding) in bindings {
                if self.pattern_binding_mode(*id) != PatternBindingMode::Move
                    || !self.type_needs_drop(&binding.tc_ty, 0)
                {
                    continue;
                }
                let binding_slots = slots
                    .iter()
                    .filter(|slot| slot.projection.starts_with(&binding.projection))
                    .cloned()
                    .collect::<Vec<_>>();
                if !binding_slots.is_empty() {
                    let source = CaptureSource::Pattern(*id);
                    self.drop_slots.insert(source.clone(), binding_slots);
                    sources.push(source);
                }
            }
        }
        self.drop_scopes
            .push(slots.into_iter().rev().collect::<Vec<_>>());
        sources
    }

    pub(super) fn moved_pattern_projections(&self) -> Vec<Vec<DropProjection>> {
        self.pattern_bindings
            .last()
            .into_iter()
            .flat_map(|bindings| bindings.iter())
            .filter(|(id, binding)| {
                self.pattern_binding_mode(**id) == PatternBindingMode::Move
                    && !self.type_result.trait_env.type_is_copy(&binding.tc_ty)
            })
            .map(|(_, binding)| binding.projection.clone())
            .collect()
    }

    pub(super) fn transfer_pattern_drop_flags(
        &self,
        builder: &mut Builder,
        source: &CaptureSource,
        base_projection: &[DropProjection],
    ) {
        let moved = self.moved_pattern_projections();
        let slots = self
            .drop_slots
            .get(source)
            .into_iter()
            .flatten()
            .filter(|slot| {
                moved.iter().any(|projection| {
                    let mut full = base_projection.to_vec();
                    full.extend(projection.iter().cloned());
                    slot.projection.starts_with(&full) || full.starts_with(&slot.projection)
                })
            })
            .cloned()
            .collect::<Vec<_>>();
        let flags = slots
            .iter()
            .map(|slot| Self::drop_slot_flag_place(builder, slot))
            .collect::<HashSet<_>>();
        for flag in flags {
            let inactive = builder.bconst(false);
            builder.store(inactive, flag);
        }
    }

    pub(super) fn pop_pattern_drop_scope(&mut self, sources: Vec<CaptureSource>) {
        self.drop_scopes.pop();
        for source in sources {
            self.drop_slots.remove(&source);
        }
    }

    pub(super) fn create_pattern_owner_slots(
        &self,
        builder: &mut Builder,
        body: &Body,
        pat: PatId,
        place: Value,
        value_ty: &type_checker::Type,
    ) -> Vec<DropSlot> {
        let value_ty = self.substitute_tc_type(value_ty);
        if self.type_result.trait_env.type_has_explicit_drop(&value_ty) {
            return self.create_drop_slots(builder, place, &value_ty, Vec::new());
        }
        let variant = match &body.pats[pat] {
            Pattern::TupleStruct { path, .. } | Pattern::Struct { path, .. } => {
                path.segments.last().and_then(|name| {
                    match self.classify_type_pattern(&value_ty, Some(&name.0)) {
                        TypePattern::EnumVariant {
                            enum_id,
                            variant_index,
                            args,
                        } => Some((enum_id, variant_index, args)),
                        TypePattern::Other => None,
                    }
                })
            }
            _ => None,
        };
        let Some((enum_id, variant_index, args)) = variant else {
            return self.create_drop_slots(builder, place, &value_ty, Vec::new());
        };

        let fields = self.enum_variant_payload_types(enum_id, &args, variant_index);
        let offset = Self::enum_payload_offset(&self.hir.item_tree.enums[enum_id], variant_index);
        let mut slots = Vec::new();
        for (index, (_, field_ty)) in fields.into_iter().enumerate() {
            if !self.type_needs_drop(&field_ty, 0) {
                continue;
            }
            let field_index = 1 + offset + index;
            let field_place = builder.field_ptr(place, field_index, self.convert_type(&field_ty));
            slots.extend(self.create_drop_slots(
                builder,
                field_place,
                &field_ty,
                vec![DropProjection::Field(field_index)],
            ));
        }
        slots
    }

    pub(super) fn classify_type_pattern(
        &self,
        value_ty: &type_checker::Type,
        name: Option<&str>,
    ) -> TypePattern {
        let type_checker::Type::Enum(enum_id, args) = value_ty else {
            return TypePattern::Other;
        };
        let Some(name) = name else {
            return TypePattern::Other;
        };
        self.hir.item_tree.enums[*enum_id]
            .variants
            .iter()
            .position(|variant| variant.name.0 == name)
            .map_or(TypePattern::Other, |variant_index| {
                TypePattern::EnumVariant {
                    enum_id: *enum_id,
                    variant_index,
                    args: args.clone(),
                }
            })
    }

    pub(super) fn enum_variant_payload_types(
        &self,
        enum_id: hir::item_tree::EnumId,
        args: &[type_checker::Type],
        variant_index: usize,
    ) -> Vec<(Option<String>, type_checker::Type)> {
        let enum_data = &self.hir.item_tree.enums[enum_id];
        let subst = enum_data
            .generics
            .iter()
            .chain(enum_data.const_generics.iter())
            .zip(args.iter())
            .map(|(name, ty)| (name.0.clone(), ty.clone()))
            .collect::<HashMap<_, _>>();
        let Some(variant) = enum_data.variants.get(variant_index) else {
            return Vec::new();
        };
        match &variant.kind {
            hir::item_tree::HirVariantKind::Unit => Vec::new(),
            hir::item_tree::HirVariantKind::Tuple(items) => items
                .iter()
                .map(|ty| (None, self.lower_hir_type_for_pattern(ty, &subst)))
                .collect(),
            hir::item_tree::HirVariantKind::Struct(items) => items
                .iter()
                .map(|field| {
                    (
                        Some(field.name.0.clone()),
                        self.lower_hir_type_for_pattern(&field.ty, &subst),
                    )
                })
                .collect(),
        }
    }

    pub(super) fn struct_pattern_field_types(
        &self,
        struct_id: hir::item_tree::StructId,
        args: &[type_checker::Type],
    ) -> Vec<(String, type_checker::Type)> {
        let strukt = &self.hir.item_tree.structs[struct_id];
        let subst = strukt
            .generics
            .iter()
            .chain(strukt.const_generics.iter())
            .zip(args.iter())
            .map(|(name, ty)| (name.0.clone(), ty.clone()))
            .collect::<HashMap<_, _>>();
        strukt
            .fields
            .iter()
            .map(|field| {
                (
                    field.name.0.clone(),
                    self.lower_hir_type_for_pattern(&field.ty, &subst),
                )
            })
            .collect()
    }

    pub(super) fn lower_hir_type_for_pattern(
        &self,
        ty: &hir::item_tree::HirTypeRef,
        subst: &HashMap<String, type_checker::Type>,
    ) -> type_checker::Type {
        self.lower_hir_type_for_pattern_mode(ty, subst, true)
    }

    fn lower_hir_type_for_pattern_mode(
        &self,
        ty: &hir::item_tree::HirTypeRef,
        subst: &HashMap<String, type_checker::Type>,
        owned_dyn: bool,
    ) -> type_checker::Type {
        use hir::item_tree::{HirConstArg, HirTypeRef};
        use type_checker::ConstArg;

        match ty {
            HirTypeRef::Never => type_checker::Type::Never,
            HirTypeRef::Named(path) => self.lower_named_hir_type_for_pattern(path, subst),
            HirTypeRef::Ref(inner, mutable) => type_checker::Type::Ref(
                Box::new(self.lower_hir_type_for_pattern_mode(inner, subst, false)),
                *mutable,
            ),
            HirTypeRef::Ptr { mutable, inner } => type_checker::Type::Ptr {
                mutable: *mutable,
                inner: Box::new(self.lower_hir_type_for_pattern_mode(inner, subst, false)),
            },
            HirTypeRef::Tuple(items) if items.is_empty() => type_checker::Type::Unit,
            HirTypeRef::Tuple(items) => type_checker::Type::Tuple(
                items
                    .iter()
                    .map(|item| self.lower_hir_type_for_pattern_mode(item, subst, true))
                    .collect(),
            ),
            HirTypeRef::Slice(inner) => type_checker::Type::Slice(Box::new(
                self.lower_hir_type_for_pattern_mode(inner, subst, true),
            )),
            HirTypeRef::Array(inner, len) => type_checker::Type::Array(
                Box::new(self.lower_hir_type_for_pattern_mode(inner, subst, true)),
                match len {
                    HirConstArg::Value(value) => ConstArg::Value(*value),
                    HirConstArg::Param(name) => match subst.get(&name.0) {
                        Some(type_checker::Type::Const(value)) => value.clone(),
                        _ => ConstArg::Param(name.0.clone()),
                    },
                    HirConstArg::Unknown => ConstArg::Unknown,
                    HirConstArg::Error => ConstArg::Error,
                },
            ),
            HirTypeRef::Const(value) => type_checker::Type::Const(match value {
                HirConstArg::Value(value) => ConstArg::Value(*value),
                HirConstArg::Param(name) => ConstArg::Param(name.0.clone()),
                HirConstArg::Unknown => ConstArg::Unknown,
                HirConstArg::Error => ConstArg::Error,
            }),
            HirTypeRef::ImplTrait { .. } => self.lower_impl_trait_for_pattern(ty, subst),
            HirTypeRef::DynTrait {
                trait_ty,
                callable,
                assoc_constraints,
                ..
            } => {
                if let Some(callable) = callable {
                    let kind = match trait_ty.as_ref() {
                        HirTypeRef::Named(path) => {
                            match path.segments.last().map(|name| name.0.as_str()) {
                                Some("Fn") => type_checker::ClosureKind::Fn,
                                Some("FnMut") => type_checker::ClosureKind::FnMut,
                                Some("FnOnce") => type_checker::ClosureKind::FnOnce,
                                _ => return type_checker::Type::Unknown,
                            }
                        }
                        _ => return type_checker::Type::Unknown,
                    };
                    return type_checker::Type::CallableConstraint(
                        type_checker::CallableSignature {
                            is_unsafe: false,
                            kind,
                            params: callable
                                .params
                                .iter()
                                .map(|param| {
                                    self.lower_hir_type_for_pattern_mode(param, subst, true)
                                })
                                .collect(),
                            ret: Box::new(self.lower_hir_type_for_pattern_mode(
                                &callable.ret,
                                subst,
                                true,
                            )),
                        },
                    );
                }
                let Some(trait_id) = self.resolve_trait_ref(trait_ty) else {
                    return type_checker::Type::Unknown;
                };
                let args = match trait_ty.as_ref() {
                    HirTypeRef::Named(path) => path
                        .type_args
                        .iter()
                        .map(|arg| self.lower_hir_type_for_pattern_mode(arg, subst, true))
                        .collect(),
                    _ => Vec::new(),
                };
                let assoc_bindings = assoc_constraints
                    .iter()
                    .map(|constraint| {
                        (
                            constraint.name.0.clone(),
                            self.lower_hir_type_for_pattern_mode(&constraint.ty, subst, true),
                        )
                    })
                    .collect();
                if owned_dyn {
                    type_checker::Type::OwnedDynTrait {
                        trait_id,
                        args,
                        assoc_bindings,
                    }
                } else {
                    type_checker::Type::DynTrait {
                        trait_id,
                        args,
                        assoc_bindings,
                    }
                }
            }
            HirTypeRef::Unknown => type_checker::Type::Unknown,
            HirTypeRef::Error => type_checker::Type::Error,
        }
    }

    fn lower_named_hir_type_for_pattern(
        &self,
        path: &hir::item_tree::HirPath,
        subst: &HashMap<String, type_checker::Type>,
    ) -> type_checker::Type {
        use type_checker::{FloatTy as TcFloatTy, IntTy as TcIntTy};

        let Some(name) = path.as_single_name().map(|name| name.0.as_str()) else {
            return type_checker::Type::Unknown;
        };
        if let Some(ty) = subst.get(name) {
            return ty.clone();
        }
        match name {
            "i8" => type_checker::Type::Int(TcIntTy::I8),
            "i16" => type_checker::Type::Int(TcIntTy::I16),
            "i32" => type_checker::Type::Int(TcIntTy::I32),
            "i64" => type_checker::Type::Int(TcIntTy::I64),
            "isize" => type_checker::Type::Int(TcIntTy::Isize),
            "u8" => type_checker::Type::Int(TcIntTy::U8),
            "u16" => type_checker::Type::Int(TcIntTy::U16),
            "u32" => type_checker::Type::Int(TcIntTy::U32),
            "u64" => type_checker::Type::Int(TcIntTy::U64),
            "usize" => type_checker::Type::Int(TcIntTy::Usize),
            "f32" => type_checker::Type::Float(TcFloatTy::F32),
            "f64" => type_checker::Type::Float(TcFloatTy::F64),
            "bool" => type_checker::Type::Bool,
            "str" => type_checker::Type::Str,
            "char" => type_checker::Type::Char,
            _ => {
                let args = path
                    .type_args
                    .iter()
                    .map(|arg| self.lower_hir_type_for_pattern(arg, subst))
                    .collect::<Vec<_>>();
                // The scope graph knows which item the name refers to; a name
                // scan cannot, and would pick a same-named item from another
                // package (a self-reference like `&Chain` would resolve to
                // std's own `Chain`).
                match self.hir.type_resolutions.get(&path.range) {
                    Some(hir::body::ResolvedName::Struct(id)) => {
                        return type_checker::Type::Struct(*id, args);
                    }
                    Some(hir::body::ResolvedName::Enum(id)) => {
                        return type_checker::Type::Enum(*id, args);
                    }
                    _ => {}
                }
                if let Some((id, _)) = self
                    .hir
                    .item_tree
                    .structs
                    .iter()
                    .find(|(_, item)| item.name.0 == name)
                {
                    type_checker::Type::Struct(id, args)
                } else if let Some((id, _)) = self
                    .hir
                    .item_tree
                    .enums
                    .iter()
                    .find(|(_, item)| item.name.0 == name)
                {
                    type_checker::Type::Enum(id, args)
                } else {
                    type_checker::Type::Unknown
                }
            }
        }
    }

    fn lower_impl_trait_for_pattern(
        &self,
        ty: &hir::item_tree::HirTypeRef,
        subst: &HashMap<String, type_checker::Type>,
    ) -> type_checker::Type {
        use hir::item_tree::HirTypeRef;

        let HirTypeRef::ImplTrait {
            trait_ty,
            trait_range,
            callable,
            hidden,
        } = ty
        else {
            unreachable!();
        };
        if let Some(hidden) = hidden {
            return subst
                .get(&hidden.0)
                .cloned()
                .unwrap_or_else(|| type_checker::Type::Param(hidden.0.clone()));
        }
        let kind = match trait_ty.as_ref() {
            HirTypeRef::Named(path) => match path.segments.last().map(|name| name.0.as_str()) {
                Some("Fn") => type_checker::ClosureKind::Fn,
                Some("FnMut") => type_checker::ClosureKind::FnMut,
                Some("FnOnce") => type_checker::ClosureKind::FnOnce,
                _ => return type_checker::Type::Unknown,
            },
            _ => return type_checker::Type::Unknown,
        };
        let Some(callable) = callable else {
            return type_checker::Type::Unknown;
        };
        type_checker::Type::OpaqueCallable {
            id: type_checker::OpaqueCallableId(*trait_range),
            signature: type_checker::CallableSignature {
                is_unsafe: false,
                kind,
                params: callable
                    .params
                    .iter()
                    .map(|param| self.lower_hir_type_for_pattern(param, subst))
                    .collect(),
                ret: Box::new(self.lower_hir_type_for_pattern(&callable.ret, subst)),
            },
        }
    }

    pub(super) fn lower_enum_variant_value(
        &self,
        builder: &mut Builder,
        enum_id: hir::item_tree::EnumId,
        variant_index: usize,
        args: Vec<Value>,
        ty: Type,
    ) -> Value {
        let tag = builder.iconst(variant_index as u64, IntTy::U32);
        let offset = Self::enum_payload_offset(&self.hir.item_tree.enums[enum_id], variant_index);
        let mut fields = vec![(0, tag)];
        fields.extend(
            args.into_iter()
                .enumerate()
                .map(|(index, value)| (1 + offset + index, value)),
        );
        builder.sparse_struct_value(fields, ty)
    }

    pub(super) fn enum_payload_offset(
        enum_data: &hir::item_tree::HirEnum,
        variant_index: usize,
    ) -> usize {
        enum_data
            .variants
            .iter()
            .take(variant_index)
            .map(|variant| match &variant.kind {
                hir::item_tree::HirVariantKind::Unit => 0,
                hir::item_tree::HirVariantKind::Tuple(items) => items.len(),
                hir::item_tree::HirVariantKind::Struct(items) => items.len(),
            })
            .sum()
    }

    /// Read a binding. `let` bindings live in `scope_map`; `match`/`for` arm
    /// bindings live in the arm-scoped `pattern_bindings` stack.
    pub(super) fn binding_value(
        &self,
        builder: &mut Builder,
        id: PatternBindingId,
        ty: &Type,
    ) -> Option<Value> {
        if let Some(value) = self.scope_map.get(&id).copied() {
            return Some(if self.storage_bindings.contains(&id) {
                builder.load(value, ty.clone())
            } else {
                value
            });
        }
        let binding = self
            .pattern_bindings
            .iter()
            .rev()
            .find_map(|scope| scope.get(&id).cloned())?;
        Some(match binding.place {
            Some(place) => builder.load(place, binding.ty),
            None => binding.value,
        })
    }

    /// The address of a binding, materializing one if it only had a value.
    pub(super) fn binding_place(
        &mut self,
        builder: &mut Builder,
        id: PatternBindingId,
    ) -> Option<Value> {
        if self.storage_bindings.contains(&id) {
            return self.scope_map.get(&id).copied();
        }
        let gc_enabled = self.gc_enabled;
        let binding = self
            .pattern_bindings
            .iter_mut()
            .rev()
            .find_map(|scope| scope.get_mut(&id))?;
        if let Some(place) = binding.place {
            return Some(place);
        }
        let place = if gc_enabled {
            builder.heap_alloc(binding.ty.clone())
        } else {
            builder.alloca(binding.ty.clone())
        };
        builder.store(binding.value, place);
        binding.place = Some(place);
        Some(place)
    }
}
