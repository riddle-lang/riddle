use super::{
    BTreeMap, Body, BodyId, Builder, CapturePlace, CaptureSource, CmpOp, DropProjection, DropSlot,
    Expr, ExprId, FuncRef, HashSet, IntTy, LowerCtx, Projection, ResolvedName,
    RuntimeDropProjection, Type, UnOp, Value, closure_drop_function_type, closure_env_type,
};

impl LowerCtx<'_> {
    pub(super) fn emit_current_drop_scope(&mut self, builder: &mut Builder) {
        if let Some(depth) = self.drop_scopes.len().checked_sub(1) {
            self.emit_drop_scopes_since(builder, depth);
        }
    }

    pub(super) fn emit_current_temporary_drop_scope(&mut self, builder: &mut Builder) {
        if let Some(depth) = self.temporary_drop_scopes.len().checked_sub(1) {
            self.emit_temporary_drop_scopes_since(builder, depth);
        }
    }

    pub(super) fn create_drop_slots(
        &self,
        builder: &mut Builder,
        place: Value,
        ty: &type_checker::Type,
        projection: Vec<DropProjection>,
    ) -> Vec<DropSlot> {
        let ty = self.substitute_tc_type(ty);
        if matches!(
            ty,
            type_checker::Type::Closure { ref generics, .. } if !generics.is_empty()
        ) {
            return Vec::new();
        }
        if self.type_result.trait_env.type_has_explicit_drop(&ty)
            || matches!(ty, type_checker::Type::Enum(..))
            || matches!(
                ty,
                type_checker::Type::FunctionItem { .. }
                    | type_checker::Type::Closure { .. }
                    | type_checker::Type::OpaqueCallable { .. }
                    | type_checker::Type::CallableConstraint(_)
                    | type_checker::Type::OwnedDynTrait { .. }
            )
        {
            let flag = builder.alloca(Type::Bool);
            let active = builder.bconst(true);
            builder.store(active, flag);
            return vec![DropSlot {
                place,
                flag,
                ty,
                projection,
            }];
        }

        let mut slots = Vec::new();
        match &ty {
            type_checker::Type::Struct(id, args) => {
                for (index, field_ty) in self.struct_field_types(*id, args).into_iter().enumerate()
                {
                    if self.type_needs_drop(&field_ty, 0) {
                        let field = builder.field_ptr(place, index, self.convert_type(&field_ty));
                        let mut field_projection = projection.clone();
                        field_projection.push(DropProjection::Field(index));
                        slots.extend(self.create_drop_slots(
                            builder,
                            field,
                            &field_ty,
                            field_projection,
                        ));
                    }
                }
            }
            type_checker::Type::Tuple(items) => {
                for (index, item) in items.iter().enumerate() {
                    if self.type_needs_drop(item, 0) {
                        let field = builder.field_ptr(place, index, self.convert_type(item));
                        let mut field_projection = projection.clone();
                        field_projection.push(DropProjection::Field(index));
                        slots.extend(self.create_drop_slots(
                            builder,
                            field,
                            item,
                            field_projection,
                        ));
                    }
                }
            }
            type_checker::Type::Array(item, type_checker::ConstArg::Value(len)) => {
                for index in 0..*len {
                    let index_value = builder.iconst(index as u64, IntTy::Usize);
                    let item_place = builder.index_ptr(place, index_value, self.convert_type(item));
                    let mut item_projection = projection.clone();
                    item_projection.push(DropProjection::Index(index));
                    slots.extend(self.create_drop_slots(
                        builder,
                        item_place,
                        item,
                        item_projection,
                    ));
                }
            }
            _ => {}
        }
        slots
    }

    pub(super) fn register_drop_slots(&mut self, source: CaptureSource, slots: &[DropSlot]) {
        self.drop_slots.insert(source, slots.to_vec());
    }

    pub(super) fn emit_drop_scopes_since(&mut self, builder: &mut Builder, depth: usize) {
        let scopes = self.drop_scopes[depth..].to_vec();
        for scope in scopes.into_iter().rev() {
            for slot in scope.into_iter().rev() {
                self.emit_drop_slot(builder, &slot);
            }
        }
    }

    pub(super) fn emit_temporary_drop_scopes_since(&mut self, builder: &mut Builder, depth: usize) {
        let scopes = self.temporary_drop_scopes[depth..].to_vec();
        for scope in scopes.into_iter().rev() {
            for slot in scope.into_iter().rev() {
                self.emit_drop_slot(builder, &slot);
            }
        }
    }

    pub(super) const fn drop_slot_flag_place(_builder: &mut Builder, slot: &DropSlot) -> Value {
        slot.flag
    }

    pub(super) fn emit_drop_slot(&mut self, builder: &mut Builder, slot: &DropSlot) {
        let ty = self.substitute_tc_type(&slot.ty);
        let drop_block = builder.func.new_block_labeled("drop");
        let continue_block = builder.func.new_block_labeled("drop_continue");
        let active = builder.load(slot.flag, Type::Bool);
        builder.set_cond_branch(active, drop_block, continue_block);
        builder.switch_to_block(drop_block);
        self.emit_drop_glue(builder, slot.place, &ty);
        builder.set_branch(continue_block);
        builder.switch_to_block(continue_block);
    }

    pub(super) fn emit_drop_glue(
        &mut self,
        builder: &mut Builder,
        place: Value,
        ty: &type_checker::Type,
    ) {
        let ty = self.substitute_tc_type(ty);
        if matches!(
            ty,
            type_checker::Type::Closure { ref generics, .. } if !generics.is_empty()
        ) {
            return;
        }
        if matches!(
            ty,
            type_checker::Type::FunctionItem { .. }
                | type_checker::Type::Closure { .. }
                | type_checker::Type::OpaqueCallable { .. }
                | type_checker::Type::CallableConstraint(_)
        ) {
            let closure = builder.load(place, self.convert_type(&ty));
            let env = builder.extract_value(closure, 1, closure_env_type());
            let drop = builder.extract_value(closure, 2, closure_drop_function_type());
            builder.call_indirect(drop, vec![env], Type::Unit);
            return;
        }
        if matches!(ty, type_checker::Type::OwnedDynTrait { .. }) {
            let object = builder.load(place, self.convert_type(&ty));
            let data = builder.extract_value(object, 0, Type::Ptr(Box::new(Type::Unit)));
            let drop_index = match self.convert_type(&ty) {
                Type::Struct(struct_ty) => struct_ty.def().fields.len().saturating_sub(1),
                _ => return,
            };
            let drop = builder.extract_value(
                object,
                drop_index,
                super::types::dyn_trait_drop_function_type(),
            );
            builder.call_indirect(drop, vec![data], Type::Unit);
            return;
        }
        if let Some(trait_id) = self
            .type_result
            .trait_env
            .lang_items
            .get(type_checker::lang_items::LangItem::Drop)
            && let Some(function) = self.find_trait_impl_method(trait_id, "drop", &ty, None)
        {
            let drop_name = self
                .mono_method_name_for_receiver(function, &ty, None)
                .unwrap_or_else(|| self.function_name(function));
            let mir_ty = self.convert_type(&ty);
            let receiver = builder.unop(UnOp::MutRef, place, Type::Ref(Box::new(mir_ty), true));
            builder.call(FuncRef::Local(drop_name), vec![receiver], Type::Unit);
        }

        let fields = match &ty {
            type_checker::Type::Struct(id, args) => self.struct_field_types(*id, args),
            type_checker::Type::Tuple(items) => items.clone(),
            _ => Vec::new(),
        };
        for (index, field_ty) in fields.into_iter().enumerate() {
            if self.type_needs_drop(&field_ty, 0) {
                let field = builder.field_ptr(place, index, self.convert_type(&field_ty));
                self.emit_drop_glue(builder, field, &field_ty);
            }
        }

        if let type_checker::Type::Array(item, type_checker::ConstArg::Value(len)) = &ty
            && self.type_needs_drop(item, 0)
        {
            for index in 0..*len {
                let index = builder.iconst(index as u64, IntTy::Usize);
                let item_place = builder.index_ptr(place, index, self.convert_type(item));
                self.emit_drop_glue(builder, item_place, item);
            }
        }

        if let type_checker::Type::Enum(id, args) = &ty {
            let variants = self.enum_variant_field_types(*id, args);
            let active = variants
                .iter()
                .enumerate()
                .filter(|(_, fields)| fields.iter().any(|field| self.type_needs_drop(field, 0)))
                .map(|(index, fields)| (index, fields.clone()))
                .collect::<Vec<_>>();
            if !active.is_empty() {
                let tag_place = builder.field_ptr(place, 0, Type::Int(IntTy::U32));
                let tag = builder.load(tag_place, Type::Int(IntTy::U32));
                let done = builder.func.new_block_labeled("enum_drop_done");
                for (position, (variant, fields)) in active.iter().enumerate() {
                    let drop_variant = builder.func.new_block_labeled("enum_drop_variant");
                    let next = if position + 1 == active.len() {
                        done
                    } else {
                        builder.func.new_block_labeled("enum_drop_next")
                    };
                    let expected = builder.iconst(*variant as u64, IntTy::U32);
                    let matches = builder.cmp(CmpOp::Eq, tag, expected);
                    builder.set_cond_branch(matches, drop_variant, next);

                    builder.switch_to_block(drop_variant);
                    let offset = 1 + variants[..*variant].iter().map(Vec::len).sum::<usize>();
                    for (field_index, field_ty) in fields.iter().enumerate() {
                        if self.type_needs_drop(field_ty, 0) {
                            let field = builder.field_ptr(
                                place,
                                offset + field_index,
                                self.convert_type(field_ty),
                            );
                            self.emit_drop_glue(builder, field, field_ty);
                        }
                    }
                    builder.set_branch(done);
                    if next != done {
                        builder.switch_to_block(next);
                    }
                }
                builder.switch_to_block(done);
            }
        }
    }

    pub(super) fn type_needs_drop(&self, ty: &type_checker::Type, depth: usize) -> bool {
        if depth > 64 {
            return false;
        }
        let ty = self.substitute_tc_type(ty);
        if matches!(
            ty,
            type_checker::Type::Closure { ref generics, .. } if !generics.is_empty()
        ) {
            return false;
        }
        if self.type_result.trait_env.type_has_explicit_drop(&ty)
            || matches!(
                &ty,
                type_checker::Type::FunctionItem { .. }
                    | type_checker::Type::Closure { .. }
                    | type_checker::Type::OpaqueCallable { .. }
                    | type_checker::Type::CallableConstraint(_)
                    | type_checker::Type::OwnedDynTrait { .. }
            )
        {
            return true;
        }
        match &ty {
            type_checker::Type::Struct(id, args) => self
                .struct_field_types(*id, args)
                .iter()
                .any(|field| self.type_needs_drop(field, depth + 1)),
            type_checker::Type::Enum(id, args) => self
                .enum_variant_field_types(*id, args)
                .iter()
                .flatten()
                .any(|field| self.type_needs_drop(field, depth + 1)),
            type_checker::Type::Tuple(items) => items
                .iter()
                .any(|item| self.type_needs_drop(item, depth + 1)),
            type_checker::Type::Array(item, _) => self.type_needs_drop(item, depth + 1),
            _ => false,
        }
    }

    pub(super) fn struct_field_types(
        &self,
        id: hir::item_tree::StructId,
        args: &[type_checker::Type],
    ) -> Vec<type_checker::Type> {
        let item = &self.hir.item_tree.structs[id];
        let subst = item
            .generics
            .iter()
            .chain(item.const_generics.iter())
            .zip(args)
            .map(|(name, ty)| (name.0.clone(), ty.clone()))
            .collect();
        item.fields
            .iter()
            .map(|field| self.lower_hir_type_for_pattern(&field.ty, &subst))
            .collect()
    }

    pub(super) fn enum_variant_field_types(
        &self,
        id: hir::item_tree::EnumId,
        args: &[type_checker::Type],
    ) -> Vec<Vec<type_checker::Type>> {
        let item = &self.hir.item_tree.enums[id];
        let subst = item
            .generics
            .iter()
            .chain(item.const_generics.iter())
            .zip(args)
            .map(|(name, ty)| (name.0.clone(), ty.clone()))
            .collect();
        item.variants
            .iter()
            .map(|variant| match &variant.kind {
                hir::item_tree::HirVariantKind::Unit => Vec::new(),
                hir::item_tree::HirVariantKind::Tuple(fields) => fields
                    .iter()
                    .map(|field| self.lower_hir_type_for_pattern(field, &subst))
                    .collect(),
                hir::item_tree::HirVariantKind::Struct(fields) => fields
                    .iter()
                    .map(|field| self.lower_hir_type_for_pattern(&field.ty, &subst))
                    .collect(),
            })
            .collect()
    }

    pub(super) fn clear_drop_flags_if_moved(
        &self,
        builder: &mut Builder,
        body: &Body,
        expr_id: ExprId,
    ) {
        let Some(body_id) = self.current_body else {
            return;
        };
        let (slots, projection) =
            if let Some((source, projection)) = self.drop_place_from_expr(body, expr_id) {
                if !self.moved_exprs.contains(&(body_id, expr_id)) {
                    return;
                }
                (self.drop_slots.get(&source), projection)
            } else if let Some((temporary, projection)) =
                self.temporary_drop_place_from_expr(body, expr_id)
            {
                if !self.expr_is_recorded_move(body_id, expr_id) {
                    return;
                }
                (self.temporary_drop_slots.get(&temporary), projection)
            } else {
                return;
            };
        let slots = slots
            .into_iter()
            .flatten()
            .filter(|slot| {
                projection.is_empty() || slot.projection.starts_with(projection.as_slice())
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

    pub(super) fn clear_drop_slots_for_source(
        &self,
        builder: &mut Builder,
        source: &CaptureSource,
    ) {
        let slots = self
            .drop_slots
            .get(source)
            .into_iter()
            .flatten()
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

    pub(super) fn clear_drop_slots_for_capture(
        &self,
        builder: &mut Builder,
        capture: &CapturePlace,
    ) {
        let projection = capture
            .projections
            .iter()
            .filter_map(|projection| match projection {
                Projection::Field(index) => Some(DropProjection::Field(*index)),
                Projection::Index(Some(index)) => Some(DropProjection::Index(*index)),
                Projection::Index(None) => None,
            })
            .collect::<Vec<_>>();
        let slots = self
            .drop_slots
            .get(&capture.source)
            .into_iter()
            .flatten()
            .filter(|slot| {
                slot.projection.starts_with(&projection) || projection.starts_with(&slot.projection)
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

    pub(super) fn drop_place_from_expr(
        &self,
        body: &Body,
        expr_id: ExprId,
    ) -> Option<(CaptureSource, Vec<DropProjection>)> {
        match &body.exprs[expr_id] {
            Expr::Path {
                resolved: Some(ResolvedName::PatternBinding(id)),
                ..
            } => Some((CaptureSource::Pattern(*id), Vec::new())),
            Expr::Path {
                resolved: Some(ResolvedName::Param(index)),
                ..
            } => Some((CaptureSource::Param(*index), Vec::new())),
            Expr::Path {
                resolved: Some(ResolvedName::LambdaParam { lambda, index }),
                ..
            } => Some((
                CaptureSource::LambdaParam {
                    lambda: *lambda,
                    index: *index,
                },
                Vec::new(),
            )),
            Expr::FieldAccess { base, field } => {
                let (source, mut projection) = self.drop_place_from_expr(body, *base)?;
                projection.push(DropProjection::Field(
                    self.resolve_field_index(*base, field),
                ));
                Some((source, projection))
            }
            Expr::IndexAccess { base, index } => {
                let Expr::IntLiteral { value, .. } = body.exprs[*index] else {
                    return None;
                };
                let (source, mut projection) = self.drop_place_from_expr(body, *base)?;
                projection.push(DropProjection::Index(usize::try_from(value).ok()?));
                Some((source, projection))
            }
            _ => None,
        }
    }

    pub(super) fn temporary_drop_place_from_expr(
        &self,
        body: &Body,
        expr_id: ExprId,
    ) -> Option<(ExprId, Vec<DropProjection>)> {
        match &body.exprs[expr_id] {
            Expr::FieldAccess { base, field } => {
                let (temporary, mut projection) =
                    self.temporary_drop_place_from_expr(body, *base)?;
                projection.push(DropProjection::Field(
                    self.resolve_field_index(*base, field),
                ));
                Some((temporary, projection))
            }
            Expr::IndexAccess { base, index } => {
                let Expr::IntLiteral { value, .. } = body.exprs[*index] else {
                    return None;
                };
                let (temporary, mut projection) =
                    self.temporary_drop_place_from_expr(body, *base)?;
                projection.push(DropProjection::Index(usize::try_from(value).ok()?));
                Some((temporary, projection))
            }
            _ => self
                .temporary_drop_slots
                .contains_key(&expr_id)
                .then_some((expr_id, Vec::new())),
        }
    }

    pub(super) fn clear_dynamic_index_drop_flags_if_moved(
        &self,
        builder: &mut Builder,
        body: &Body,
        expr_id: ExprId,
        index: ExprId,
        index_value: Value,
    ) {
        let Some(body_id) = self.current_body else {
            return;
        };
        let (slots, projection) = if let Some((source, projection)) =
            self.drop_place_from_expr_with_runtime_indices(body, expr_id, (index, index_value))
        {
            if !self.moved_exprs.contains(&(body_id, expr_id)) {
                return;
            }
            (self.drop_slots.get(&source), projection)
        } else if let Some((temporary, projection)) = self
            .temporary_drop_place_from_expr_with_runtime_indices(
                body,
                expr_id,
                (index, index_value),
            )
        {
            if !self.expr_is_recorded_move(body_id, expr_id) {
                return;
            }
            (self.temporary_drop_slots.get(&temporary), projection)
        } else {
            return;
        };
        let dynamic_indices = projection
            .iter()
            .filter_map(|projection| match projection {
                RuntimeDropProjection::Index(value, ty) => Some((*value, *ty)),
                RuntimeDropProjection::Exact(_) => None,
            })
            .collect::<Vec<_>>();
        if dynamic_indices.is_empty() {
            return;
        }

        let mut flags_by_indices = BTreeMap::<Vec<usize>, HashSet<Value>>::new();
        'slots: for slot in slots.into_iter().flatten() {
            if slot.projection.len() < projection.len() {
                continue;
            }
            let mut indices = Vec::new();
            for (expected, actual) in projection.iter().zip(&slot.projection) {
                match (expected, actual) {
                    (RuntimeDropProjection::Exact(expected), actual) if expected == actual => {}
                    (RuntimeDropProjection::Index(_, _), DropProjection::Index(index)) => {
                        indices.push(*index);
                    }
                    _ => continue 'slots,
                }
            }
            let flag = Self::drop_slot_flag_place(builder, slot);
            flags_by_indices.entry(indices).or_default().insert(flag);
        }
        for (indices, flags) in flags_by_indices {
            let condition = dynamic_indices.iter().zip(indices).fold(
                None,
                |condition, ((index_value, index_ty), expected_index)| {
                    let expected = builder.iconst(expected_index as u64, *index_ty);
                    let matches = builder.cmp(CmpOp::Eq, *index_value, expected);
                    Self::and_pattern_conditions(builder, condition, Some(matches))
                },
            );
            let Some(condition) = condition else {
                continue;
            };
            let clear = builder.func.new_block_labeled("move_array_element");
            let next = builder.func.new_block_labeled("move_array_continue");
            builder.set_cond_branch(condition, clear, next);
            builder.switch_to_block(clear);
            for flag in flags {
                let inactive = builder.bconst(false);
                builder.store(inactive, flag);
            }
            builder.set_branch(next);
            builder.switch_to_block(next);
        }
    }

    pub(super) fn expr_is_recorded_move(&self, body_id: BodyId, expr_id: ExprId) -> bool {
        self.type_result
            .value_uses
            .get(&(body_id, expr_id))
            .copied()
            == Some(type_checker::ValueUse::Move)
            && self
                .type_result
                .expr_types
                .get(&(body_id, expr_id))
                .is_some_and(|ty| !self.type_result.trait_env.type_is_copy(ty))
    }

    pub(super) fn drop_place_from_expr_with_runtime_indices(
        &self,
        body: &Body,
        expr_id: ExprId,
        current_index: (ExprId, Value),
    ) -> Option<(CaptureSource, Vec<RuntimeDropProjection>)> {
        match &body.exprs[expr_id] {
            Expr::Path {
                resolved: Some(ResolvedName::PatternBinding(id)),
                ..
            } => Some((CaptureSource::Pattern(*id), Vec::new())),
            Expr::Path {
                resolved: Some(ResolvedName::Param(index)),
                ..
            } => Some((CaptureSource::Param(*index), Vec::new())),
            Expr::Path {
                resolved: Some(ResolvedName::LambdaParam { lambda, index }),
                ..
            } => Some((
                CaptureSource::LambdaParam {
                    lambda: *lambda,
                    index: *index,
                },
                Vec::new(),
            )),
            Expr::FieldAccess { base, field } => {
                let (source, mut projection) =
                    self.drop_place_from_expr_with_runtime_indices(body, *base, current_index)?;
                projection.push(RuntimeDropProjection::Exact(DropProjection::Field(
                    self.resolve_field_index(*base, field),
                )));
                Some((source, projection))
            }
            Expr::IndexAccess { base, index } => {
                let (source, mut projection) =
                    self.drop_place_from_expr_with_runtime_indices(body, *base, current_index)?;
                if let Expr::IntLiteral { value, .. } = body.exprs[*index] {
                    projection.push(RuntimeDropProjection::Exact(DropProjection::Index(
                        usize::try_from(value).ok()?,
                    )));
                } else {
                    let value = if *index == current_index.0 {
                        current_index.1
                    } else {
                        *self.expr_cache.get(index)?
                    };
                    let index_ty = self
                        .current_body
                        .and_then(|body_id| self.type_result.expr_types.get(&(body_id, *index)))
                        .map(|ty| self.convert_type(ty))
                        .and_then(|ty| match ty {
                            Type::Int(width) => Some(width),
                            _ => None,
                        })
                        .unwrap_or(IntTy::Usize);
                    projection.push(RuntimeDropProjection::Index(value, index_ty));
                }
                Some((source, projection))
            }
            _ => None,
        }
    }

    pub(super) fn temporary_drop_place_from_expr_with_runtime_indices(
        &self,
        body: &Body,
        expr_id: ExprId,
        current_index: (ExprId, Value),
    ) -> Option<(ExprId, Vec<RuntimeDropProjection>)> {
        match &body.exprs[expr_id] {
            Expr::FieldAccess { base, field } => {
                let (temporary, mut projection) = self
                    .temporary_drop_place_from_expr_with_runtime_indices(
                        body,
                        *base,
                        current_index,
                    )?;
                projection.push(RuntimeDropProjection::Exact(DropProjection::Field(
                    self.resolve_field_index(*base, field),
                )));
                Some((temporary, projection))
            }
            Expr::IndexAccess { base, index } => {
                let (temporary, mut projection) = self
                    .temporary_drop_place_from_expr_with_runtime_indices(
                        body,
                        *base,
                        current_index,
                    )?;
                if let Expr::IntLiteral { value, .. } = body.exprs[*index] {
                    projection.push(RuntimeDropProjection::Exact(DropProjection::Index(
                        usize::try_from(value).ok()?,
                    )));
                } else {
                    let value = if *index == current_index.0 {
                        current_index.1
                    } else {
                        *self.expr_cache.get(index)?
                    };
                    let index_ty = self
                        .current_body
                        .and_then(|body_id| self.type_result.expr_types.get(&(body_id, *index)))
                        .map(|ty| self.convert_type(ty))
                        .and_then(|ty| match ty {
                            Type::Int(width) => Some(width),
                            _ => None,
                        })
                        .unwrap_or(IntTy::Usize);
                    projection.push(RuntimeDropProjection::Index(value, index_ty));
                }
                Some((temporary, projection))
            }
            _ => self
                .temporary_drop_slots
                .contains_key(&expr_id)
                .then_some((expr_id, Vec::new())),
        }
    }

    pub(super) fn clear_indexed_drop_slots(
        builder: &mut Builder,
        slots: &[DropSlot],
        index_value: Value,
        index_ty: IntTy,
    ) {
        let mut flags_by_index = BTreeMap::<usize, HashSet<Value>>::new();
        for slot in slots {
            if let Some(index) = slot
                .projection
                .iter()
                .find_map(|projection| match projection {
                    DropProjection::Index(index) => Some(*index),
                    DropProjection::Field(_) => None,
                })
            {
                let flag = Self::drop_slot_flag_place(builder, slot);
                flags_by_index.entry(index).or_default().insert(flag);
            }
        }
        for (index, flags) in flags_by_index {
            let clear = builder.func.new_block_labeled("move_array_element");
            let next = builder.func.new_block_labeled("move_array_continue");
            let expected = builder.iconst(index as u64, index_ty);
            let matches = builder.cmp(CmpOp::Eq, index_value, expected);
            builder.set_cond_branch(matches, clear, next);
            builder.switch_to_block(clear);
            for flag in flags {
                let inactive = builder.bconst(false);
                builder.store(inactive, flag);
            }
            builder.set_branch(next);
            builder.switch_to_block(next);
        }
    }
}
