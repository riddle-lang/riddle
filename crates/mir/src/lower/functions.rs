use std::fmt::Write as _;

use super::{
    Body, BodyId, Builder, CaptureAccess, CaptureMode, CapturePlace, CaptureSource, CastOp, Expr,
    ExprId, FuncRef, Function, IntTy, LambdaCapture, LambdaExprInput, LambdaFunctionInput,
    LambdaInfo, LetPatternInput, LetSource, LowerCtx, Projection, StructType, Type, Value,
    closure_call_signature, closure_drop_function_type, closure_env_type,
};

impl LowerCtx<'_> {
    pub(super) fn ensure_dyn_trait_drop_adapter(
        &mut self,
        receiver_ty: &type_checker::Type,
    ) -> String {
        let receiver_mir_ty = self.convert_type(receiver_ty);
        let name = format!(
            "__riddle_dyn_drop_{}",
            super::mono_type_name(&receiver_mir_ty)
        );
        if self
            .module
            .functions
            .values()
            .any(|function| function.name == name)
        {
            return name;
        }

        let mut function = Function::new(name.clone(), Type::Unit);
        self.inherit_function_ownership(&mut function);
        let data = function.add_param("__data".into(), Type::Ptr(Box::new(Type::Unit)));
        function.blocks[function.entry].start_value = function.next_value;
        {
            let mut builder = Builder::new(&mut function);
            let typed = builder.cast(CastOp::PtrToPtr, data, Type::Ptr(Box::new(receiver_mir_ty)));
            self.emit_drop_glue(&mut builder, typed, receiver_ty);
            builder.heap_free(data);
            builder.set_return(None);
        }
        self.module.add_function(function);
        name
    }

    pub(super) fn ensure_dyn_trait_adapter(
        &mut self,
        trait_id: hir::item_tree::TraitId,
        method_fid: hir::item_tree::FunctionId,
        receiver_ty: &type_checker::Type,
        signature: &crate::types::FnPtrType,
    ) -> String {
        let receiver_mir_ty = self.convert_type(receiver_ty);
        let name = format!(
            "__riddle_dyn_adapter_{}_{}_{}",
            trait_id.into_raw().into_u32(),
            method_fid.into_raw().into_u32(),
            super::mono_type_name(&receiver_mir_ty)
        );
        if self
            .module
            .functions
            .values()
            .any(|function| function.name == name)
        {
            return name;
        }

        let mut function = Function::new(name.clone(), (*signature.ret).clone());
        self.inherit_function_ownership(&mut function);
        let data = function.add_param("__data".into(), Type::Ptr(Box::new(Type::Unit)));
        let args = signature
            .params
            .iter()
            .skip(1)
            .enumerate()
            .map(|(index, ty)| function.add_param(format!("p{index}"), ty.clone()))
            .collect::<Vec<_>>();
        function.blocks[function.entry].start_value = function.next_value;

        let receiver_param = self.hir.item_tree.functions[method_fid]
            .params
            .first()
            .map(|param| param.ty.clone());
        let receiver_mut = receiver_param
            .as_ref()
            .and_then(|ty| match ty {
                hir::item_tree::HirTypeRef::Ref(_, mutable) => Some(*mutable),
                _ => None,
            })
            .unwrap_or(false);
        let receiver = Type::Ref(Box::new(receiver_mir_ty), receiver_mut);
        let target = self
            .mono_method_name_for_receiver(method_fid, receiver_ty, None)
            .unwrap_or_else(|| self.function_name(method_fid));
        {
            let mut builder = Builder::new(&mut function);
            let receiver = builder.cast(CastOp::PtrToPtr, data, receiver);
            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(receiver);
            call_args.extend(args);
            let result = builder.call(FuncRef::Local(target), call_args, (*signature.ret).clone());
            builder.set_return(
                (!matches!(signature.ret.as_ref(), Type::Unit | Type::Never)).then_some(result),
            );
        }
        self.module.add_function(function);
        name
    }

    pub(super) fn reference_storage(&self, builder: &mut Builder, ty: Type) -> Value {
        if self.gc_enabled {
            builder.heap_alloc(ty)
        } else {
            builder.alloca(ty)
        }
    }

    pub(super) fn lower_expr_sequence(
        &mut self,
        builder: &mut Builder,
        param_values: &[Value],
        body: &Body,
        _owner: ExprId,
        _key_offset: usize,
        expressions: &[ExprId],
    ) -> Vec<Value> {
        expressions
            .iter()
            .copied()
            .map(|expression| self.lower_expr(builder, param_values, body, expression))
            .collect()
    }

    pub(super) fn lower_function(
        &mut self,
        fid: hir::item_tree::FunctionId,
        name: String,
        body_id: BodyId,
    ) -> Function {
        let body = &self.hir.bodies[body_id];
        self.expr_cache.clear();
        self.coerced_values.clear();
        self.scope_map.clear();
        self.drop_scopes.clear();
        self.drop_slots.clear();
        self.temporary_drop_scopes.clear();
        self.temporary_drop_slots.clear();
        self.storage_bindings.clear();
        self.parameter_storage.clear();
        self.pattern_bindings.clear();
        self.capture_access.clear();
        self.current_lambda = None;
        self.current_body = Some(body_id);
        let old_current_function = self.current_function;
        let old_generic_subst = self.generic_subst.clone();
        let old_generic_tc_subst = self.generic_tc_subst.clone();
        let old_generic_const_subst = self.generic_const_subst.clone();
        let old_loop_targets = std::mem::take(&mut self.loop_targets);
        self.current_function = Some(fid);
        if !self.generic_subst.contains_key("Self")
            && let Some(self_ty) = self.impl_self_mir_type(fid)
        {
            self.generic_subst.insert("Self".into(), self_ty);
        }

        let func_item = &self.hir.item_tree.functions[fid];

        let ret_type = func_item
            .ret_type
            .as_ref()
            .map_or(Type::Unit, |rt| self.convert_hir_type(rt));

        let mut func = Function::new(name, ret_type);
        func.package = self.hir.package_for_range(func_item.name_range);
        func.generic_instance = self.function_is_generic_instance(fid);
        func.uses_c_string_abi = func_item.attrs.iter().any(|attr| attr.name.0 == "c_export");
        func.is_c_export =
            self.hir.item_tree.extern_function_ids.contains(&fid) || func.uses_c_string_abi;
        let mut param_values: Vec<Value> = Vec::new();

        for param in &func_item.params {
            let pty = self.convert_hir_type(&param.ty);
            let v = func.add_param(param.name.0.clone(), pty);
            param_values.push(v);
        }

        // Fix entry block start_value: params were allocated after the entry block
        // was created, so its start_value=0 overlaps with param values. Move it past
        // the last param.
        func.blocks[func.entry].start_value = func.next_value;

        // 降级函数体
        let is_unit_ret = func.ret_type == Type::Unit || func.ret_type == Type::Never;
        {
            let mut builder = Builder::new(&mut func);
            self.drop_scopes.push(Vec::new());
            for (index, (param, value)) in func_item.params.iter().zip(&param_values).enumerate() {
                let tc_ty = self.lower_hir_type_for_pattern(&param.ty, &self.generic_tc_subst);
                let needs_drop = self.type_needs_drop(&tc_ty, 0);
                let storage = if self.analysis.param_escapes(body_id, index) {
                    Some(self.reference_storage(&mut builder, self.convert_hir_type(&param.ty)))
                } else if param.is_mut
                    || self.analysis.param_needs_address(body_id, index)
                    || needs_drop
                {
                    Some(builder.alloca(self.convert_hir_type(&param.ty)))
                } else {
                    None
                };
                if let Some(storage) = storage {
                    builder.store(*value, storage);
                    self.parameter_storage
                        .insert(CaptureSource::Param(index), storage);
                }
                if needs_drop {
                    let place = storage.expect("Drop parameter has storage");
                    let source = CaptureSource::Param(index);
                    let slots = self.create_drop_slots(&mut builder, place, &tc_ty, Vec::new());
                    self.register_drop_slots(source, &slots);
                    self.drop_scopes[0].splice(0..0, slots.into_iter().rev());
                }
            }
            let root_result = self.lower_expr(&mut builder, &param_values, body, body.root_block);
            if builder.needs_return() {
                self.emit_current_drop_scope(&mut builder);
            }
            self.drop_scopes.pop();

            // Set the implicit return only when lowering did not terminate the block.
            if is_unit_ret && builder.needs_return() {
                builder.set_return(None);
            } else if builder.needs_return() {
                builder.set_return(Some(root_result));
            }
        }

        self.current_function = old_current_function;
        self.generic_subst = old_generic_subst;
        self.generic_tc_subst = old_generic_tc_subst;
        self.generic_const_subst = old_generic_const_subst;
        self.loop_targets = old_loop_targets;
        func
    }

    pub(super) fn lower_lambda(
        &mut self,
        builder: &mut Builder,
        outer_params: &[Value],
        input: &LambdaExprInput<'_>,
    ) -> Value {
        let LambdaExprInput {
            body_id,
            expr_id,
            params,
            body: lambda_body,
            ty,
        } = *input;
        let Some(call_signature) = closure_call_signature(ty) else {
            return builder.unit_const();
        };
        let info = self
            .type_result
            .lambda_infos
            .get(&(body_id, expr_id))
            .cloned()
            .unwrap_or(LambdaInfo {
                captures: Vec::new(),
                kind: type_checker::ClosureKind::Fn,
            });
        let instance_suffix = self.current_mono_instance_suffix();
        let (name, needs_lowering) = if instance_suffix.is_empty() {
            if let Some(name) = self.lambda_functions.get(&(body_id, expr_id)) {
                (name.clone(), false)
            } else {
                self.lambda_counter += 1;
                let name =
                    self.qualify_current_symbol(format!("__riddle_lambda_{}", self.lambda_counter));
                self.lambda_functions
                    .insert((body_id, expr_id), name.clone());
                (name, true)
            }
        } else {
            // Inside a monomorphized instance the capture types depend on the
            // substitution, so each instance needs its own lambda symbol and
            // environment struct.
            let key = (body_id, expr_id, instance_suffix.clone());
            if let Some(name) = self.generic_lambda_functions.get(&key) {
                (name.clone(), false)
            } else {
                self.lambda_counter += 1;
                let name = self.qualify_current_symbol(format!(
                    "__riddle_lambda_{}_{}",
                    self.lambda_counter, instance_suffix
                ));
                self.generic_lambda_functions.insert(key, name.clone());
                (name, true)
            }
        };
        let (capture_types, env_struct) = self.lambda_environment_type(&name, &info);

        let heap_env = !info.captures.is_empty() && self.analysis.lambda_escapes(body_id, expr_id);
        let env_value = self.lower_lambda_environment(
            builder,
            outer_params,
            &info,
            &capture_types,
            &env_struct,
            heap_env,
        );

        if needs_lowering {
            self.lower_lambda_function(&LambdaFunctionInput {
                body_id,
                expr_id,
                params,
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
                heap_env,
            );
        }

        let call = builder.function_ref(FuncRef::Local(name.clone()), Type::FnPtr(call_signature));
        let drop = builder.function_ref(
            FuncRef::Local(format!("{name}_drop")),
            closure_drop_function_type(),
        );
        builder.struct_value(vec![call, env_value, drop], ty.clone())
    }

    /// Stable discriminator of the monomorphized instance currently being
    /// lowered, or empty outside generic substitution.
    pub(super) fn current_mono_instance_suffix(&self) -> String {
        if self.generic_subst.is_empty() && self.generic_const_subst.is_empty() {
            return String::new();
        }
        let mut parts: Vec<String> = self
            .generic_subst
            .iter()
            .map(|(name, ty)| format!("{}{}", name, super::mono_type_name(ty)))
            .collect();
        parts.extend(
            self.generic_const_subst
                .iter()
                .map(|(name, value)| format!("{}{}", name, value)),
        );
        parts.sort();
        parts.join("_")
    }

    pub(super) fn lambda_environment_type(
        &self,
        name: &str,
        info: &LambdaInfo,
    ) -> (Vec<Type>, StructType) {
        let capture_types = info
            .captures
            .iter()
            .map(|capture| self.convert_type(&capture.ty))
            .collect::<Vec<_>>();
        let fields = info
            .captures
            .iter()
            .zip(&capture_types)
            .enumerate()
            .map(|(index, (capture, ty))| {
                let field_ty = match capture.mode {
                    CaptureMode::Shared | CaptureMode::Mutable => Type::Ptr(Box::new(ty.clone())),
                    CaptureMode::Value => ty.clone(),
                };
                (
                    format!(
                        "capture_{}_{}",
                        index,
                        self.capture_environment_name(capture)
                    ),
                    field_ty,
                )
            })
            .collect();
        (
            capture_types,
            StructType::defined(format!("{name}_env"), format!("{name}_env"), fields),
        )
    }

    pub(super) fn lower_lambda_environment(
        &mut self,
        builder: &mut Builder,
        outer_params: &[Value],
        info: &LambdaInfo,
        capture_types: &[Type],
        env_struct: &StructType,
        heap_env: bool,
    ) -> Value {
        if info.captures.is_empty() {
            return Self::null_env(builder);
        }
        let env_ty = Type::Struct(env_struct.clone());
        let env_ptr = if heap_env {
            builder.heap_alloc(env_ty)
        } else {
            builder.alloca(env_ty)
        };
        for (index, (capture, capture_ty)) in info.captures.iter().zip(capture_types).enumerate() {
            let field_ty = env_struct.def().fields[index].1.clone();
            let value = match capture.mode {
                CaptureMode::Shared | CaptureMode::Mutable => {
                    self.capture_place(builder, outer_params, &capture.place, capture_ty)
                }
                CaptureMode::Value => {
                    self.capture_value(builder, outer_params, &capture.place, capture_ty)
                }
            };
            let field = builder.field_ptr(env_ptr, index, field_ty);
            builder.store(value, field);
            if capture.mode == CaptureMode::Value && self.type_needs_drop(&capture.ty, 0) {
                self.clear_drop_slots_for_capture(builder, &capture.place);
            }
        }
        builder.cast(CastOp::PtrToPtr, env_ptr, closure_env_type())
    }

    pub(super) fn capture_environment_name(&self, capture: &LambdaCapture) -> String {
        let mut name = capture.name.clone();
        let root_ty = self.capture_root_type(&capture.place.source);
        for (position, projection) in capture.place.projections.iter().enumerate() {
            match projection {
                Projection::Field(index) => {
                    let field_name = if position == 0 {
                        match &root_ty {
                            Some(type_checker::Type::Struct(struct_id, _)) => {
                                self.hir.item_tree.structs[*struct_id]
                                    .fields
                                    .get(*index)
                                    .map(|field| field.name.0.as_str())
                            }
                            _ => None,
                        }
                    } else {
                        None
                    };
                    name.push('_');
                    name.push_str(
                        &field_name.map_or_else(|| format!("field_{index}"), str::to_owned),
                    );
                }
                Projection::Index(Some(index)) => {
                    write!(name, "_{index}").expect("writing to a String cannot fail");
                }
                Projection::Index(None) => name.push_str("_index"),
            }
        }
        name
    }

    pub(super) fn lower_lambda_function(&mut self, input: &LambdaFunctionInput<'_>) {
        let LambdaFunctionInput {
            body_id,
            expr_id,
            params,
            body: lambda_body,
            name,
            call_signature,
            ..
        } = *input;
        let body = &self.hir.bodies[body_id];
        let old_state = self.take_lowering_state();
        let old_loop_targets = std::mem::take(&mut self.loop_targets);
        self.current_lambda = Some(expr_id);
        self.current_body = Some(body_id);

        let mut function = Function::new(name.to_string(), (*call_signature.ret).clone());
        self.inherit_function_ownership(&mut function);
        let env_param = function.add_param("__env".into(), closure_env_type());
        self.current_lambda_env = Some(env_param);
        let param_values = params
            .iter()
            .zip(call_signature.params.iter().skip(1))
            .map(|(param, ty)| function.add_param(param.name.0.clone(), ty.clone()))
            .collect::<Vec<_>>();
        function.blocks[function.entry].start_value = function.next_value;
        let is_unit = matches!(function.ret_type, Type::Unit | Type::Never);
        {
            let mut lambda_builder = Builder::new(&mut function);
            self.drop_scopes.push(Vec::new());
            self.initialize_lambda_parameters(&mut lambda_builder, input, &param_values);
            self.initialize_lambda_captures(&mut lambda_builder, input, env_param);
            let result = self.lower_expr(&mut lambda_builder, &param_values, body, lambda_body);
            if lambda_builder.needs_return() {
                self.emit_current_drop_scope(&mut lambda_builder);
            }
            self.drop_scopes.pop();
            if lambda_builder.needs_return() {
                lambda_builder.set_return((!is_unit).then_some(result));
            }
        }
        self.module.add_function(function);

        self.loop_targets = old_loop_targets;
        self.restore_lowering_state(old_state);
    }

    fn initialize_lambda_parameters(
        &mut self,
        builder: &mut Builder,
        input: &LambdaFunctionInput<'_>,
        values: &[Value],
    ) {
        let body = &self.hir.bodies[input.body_id];
        for (index, value) in values.iter().enumerate() {
            let ty = input.call_signature.params[index + 1].clone();
            let tc_ty =
                self.lower_hir_type_for_pattern(&input.params[index].ty, &self.generic_tc_subst);
            let needs_drop = self.type_needs_drop(&tc_ty, 0);
            let storage = if self
                .analysis
                .lambda_param_escapes(input.body_id, input.expr_id, index)
            {
                Some(self.reference_storage(builder, ty.clone()))
            } else if input.params[index].is_mut
                || self
                    .analysis
                    .lambda_param_needs_address(input.body_id, input.expr_id, index)
                || needs_drop
            {
                Some(builder.alloca(ty))
            } else {
                None
            };
            if let Some(storage) = storage {
                builder.store(*value, storage);
                if input.params[index].pat.is_none() {
                    self.parameter_storage.insert(
                        CaptureSource::LambdaParam {
                            lambda: input.expr_id,
                            index,
                        },
                        storage,
                    );
                }
            }
            let slots = if needs_drop {
                let place = storage.expect("Drop lambda parameter has storage");
                self.create_drop_slots(builder, place, &tc_ty, Vec::new())
            } else {
                Vec::new()
            };
            if let Some(pat) = input.params[index].pat {
                let source = storage.map_or(LetSource::Value(*value), LetSource::Place);
                let mut bound = Vec::new();
                self.bind_let_pattern(
                    builder,
                    body,
                    LetPatternInput {
                        pat,
                        source,
                        value_ty: &tc_ty,
                        projection: Vec::new(),
                    },
                    &mut bound,
                );
                for (id, projection) in bound {
                    let owned = slots
                        .iter()
                        .filter(|slot| slot.projection.starts_with(&projection))
                        .cloned()
                        .collect::<Vec<_>>();
                    self.register_drop_slots(CaptureSource::Pattern(id), &owned);
                }
            } else if needs_drop {
                self.register_drop_slots(
                    CaptureSource::LambdaParam {
                        lambda: input.expr_id,
                        index,
                    },
                    &slots,
                );
            }
            if !slots.is_empty() {
                self.drop_scopes[0].splice(0..0, slots.into_iter().rev());
            }
        }
    }

    fn initialize_lambda_captures(
        &mut self,
        builder: &mut Builder,
        input: &LambdaFunctionInput<'_>,
        env_param: Value,
    ) {
        if input.info.captures.is_empty() {
            return;
        }
        let env_ptr_ty = Type::Ptr(Box::new(Type::Struct(input.env_struct.clone())));
        let env_ptr = builder.cast(CastOp::PtrToPtr, env_param, env_ptr_ty);
        for (index, (capture, capture_ty)) in input
            .info
            .captures
            .iter()
            .zip(input.capture_types)
            .enumerate()
        {
            let field_ty = input.env_struct.def().fields[index].1.clone();
            let field = builder.field_ptr(env_ptr, index, field_ty.clone());
            let place = match capture.mode {
                CaptureMode::Shared | CaptureMode::Mutable => builder.load(field, field_ty),
                CaptureMode::Value => field,
            };
            self.capture_access.insert(
                capture.place.clone(),
                CaptureAccess {
                    place,
                    ty: capture_ty.clone(),
                },
            );
        }
    }

    pub(super) fn lower_lambda_drop_function(
        &mut self,
        name: &str,
        info: &LambdaInfo,
        capture_types: &[Type],
        env_struct: &StructType,
        heap_env: bool,
    ) {
        let mut function = Function::new(name.to_string(), Type::Unit);
        self.inherit_function_ownership(&mut function);
        let env = function.add_param("__env".into(), closure_env_type());
        function.blocks[function.entry].start_value = function.next_value;
        {
            let mut builder = Builder::new(&mut function);
            let env_ptr_ty = Type::Ptr(Box::new(Type::Struct(env_struct.clone())));
            let env_ptr = builder.cast(CastOp::PtrToPtr, env, env_ptr_ty);
            for (index, (capture, capture_ty)) in
                info.captures.iter().zip(capture_types).enumerate()
            {
                if capture.mode == CaptureMode::Value && self.type_needs_drop(&capture.ty, 0) {
                    let field = builder.field_ptr(env_ptr, index, capture_ty.clone());
                    self.emit_drop_glue(&mut builder, field, &capture.ty);
                }
            }
            if heap_env {
                builder.heap_free(env_ptr);
            }
            if builder.needs_return() {
                builder.set_return(None);
            }
        }
        self.module.add_function(function);
    }

    pub(super) fn capture_value(
        &mut self,
        builder: &mut Builder,
        params: &[Value],
        capture: &CapturePlace,
        ty: &Type,
    ) -> Value {
        if let Some(access) = self.capture_access_for_place(builder, capture) {
            return builder.load(access.place, access.ty);
        }
        let place = self.capture_place(builder, params, capture, ty);
        builder.load(place, ty.clone())
    }

    pub(super) fn capture_place(
        &mut self,
        builder: &mut Builder,
        params: &[Value],
        capture: &CapturePlace,
        ty: &Type,
    ) -> Value {
        if let Some(access) = self.capture_access_for_place(builder, capture) {
            return access.place;
        }
        let root_ty = self.capture_root_mir_type(builder, params, &capture.source, ty);
        let root_place = match &capture.source {
            CaptureSource::Pattern(id) => self.binding_place(builder, *id),
            source @ CaptureSource::Param(..) => self.parameter_place(builder, source),
            source @ CaptureSource::LambdaParam { lambda, .. }
                if self.current_lambda == Some(*lambda) =>
            {
                self.parameter_place(builder, source)
            }
            CaptureSource::LambdaParam { .. } => None,
        }
        .unwrap_or_else(|| {
            let value = self.capture_root_value(builder, params, &capture.source, &root_ty);
            let place = self.reference_storage(builder, root_ty.clone());
            builder.store(value, place);
            place
        });
        Self::project_capture_access(
            builder,
            CaptureAccess {
                place: root_place,
                ty: root_ty,
            },
            &capture.projections,
        )
        .map_or(root_place, |access| access.place)
    }

    pub(super) fn parameter_place(
        &self,
        _builder: &mut Builder,
        source: &CaptureSource,
    ) -> Option<Value> {
        self.parameter_storage.get(source).copied()
    }

    pub(super) fn capture_root_value(
        &self,
        builder: &mut Builder,
        params: &[Value],
        source: &CaptureSource,
        ty: &Type,
    ) -> Value {
        match source {
            CaptureSource::Pattern(id) => self
                .binding_value(builder, *id, ty)
                .unwrap_or_else(|| builder.unit_const()),
            CaptureSource::Param(index) => self
                .parameter_place(builder, source)
                .map(|place| builder.load(place, ty.clone()))
                .or_else(|| params.get(*index).copied())
                .unwrap_or_else(|| builder.unit_const()),
            CaptureSource::LambdaParam { lambda, index }
                if self.current_lambda == Some(*lambda) =>
            {
                self.parameter_place(builder, source)
                    .map(|place| builder.load(place, ty.clone()))
                    .or_else(|| params.get(*index).copied())
                    .unwrap_or_else(|| builder.unit_const())
            }
            CaptureSource::LambdaParam { .. } => builder.unit_const(),
        }
    }

    pub(super) fn capture_access_for_place(
        &self,
        builder: &mut Builder,
        requested: &CapturePlace,
    ) -> Option<CaptureAccess> {
        let (ancestor, access) = self
            .capture_access
            .iter()
            .filter(|(place, _)| place.is_prefix_of(requested))
            .max_by_key(|(place, _)| place.projections.len())
            .map(|(place, access)| (place.clone(), access.clone()))?;
        Self::project_capture_access(
            builder,
            access,
            &requested.projections[ancestor.projections.len()..],
        )
    }

    pub(super) fn project_capture_access(
        builder: &mut Builder,
        mut access: CaptureAccess,
        projections: &[Projection],
    ) -> Option<CaptureAccess> {
        for projection in projections {
            let ty = match (projection, &access.ty) {
                (Projection::Field(index), Type::Struct(strukt)) => {
                    strukt.def().fields.get(*index)?.1.clone()
                }
                (Projection::Field(index), Type::Tuple(elements)) => elements.get(*index)?.clone(),
                (Projection::Index(Some(_)), Type::Array(element, _)) => *element.clone(),
                _ => return None,
            };
            access.place = match projection {
                Projection::Field(index) => builder.field_ptr(access.place, *index, ty.clone()),
                Projection::Index(Some(index)) => {
                    let index = builder.iconst(*index as u64, IntTy::Usize);
                    builder.index_ptr(access.place, index, ty.clone())
                }
                Projection::Index(None) => return None,
            };
            access.ty = ty;
        }
        Some(access)
    }

    pub(super) fn capture_root_mir_type(
        &self,
        builder: &Builder,
        params: &[Value],
        source: &CaptureSource,
        fallback: &Type,
    ) -> Type {
        if let Some(tc_ty) = self.capture_root_type(source) {
            return self.convert_type(&tc_ty);
        }
        let index = match source {
            CaptureSource::Param(index) | CaptureSource::LambdaParam { index, .. } => *index,
            CaptureSource::Pattern(_) => return fallback.clone(),
        };
        params
            .get(index)
            .and_then(|value| {
                builder
                    .func
                    .params
                    .iter()
                    .find(|param| param.value == *value)
                    .map(|param| param.ty.clone())
            })
            .unwrap_or_else(|| fallback.clone())
    }

    pub(super) fn capture_root_type(&self, source: &CaptureSource) -> Option<type_checker::Type> {
        match source {
            CaptureSource::Pattern(id) => self.current_body.and_then(|body_id| {
                self.type_result
                    .pattern_binding_types
                    .get(&(body_id, *id))
                    .cloned()
            }),
            CaptureSource::Param(index) => self.current_function.and_then(|function| {
                self.hir.item_tree.functions[function]
                    .params
                    .get(*index)
                    .map(|param| self.lower_hir_type_for_pattern(&param.ty, &self.generic_tc_subst))
            }),
            CaptureSource::LambdaParam { lambda, index } => {
                let body_id = self.current_body?;
                let Expr::Lambda { params, .. } = &self.hir.bodies[body_id].exprs[*lambda] else {
                    return None;
                };
                params
                    .get(*index)
                    .map(|param| self.lower_hir_type_for_pattern(&param.ty, &self.generic_tc_subst))
            }
        }
    }

    pub(super) fn null_env(builder: &mut Builder) -> Value {
        let zero = builder.iconst(0, IntTy::Usize);
        builder.cast(CastOp::IntToPtr, zero, closure_env_type())
    }

    pub(super) fn lower_function_value(
        &mut self,
        builder: &mut Builder,
        fid: hir::item_tree::FunctionId,
        args: &[type_checker::Type],
        ty: &Type,
    ) -> Value {
        let Some(signature) = closure_call_signature(ty) else {
            return builder.unit_const();
        };
        let args = args
            .iter()
            .map(|arg| self.substitute_tc_type(arg))
            .collect::<Vec<_>>();
        let key = (fid, args.clone());
        let adapter = if let Some(name) = self.function_adapters.get(&key) {
            name.clone()
        } else {
            let target = self
                .mono_function_name_for_args(fid, &args, None)
                .unwrap_or_else(|| self.function_name(fid));
            let name = format!("__riddle_fn_adapter_{target}");
            self.function_adapters.insert(key, name.clone());

            let mut function = Function::new(name.clone(), (*signature.ret).clone());
            self.inherit_function_ownership(&mut function);
            function.add_param("__env".into(), closure_env_type());
            let parameter_names = self.hir.item_tree.functions[fid]
                .params
                .iter()
                .map(|param| param.name.0.clone())
                .collect::<Vec<_>>();
            let arguments = signature
                .params
                .iter()
                .skip(1)
                .enumerate()
                .map(|(index, param_ty)| {
                    let param_name = parameter_names
                        .get(index)
                        .cloned()
                        .unwrap_or_else(|| format!("p{index}"));
                    function.add_param(param_name, param_ty.clone())
                })
                .collect::<Vec<_>>();
            function.blocks[function.entry].start_value = function.next_value;
            let is_extern = self.hir.item_tree.extern_function_ids.contains(&fid)
                && !self.hir.function_bodies.contains_key(&fid);
            let target = if is_extern {
                FuncRef::Extern(target)
            } else {
                FuncRef::Local(target)
            };
            {
                let mut adapter_builder = Builder::new(&mut function);
                let result = adapter_builder.call(target, arguments, (*signature.ret).clone());
                adapter_builder.set_return(
                    (!matches!(signature.ret.as_ref(), Type::Unit | Type::Never)).then_some(result),
                );
            }
            self.module.add_function(function);
            name
        };

        let call = builder.function_ref(FuncRef::Local(adapter), Type::FnPtr(signature));
        let env = Self::null_env(builder);
        let drop_name = self.ensure_noop_closure_drop();
        let drop = builder.function_ref(FuncRef::Local(drop_name), closure_drop_function_type());
        builder.struct_value(vec![call, env, drop], ty.clone())
    }

    pub(super) fn ensure_noop_closure_drop(&mut self) -> String {
        let name = "__riddle_closure_drop_noop".to_string();
        if self
            .module
            .functions
            .values()
            .all(|function| function.name != name)
        {
            let mut function = Function::new(name.clone(), Type::Unit);
            function.add_param("__env".into(), closure_env_type());
            function.blocks[function.entry].start_value = function.next_value;
            Builder::new(&mut function).set_return(None);
            self.module.add_function(function);
        }
        name
    }

    fn inherit_function_ownership(&self, function: &mut Function) {
        let Some(fid) = self.current_function else {
            return;
        };
        function.package = self
            .hir
            .package_for_range(self.hir.item_tree.functions[fid].name_range);
        function.generic_instance = self.function_is_generic_instance(fid);
    }

    fn function_is_generic_instance(&self, fid: hir::item_tree::FunctionId) -> bool {
        let function = &self.hir.item_tree.functions[fid];
        !function.generics.is_empty()
            || !function.implicit_generics.is_empty()
            || !function.const_generics.is_empty()
            || self.default_methods.contains_key(&fid)
            || self
                .impl_for_method(fid)
                .is_some_and(|imp| !imp.generics.is_empty() || !imp.const_generics.is_empty())
    }
}
