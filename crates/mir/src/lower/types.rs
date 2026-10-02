use super::{
    FloatTy, FnPtrType, HashMap, HashSet, IntTy, LowerCtx, ResolvedName, StructDef, StructType,
    Type, closure_value_type, is_dyn_object_safe_method, is_self_associated_path,
    mono_name_from_parts, mono_type_name, mono_type_symbol, tc_const_arg_to_usize,
};

impl LowerCtx<'_> {
    pub(super) fn convert_type(&self, t: &type_checker::Type) -> Type {
        use type_checker::FloatTy as TcFloat;
        use type_checker::IntTy as TcInt;
        use type_checker::Type as TcType;

        let t = self.substitute_tc_type(t);
        match &t {
            TcType::Int(ity) => Type::Int(match ity {
                TcInt::I8 => IntTy::I8,
                TcInt::I16 => IntTy::I16,
                TcInt::I32 => IntTy::I32,
                TcInt::I64 => IntTy::I64,
                TcInt::Isize => IntTy::Isize,
                TcInt::U8 => IntTy::U8,
                TcInt::U16 => IntTy::U16,
                TcInt::U32 => IntTy::U32,
                TcInt::U64 => IntTy::U64,
                TcInt::Usize => IntTy::Usize,
            }),
            TcType::Float(fty) => Type::Float(match fty {
                TcFloat::F32 => FloatTy::F32,
                TcFloat::F64 => FloatTy::F64,
            }),
            TcType::InferInt => Type::Int(IntTy::I32),
            TcType::InferFloat => Type::Float(FloatTy::F64),
            TcType::Bool => Type::Bool,
            TcType::Str => Type::Str,
            TcType::Char => Type::Char,
            TcType::Unit
            | TcType::InferVar(_)
            | TcType::Const(_)
            | TcType::Unknown
            | TcType::Error => Type::Unit,
            TcType::Never => Type::Never,
            TcType::Ref(inner, mutable) => match inner.as_ref() {
                TcType::DynTrait {
                    trait_id,
                    args,
                    assoc_bindings,
                }
                | TcType::OwnedDynTrait {
                    trait_id,
                    args,
                    assoc_bindings,
                } => self.dyn_trait_type(*trait_id, args, assoc_bindings, false),
                _ => Type::Ref(Box::new(self.convert_type(inner)), *mutable),
            },
            TcType::Ptr { inner, .. } => Type::Ptr(Box::new(self.convert_type(inner))),
            TcType::Tuple(elems) => {
                Type::Tuple(elems.iter().map(|e| self.convert_type(e)).collect())
            }
            TcType::Slice(inner) => Type::Slice(Box::new(self.convert_type(inner))),
            TcType::Array(inner, len) => Type::Array(
                Box::new(self.convert_type(inner)),
                len.as_usize().unwrap_or(0),
            ),
            TcType::Struct(sid, args) => self.convert_struct_type(*sid, args),
            TcType::Enum(eid, args) => self.convert_enum_type(*eid, args),
            TcType::FunctionItem {
                function: fid,
                args,
            } => closure_value_type(self.function_item_signature(*fid, args)),
            TcType::CallableConstraint(signature) => closure_value_type(FnPtrType {
                params: signature
                    .params
                    .iter()
                    .map(|param| self.convert_type(param))
                    .collect(),
                ret: Box::new(self.convert_type(&signature.ret)),
            }),
            TcType::Closure { generics, .. } if !generics.is_empty() => Type::Unit,
            TcType::Closure { signature, .. } | TcType::OpaqueCallable { signature, .. } => {
                closure_value_type(FnPtrType {
                    params: signature
                        .params
                        .iter()
                        .map(|param| self.convert_type(param))
                        .collect(),
                    ret: Box::new(self.convert_type(&signature.ret)),
                })
            }
            TcType::OpaqueTrait { id, .. } => self
                .type_result
                .opaque_hidden_types
                .get(id)
                .map_or(Type::Unit, |hidden| self.convert_type(hidden)),
            TcType::Param(name) => self.generic_subst.get(name).cloned().unwrap_or(Type::Unit),
            TcType::DynTrait {
                trait_id,
                args,
                assoc_bindings,
            } => self.dyn_trait_type(*trait_id, args, assoc_bindings, false),
            TcType::OwnedDynTrait {
                trait_id,
                args,
                assoc_bindings,
            } => self.dyn_trait_type(*trait_id, args, assoc_bindings, true),
        }
    }

    pub(super) fn dyn_trait_type(
        &self,
        trait_id: hir::item_tree::TraitId,
        args: &[type_checker::Type],
        assoc_bindings: &[(String, type_checker::Type)],
        owned: bool,
    ) -> Type {
        let mir_args = args
            .iter()
            .map(|arg| self.convert_type(arg))
            .collect::<Vec<_>>();
        let mir_assoc_bindings = assoc_bindings
            .iter()
            .map(|(name, ty)| (name.clone(), self.convert_type(ty)))
            .collect::<Vec<_>>();
        self.dyn_trait_type_from_mir_args(trait_id, &mir_args, &mir_assoc_bindings, owned)
    }

    pub(super) fn convert_dyn_assoc_bindings(
        &self,
        assoc_bindings: &[(String, type_checker::Type)],
    ) -> Vec<(String, Type)> {
        assoc_bindings
            .iter()
            .map(|(name, ty)| (name.clone(), self.convert_type(ty)))
            .collect()
    }

    pub(super) fn dyn_trait_type_from_mir_args(
        &self,
        trait_id: hir::item_tree::TraitId,
        mir_args: &[Type],
        assoc_bindings: &[(String, Type)],
        owned: bool,
    ) -> Type {
        let trait_data = &self.hir.item_tree.traits[trait_id];
        let mut trait_subst = trait_data
            .generics
            .iter()
            .map(|name| name.0.clone())
            .zip(mir_args.iter().cloned())
            .collect::<HashMap<_, _>>();
        for (name, ty) in assoc_bindings {
            trait_subst.insert(format!("Self::{name}"), ty.clone());
        }
        let mut fields = vec![("data".into(), Type::Ptr(Box::new(Type::Unit)))];
        let methods = self.dyn_trait_methods(trait_id, &trait_subst);
        for (method_trait_id, method, method_subst) in &methods {
            if !is_dyn_object_safe_method(method) {
                continue;
            }
            let Some(receiver) = method.params.first() else {
                continue;
            };
            if !matches!(receiver.ty, hir::item_tree::HirTypeRef::Ref(_, _)) {
                continue;
            }
            let mut params = vec![Type::Ptr(Box::new(Type::Unit))];
            let method_subst = method_subst
                .iter()
                .map(|(name, ty)| (name.as_str(), ty))
                .collect::<HashMap<_, _>>();
            params.extend(method.params.iter().skip(1).map(|param| {
                self.convert_hir_type_with_substs(&param.ty, &method_subst, &HashMap::new())
            }));
            let ret = method.ret_type.as_ref().map_or(Type::Unit, |ty| {
                self.convert_hir_type_with_substs(ty, &method_subst, &HashMap::new())
            });
            fields.push((
                self.dyn_trait_method_field_name(&methods, *method_trait_id, &method.name.0),
                Type::FnPtr(FnPtrType {
                    params,
                    ret: Box::new(ret),
                }),
            ));
        }
        if owned {
            fields.push(("drop".into(), dyn_trait_drop_function_type()));
        }
        let id = trait_id.into_raw().into_u32();
        let symbol_args = mir_args
            .iter()
            .map(mono_type_symbol)
            .chain(
                assoc_bindings
                    .iter()
                    .map(|(name, ty)| format!("{name}={}", mono_type_symbol(ty))),
            )
            .collect::<Vec<_>>();
        let name = mono_name_from_parts(&trait_data.name.0, &symbol_args);
        let prefix = if owned { "owned_dyn" } else { "dyn" };
        Type::Struct(StructType::defined(
            format!("{prefix}_{name}"),
            format!("{prefix}_trait::{id}::{name}"),
            fields,
        ))
    }

    pub(super) fn dyn_trait_hir_args_with_substs(
        &self,
        trait_id: hir::item_tree::TraitId,
        ty: &hir::item_tree::HirTypeRef,
        subst: &HashMap<&str, &Type>,
        const_subst: &HashMap<&str, usize>,
    ) -> Vec<Type> {
        let explicit = match ty {
            hir::item_tree::HirTypeRef::Named(path) => path.type_args.as_slice(),
            _ => &[],
        };
        let trait_data = &self.hir.item_tree.traits[trait_id];
        let mut values = subst
            .iter()
            .map(|(name, ty)| ((*name).to_string(), (*ty).clone()))
            .collect::<HashMap<_, _>>();
        trait_data
            .generics
            .iter()
            .enumerate()
            .map(|(index, name)| {
                let arg = explicit.get(index).or_else(|| {
                    trait_data
                        .generic_defaults
                        .get(index)
                        .and_then(Option::as_ref)
                });
                let ty = arg.map_or(Type::Unit, |arg| {
                    let refs = values
                        .iter()
                        .map(|(name, ty)| (name.as_str(), ty))
                        .collect::<HashMap<_, _>>();
                    self.convert_hir_type_with_substs(arg, &refs, const_subst)
                });
                values.insert(name.0.clone(), ty.clone());
                ty
            })
            .collect()
    }

    fn dyn_trait_hir_args(
        &self,
        trait_id: hir::item_tree::TraitId,
        ty: &hir::item_tree::HirTypeRef,
    ) -> Vec<Type> {
        self.dyn_trait_hir_args_with_substs(trait_id, ty, &HashMap::new(), &HashMap::new())
    }

    pub(super) fn dyn_trait_methods(
        &self,
        trait_id: hir::item_tree::TraitId,
        root_subst: &HashMap<String, Type>,
    ) -> Vec<(
        hir::item_tree::TraitId,
        hir::item_tree::HirFunction,
        HashMap<String, Type>,
    )> {
        fn collect(
            ctx: &LowerCtx<'_>,
            trait_id: hir::item_tree::TraitId,
            subst: HashMap<String, Type>,
            visited: &mut HashSet<hir::item_tree::TraitId>,
            methods: &mut Vec<(
                hir::item_tree::TraitId,
                hir::item_tree::HirFunction,
                HashMap<String, Type>,
            )>,
        ) {
            if !visited.insert(trait_id) {
                return;
            }
            let trait_data = &ctx.hir.item_tree.traits[trait_id];
            for method in &trait_data.methods {
                if !methods.iter().any(|(existing_trait, existing, _)| {
                    *existing_trait == trait_id && existing.name == method.name
                }) {
                    methods.push((trait_id, method.clone(), subst.clone()));
                }
            }
            for fid in &trait_data.default_methods {
                let method = ctx.hir.item_tree.functions[*fid].clone();
                if !methods.iter().any(|(existing_trait, existing, _)| {
                    *existing_trait == trait_id && existing.name == method.name
                }) {
                    methods.push((trait_id, method, subst.clone()));
                }
            }
            for bound in &trait_data.supertraits {
                if let Some(supertrait_id) = ctx.resolve_trait_ref(&bound.trait_ty) {
                    let supertrait = &ctx.hir.item_tree.traits[supertrait_id];
                    let explicit = match &bound.trait_ty {
                        hir::item_tree::HirTypeRef::Named(path) => path.type_args.as_slice(),
                        _ => &[],
                    };
                    let mut values = subst.clone();
                    let mut next_subst: HashMap<String, Type> = supertrait
                        .generics
                        .iter()
                        .enumerate()
                        .map(|(index, name)| {
                            let arg = explicit.get(index).or_else(|| {
                                supertrait
                                    .generic_defaults
                                    .get(index)
                                    .and_then(Option::as_ref)
                            });
                            let ty = arg.map_or(Type::Unit, |arg| {
                                let refs = values
                                    .iter()
                                    .map(|(name, ty)| (name.as_str(), ty))
                                    .collect::<HashMap<_, _>>();
                                ctx.convert_hir_type_with_substs(arg, &refs, &HashMap::new())
                            });
                            values.insert(name.0.clone(), ty.clone());
                            (name.0.clone(), ty)
                        })
                        .collect();
                    for (name, ty) in &values {
                        if name.starts_with("Self::") {
                            next_subst.insert(name.clone(), ty.clone());
                        }
                    }
                    collect(ctx, supertrait_id, next_subst, visited, methods);
                }
            }
        }

        let mut methods = Vec::new();
        collect(
            self,
            trait_id,
            root_subst.clone(),
            &mut HashSet::new(),
            &mut methods,
        );
        methods
    }

    pub(super) fn dyn_trait_method_field_name(
        &self,
        methods: &[(
            hir::item_tree::TraitId,
            hir::item_tree::HirFunction,
            HashMap<String, Type>,
        )],
        method_trait_id: hir::item_tree::TraitId,
        method_name: &str,
    ) -> String {
        let duplicate = methods
            .iter()
            .filter(|(_, method, _)| method.name.0 == method_name)
            .count()
            > 1;
        if duplicate {
            format!(
                "method_{}_{}",
                method_trait_id.into_raw().into_u32(),
                method_name
            )
        } else {
            format!("method_{method_name}")
        }
    }

    pub(super) fn function_item_signature(
        &self,
        fid: hir::item_tree::FunctionId,
        args: &[type_checker::Type],
    ) -> FnPtrType {
        let function = &self.hir.item_tree.functions[fid];
        let mut names = Vec::new();
        if let Some(imp) = self.impl_for_method(fid) {
            names.extend(imp.generics.iter().map(|name| name.0.clone()));
        }
        names.extend(function.generics.iter().map(|name| name.0.clone()));
        names.extend(function.implicit_generics.iter().map(|name| name.0.clone()));
        if let Some(imp) = self.impl_for_method(fid) {
            names.extend(imp.const_generics.iter().map(|name| name.0.clone()));
        }
        names.extend(function.const_generics.iter().map(|name| name.0.clone()));

        let mut subst = names
            .into_iter()
            .zip(args.iter().map(|arg| self.substitute_tc_type(arg)))
            .collect::<HashMap<_, _>>();
        if let Some(imp) = self.impl_for_method(fid) {
            let self_ty = self.lower_hir_type_for_pattern(&imp.self_ty, &subst);
            subst.insert("Self".into(), self_ty);
        }

        FnPtrType {
            params: function
                .params
                .iter()
                .map(|param| self.convert_type(&self.lower_hir_type_for_pattern(&param.ty, &subst)))
                .collect(),
            ret: Box::new(function.ret_type.as_ref().map_or(Type::Unit, |ret| {
                self.convert_type(&self.lower_hir_type_for_pattern(ret, &subst))
            })),
        }
    }

    pub(super) fn convert_hir_type(&self, t: &hir::item_tree::HirTypeRef) -> Type {
        self.convert_hir_type_with_mode(t, true)
    }

    fn convert_hir_type_with_mode(&self, t: &hir::item_tree::HirTypeRef, owned_dyn: bool) -> Type {
        match t {
            hir::item_tree::HirTypeRef::Never => Type::Never,
            hir::item_tree::HirTypeRef::Named(path) => {
                let resolved = self.hir.type_resolutions.get(&path.range);
                if let Some(ResolvedName::TypeAlias(alias)) = resolved
                    && let Some(ty) = &self.hir.item_tree.type_aliases[*alias].ty
                {
                    return self.convert_hir_type_with_mode(ty, owned_dyn);
                }
                if let Some(ty) = self.convert_self_associated_type(path) {
                    return ty;
                }
                if is_self_associated_path(path) {
                    return Type::Unit;
                }
                if let Some(name) = path.as_single_name().map(|name| name.0.as_str())
                    && let Some(ty) = self.generic_subst.get(name)
                {
                    return ty.clone();
                }
                match resolved {
                    Some(ResolvedName::Struct(sid)) => {
                        return self.convert_struct_type_from_hir_args(*sid, &path.type_args);
                    }
                    Some(ResolvedName::Enum(eid)) => {
                        return self.convert_enum_type_from_hir_args(*eid, &path.type_args);
                    }
                    _ => {}
                }
                match path.segments.last().map(|n| n.0.as_str()) {
                    Some("bool") => Type::Bool,
                    Some("i8") => Type::Int(IntTy::I8),
                    Some("i16") => Type::Int(IntTy::I16),
                    Some("i64") => Type::Int(IntTy::I64),
                    Some("u8") => Type::Int(IntTy::U8),
                    Some("u16") => Type::Int(IntTy::U16),
                    Some("u32") => Type::Int(IntTy::U32),
                    Some("u64") => Type::Int(IntTy::U64),
                    Some("isize") => Type::Int(IntTy::Isize),
                    Some("usize") => Type::Int(IntTy::Usize),
                    Some("f32") => Type::Float(FloatTy::F32),
                    Some("f64") => Type::Float(FloatTy::F64),
                    Some("str") => Type::Str,
                    Some("char") => Type::Char,
                    Some("i32") | None => Type::Int(IntTy::I32),
                    Some(name) => {
                        if let Some(type_alias) = self.find_associated_type_alias(path)
                            && let Some(ty) = &self.hir.item_tree.type_aliases[type_alias].ty
                        {
                            return self.convert_hir_type(ty);
                        }
                        // Look up user-defined struct by name
                        for (sid, s) in self.hir.item_tree.structs.iter() {
                            if s.name.0 == name {
                                return self
                                    .convert_struct_type_from_hir_args(sid, &path.type_args);
                            }
                        }
                        for (eid, e) in self.hir.item_tree.enums.iter() {
                            if e.name.0 == name {
                                return self.convert_enum_type_from_hir_args(eid, &path.type_args);
                            }
                        }
                        Type::Int(IntTy::I32)
                    }
                }
            }
            hir::item_tree::HirTypeRef::Ref(inner, mutable) => {
                let inner_ty = self.convert_hir_type_with_mode(inner, false);
                if self.hir_type_is_dyn_trait(inner) {
                    inner_ty
                } else {
                    Type::Ref(Box::new(inner_ty), *mutable)
                }
            }
            hir::item_tree::HirTypeRef::Ptr { inner, .. } => {
                Type::Ptr(Box::new(self.convert_hir_type_with_mode(inner, false)))
            }
            hir::item_tree::HirTypeRef::Tuple(elems) if elems.is_empty() => Type::Unit,
            hir::item_tree::HirTypeRef::Tuple(elems) => Type::Tuple(
                elems
                    .iter()
                    .map(|e| self.convert_hir_type_with_mode(e, true))
                    .collect(),
            ),
            hir::item_tree::HirTypeRef::Slice(inner) => {
                Type::Slice(Box::new(self.convert_hir_type_with_mode(inner, true)))
            }
            hir::item_tree::HirTypeRef::Array(inner, len) => Type::Array(
                Box::new(self.convert_hir_type_with_mode(inner, true)),
                self.hir_const_arg_to_usize(len, &HashMap::new()),
            ),
            hir::item_tree::HirTypeRef::Const(_)
            | hir::item_tree::HirTypeRef::Unknown
            | hir::item_tree::HirTypeRef::Error => Type::Unit,
            hir::item_tree::HirTypeRef::ImplTrait {
                callable,
                hidden,
                trait_range,
                ..
            } => {
                if let Some(hidden) = hidden
                    && let Some(ty) = self.generic_subst.get(&hidden.0)
                {
                    return ty.clone();
                }
                if callable.is_none()
                    && let Some(hidden) = self
                        .type_result
                        .opaque_hidden_types
                        .get(&type_checker::OpaqueCallableId(*trait_range))
                        .cloned()
                {
                    return self.convert_type(&hidden);
                }
                callable.as_ref().map_or(Type::Unit, |signature| {
                    closure_value_type(FnPtrType {
                        params: signature
                            .params
                            .iter()
                            .map(|param| self.convert_hir_type_with_mode(param, true))
                            .collect(),
                        ret: Box::new(self.convert_hir_type_with_mode(&signature.ret, true)),
                    })
                })
            }
            hir::item_tree::HirTypeRef::DynTrait {
                trait_ty,
                callable,
                assoc_constraints,
                ..
            } => callable.as_ref().map_or_else(
                || {
                    self.resolve_trait_ref(trait_ty)
                        .map_or(Type::Unit, |trait_id| {
                            let args = self.dyn_trait_hir_args(trait_id, trait_ty);
                            let assoc_bindings = assoc_constraints
                                .iter()
                                .map(|constraint| {
                                    (
                                        constraint.name.0.clone(),
                                        self.convert_hir_type_with_mode(&constraint.ty, true),
                                    )
                                })
                                .collect::<Vec<_>>();
                            self.dyn_trait_type_from_mir_args(
                                trait_id,
                                &args,
                                &assoc_bindings,
                                owned_dyn,
                            )
                        })
                },
                |signature| {
                    closure_value_type(FnPtrType {
                        params: signature
                            .params
                            .iter()
                            .map(|param| self.convert_hir_type_with_mode(param, true))
                            .collect(),
                        ret: Box::new(self.convert_hir_type_with_mode(&signature.ret, true)),
                    })
                },
            ),
        }
    }

    pub(super) fn hir_type_is_dyn_trait(&self, ty: &hir::item_tree::HirTypeRef) -> bool {
        fn visit(
            ctx: &LowerCtx<'_>,
            ty: &hir::item_tree::HirTypeRef,
            visited: &mut HashSet<hir::item_tree::TypeAliasId>,
        ) -> bool {
            let hir::item_tree::HirTypeRef::Named(path) = ty else {
                return matches!(
                    ty,
                    hir::item_tree::HirTypeRef::DynTrait { callable: None, .. }
                );
            };
            let alias = ctx
                .hir
                .type_resolutions
                .get(&path.range)
                .and_then(|resolved| match resolved {
                    ResolvedName::TypeAlias(alias) => Some(*alias),
                    _ => None,
                })
                .or_else(|| ctx.find_associated_type_alias(path));
            let Some(alias) = alias else {
                return false;
            };
            if !visited.insert(alias) {
                return false;
            }
            ctx.hir.item_tree.type_aliases[alias]
                .ty
                .as_ref()
                .is_some_and(|target| visit(ctx, target, visited))
        }

        visit(self, ty, &mut HashSet::new())
    }

    pub(super) fn find_associated_type_alias(
        &self,
        path: &hir::item_tree::HirPath,
    ) -> Option<hir::item_tree::TypeAliasId> {
        if !matches!(path.anchor, hir::item_tree::PathAnchor::Plain) || path.segments.len() != 2 {
            return None;
        }
        let self_ty_name = path.segments[0].0.as_str();
        let alias_name = path.segments[1].0.as_str();

        self.hir.item_tree.impls.iter().find_map(|(_, imp)| {
            let hir::item_tree::HirTypeRef::Named(self_ty_path) = &imp.self_ty else {
                return None;
            };
            if self_ty_path.as_single_name().map(|name| name.0.as_str()) != Some(self_ty_name) {
                return None;
            }
            imp.type_aliases.iter().find_map(|alias_id| {
                (self.hir.item_tree.type_aliases[*alias_id].name.0 == alias_name)
                    .then_some(*alias_id)
            })
        })
    }

    pub(super) fn convert_self_associated_type(
        &self,
        path: &hir::item_tree::HirPath,
    ) -> Option<Type> {
        if !is_self_associated_path(path) {
            return None;
        }
        let alias_name = path.segments[1].0.as_str();
        // Monomorphized default methods carry the concrete associated types
        // of the implementing impl in their substitution.
        let key = format!("Self::{}", alias_name);
        if let Some(ty) = self.generic_subst.get(&key) {
            return Some(ty.clone());
        }
        let imp = self.impl_for_method(self.current_function?)?;
        if let Some(alias_id) = imp
            .type_aliases
            .iter()
            .find(|alias_id| self.hir.item_tree.type_aliases[**alias_id].name.0 == alias_name)
        {
            return Some(
                self.hir.item_tree.type_aliases[*alias_id]
                    .ty
                    .as_ref()
                    .map_or(Type::Unit, |ty| self.convert_hir_type(ty)),
            );
        }
        // The alias lives in a sibling impl (e.g. `impl<T> Index for Vector<T>`),
        // so match the concrete Self against all impls, like the type-checker's
        // `lower_self_associated_type` does.
        let self_ty = self.generic_subst.get("Self")?;
        let self_tc_ty = self.substitute_tc_type(
            &self.lower_hir_type_for_pattern(&imp.self_ty, &self.generic_tc_subst),
        );
        self.hir.item_tree.impls.iter().find_map(|(_, candidate)| {
            let alias_id = candidate.type_aliases.iter().find(|alias_id| {
                self.hir.item_tree.type_aliases[**alias_id].name.0 == alias_name
            })?;
            if !self.impl_type_matches(candidate, &self_tc_ty) {
                return None;
            }
            let subst = self.impl_mir_subst(candidate, &self_tc_ty)?;
            let mut type_subst = subst
                .types
                .iter()
                .map(|(name, ty)| (name.as_str(), ty))
                .collect::<HashMap<_, _>>();
            type_subst.insert("Self", self_ty);
            let const_subst = subst
                .consts
                .iter()
                .map(|(name, value)| (name.as_str(), *value))
                .collect::<HashMap<_, _>>();
            let alias = &self.hir.item_tree.type_aliases[*alias_id];
            Some(alias.ty.as_ref().map_or(Type::Unit, |ty| {
                self.convert_hir_type_with_substs(ty, &type_subst, &const_subst)
            }))
        })
    }

    pub(super) fn convert_struct_type(
        &self,
        sid: hir::item_tree::StructId,
        args: &[type_checker::Type],
    ) -> Type {
        let s = &self.hir.item_tree.structs[sid];
        let type_count = s.generics.len();
        let mir_args = args
            .iter()
            .take(type_count)
            .map(|arg| self.convert_type(arg))
            .collect::<Vec<_>>();
        let const_args = args
            .iter()
            .skip(type_count)
            .filter_map(tc_const_arg_to_usize)
            .collect::<Vec<_>>();
        self.convert_struct_type_from_parts(sid, &mir_args, &const_args)
    }

    pub(super) fn convert_struct_type_from_hir_args(
        &self,
        sid: hir::item_tree::StructId,
        args: &[hir::item_tree::HirTypeRef],
    ) -> Type {
        let s = &self.hir.item_tree.structs[sid];
        let type_count = s.generics.len();
        let mir_args = args
            .iter()
            .take(type_count)
            .map(|arg| self.convert_hir_type(arg))
            .collect::<Vec<_>>();
        let const_args = args
            .iter()
            .skip(type_count)
            .map(|arg| self.hir_type_ref_const_arg_to_usize(arg, &HashMap::new()))
            .collect::<Vec<_>>();
        self.convert_struct_type_from_parts(sid, &mir_args, &const_args)
    }

    pub(super) fn convert_struct_type_from_parts(
        &self,
        sid: hir::item_tree::StructId,
        type_args: &[Type],
        const_args: &[usize],
    ) -> Type {
        let s = &self.hir.item_tree.structs[sid];
        let name = mono_name_from_parts(&s.name.0, &type_name_args(type_args, const_args));
        let symbol = format!(
            "struct::{}::{}",
            sid.into_raw().into_u32(),
            mono_name_from_parts(&s.name.0, &type_symbol_args(type_args, const_args))
        );
        if let Some(cached) = self.struct_types.borrow().get(&symbol).cloned() {
            return Type::Struct(cached);
        }
        let subst = s
            .generics
            .iter()
            .zip(type_args.iter())
            .map(|(name, ty)| (name.0.as_str(), ty))
            .collect::<HashMap<_, _>>();
        let const_subst = s
            .const_generics
            .iter()
            .zip(const_args.iter())
            .map(|(name, value)| (name.0.as_str(), *value))
            .collect::<HashMap<_, _>>();
        // The handle is registered before the fields are converted, so a field
        // that reaches this struct again resolves to the definition being built
        // instead of converting it a second time.
        let handle = StructType::new_cyclic(|def| {
            self.struct_types.borrow_mut().insert(
                symbol.clone(),
                StructType::recursive(def, name.clone(), symbol.clone()),
            );
            let fields = s
                .fields
                .iter()
                .map(|f| {
                    (
                        f.name.0.clone(),
                        self.convert_hir_type_with_substs(&f.ty, &subst, &const_subst),
                    )
                })
                .collect();
            StructDef {
                name: name.clone(),
                symbol: symbol.clone(),
                fields,
            }
        });
        self.struct_defs.borrow_mut().push(handle.def());
        self.struct_types
            .borrow_mut()
            .insert(symbol, handle.clone());
        Type::Struct(handle)
    }

    pub(super) fn convert_enum_type(
        &self,
        eid: hir::item_tree::EnumId,
        args: &[type_checker::Type],
    ) -> Type {
        let e = &self.hir.item_tree.enums[eid];
        let type_count = e.generics.len();
        let mir_args = args
            .iter()
            .take(type_count)
            .map(|arg| self.convert_type(arg))
            .collect::<Vec<_>>();
        let const_args = args
            .iter()
            .skip(type_count)
            .filter_map(tc_const_arg_to_usize)
            .collect::<Vec<_>>();
        self.convert_enum_type_from_parts(eid, &mir_args, &const_args)
    }

    pub(super) fn convert_enum_type_from_hir_args(
        &self,
        eid: hir::item_tree::EnumId,
        args: &[hir::item_tree::HirTypeRef],
    ) -> Type {
        let e = &self.hir.item_tree.enums[eid];
        let type_count = e.generics.len();
        let mir_args = args
            .iter()
            .take(type_count)
            .map(|arg| self.convert_hir_type(arg))
            .collect::<Vec<_>>();
        let const_args = args
            .iter()
            .skip(type_count)
            .map(|arg| self.hir_type_ref_const_arg_to_usize(arg, &HashMap::new()))
            .collect::<Vec<_>>();
        self.convert_enum_type_from_parts(eid, &mir_args, &const_args)
    }

    pub(super) fn convert_enum_type_from_parts(
        &self,
        eid: hir::item_tree::EnumId,
        type_args: &[Type],
        const_args: &[usize],
    ) -> Type {
        let e = &self.hir.item_tree.enums[eid];
        // Enums lower to a tagged struct, so they share the nominal cache and
        // the recursive-definition handling with structs.
        let name = mono_name_from_parts(&e.name.0, &type_name_args(type_args, const_args));
        let symbol = format!(
            "enum::{}::{}",
            eid.into_raw().into_u32(),
            mono_name_from_parts(&e.name.0, &type_symbol_args(type_args, const_args))
        );
        if let Some(cached) = self.struct_types.borrow().get(&symbol).cloned() {
            return Type::Struct(cached);
        }
        let subst = e
            .generics
            .iter()
            .zip(type_args.iter())
            .map(|(name, ty)| (name.0.as_str(), ty))
            .collect::<HashMap<_, _>>();
        let const_subst = e
            .const_generics
            .iter()
            .zip(const_args.iter())
            .map(|(name, value)| (name.0.as_str(), *value))
            .collect::<HashMap<_, _>>();
        let handle = StructType::new_cyclic(|def| {
            self.struct_types.borrow_mut().insert(
                symbol.clone(),
                StructType::recursive(def, name.clone(), symbol.clone()),
            );
            let mut fields = vec![("tag".to_string(), Type::Int(IntTy::U32))];
            for variant in &e.variants {
                match &variant.kind {
                    hir::item_tree::HirVariantKind::Tuple(items) => {
                        for (index, item) in items.iter().enumerate() {
                            fields.push((
                                format!("{}_{}", variant.name.0, index),
                                self.convert_hir_type_with_substs(item, &subst, &const_subst),
                            ));
                        }
                    }
                    hir::item_tree::HirVariantKind::Struct(items) => {
                        for item in items {
                            fields.push((
                                format!("{}_{}", variant.name.0, item.name.0),
                                self.convert_hir_type_with_substs(&item.ty, &subst, &const_subst),
                            ));
                        }
                    }
                    hir::item_tree::HirVariantKind::Unit => {}
                }
            }
            StructDef {
                name: name.clone(),
                symbol: symbol.clone(),
                fields,
            }
        });
        self.struct_defs.borrow_mut().push(handle.def());
        self.struct_types
            .borrow_mut()
            .insert(symbol, handle.clone());
        Type::Struct(handle)
    }
}

fn type_name_args(type_args: &[Type], const_args: &[usize]) -> Vec<String> {
    type_args
        .iter()
        .map(mono_type_name)
        .chain(const_args.iter().map(std::string::ToString::to_string))
        .collect()
}

fn type_symbol_args(type_args: &[Type], const_args: &[usize]) -> Vec<String> {
    type_args
        .iter()
        .map(mono_type_symbol)
        .chain(const_args.iter().map(std::string::ToString::to_string))
        .collect()
}

pub(super) fn dyn_trait_drop_function_type() -> Type {
    Type::FnPtr(FnPtrType {
        params: vec![Type::Ptr(Box::new(Type::Unit))],
        ret: Box::new(Type::Unit),
    })
}
