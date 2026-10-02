use super::{
    BuiltinOperator, Expr, ExprId, FnPtrType, HashMap, HashSet, LowerCtx, MirSubst, ResolvedName,
    Type, builtin_operator, builtin_operator_supports, closure_value_type, is_self_associated_path,
    is_self_output, mono_type_name, operator_params_match, primitive_scalar_name, returns_unit,
    tc_const_arg_to_usize, trait_operator_contract, type_matches_self,
};

impl LowerCtx<'_> {
    pub(super) fn mono_method_name(
        &mut self,
        fid: hir::item_tree::FunctionId,
        base: ExprId,
        rhs: Option<ExprId>,
    ) -> Option<String> {
        let body_id = self.current_body?;
        let receiver_ty = self.type_result.expr_types.get(&(body_id, base))?;
        let rhs_ty = rhs.and_then(|rhs| self.type_result.expr_types.get(&(body_id, rhs)));
        self.mono_method_name_for_receiver(fid, receiver_ty, rhs_ty)
    }

    pub(super) fn mono_method_name_for_receiver(
        &mut self,
        fid: hir::item_tree::FunctionId,
        receiver_ty: &type_checker::Type,
        rhs_ty: Option<&type_checker::Type>,
    ) -> Option<String> {
        let receiver_ty = self.substitute_tc_type(receiver_ty);
        if self.default_methods.contains_key(&fid) {
            return self.mono_default_method_name(fid, &receiver_ty, rhs_ty);
        }
        let imp = self.impl_for_method(fid)?.clone();
        if imp.generics.is_empty() && imp.const_generics.is_empty() {
            return None;
        }
        // An impl method reached through a reference receiver (the implicit
        // `self` of a default trait method, say) keys its instance by the
        // type the impl actually unifies with — the dereferenced self — so
        // direct calls and reentrant default-method calls share one instance
        // instead of spawning a `ref_`-suffixed twin with drifted arity.
        let receiver_ty = match &receiver_ty {
            type_checker::Type::Ref(inner, _)
                if self.impl_mir_subst(&imp, &receiver_ty).is_none() =>
            {
                (**inner).clone()
            }
            _ => receiver_ty,
        };
        let receiver_mir_ty = self.convert_type(&receiver_ty);
        let subst = self
            .impl_mir_subst(&imp, &receiver_ty)
            .or_else(|| match &receiver_ty {
                type_checker::Type::Ref(inner, _) => self.impl_mir_subst(&imp, inner),
                _ => None,
            })?;
        let type_subst = subst
            .types
            .iter()
            .map(|(name, ty)| (name.as_str(), ty))
            .collect::<HashMap<_, _>>();
        let const_subst = subst
            .consts
            .iter()
            .map(|(name, value)| (name.as_str(), *value))
            .collect::<HashMap<_, _>>();
        let trait_args = match imp.trait_ty.as_ref() {
            Some(hir::item_tree::HirTypeRef::Named(path)) => path
                .type_args
                .iter()
                .map(|arg| {
                    mono_type_name(&self.convert_hir_type_with_substs(
                        arg,
                        &type_subst,
                        &const_subst,
                    ))
                })
                .collect::<Vec<_>>(),
            _ => Vec::new(),
        };
        let suffix = std::iter::once(mono_type_name(&receiver_mir_ty))
            .chain(trait_args)
            .collect::<Vec<_>>()
            .join("_");
        let key = (fid, suffix.clone());
        if let Some(name) = self.mono_methods.get(&key) {
            return Some(name.clone());
        }
        let original_name = self.method_symbol_base(fid);
        let mono_name = format!("{original_name}__{suffix}");
        if !self.mono_generated_symbols.insert(mono_name.clone()) {
            self.mono_methods.insert(key, mono_name.clone());
            return Some(mono_name);
        }
        self.mono_methods.insert(key, mono_name.clone());
        // Seed the impl's own associated type aliases (`type Item = &T`)
        // under both `Self::Name` and `Name`, resolved with the instance's
        // generic substitution — the body's `Self::Item` otherwise lowers to
        // a placeholder when an impl method is instantiated out of a generic
        // caller. Mirrors what `mono_default_method_name` does for defaults.
        let mut mir_subst = subst.types;
        let mut tc_subst = subst.tc_types;
        let const_view: HashMap<&str, usize> = subst
            .consts
            .iter()
            .map(|(name, value)| (name.as_str(), *value))
            .collect();
        for alias_id in &imp.type_aliases {
            let alias = &self.hir.item_tree.type_aliases[*alias_id];
            let Some(alias_ty) = alias.ty.as_ref() else {
                continue;
            };
            let type_view: HashMap<&str, &Type> = mir_subst
                .iter()
                .map(|(name, ty)| (name.as_str(), ty))
                .collect();
            let resolved_mir = self.convert_hir_type_with_substs(alias_ty, &type_view, &const_view);
            let resolved_tc = self.lower_hir_type_for_pattern(alias_ty, &tc_subst);
            let self_key = format!("Self::{}", alias.name.0);
            tc_subst.insert(self_key.clone(), resolved_tc.clone());
            tc_subst.insert(alias.name.0.clone(), resolved_tc);
            mir_subst.insert(self_key.clone(), resolved_mir.clone());
            mir_subst.insert(alias.name.0.clone(), resolved_mir);
        }
        let old_subst = std::mem::replace(&mut self.generic_subst, mir_subst);
        let old_tc_subst = std::mem::replace(&mut self.generic_tc_subst, tc_subst);
        let old_const_subst = std::mem::replace(&mut self.generic_const_subst, subst.consts);
        let old_state = self.take_lowering_state();
        let body_id = *self.hir.function_bodies.get(&fid)?;
        let func = self.lower_function(fid, mono_name.clone(), body_id);
        self.restore_lowering_state(old_state);
        self.generic_subst = old_subst;
        self.generic_tc_subst = old_tc_subst;
        self.generic_const_subst = old_const_subst;
        self.module.add_function(func);
        Some(mono_name)
    }

    fn mono_default_method_name(
        &mut self,
        fid: hir::item_tree::FunctionId,
        receiver_ty: &type_checker::Type,
        rhs_ty: Option<&type_checker::Type>,
    ) -> Option<String> {
        let receiver_ty = match receiver_ty {
            type_checker::Type::Ref(inner, _) => inner.as_ref(),
            other => other,
        };
        let receiver_mir_ty = self.convert_type(receiver_ty);
        let trait_id = self.default_methods[&fid];
        let trait_generics = self.hir.item_tree.traits[trait_id].generics.clone();
        // The first trait generic (e.g. `Rhs`) is derived from the rhs
        // operand's expression type — but the argument convention passes it
        // by reference (`fun lt(&self, other: &Rhs)`), so a ref-typed rhs
        // names the pointee, not the reference.
        let rhs_ty = rhs_ty
            .map(|ty| self.substitute_tc_type(ty))
            .map(|ty| match &ty {
                type_checker::Type::Ref(inner, _) => (**inner).clone(),
                _ => ty,
            });
        let trait_args = trait_generics
            .iter()
            .enumerate()
            .map(|(index, _)| {
                if index == 0 {
                    rhs_ty.clone().unwrap_or_else(|| receiver_ty.clone())
                } else {
                    receiver_ty.clone()
                }
            })
            .collect::<Vec<_>>();
        let suffix = std::iter::once(mono_type_name(&receiver_mir_ty))
            .chain(
                trait_args
                    .iter()
                    .map(|arg| mono_type_name(&self.convert_type(arg))),
            )
            .collect::<Vec<_>>()
            .join("_");
        let key = (fid, suffix.clone());
        if let Some(name) = self.mono_methods.get(&key) {
            return Some(name.clone());
        }

        let mono_name = format!("{}__{}", self.method_symbol_base(fid), suffix);
        if !self.mono_generated_symbols.insert(mono_name.clone()) {
            self.mono_methods.insert(key, mono_name.clone());
            return Some(mono_name);
        }
        self.mono_methods.insert(key, mono_name.clone());
        let mut tc_subst = HashMap::from([("Self".into(), receiver_ty.clone())]);
        tc_subst.extend(
            trait_generics
                .iter()
                .zip(trait_args)
                .map(|(name, ty)| (name.0.clone(), ty)),
        );
        // The default body refers to the trait's associated types
        // (`Self::Item`), so resolve them from the concrete impl that is
        // monomorphizing this body; otherwise they lower to placeholders.
        self.insert_impl_assoc_types(trait_id, receiver_ty, &mut tc_subst, None);
        let mir_subst = tc_subst
            .iter()
            .map(|(name, ty)| (name.clone(), self.convert_type(ty)))
            .collect();
        let old_subst = std::mem::replace(&mut self.generic_subst, mir_subst);
        let old_tc_subst = std::mem::replace(&mut self.generic_tc_subst, tc_subst);
        let old_state = self.take_lowering_state();
        let body_id = *self.hir.function_bodies.get(&fid)?;
        let func = self.lower_function(fid, mono_name.clone(), body_id);
        self.restore_lowering_state(old_state);
        self.generic_subst = old_subst;
        self.generic_tc_subst = old_tc_subst;
        self.module.add_function(func);
        Some(mono_name)
    }

    /// Resolves the trait's associated types from the concrete impl that
    /// matches `receiver_ty`, inserting both `Self::Name` and `Name` keys so
    /// default-method bodies and signatures substitute them concretely.
    fn insert_impl_assoc_types(
        &mut self,
        trait_id: hir::item_tree::TraitId,
        receiver_ty: &type_checker::Type,
        tc_subst: &mut HashMap<String, type_checker::Type>,
        mut mir_subst: Option<&mut HashMap<String, Type>>,
    ) {
        // Associated types may come from supertrait impls (e.g. `Item` lives
        // on `Iterator` while the default method belongs to an extending
        // trait), so walk the whole trait family.
        let mut family = std::collections::VecDeque::from([trait_id]);
        let mut seen: HashSet<hir::item_tree::TraitId> = HashSet::from([trait_id]);
        while let Some(current) = family.pop_front() {
            for bound in &self.hir.item_tree.traits[current].supertraits {
                if let Some(super_id) = self.resolve_trait_ref(&bound.trait_ty)
                    && seen.insert(super_id)
                {
                    family.push_back(super_id);
                }
            }
        }
        for (_, candidate) in self.hir.item_tree.impls.iter() {
            let Some(candidate_trait) = candidate.trait_ty.as_ref() else {
                continue;
            };
            let Some(candidate_id) = self.resolve_trait_ref(candidate_trait) else {
                continue;
            };
            if !seen.contains(&candidate_id) || !self.impl_type_matches(candidate, receiver_ty) {
                continue;
            }
            if let Some(subst) = self.impl_mir_subst(candidate, receiver_ty) {
                for alias_id in &candidate.type_aliases {
                    let alias = &self.hir.item_tree.type_aliases[*alias_id];
                    let Some(alias_ty) = alias.ty.as_ref() else {
                        continue;
                    };
                    let resolved = self.lower_hir_type_for_pattern(alias_ty, &subst.tc_types);
                    let resolved_mir = self.convert_type(&resolved);
                    tc_subst.insert(format!("Self::{}", alias.name.0), resolved.clone());
                    tc_subst.insert(alias.name.0.clone(), resolved);
                    if let Some(mir_subst) = mir_subst.as_deref_mut() {
                        mir_subst.insert(format!("Self::{}", alias.name.0), resolved_mir.clone());
                        mir_subst.insert(alias.name.0.clone(), resolved_mir);
                    }
                }
            }
            break;
        }
    }

    pub(super) fn mono_function_name(
        &mut self,
        fid: hir::item_tree::FunctionId,
        callee: ExprId,
    ) -> Option<String> {
        let body_id = self.current_body?;
        // Trait default methods are reached through a receiver even though
        // they belong to no impl; carry the receiver so `Self` and the
        // trait's associated types substitute concretely.
        let receiver = if self.default_methods.contains_key(&fid)
            && let Expr::FieldAccess { base, .. } = &self.hir.bodies[body_id].exprs[callee]
        {
            self.type_result.expr_types.get(&(body_id, *base)).cloned()
        } else {
            None
        };
        let tc_args = if let Some(call) = self.type_result.generic_calls.get(&(body_id, callee)) {
            call.args.clone()
        } else {
            match self.type_result.expr_types.get(&(body_id, callee))? {
                type_checker::Type::FunctionItem { args, .. } if !args.is_empty() => args.clone(),
                _ => return None,
            }
        };
        self.mono_function_name_for_args(fid, &tc_args, receiver)
    }

    pub(super) fn mono_function_name_for_args(
        &mut self,
        fid: hir::item_tree::FunctionId,
        tc_args: &[type_checker::Type],
        receiver_tc: Option<type_checker::Type>,
    ) -> Option<String> {
        if !self.hir.function_bodies.contains_key(&fid) {
            return None;
        }
        let function = self.hir.item_tree.functions[fid].clone();
        let imp = self.impl_for_method(fid).cloned();
        let outer_generics = imp
            .as_ref()
            .map(|imp| imp.generics.as_slice())
            .unwrap_or_default();
        let outer_const_generics = imp
            .as_ref()
            .map(|imp| imp.const_generics.as_slice())
            .unwrap_or_default();
        if outer_generics.is_empty()
            && outer_const_generics.is_empty()
            && function.generics.is_empty()
            && function.implicit_generics.is_empty()
            && function.const_generics.is_empty()
        {
            return None;
        }
        let tc_args = tc_args
            .iter()
            .map(|arg| self.substitute_tc_type(arg))
            .collect::<Vec<_>>();
        let type_names = outer_generics
            .iter()
            .chain(function.generics.iter())
            .chain(function.implicit_generics.iter())
            .map(|name| name.0.clone())
            .collect::<Vec<_>>();
        let const_names = outer_const_generics
            .iter()
            .chain(function.const_generics.iter())
            .map(|name| name.0.clone())
            .collect::<Vec<_>>();
        let tc_subst = type_names
            .iter()
            .chain(const_names.iter())
            .zip(tc_args.iter())
            .map(|(name, ty)| (name.clone(), ty.clone()))
            .collect::<HashMap<_, _>>();
        let outer_tc_subst = outer_generics
            .iter()
            .zip(tc_args.iter())
            .map(|(name, ty)| (name.0.clone(), ty.clone()))
            .chain(
                outer_const_generics
                    .iter()
                    .zip(tc_args.iter().skip(type_names.len()))
                    .map(|(name, ty)| (name.0.clone(), ty.clone())),
            )
            .collect::<HashMap<_, _>>();
        let mut subst = type_names
            .iter()
            .zip(tc_args.iter())
            .map(|(name, ty)| (name.clone(), self.convert_type(ty)))
            .collect::<HashMap<_, _>>();
        let const_subst = const_names
            .iter()
            .zip(tc_args.iter().skip(type_names.len()))
            .filter_map(|(name, ty)| tc_const_arg_to_usize(ty).map(|value| (name.clone(), value)))
            .collect::<HashMap<_, _>>();
        let self_tc_ty = imp
            .as_ref()
            .map(|imp| self.lower_hir_type_for_pattern(&imp.self_ty, &outer_tc_subst))
            .or_else(|| {
                receiver_tc.as_ref().map(|receiver| {
                    let receiver = match receiver {
                        type_checker::Type::Ref(inner, _) => inner.as_ref().clone(),
                        other => other.clone(),
                    };
                    self.substitute_tc_type(&receiver)
                })
            });
        let self_mir_ty = self_tc_ty.as_ref().map(|ty| self.convert_type(ty));
        if let Some(self_ty) = &self_mir_ty {
            subst.insert("Self".into(), self_ty.clone());
        }
        let outer_const_start = type_names.len();
        let outer_const_end = outer_const_start + outer_const_generics.len();
        let suffix = self.mono_function_suffix(
            &tc_args,
            self_mir_ty.as_ref(),
            outer_generics.len(),
            outer_const_start..outer_const_end,
        );
        let key = (fid, suffix.clone());
        if let Some(name) = self.mono_functions.get(&key) {
            return Some(name.clone());
        }

        let mut tc_subst = tc_subst;
        if let Some(self_ty) = self_tc_ty {
            tc_subst.insert("Self".into(), self_ty);
        }
        if self.default_methods.contains_key(&fid)
            && let Some(receiver) = &receiver_tc
            && let Some(trait_id) = self.default_methods.get(&fid)
        {
            let receiver = match receiver {
                type_checker::Type::Ref(inner, _) => inner.as_ref().clone(),
                other => other.clone(),
            };
            let receiver = self.substitute_tc_type(&receiver);
            self.insert_impl_assoc_types(*trait_id, &receiver, &mut tc_subst, Some(&mut subst));
        }
        // Implicit generic slots introduced by associated-type bounds
        // (`fun f<I: Iterator<Item = T>>(...)` gets an implicit `T`) arrive
        // as Unit placeholders when the checker could not infer them in the
        // generic body; resolve them now from the bound's trait impl for
        // the substituted parameter type.
        for bound in &function.generic_bounds {
            let Some(param_ty) = tc_subst.get(&bound.param.0).cloned() else {
                continue;
            };
            let Some(trait_id) = self.resolve_trait_ref(&bound.trait_ty) else {
                continue;
            };
            let constraint_names = bound
                .assoc_constraints
                .iter()
                .map(|constraint| {
                    let name = match &constraint.ty {
                        hir::item_tree::HirTypeRef::Named(path) => {
                            path.as_single_name().map(|name| name.0.clone())
                        }
                        _ => None,
                    };
                    (constraint.name.0.clone(), name)
                })
                .collect::<Vec<_>>();
            for (assoc_name, slot_name) in constraint_names {
                let Some(slot) = slot_name else { continue };
                let unresolved = !tc_subst.contains_key(&slot)
                    || matches!(tc_subst.get(&slot), Some(type_checker::Type::Unit));
                if !unresolved {
                    continue;
                }
                for candidate in self.hir.item_tree.impls.values() {
                    let Some(candidate_trait) = candidate.trait_ty.as_ref() else {
                        continue;
                    };
                    let Some(candidate_id) = self.resolve_trait_ref(candidate_trait) else {
                        continue;
                    };
                    if candidate_id != trait_id || !self.impl_type_matches(candidate, &param_ty) {
                        continue;
                    }
                    let Some(alias_id) = candidate.type_aliases.iter().find(|alias_id| {
                        self.hir.item_tree.type_aliases[**alias_id].name.0 == assoc_name
                    }) else {
                        continue;
                    };
                    let alias = &self.hir.item_tree.type_aliases[*alias_id];
                    let Some(alias_ty) = alias.ty.as_ref() else {
                        continue;
                    };
                    let Some(impl_subst) = self.impl_mir_subst(candidate, &param_ty) else {
                        continue;
                    };
                    let resolved_tc =
                        self.lower_hir_type_for_pattern(alias_ty, &impl_subst.tc_types);
                    let resolved_mir = self.convert_type(&resolved_tc);
                    tc_subst.insert(slot.clone(), resolved_tc);
                    subst.insert(slot.clone(), resolved_mir);
                }
            }
        }
        // An impl method's own associated type aliases (`type Item = &T`)
        // resolve through the impl's generics; seed them under `Self::Name`
        // and `Name` so the body's `Self::Item` substitutes concretely —
        // same treatment `mono_method_name_for_receiver` gives its instances.
        if let Some(imp) = &imp {
            for alias_id in imp.type_aliases.clone() {
                let alias = &self.hir.item_tree.type_aliases[alias_id];
                let Some(alias_ty) = alias.ty.as_ref() else {
                    continue;
                };
                let resolved_tc = self.lower_hir_type_for_pattern(alias_ty, &tc_subst);
                let resolved_mir = self.convert_type(&resolved_tc);
                let self_key = format!("Self::{}", alias.name.0);
                tc_subst.insert(self_key.clone(), resolved_tc.clone());
                tc_subst.insert(alias.name.0.clone(), resolved_tc);
                subst.insert(self_key.clone(), resolved_mir.clone());
                subst.insert(alias.name.0.clone(), resolved_mir);
            }
        }
        let mono_name = format!("{}__{}", self.method_symbol_base(fid), suffix);
        if !self.mono_generated_symbols.insert(mono_name.clone()) {
            self.mono_functions.insert(key, mono_name.clone());
            return Some(mono_name);
        }
        self.mono_functions.insert(key, mono_name.clone());

        let old_subst = std::mem::replace(&mut self.generic_subst, subst);
        let old_tc_subst = std::mem::replace(&mut self.generic_tc_subst, tc_subst);
        let old_const_subst = std::mem::replace(&mut self.generic_const_subst, const_subst);
        let old_state = self.take_lowering_state();
        let body_id = *self.hir.function_bodies.get(&fid)?;
        let func = self.lower_function(fid, mono_name.clone(), body_id);
        self.restore_lowering_state(old_state);
        self.generic_subst = old_subst;
        self.generic_tc_subst = old_tc_subst;
        self.generic_const_subst = old_const_subst;
        self.module.add_function(func);
        Some(mono_name)
    }

    fn mono_function_suffix(
        &self,
        args: &[type_checker::Type],
        self_ty: Option<&Type>,
        outer_type_count: usize,
        outer_const_range: std::ops::Range<usize>,
    ) -> String {
        let mut suffix = self_ty
            .map(|ty| vec![mono_type_name(ty)])
            .unwrap_or_default();
        for (index, arg) in args.iter().enumerate() {
            if self_ty.is_some() && (index < outer_type_count || outer_const_range.contains(&index))
            {
                continue;
            }
            suffix.push(tc_const_arg_to_usize(arg).map_or_else(
                || mono_type_name(&self.convert_type(arg)),
                |value| value.to_string(),
            ));
        }
        suffix.join("_")
    }

    pub(super) fn impl_for_method(
        &self,
        fid: hir::item_tree::FunctionId,
    ) -> Option<&hir::item_tree::HirImpl> {
        self.method_impls
            .get(&fid)
            .map(|impl_id| &self.hir.item_tree.impls[*impl_id])
    }

    pub(super) fn builtin_operator_for_method(
        &self,
        fid: hir::item_tree::FunctionId,
    ) -> Option<BuiltinOperator> {
        let imp = self.impl_for_method(fid)?;
        if !imp.generics.is_empty() || !imp.const_generics.is_empty() {
            return None;
        }
        let scalar = primitive_scalar_name(&imp.self_ty)?;
        let trait_id = self.resolve_trait_ref(imp.trait_ty.as_ref()?)?;
        let trait_item = &self.hir.item_tree.traits[trait_id];
        let lang_item = self.type_result.trait_env.lang_items.lang_of(trait_id)?;
        let function = &self.hir.item_tree.functions[fid];
        if !function.generics.is_empty() || !function.const_generics.is_empty() {
            return None;
        }
        let method = function.name.0.as_str();
        let op = builtin_operator(lang_item.as_str(), method)?;
        builtin_operator_supports(op, scalar).then_some(())?;
        trait_operator_contract(trait_item, method, op).then_some(())?;
        self.impl_operator_contract(imp, function, op).then_some(op)
    }

    pub(super) fn impl_operator_contract(
        &self,
        imp: &hir::item_tree::HirImpl,
        function: &hir::item_tree::HirFunction,
        op: BuiltinOperator,
    ) -> bool {
        if !operator_params_match(function, &imp.self_ty, op) {
            return false;
        }
        match op {
            BuiltinOperator::Assign(_) => returns_unit(function),
            BuiltinOperator::Binary(_) | BuiltinOperator::Unary(_) => {
                let Some(ret) = function.ret_type.as_ref() else {
                    return false;
                };
                if type_matches_self(ret, &imp.self_ty) {
                    return true;
                }
                is_self_output(ret)
                    && imp.type_aliases.iter().any(|alias_id| {
                        let alias = &self.hir.item_tree.type_aliases[*alias_id];
                        alias.name.0 == "Output"
                            && alias
                                .ty
                                .as_ref()
                                .is_some_and(|ty| type_matches_self(ty, &imp.self_ty))
                    })
            }
        }
    }

    pub(super) fn function_name(&self, fid: hir::item_tree::FunctionId) -> String {
        self.static_method_name(fid).unwrap_or_else(|| {
            self.qualify_symbol(fid, self.hir.item_tree.functions[fid].name.0.clone())
        })
    }

    pub(super) fn method_symbol_base(&self, fid: hir::item_tree::FunctionId) -> String {
        let name = self.hir.item_tree.functions[fid].name.0.clone();
        let collides_with_free_function =
            self.hir
                .item_tree
                .functions
                .iter()
                .any(|(other_fid, function)| {
                    other_fid != fid
                        && !self.method_impls.contains_key(&other_fid)
                        && !self.default_methods.contains_key(&other_fid)
                        && function.name.0 == name
                });
        let base = if collides_with_free_function {
            format!("method::{}::{name}", fid.into_raw().into_u32())
        } else if self.impl_for_method(fid).is_some_and(|imp| {
            let trait_args: &[hir::item_tree::HirTypeRef] = match &imp.trait_ty {
                Some(hir::item_tree::HirTypeRef::Named(path)) => &path.type_args,
                _ => &[],
            };
            self.method_impls.iter().any(|(other_fid, other_impl)| {
                let other = &self.hir.item_tree.impls[*other_impl];
                let other_trait_args: &[hir::item_tree::HirTypeRef] = match &other.trait_ty {
                    Some(hir::item_tree::HirTypeRef::Named(path)) => &path.type_args,
                    _ => &[],
                };
                *other_fid != fid
                    && self.hir.item_tree.functions[*other_fid].name.0 == name
                    && other.self_ty == imp.self_ty
                    && other_trait_args == trait_args
            })
        }) {
            format!("{name}__trait{}", fid.into_raw().into_u32())
        } else {
            name
        };
        self.qualify_symbol(fid, base)
    }

    fn qualify_symbol(&self, fid: hir::item_tree::FunctionId, base: String) -> String {
        let candidate = self.package_qualified_symbol(fid, base);
        self.disambiguate_free_symbol(fid, candidate)
    }

    /// Package qualification only: extern/C-export symbols keep their bare
    /// names, and so do package-less functions (std, synthesized helpers)
    /// and every function in single-file builds.
    pub(super) fn package_qualified_symbol(
        &self,
        fid: hir::item_tree::FunctionId,
        base: String,
    ) -> String {
        let function = &self.hir.item_tree.functions[fid];
        if self.hir.item_tree.extern_function_ids.contains(&fid)
            || function.attrs.iter().any(|attr| attr.name.0 == "c_export")
        {
            return base;
        }
        let Some(package) = self.hir.package_for_range(function.name_range) else {
            return base;
        };
        if self.package_names.is_empty() {
            return base;
        }
        if base == "main" && package + 1 == self.hir.package_ranges.len() {
            return base;
        }
        let name = self
            .package_names
            .get(package)
            .map_or_else(|| format!("package_{package}"), Clone::clone);
        format!("package::{name}::{base}")
    }

    /// Free functions that end up with the same final symbol — a private std
    /// helper colliding with a user function in single-file builds, or
    /// same-named privates in different modules of one package — would emit
    /// duplicate C symbols and shadow each other in the interpreter's name
    /// table. Rename every colliding party by fid, mirroring
    /// `method_symbol_base`'s `method::<fid>::` fallback. Extern and
    /// `c_export` functions never participate: their names are ABI (runtime
    /// symbols like `rgc_free`, or the user's chosen C identifier).
    fn disambiguate_free_symbol(
        &self,
        fid: hir::item_tree::FunctionId,
        candidate: String,
    ) -> String {
        if self.method_impls.contains_key(&fid)
            || self.default_methods.contains_key(&fid)
            || self.hir.item_tree.extern_function_ids.contains(&fid)
            || self.hir.item_tree.functions[fid]
                .attrs
                .iter()
                .any(|attr| attr.name.0 == "c_export")
            || self.hir.item_tree.functions[fid].name.0 == "main"
        {
            return candidate;
        }
        if self
            .free_symbol_counts
            .get(&candidate)
            .copied()
            .unwrap_or(0)
            > 1
        {
            return format!("fun::{}::{candidate}", fid.into_raw().into_u32());
        }
        candidate
    }

    pub(super) fn qualify_current_symbol(&self, base: String) -> String {
        self.current_function
            .map_or(base.clone(), |fid| self.qualify_symbol(fid, base))
    }

    pub(super) fn static_method_name(&self, fid: hir::item_tree::FunctionId) -> Option<String> {
        let imp = self.impl_for_method(fid)?;
        if !imp.generics.is_empty() || !imp.const_generics.is_empty() {
            return None;
        }
        let self_ty = self.convert_hir_type(&imp.self_ty);
        let trait_args = match imp.trait_ty.as_ref() {
            Some(hir::item_tree::HirTypeRef::Named(path)) => path
                .type_args
                .iter()
                .map(|arg| mono_type_name(&self.convert_hir_type(arg)))
                .collect::<Vec<_>>(),
            _ => Vec::new(),
        };
        let suffix = std::iter::once(mono_type_name(&self_ty))
            .chain(trait_args)
            .collect::<Vec<_>>()
            .join("_");
        Some(format!("{}__{}", self.method_symbol_base(fid), suffix))
    }

    pub(super) fn impl_self_mir_type(&self, fid: hir::item_tree::FunctionId) -> Option<Type> {
        self.impl_for_method(fid)
            .map(|imp| self.convert_hir_type(&imp.self_ty))
    }

    /// Substituted MIR type of a method's declared parameter in the instance
    /// `mono_method_name_for_receiver`/`mono_default_method_name` selects for
    /// `receiver_ty` — what the callee's parameter actually holds once the
    /// instance's generics, `Self`, and trait generics (`Rhs` for the
    /// comparison traits) are applied. Pure lookup: never generates an
    /// instance. Callers use it to decide whether an operand needs the extra
    /// reference level for an impl whose self type is itself a reference.
    pub(super) fn mono_method_param_type(
        &self,
        fid: hir::item_tree::FunctionId,
        param_index: usize,
        receiver_ty: &type_checker::Type,
        rhs_ty: Option<&type_checker::Type>,
    ) -> Option<Type> {
        let receiver_ty = self.substitute_tc_type(receiver_ty);
        let param = self.hir.item_tree.functions[fid].params.get(param_index)?;
        if let Some(&trait_id) = self.default_methods.get(&fid) {
            // Default-method instances instantiate with the dereferenced
            // receiver as `Self`, and the first trait generic (e.g. `Rhs`)
            // with the rhs operand, dereferenced to match the
            // by-reference argument convention.
            let self_ty = match &receiver_ty {
                type_checker::Type::Ref(inner, _) => (**inner).clone(),
                other => other.clone(),
            };
            let rhs_ty = rhs_ty
                .map(|ty| self.substitute_tc_type(ty))
                .map(|ty| match &ty {
                    type_checker::Type::Ref(inner, _) => (**inner).clone(),
                    _ => ty,
                });
            let mut tc_subst: HashMap<String, type_checker::Type> =
                HashMap::from([("Self".to_string(), self_ty.clone())]);
            for (index, name) in self.hir.item_tree.traits[trait_id]
                .generics
                .iter()
                .enumerate()
            {
                let arg = if index == 0 {
                    rhs_ty.clone().unwrap_or_else(|| self_ty.clone())
                } else {
                    self_ty.clone()
                };
                tc_subst.insert(name.0.clone(), arg);
            }
            let owned: HashMap<String, Type> = tc_subst
                .iter()
                .map(|(name, ty)| (name.clone(), self.convert_type(ty)))
                .collect();
            let type_subst: HashMap<&str, &Type> =
                owned.iter().map(|(name, ty)| (name.as_str(), ty)).collect();
            return Some(self.convert_hir_type_with_substs(
                &param.ty,
                &type_subst,
                &HashMap::new(),
            ));
        }
        let imp = self.impl_for_method(fid)?;
        let receiver_ty = match &receiver_ty {
            type_checker::Type::Ref(inner, _)
                if self.impl_mir_subst(imp, &receiver_ty).is_none() =>
            {
                (**inner).clone()
            }
            other => other.clone(),
        };
        let mut owned: HashMap<String, Type> = HashMap::new();
        let mut const_subst: HashMap<String, usize> = HashMap::new();
        if !imp.generics.is_empty() || !imp.const_generics.is_empty() {
            let subst = self
                .impl_mir_subst(imp, &receiver_ty)
                .or_else(|| match &receiver_ty {
                    type_checker::Type::Ref(inner, _) => self.impl_mir_subst(imp, inner),
                    _ => None,
                })?;
            owned = subst.types.clone();
            const_subst = subst.consts.clone();
        }
        if !owned.contains_key("Self") {
            let type_subst: HashMap<&str, &Type> =
                owned.iter().map(|(name, ty)| (name.as_str(), ty)).collect();
            let const_view: HashMap<&str, usize> = const_subst
                .iter()
                .map(|(name, value)| (name.as_str(), *value))
                .collect();
            let self_ty = self.convert_hir_type_with_substs(&imp.self_ty, &type_subst, &const_view);
            owned.insert("Self".to_string(), self_ty);
        }
        let type_subst: HashMap<&str, &Type> =
            owned.iter().map(|(name, ty)| (name.as_str(), ty)).collect();
        let const_view: HashMap<&str, usize> = const_subst
            .iter()
            .map(|(name, value)| (name.as_str(), *value))
            .collect();
        Some(self.convert_hir_type_with_substs(&param.ty, &type_subst, &const_view))
    }

    pub(super) fn impl_type_matches(
        &self,
        imp: &hir::item_tree::HirImpl,
        receiver_ty: &type_checker::Type,
    ) -> bool {
        let receiver_mir_ty = self.convert_type(receiver_ty);
        if imp.generics.is_empty() && imp.const_generics.is_empty() {
            return self.convert_hir_type(&imp.self_ty) == receiver_mir_ty;
        }
        self.impl_mir_subst(imp, receiver_ty).is_some_and(|subst| {
            let type_subst = subst
                .types
                .iter()
                .map(|(name, ty)| (name.as_str(), ty))
                .collect::<HashMap<_, _>>();
            let const_subst = subst
                .consts
                .iter()
                .map(|(name, value)| (name.as_str(), *value))
                .collect::<HashMap<_, _>>();
            self.convert_hir_type_with_substs(&imp.self_ty, &type_subst, &const_subst)
                == receiver_mir_ty
        })
    }

    pub(super) fn impl_trait_args_match(
        &self,
        imp: &hir::item_tree::HirImpl,
        receiver_ty: &type_checker::Type,
        rhs_ty: Option<&type_checker::Type>,
    ) -> bool {
        let Some(rhs_ty) = rhs_ty else {
            return true;
        };
        let Some(trait_ty) = imp.trait_ty.as_ref() else {
            return false;
        };
        let Some(trait_id) = self.resolve_trait_ref(trait_ty) else {
            return false;
        };
        let tr = &self.hir.item_tree.traits[trait_id];
        let Some(default) = tr.generic_defaults.first() else {
            return true;
        };
        let explicit = match trait_ty {
            hir::item_tree::HirTypeRef::Named(path) => path.type_args.first(),
            _ => None,
        };
        let Some(expected) = explicit.or(default.as_ref()) else {
            return false;
        };

        let receiver_ty = self.substitute_tc_type(receiver_ty);
        let receiver_mir = self.convert_type(&receiver_ty);
        let subst = self.impl_mir_subst(imp, &receiver_ty).unwrap_or_default();
        let mut type_subst = subst
            .types
            .iter()
            .map(|(name, ty)| (name.as_str(), ty))
            .collect::<HashMap<_, _>>();
        type_subst.insert("Self", &receiver_mir);
        let expected = self.convert_hir_type_with_substs(expected, &type_subst, &HashMap::new());
        let actual = self.convert_type(&self.substitute_tc_type(rhs_ty));
        expected == actual
    }

    pub(super) fn impl_trait_args_match_mir(
        &self,
        imp: &hir::item_tree::HirImpl,
        receiver_ty: &type_checker::Type,
        expected_args: &[Type],
    ) -> bool {
        let Some(trait_ty) = imp.trait_ty.as_ref() else {
            return false;
        };
        let Some(trait_id) = self.resolve_trait_ref(trait_ty) else {
            return false;
        };
        let receiver_ty = self.substitute_tc_type(receiver_ty);
        let receiver_mir = self.convert_type(&receiver_ty);
        let impl_subst = self.impl_mir_subst(imp, &receiver_ty).unwrap_or_default();
        let mut values = impl_subst
            .types
            .iter()
            .map(|(name, ty)| (name.clone(), ty.clone()))
            .collect::<HashMap<_, _>>();
        values.insert("Self".into(), receiver_mir);
        let const_subst = impl_subst
            .consts
            .iter()
            .map(|(name, value)| (name.as_str(), *value))
            .collect::<HashMap<_, _>>();
        let explicit = match trait_ty {
            hir::item_tree::HirTypeRef::Named(path) => path.type_args.as_slice(),
            _ => &[],
        };
        let trait_data = &self.hir.item_tree.traits[trait_id];
        let actual_args = trait_data
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
                    self.convert_hir_type_with_substs(arg, &refs, &const_subst)
                });
                values.insert(name.0.clone(), ty.clone());
                ty
            })
            .collect::<Vec<_>>();
        actual_args.len() == expected_args.len()
            && actual_args
                .iter()
                .zip(expected_args)
                .all(|(actual, expected)| actual == expected)
    }

    pub(super) fn resolve_trait_ref(
        &self,
        ty: &hir::item_tree::HirTypeRef,
    ) -> Option<hir::item_tree::TraitId> {
        let hir::item_tree::HirTypeRef::Named(path) = ty else {
            return None;
        };
        let name = path.segments.last()?.0.as_str();
        self.hir
            .item_tree
            .traits
            .iter()
            .find_map(|(id, tr)| (tr.name.0 == name).then_some(id))
    }

    pub(super) fn impl_mir_subst(
        &self,
        imp: &hir::item_tree::HirImpl,
        receiver_ty: &type_checker::Type,
    ) -> Option<MirSubst> {
        match receiver_ty {
            type_checker::Type::Struct(_, args) | type_checker::Type::Enum(_, args) => {
                Some(self.nominal_impl_subst(imp, args))
            }
            type_checker::Type::Array(..) => self.array_impl_subst(imp, receiver_ty),
            type_checker::Type::Slice(..) => self.slice_impl_subst(imp, receiver_ty),
            type_checker::Type::Ref(..) => self.reference_impl_subst(imp, receiver_ty),
            type_checker::Type::Ptr { .. } => self.pointer_impl_subst(imp, receiver_ty),
            type_checker::Type::Tuple(..) => self.tuple_impl_subst(imp, receiver_ty),
            _ => None,
        }
    }

    /// Unifies a generic tuple self type like `(A, B)` with a concrete tuple
    /// receiver, collecting the element substitutions.
    fn tuple_impl_subst(
        &self,
        imp: &hir::item_tree::HirImpl,
        receiver_ty: &type_checker::Type,
    ) -> Option<MirSubst> {
        let type_checker::Type::Tuple(elements) = receiver_ty else {
            return None;
        };
        let hir::item_tree::HirTypeRef::Tuple(pattern_elements) = &imp.self_ty else {
            return None;
        };
        if pattern_elements.len() != elements.len() {
            return None;
        }
        let mut subst = MirSubst::default();
        let generics = Self::impl_generic_names(imp);
        for (pattern, actual) in pattern_elements.iter().zip(elements) {
            if !self.collect_hir_type_subst(
                pattern,
                actual,
                &generics,
                &mut subst.types,
                &mut subst.tc_types,
            ) {
                return None;
            }
        }
        Some(subst)
    }

    fn nominal_impl_subst(
        &self,
        imp: &hir::item_tree::HirImpl,
        args: &[type_checker::Type],
    ) -> MirSubst {
        let mut subst = MirSubst::default();
        for (name, ty) in imp.generics.iter().zip(args) {
            subst.types.insert(name.0.clone(), self.convert_type(ty));
            subst.tc_types.insert(name.0.clone(), ty.clone());
        }
        for (name, ty) in imp
            .const_generics
            .iter()
            .zip(args.iter().skip(imp.generics.len()))
        {
            if let Some(value) = tc_const_arg_to_usize(ty) {
                subst.consts.insert(name.0.clone(), value);
                subst.tc_types.insert(name.0.clone(), ty.clone());
            }
        }
        subst
    }

    fn array_impl_subst(
        &self,
        imp: &hir::item_tree::HirImpl,
        receiver_ty: &type_checker::Type,
    ) -> Option<MirSubst> {
        let type_checker::Type::Array(inner, len) = receiver_ty else {
            return None;
        };
        let hir::item_tree::HirTypeRef::Array(pattern_inner, pattern_len) = &imp.self_ty else {
            return None;
        };
        let mut subst = MirSubst::default();
        let generics = Self::impl_generic_names(imp);
        if !self.collect_hir_type_subst(
            pattern_inner,
            inner,
            &generics,
            &mut subst.types,
            &mut subst.tc_types,
        ) {
            return None;
        }
        if let hir::item_tree::HirConstArg::Param(name) = pattern_len
            && let Some(value) = len.as_usize()
        {
            subst.consts.insert(name.0.clone(), value);
            subst
                .tc_types
                .insert(name.0.clone(), type_checker::Type::Const(len.clone()));
        }
        Some(subst)
    }

    fn slice_impl_subst(
        &self,
        imp: &hir::item_tree::HirImpl,
        receiver_ty: &type_checker::Type,
    ) -> Option<MirSubst> {
        let type_checker::Type::Slice(inner) = receiver_ty else {
            return None;
        };
        let hir::item_tree::HirTypeRef::Slice(pattern_inner) = &imp.self_ty else {
            return None;
        };
        let mut subst = MirSubst::default();
        let generics = Self::impl_generic_names(imp);
        self.collect_hir_type_subst(
            pattern_inner,
            inner,
            &generics,
            &mut subst.types,
            &mut subst.tc_types,
        )
        .then_some(subst)
    }

    fn reference_impl_subst(
        &self,
        imp: &hir::item_tree::HirImpl,
        receiver_ty: &type_checker::Type,
    ) -> Option<MirSubst> {
        let type_checker::Type::Ref(inner, mutable) = receiver_ty else {
            return None;
        };
        let hir::item_tree::HirTypeRef::Ref(pattern_inner, pattern_mut) = &imp.self_ty else {
            return None;
        };
        if mutable != pattern_mut {
            return None;
        }
        let (pattern_inner, pattern_len, actual_inner, actual_len) =
            match (pattern_inner.as_ref(), inner.as_ref()) {
                (
                    hir::item_tree::HirTypeRef::Array(pattern_inner, pattern_len),
                    type_checker::Type::Array(actual_inner, actual_len),
                ) => (
                    pattern_inner.as_ref(),
                    Some(pattern_len),
                    actual_inner.as_ref(),
                    Some(actual_len),
                ),
                (pattern_inner, actual_inner) => (pattern_inner, None, actual_inner, None),
            };
        let mut subst = MirSubst::default();
        let generics = Self::impl_generic_names(imp);
        if !self.collect_hir_type_subst(
            pattern_inner,
            actual_inner,
            &generics,
            &mut subst.types,
            &mut subst.tc_types,
        ) {
            return None;
        }
        if let (Some(pattern_len), Some(actual_len)) = (pattern_len, actual_len)
            && let hir::item_tree::HirConstArg::Param(name) = pattern_len
            && let Some(value) = actual_len.as_usize()
        {
            subst.consts.insert(name.0.clone(), value);
            subst.tc_types.insert(
                name.0.clone(),
                type_checker::Type::Const(actual_len.clone()),
            );
        }
        Some(subst)
    }

    fn pointer_impl_subst(
        &self,
        imp: &hir::item_tree::HirImpl,
        receiver_ty: &type_checker::Type,
    ) -> Option<MirSubst> {
        let type_checker::Type::Ptr { inner, mutable } = receiver_ty else {
            return None;
        };
        let hir::item_tree::HirTypeRef::Ptr {
            inner: pattern_inner,
            mutable: pattern_mut,
        } = &imp.self_ty
        else {
            return None;
        };
        if mutable != pattern_mut {
            return None;
        }
        let mut subst = MirSubst::default();
        let generics = Self::impl_generic_names(imp);
        self.collect_hir_type_subst(
            pattern_inner,
            inner,
            &generics,
            &mut subst.types,
            &mut subst.tc_types,
        )
        .then_some(subst)
    }

    fn impl_generic_names(imp: &hir::item_tree::HirImpl) -> HashSet<&str> {
        imp.generics.iter().map(|name| name.0.as_str()).collect()
    }

    pub(super) fn collect_hir_type_subst(
        &self,
        pattern: &hir::item_tree::HirTypeRef,
        actual: &type_checker::Type,
        generics: &HashSet<&str>,
        subst: &mut HashMap<String, Type>,
        tc_subst: &mut HashMap<String, type_checker::Type>,
    ) -> bool {
        match pattern {
            hir::item_tree::HirTypeRef::Named(path)
                if path
                    .as_single_name()
                    .is_some_and(|name| generics.contains(name.0.as_str())) =>
            {
                let name = path.as_single_name().unwrap().0.clone();
                match (subst.get(&name), tc_subst.get(&name)) {
                    (Some(existing), Some(tc_existing)) => {
                        existing == &self.convert_type(actual) && tc_existing == actual
                    }
                    (None, None) => {
                        subst.insert(name.clone(), self.convert_type(actual));
                        tc_subst.insert(name, actual.clone());
                        true
                    }
                    _ => false,
                }
            }
            hir::item_tree::HirTypeRef::Named(path) => match actual {
                type_checker::Type::Struct(_, args) | type_checker::Type::Enum(_, args) => {
                    path.type_args.iter().zip(args).all(|(pattern, actual)| {
                        self.collect_hir_type_subst(pattern, actual, generics, subst, tc_subst)
                    })
                }
                _ => true,
            },
            hir::item_tree::HirTypeRef::Ref(inner, expected_mut) => match actual {
                type_checker::Type::Ref(actual_inner, actual_mut) => {
                    expected_mut == actual_mut
                        && self.collect_hir_type_subst(
                            inner,
                            actual_inner,
                            generics,
                            subst,
                            tc_subst,
                        )
                }
                _ => false,
            },
            hir::item_tree::HirTypeRef::Ptr { inner, .. } => match actual {
                type_checker::Type::Ptr {
                    inner: actual_inner,
                    ..
                } => self.collect_hir_type_subst(inner, actual_inner, generics, subst, tc_subst),
                _ => false,
            },
            hir::item_tree::HirTypeRef::Array(inner, _) => match actual {
                type_checker::Type::Array(actual_inner, _) => {
                    self.collect_hir_type_subst(inner, actual_inner, generics, subst, tc_subst)
                }
                _ => false,
            },
            hir::item_tree::HirTypeRef::Slice(inner) => match actual {
                type_checker::Type::Slice(actual_inner) => {
                    self.collect_hir_type_subst(inner, actual_inner, generics, subst, tc_subst)
                }
                _ => false,
            },
            hir::item_tree::HirTypeRef::Tuple(elements) => match actual {
                type_checker::Type::Tuple(actual_elements)
                    if elements.len() == actual_elements.len() =>
                {
                    elements
                        .iter()
                        .zip(actual_elements)
                        .all(|(pattern, actual)| {
                            self.collect_hir_type_subst(pattern, actual, generics, subst, tc_subst)
                        })
                }
                _ => false,
            },
            _ => true,
        }
    }

    pub(super) fn convert_hir_type_with_substs(
        &self,
        t: &hir::item_tree::HirTypeRef,
        subst: &HashMap<&str, &Type>,
        const_subst: &HashMap<&str, usize>,
    ) -> Type {
        self.convert_hir_type_with_substs_mode(t, subst, const_subst, true)
    }

    fn convert_hir_type_with_substs_mode(
        &self,
        t: &hir::item_tree::HirTypeRef,
        subst: &HashMap<&str, &Type>,
        const_subst: &HashMap<&str, usize>,
        owned_dyn: bool,
    ) -> Type {
        match t {
            hir::item_tree::HirTypeRef::Never => Type::Never,
            hir::item_tree::HirTypeRef::Named(path) => {
                if let Some(ResolvedName::TypeAlias(alias)) =
                    self.hir.type_resolutions.get(&path.range)
                    && let Some(ty) = &self.hir.item_tree.type_aliases[*alias].ty
                {
                    return self.convert_hir_type_with_substs_mode(
                        ty,
                        subst,
                        const_subst,
                        owned_dyn,
                    );
                }
                if is_self_associated_path(path) {
                    let key = format!("Self::{}", path.segments[1].0);
                    if let Some((_, ty)) = subst.iter().find(|(name, _)| **name == key) {
                        return (*ty).clone();
                    }
                }
                if let Some(name) = path.as_single_name().map(|name| name.0.as_str())
                    && let Some(ty) = subst.get(name)
                {
                    return (*ty).clone();
                }
                // Resolve through the scope graph before falling back to a
                // name scan: the scan below cannot tell a local item from a
                // same-named one in another package (std declares `Chain`,
                // `String`, `Formatter`, …), and for a self-reference such as
                // `enum Chain { Link(&Chain) }` it would silently pick the
                // other package's item.
                match self.hir.type_resolutions.get(&path.range) {
                    Some(ResolvedName::Struct(sid)) => {
                        return self.convert_struct_type_from_hir_args_with_substs(
                            *sid,
                            &path.type_args,
                            subst,
                            const_subst,
                        );
                    }
                    Some(ResolvedName::Enum(eid)) => {
                        return self.convert_enum_type_from_hir_args_with_substs(
                            *eid,
                            &path.type_args,
                            subst,
                            const_subst,
                        );
                    }
                    _ => {}
                }
                if let Some(name) = path.segments.last().map(|n| n.0.as_str()) {
                    for (sid, s) in self.hir.item_tree.structs.iter() {
                        if s.name.0 == name {
                            return self.convert_struct_type_from_hir_args_with_substs(
                                sid,
                                &path.type_args,
                                subst,
                                const_subst,
                            );
                        }
                    }
                    for (eid, e) in self.hir.item_tree.enums.iter() {
                        if e.name.0 == name {
                            return self.convert_enum_type_from_hir_args_with_substs(
                                eid,
                                &path.type_args,
                                subst,
                                const_subst,
                            );
                        }
                    }
                }
                self.convert_hir_type(t)
            }
            hir::item_tree::HirTypeRef::Ref(inner, mutable) => {
                let inner_ty =
                    self.convert_hir_type_with_substs_mode(inner, subst, const_subst, false);
                if self.hir_type_is_dyn_trait(inner) {
                    inner_ty
                } else {
                    Type::Ref(Box::new(inner_ty), *mutable)
                }
            }
            hir::item_tree::HirTypeRef::Ptr { inner, .. } => Type::Ptr(Box::new(
                self.convert_hir_type_with_substs_mode(inner, subst, const_subst, false),
            )),
            hir::item_tree::HirTypeRef::Tuple(elems) if elems.is_empty() => Type::Unit,
            hir::item_tree::HirTypeRef::Tuple(elems) => Type::Tuple(
                elems
                    .iter()
                    .map(|elem| {
                        self.convert_hir_type_with_substs_mode(elem, subst, const_subst, true)
                    })
                    .collect(),
            ),
            hir::item_tree::HirTypeRef::Slice(inner) => Type::Slice(Box::new(
                self.convert_hir_type_with_substs_mode(inner, subst, const_subst, true),
            )),
            hir::item_tree::HirTypeRef::Array(inner, len) => Type::Array(
                Box::new(self.convert_hir_type_with_substs_mode(inner, subst, const_subst, true)),
                self.hir_const_arg_to_usize(len, const_subst),
            ),
            hir::item_tree::HirTypeRef::Const(_)
            | hir::item_tree::HirTypeRef::ImplTrait { .. }
            | hir::item_tree::HirTypeRef::Unknown
            | hir::item_tree::HirTypeRef::Error => Type::Unit,
            hir::item_tree::HirTypeRef::DynTrait {
                trait_ty,
                callable,
                assoc_constraints,
                ..
            } => callable.as_ref().map_or_else(
                || {
                    self.resolve_trait_ref(trait_ty)
                        .map_or(Type::Unit, |trait_id| {
                            let args = self.dyn_trait_hir_args_with_substs(
                                trait_id,
                                trait_ty,
                                subst,
                                const_subst,
                            );
                            let assoc_bindings = assoc_constraints
                                .iter()
                                .map(|constraint| {
                                    (
                                        constraint.name.0.clone(),
                                        self.convert_hir_type_with_substs(
                                            &constraint.ty,
                                            subst,
                                            const_subst,
                                        ),
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
                            .map(|param| {
                                self.convert_hir_type_with_substs_mode(
                                    param,
                                    subst,
                                    const_subst,
                                    true,
                                )
                            })
                            .collect(),
                        ret: Box::new(self.convert_hir_type_with_substs_mode(
                            &signature.ret,
                            subst,
                            const_subst,
                            true,
                        )),
                    })
                },
            ),
        }
    }

    pub(super) fn convert_struct_type_from_hir_args_with_substs(
        &self,
        sid: hir::item_tree::StructId,
        args: &[hir::item_tree::HirTypeRef],
        subst: &HashMap<&str, &Type>,
        const_subst: &HashMap<&str, usize>,
    ) -> Type {
        let s = &self.hir.item_tree.structs[sid];
        let type_count = s.generics.len();
        let mir_args = args
            .iter()
            .take(type_count)
            .map(|arg| self.convert_hir_type_with_substs(arg, subst, const_subst))
            .collect::<Vec<_>>();
        let const_args = args
            .iter()
            .skip(type_count)
            .map(|arg| self.hir_type_ref_const_arg_to_usize(arg, const_subst))
            .collect::<Vec<_>>();
        self.convert_struct_type_from_parts(sid, &mir_args, &const_args)
    }

    pub(super) fn convert_enum_type_from_hir_args_with_substs(
        &self,
        eid: hir::item_tree::EnumId,
        args: &[hir::item_tree::HirTypeRef],
        subst: &HashMap<&str, &Type>,
        const_subst: &HashMap<&str, usize>,
    ) -> Type {
        let e = &self.hir.item_tree.enums[eid];
        let type_count = e.generics.len();
        let mir_args = args
            .iter()
            .take(type_count)
            .map(|arg| self.convert_hir_type_with_substs(arg, subst, const_subst))
            .collect::<Vec<_>>();
        let const_args = args
            .iter()
            .skip(type_count)
            .map(|arg| self.hir_type_ref_const_arg_to_usize(arg, const_subst))
            .collect::<Vec<_>>();
        self.convert_enum_type_from_parts(eid, &mir_args, &const_args)
    }

    pub(super) fn hir_type_ref_const_arg_to_usize(
        &self,
        ty: &hir::item_tree::HirTypeRef,
        const_subst: &HashMap<&str, usize>,
    ) -> usize {
        match ty {
            hir::item_tree::HirTypeRef::Const(value) => {
                self.hir_const_arg_to_usize(value, const_subst)
            }
            hir::item_tree::HirTypeRef::Named(path) => path
                .as_single_name()
                .and_then(|name| const_subst.get(name.0.as_str()).copied())
                .or_else(|| {
                    path.as_single_name()
                        .and_then(|name| self.generic_const_subst.get(&name.0).copied())
                })
                .unwrap_or(0),
            _ => 0,
        }
    }

    pub(super) fn hir_const_arg_to_usize(
        &self,
        arg: &hir::item_tree::HirConstArg,
        const_subst: &HashMap<&str, usize>,
    ) -> usize {
        match arg {
            hir::item_tree::HirConstArg::Value(value) => *value,
            hir::item_tree::HirConstArg::Param(name) => const_subst
                .get(name.0.as_str())
                .copied()
                .or_else(|| self.generic_const_subst.get(&name.0).copied())
                .unwrap_or(0),
            hir::item_tree::HirConstArg::Unknown | hir::item_tree::HirConstArg::Error => 0,
        }
    }
}
