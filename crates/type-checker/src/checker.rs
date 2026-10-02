use std::collections::{HashMap, HashSet};

use rowan::TextRange;

use hir::{
    HirFile,
    body::{
        BinaryOp as HirBinOp, Body, BodyId, Expr, ExprId, PatternBindingId, ResolvedName, UnaryOp,
    },
    item_tree::{
        ConstId, EnumId, FunctionId, HirConstArg, HirFunction, HirPath, HirTrait, HirTypeAlias,
        HirTypeRef, HirVariantKind, InternalAttrTarget, PathAnchor, StructId, TypeAliasId,
    },
};

use crate::{
    body::bound_target_param,
    context::BodyCtx,
    lang_items::{LangItem, RegisterResult},
    result::{Diagnostic, LabelStyle, Severity, SourceLabel, TypeCheckResult},
    trait_env::{TraitAssocConstraint, TraitBound},
    types::{CallableSignature, ClosureId, ClosureKind, FloatTy, IntTy, OpaqueCallableId, Type},
};

pub struct TypeChecker<'a> {
    pub(crate) hir: &'a HirFile,
    pub(crate) result: TypeCheckResult,
    pub(crate) generic_edges: Vec<GenericEdge>,
    infinite_layout_types: HashSet<NominalType>,
    pub(crate) lowering_type_aliases: HashSet<TypeAliasId>,
    next_infer: u32,
    infer_values: HashMap<u32, Type>,
    pub(crate) last_occurs_error: Option<(u32, Type)>,
    pending_lambdas: Vec<PendingLambda>,
    pub(crate) pending_move_uses: HashSet<(BodyId, ExprId)>,
    pub(crate) pending_delayed_bindings: Vec<(BodyId, PatternBindingId, Option<TextRange>)>,
    pub(crate) pending_generic_calls: Vec<PendingGenericCall>,
    pub(crate) active_trait_assumptions: Vec<TraitBound>,
}

struct PendingLambda {
    body_id: BodyId,
    expr: ExprId,
    params: Vec<(String, Option<TextRange>, Type)>,
}

#[derive(Clone)]
pub struct PendingGenericCall {
    pub(crate) body_id: BodyId,
    pub(crate) callee: ExprId,
    pub(crate) function: FunctionId,
    pub(crate) inferred_names: Vec<String>,
    pub(crate) subst: HashMap<String, Type>,
    pub(crate) generic_arg_spans: HashMap<String, TextRange>,
    pub(crate) callee_span: Option<TextRange>,
    pub(crate) span: Option<TextRange>,
    pub(crate) kind: &'static str,
    pub(crate) caller: Option<FunctionId>,
    pub(crate) check_sized: bool,
}

#[derive(Debug, Clone)]
pub struct GenericEdge {
    pub(crate) caller: FunctionId,
    pub(crate) callee: FunctionId,
    pub(crate) grows: bool,
    pub(crate) span: Option<TextRange>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum NominalType {
    Struct(StructId),
    Enum(EnumId),
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum CallableIdentity {
    Closure(ClosureId),
    Opaque(OpaqueCallableId),
}

const fn type_nominal_identity(ty: &Type) -> Option<NominalType> {
    match ty {
        Type::Struct(id, _) => Some(NominalType::Struct(*id)),
        Type::Enum(id, _) => Some(NominalType::Enum(*id)),
        _ => None,
    }
}

const fn type_callable_identity(ty: &Type) -> Option<CallableIdentity> {
    match ty {
        Type::Closure { id, .. } => Some(CallableIdentity::Closure(*id)),
        Type::OpaqueCallable { id, .. } => Some(CallableIdentity::Opaque(*id)),
        _ => None,
    }
}

#[must_use]
pub fn check_hir(hir: &HirFile) -> TypeCheckResult {
    TypeChecker::new(hir).check()
}

impl<'a> TypeChecker<'a> {
    #[must_use]
    pub fn new(hir: &'a HirFile) -> Self {
        Self {
            hir,
            result: TypeCheckResult::default(),
            generic_edges: Vec::new(),
            infinite_layout_types: HashSet::new(),
            lowering_type_aliases: HashSet::new(),
            next_infer: 0,
            infer_values: HashMap::new(),
            last_occurs_error: None,
            pending_lambdas: Vec::new(),
            pending_move_uses: HashSet::new(),
            pending_delayed_bindings: Vec::new(),
            pending_generic_calls: Vec::new(),
            active_trait_assumptions: Vec::new(),
        }
    }

    #[must_use]
    pub fn check(mut self) -> TypeCheckResult {
        self.check_value_type_declarations();
        self.check_type_layouts();
        self.check_traits();
        self.check_trait_ref_arities();
        self.check_impls();
        self.build_trait_env();
        self.validate_copy_impls();
        self.check_const_bodies();
        self.check_function_bodies();
        self.check_generic_recursion();
        self.result
    }

    pub(crate) fn check_value_type_declarations(&mut self) {
        self.check_aggregate_value_type_declarations();

        let functions = self
            .hir
            .item_tree
            .functions
            .iter()
            .map(|(id, function)| (id, function.clone()))
            .collect::<Vec<_>>();
        for (id, function) in functions {
            let outer_generics = self.impl_generic_names(id);
            let outer_const_generics = self.impl_const_generic_names(id);
            let params =
                self.function_generic_params(id, &function, &outer_generics, &outer_const_generics);
            self.check_function_value_types(&function, &params);
        }

        let traits = self
            .hir
            .item_tree
            .traits
            .iter()
            .map(|(_, item)| item.clone())
            .collect::<Vec<_>>();
        for item in traits {
            for method in item.methods {
                let mut params = crate::lowering::generic_param_map_with_consts(
                    item.generics
                        .iter()
                        .chain(method.generics.iter())
                        .chain(method.implicit_generics.iter())
                        .map(|name| name.0.as_str()),
                    method.const_generics.iter().map(|name| name.0.as_str()),
                );
                params.insert("Self".into(), Type::Param("Self".into()));
                self.check_function_value_types(&method, &params);
            }
            for alias in item.type_aliases {
                let Some(alias_ty) = alias.ty else {
                    continue;
                };
                let params = HashMap::from([("Self".into(), Type::Param("Self".into()))]);
                let range = alias.ty_range.unwrap_or(alias.name_range);
                let ty = self.lower_type_ref_with_params_at(&alias_ty, &params, Some(range));
                self.expect_sized_value(&ty, Some(range));
            }
        }

        let consts = self
            .hir
            .item_tree
            .consts
            .iter()
            .map(|(id, item)| (id, item.clone()))
            .collect::<Vec<_>>();
        for (id, item) in consts {
            let params = self.impl_item_type_params(|imp| imp.consts.contains(&id));
            let ty = self.lower_type_ref_with_params_at(&item.ty, &params, Some(item.ty_range));
            self.expect_sized_value(&ty, Some(item.ty_range));
        }

        let aliases = self
            .hir
            .item_tree
            .type_aliases
            .iter()
            .map(|(id, item)| (id, item.clone()))
            .collect::<Vec<_>>();
        for (id, item) in aliases {
            let Some(alias) = item.ty else {
                continue;
            };
            let params = self.impl_item_type_params(|imp| imp.type_aliases.contains(&id));
            let range = item.ty_range.unwrap_or(item.name_range);
            let ty = self.lower_type_ref_with_params_at(&alias, &params, Some(range));
            self.expect_sized_value(&ty, Some(range));
        }
    }

    fn check_aggregate_value_type_declarations(&mut self) {
        let structs = self
            .hir
            .item_tree
            .structs
            .iter()
            .map(|(_, item)| item.clone())
            .collect::<Vec<_>>();
        for item in structs {
            let params = crate::lowering::generic_param_map_with_consts(
                item.generics.iter().map(|name| name.0.as_str()),
                item.const_generics.iter().map(|name| name.0.as_str()),
            );
            for field in item.fields {
                let ty =
                    self.lower_type_ref_with_params_at(&field.ty, &params, Some(field.ty_range));
                self.expect_sized_value(&ty, Some(field.ty_range));
            }
        }

        let enums = self
            .hir
            .item_tree
            .enums
            .iter()
            .map(|(_, item)| item.clone())
            .collect::<Vec<_>>();
        for item in enums {
            let params = crate::lowering::generic_param_map_with_consts(
                item.generics.iter().map(|name| name.0.as_str()),
                item.const_generics.iter().map(|name| name.0.as_str()),
            );
            for variant in item.variants {
                let field_ranges = variant.field_ranges;
                match variant.kind {
                    HirVariantKind::Unit => {}
                    HirVariantKind::Tuple(fields) => {
                        for (field, range) in fields.into_iter().zip(field_ranges) {
                            let ty =
                                self.lower_type_ref_with_params_at(&field, &params, Some(range));
                            self.expect_sized_value(&ty, Some(range));
                        }
                    }
                    HirVariantKind::Struct(fields) => {
                        for field in fields {
                            // A variant's fields are only reachable through a
                            // pattern, and a pattern binding is a local of its
                            // own — no write can travel through a shared
                            // reference to the enum — so `mut` here would parse
                            // and then do nothing.
                            if field.is_mut {
                                self.diagnostic(
                                    "E0014",
                                    format!(
                                        "`mut` is not supported on enum variant fields: `{}` is only reachable through a pattern",
                                        field.name.0
                                    ),
                                    Some(field.name_range),
                                );
                            }
                            let ty = self.lower_type_ref_with_params_at(
                                &field.ty,
                                &params,
                                Some(field.ty_range),
                            );
                            self.expect_sized_value(&ty, Some(field.ty_range));
                        }
                    }
                }
            }
        }
    }

    fn check_function_value_types(
        &mut self,
        function: &HirFunction,
        params: &HashMap<String, Type>,
    ) {
        for param in &function.params {
            let ty = self.lower_type_ref_with_params_at(&param.ty, params, Some(param.ty_range));
            self.expect_sized_value(&ty, Some(param.ty_range));
        }
        if let Some(return_ty) = &function.ret_type {
            let range = function.ret_type_range.unwrap_or(function.name_range);
            let ty = self.lower_type_ref_with_params_at(return_ty, params, Some(range));
            self.expect_sized_value(&ty, Some(range));
        }
    }

    fn impl_item_type_params(
        &mut self,
        owns: impl Fn(&hir::item_tree::HirImpl) -> bool,
    ) -> HashMap<String, Type> {
        let owner = self
            .hir
            .item_tree
            .impls
            .iter()
            .find_map(|(_, imp)| owns(imp).then(|| imp.clone()));
        let Some(owner) = owner else {
            return HashMap::new();
        };
        let mut params = crate::lowering::generic_param_map_with_consts(
            owner.generics.iter().map(|name| name.0.as_str()),
            owner.const_generics.iter().map(|name| name.0.as_str()),
        );
        let self_ty =
            self.lower_type_ref_with_params_at(&owner.self_ty, &params, Some(owner.self_ty_range));
        params.insert("Self".into(), self_ty);
        params
    }

    pub(crate) fn build_trait_env(&mut self) {
        let invalid_internal_attrs = self.validate_internal_attrs();

        // ── Validate and register #[lang] traits ──────────────────────────────
        for (tid, tr) in self.hir.item_tree.traits.iter() {
            for attr in &tr.attrs {
                if attr.name.0 != "lang" {
                    continue;
                }
                if invalid_internal_attrs.contains(&attr.range) {
                    continue;
                }
                let Some(lang) = attr.value.as_deref() else {
                    continue;
                };

                // Unknown lang name.
                let Some(item) = LangItem::from_name(lang) else {
                    self.result.diagnostics.push(Diagnostic {
                        code: "E0053",
                        severity: Severity::Error,
                        message: format!("unknown lang item `{lang}`"),
                        labels: vec![SourceLabel {
                            range: tr.name_range,
                            message: String::new(),
                            style: LabelStyle::Primary,
                        }],
                    help: Some(
                        "recognized lang items: drop, copy, clone, partial_eq, eq, partial_ord, ord, \
                         add, sub, mul, div, rem, neg, not, bitand, bitor, bitxor, \
                         shl, shr, index, index_mut, the *_assign variants, debug, \
                         option, result"
                            .to_string(),
                    ),
                        notes: Vec::new(),
                    });
                    continue;
                };

                // Signature validation.
                if let Some(reason) = validate_lang_item_signature(item, tr) {
                    self.result.diagnostics.push(Diagnostic {
                        code: "E0053",
                        severity: Severity::Error,
                        message: format!(
                            "lang item `{}` has an invalid trait signature: {reason}",
                            item.as_str()
                        ),
                        labels: vec![SourceLabel {
                            range: tr.name_range,
                            message: String::new(),
                            style: LabelStyle::Primary,
                        }],
                        help: None,
                        notes: Vec::new(),
                    });
                    continue;
                }

                // Duplicate lang item / duplicate trait annotation.
                let generic_count = tr.generics.len();
                match self
                    .result
                    .trait_env
                    .register_lang_item(item, tid, generic_count)
                {
                    RegisterResult::Ok => {}
                    RegisterResult::DuplicateItem => {
                        let notes = vec!["first definition is elsewhere in the source".into()];
                        self.result.diagnostics.push(Diagnostic {
                            code: "E0053",
                            severity: Severity::Error,
                            message: format!(
                                "lang item `{}` is defined more than once",
                                item.as_str()
                            ),
                            labels: vec![SourceLabel {
                                range: tr.name_range,
                                message: "duplicate definition here".into(),
                                style: LabelStyle::Primary,
                            }],
                            help: None,
                            notes,
                        });
                    }
                    RegisterResult::DuplicateTrait => {
                        self.result.diagnostics.push(Diagnostic {
                            code: "E0053",
                            severity: Severity::Error,
                            message: "a trait can carry at most one `#[lang]` attribute; \
                                      this trait is already registered as a different lang item"
                                .to_string(),
                            labels: vec![SourceLabel {
                                range: tr.name_range,
                                message: "second `#[lang]` attribute here".into(),
                                style: LabelStyle::Primary,
                            }],
                            help: None,
                            notes: Vec::new(),
                        });
                    }
                }
            }
        }

        // ── Validate and register #[lang] enums ──────────────────────────────
        for (eid, en) in self.hir.item_tree.enums.iter() {
            for attr in &en.attrs {
                if attr.name.0 != "lang" {
                    continue;
                }
                if invalid_internal_attrs.contains(&attr.range) {
                    continue;
                }
                let Some(lang) = attr.value.as_deref() else {
                    continue;
                };
                // Unknown lang name.
                let Some(item) = LangItem::from_name(lang) else {
                    self.result.diagnostics.push(Diagnostic {
                        code: "E0053",
                        severity: Severity::Error,
                        message: format!("unknown lang item `{lang}`"),
                        labels: vec![SourceLabel {
                            range: en.name_range,
                            message: String::new(),
                            style: LabelStyle::Primary,
                        }],
                        help: Some(
                            "recognized lang items: drop, copy, clone, partial_eq, eq, \
                             partial_ord, ord, add, sub, mul, div, rem, neg, not, bitand, \
                             bitor, bitxor, shl, shr, index, index_mut, the *_assign \
                             variants, debug, option, result"
                                .to_string(),
                        ),
                        notes: Vec::new(),
                    });
                    continue;
                };
                if !item.is_enum() {
                    self.result.diagnostics.push(Diagnostic {
                        code: "E0053",
                        severity: Severity::Error,
                        message: format!(
                            "lang item `{}` is carried by a trait, not an enum",
                            item.as_str()
                        ),
                        labels: vec![SourceLabel {
                            range: en.name_range,
                            message: String::new(),
                            style: LabelStyle::Primary,
                        }],
                        help: None,
                        notes: Vec::new(),
                    });
                    continue;
                }
                if let Some(expected) = item.enum_generic_count()
                    && en.generics.len() != expected
                {
                    self.result.diagnostics.push(Diagnostic {
                        code: "E0053",
                        severity: Severity::Error,
                        message: format!(
                            "lang item `{}` requires exactly {expected} generic parameter(s)",
                            item.as_str()
                        ),
                        labels: vec![SourceLabel {
                            range: en.name_range,
                            message: String::new(),
                            style: LabelStyle::Primary,
                        }],
                        help: None,
                        notes: Vec::new(),
                    });
                    continue;
                }
                if self.result.trait_env.lang_items.register_enum(item, eid)
                    == RegisterResult::DuplicateItem
                {
                    self.result.diagnostics.push(Diagnostic {
                        code: "E0053",
                        severity: Severity::Error,
                        message: format!("lang item `{}` is defined more than once", item.as_str()),
                        labels: vec![SourceLabel {
                            range: en.name_range,
                            message: "duplicate definition here".into(),
                            style: LabelStyle::Primary,
                        }],
                        help: None,
                        notes: vec!["first definition is elsewhere in the source".into()],
                    });
                }
            }
        }

        self.register_trait_impls();
    }

    fn register_trait_impls(&mut self) {
        let impls = self
            .hir
            .item_tree
            .impls
            .iter()
            .map(|(_, imp)| imp.clone())
            .collect::<Vec<_>>();
        for imp in &impls {
            let Some(trait_ty) = &imp.trait_ty else {
                continue;
            };
            let Some(trait_id) = self.resolve_trait_ref(trait_ty) else {
                continue;
            };
            let params = crate::lowering::generic_param_map_with_consts(
                imp.generics.iter().map(|name| name.0.as_str()),
                imp.const_generics.iter().map(|name| name.0.as_str()),
            );
            let self_ty =
                self.lower_type_ref_with_params_at(&imp.self_ty, &params, Some(imp.self_ty_range));
            let trait_args =
                self.trait_ref_args(trait_id, trait_ty, &self_ty, &params, imp.trait_ty_range);
            let bounds = self.lower_trait_env_bounds(&imp.generic_bounds, &params);
            let assoc_types = imp
                .type_aliases
                .iter()
                .filter_map(|alias_id| {
                    let alias = &self.hir.item_tree.type_aliases[*alias_id];
                    alias.ty.as_ref().map(|ty| {
                        (
                            alias.name.0.clone(),
                            self.lower_type_ref_with_params_at(
                                ty,
                                &params,
                                alias.ty_range.or(Some(alias.name_range)),
                            ),
                        )
                    })
                })
                .collect();
            self.result
                .trait_env
                .insert_impl(trait_id, self_ty, trait_args, bounds, assoc_types);
        }
    }

    fn validate_internal_attrs(&mut self) -> HashSet<TextRange> {
        let mut invalid = HashSet::new();
        for internal in &self.hir.internal_attrs {
            let attr = &internal.attr;
            let is_user_attr =
                self.hir.std_loaded && self.hir.package_for_range(attr.range).is_some();
            let (code, message) = if is_user_attr {
                (
                    "E0049",
                    format!("`{}` is reserved for the standard library", attr.raw),
                )
            } else {
                match attr.name.0.as_str() {
                    "lang"
                        if !matches!(
                            internal.target,
                            InternalAttrTarget::Trait | InternalAttrTarget::Enum
                        ) =>
                    {
                        (
                            "E0053",
                            "`#[lang]` can only be applied to a trait or enum".to_string(),
                        )
                    }
                    "lang" if attr.value.is_none() => (
                        "E0053",
                        "`#[lang]` requires a string value: write `#[lang = \"...\"]`".to_string(),
                    ),
                    "fundamental"
                        if !matches!(
                            internal.target,
                            InternalAttrTarget::Struct | InternalAttrTarget::Enum
                        ) =>
                    {
                        (
                            "E0053",
                            "`#[fundamental]` can only be applied to a struct or enum".to_string(),
                        )
                    }
                    "fundamental" if attr.value.is_some() => (
                        "E0053",
                        "`#[fundamental]` does not accept a value".to_string(),
                    ),
                    _ => continue,
                }
            };
            invalid.insert(attr.range);
            self.result.diagnostics.push(Diagnostic {
                code,
                severity: Severity::Error,
                message,
                labels: vec![SourceLabel {
                    range: attr.range,
                    message: String::new(),
                    style: LabelStyle::Primary,
                }],
                help: None,
                notes: Vec::new(),
            });
        }
        invalid
    }

    pub(crate) fn lower_trait_env_bounds(
        &mut self,
        bounds: &[hir::item_tree::HirGenericBound],
        params: &HashMap<String, Type>,
    ) -> Vec<TraitBound> {
        bounds
            .iter()
            .filter_map(|bound| {
                let trait_id = self.resolve_trait_ref(&bound.trait_ty)?;
                let ty = self.lower_type_ref_with_params_at(
                    &bound.target_ty,
                    params,
                    Some(bound.target_range),
                );
                let trait_args = self.trait_ref_args(
                    trait_id,
                    &bound.trait_ty,
                    &ty,
                    params,
                    Some(bound.trait_range),
                );
                Some(TraitBound {
                    ty,
                    trait_id,
                    trait_args,
                    assoc_constraints: bound
                        .assoc_constraints
                        .iter()
                        .map(|constraint| TraitAssocConstraint {
                            name: constraint.name.0.clone(),
                            ty: self.lower_type_ref_with_params_at(
                                &constraint.ty,
                                params,
                                Some(constraint.range),
                            ),
                        })
                        .collect(),
                })
            })
            .collect()
    }

    pub(crate) fn check_function_bodies(&mut self) {
        for (fid, function) in self.hir.item_tree.functions.iter() {
            if let Some(body_id) = self.hir.function_bodies.get(&fid).copied() {
                let outer_generics = self.impl_generic_names(fid);
                let outer_const_generics = self.impl_const_generic_names(fid);
                self.check_function(
                    fid,
                    function,
                    body_id,
                    &outer_generics,
                    &outer_const_generics,
                );
            }
        }
    }

    pub(crate) fn check_const_bodies(&mut self) {
        let mut dependencies = HashMap::<ConstId, Vec<ConstId>>::new();
        for (const_id, konst) in self.hir.item_tree.consts.iter() {
            let Some(body_id) = self.hir.const_bodies.get(&const_id).copied() else {
                continue;
            };
            let body = &self.hir.bodies[body_id];
            let params = self.impl_item_type_params(|imp| imp.consts.contains(&const_id));
            let declared =
                self.lower_type_ref_with_params_at(&konst.ty, &params, Some(konst.ty_range));
            let mut ctx =
                BodyCtx::new_const(body_id, body, const_id, konst, declared.clone(), params);
            let actual = self.check_expr_expected(&mut ctx, body.root_block, &declared);
            self.expect_assignable(
                &declared,
                &actual,
                "constant initializer",
                ctx.expr_range(body.root_block),
            );

            let mut deps = Vec::new();
            if let Some(span) = first_non_const_expr(body, body.root_block, &mut deps) {
                self.diagnostic(
                    "E0060",
                    "constant initializer is not a constant expression",
                    Some(span),
                );
            }
            let dep_count = deps.len();
            dependencies.insert(const_id, deps);
            // The initializer must evaluate, and its value must fit the
            // declared type: `const N: usize = 0 - 1` previously wrapped
            // silently into `u64::MAX` and became a legal array length.
            match self.const_item_value(const_id, &mut Vec::new()) {
                Some(value) => {
                    if !Self::const_value_fits_type(&declared, value) {
                        self.diagnostic(
                            "E0011",
                            format!(
                                "constant value `{value}` is out of range for `{}`",
                                declared.display(self.hir)
                            ),
                            Some(konst.ty_range),
                        );
                    }
                }
                None if dep_count == 0 => {
                    let span = body.source_map.expr_ranges.get(&body.root_block).copied();
                    self.diagnostic(
                        "E0060",
                        "constant initializer is not a constant expression",
                        span,
                    );
                }
                None => {}
            }
            self.finish_inference(&ctx);
        }

        let mut states = HashMap::<ConstId, u8>::new();
        for const_id in dependencies.keys().copied() {
            if const_cycle_from(const_id, &dependencies, &mut states) {
                let konst = &self.hir.item_tree.consts[const_id];
                self.diagnostic(
                    "E0060",
                    format!(
                        "constant `{}` is part of an initialization cycle",
                        konst.name.0
                    ),
                    Some(konst.name_range),
                );
                break;
            }
        }
    }

    pub(crate) fn check_function(
        &mut self,
        function_id: FunctionId,
        function: &HirFunction,
        body_id: BodyId,
        outer_generics: &[String],
        outer_const_generics: &[String],
    ) {
        let body = &self.hir.bodies[body_id];
        let params = self.function_generic_params(
            function_id,
            function,
            outer_generics,
            outer_const_generics,
        );
        let return_ty = function.ret_type.as_ref().map_or(Type::Unit, |ty| {
            self.lower_type_ref_with_params_at(
                ty,
                &params,
                function.ret_type_range.or(Some(function.name_range)),
            )
        });
        let mut ctx = BodyCtx::new(
            body_id,
            body,
            function_id,
            function,
            return_ty.clone(),
            params,
        );
        let previous_assumptions = std::mem::take(&mut self.active_trait_assumptions);
        let function_bounds = self.current_generic_bounds(&ctx);
        self.active_trait_assumptions =
            self.lower_trait_env_bounds(&function_bounds, &ctx.generic_params);
        self.result
            .body_bounds
            .insert(body_id, self.active_trait_assumptions.clone());
        self.check_type_bounds_inner(
            &ctx,
            &return_ty,
            function.ret_type_range.or(Some(function.name_range)),
        );
        for param in &function.params {
            let param_ty = self.lower_type_ref_with_params_at(
                &param.ty,
                &ctx.generic_params,
                Some(param.ty_range),
            );
            self.check_type_bounds_inner(&ctx, &param_ty, Some(param.ty_range));
        }
        let actual = self.check_expr_expected(&mut ctx, body.root_block, &return_ty);
        self.record_value_use(&mut ctx, body.root_block, crate::result::ValueUse::Move);

        if !actual.is_never() {
            let diagnostic_start = self.result.diagnostics.len();
            let tail_range = match &body.exprs[body.root_block] {
                Expr::Block {
                    tail: Some(tail), ..
                } => body.source_map.expr_ranges.get(tail).copied(),
                Expr::Block { tail: None, .. } => None,
                _ => None,
            };
            let body_range = ctx.expr_range(body.root_block);
            self.expect_assignable(&return_ty, &actual, "function return", body_range);
            if let Some(diagnostic) = self.result.diagnostics.get_mut(diagnostic_start) {
                let return_range = function.ret_type_range.unwrap_or(function.name_range);
                let expected = return_ty.display(self.hir);
                let actual = actual.display(self.hir);
                let (primary_range, secondary_range, secondary_message) =
                    tail_range.map_or_else(
                        || {
                            (
                                return_range,
                                function.name_range,
                                "implicitly returns `()` as its body has no tail or `return` expression"
                                    .into(),
                            )
                        },
                        |tail_range| {
                            (
                                tail_range,
                                return_range,
                                format!("expected `{expected}` because of return type"),
                            )
                        },
                    );
                diagnostic.labels[0].range = primary_range;
                diagnostic.labels[0].message = format!("expected `{expected}`, found `{actual}`");
                diagnostic.labels.push(SourceLabel {
                    range: secondary_range,
                    message: secondary_message,
                    style: LabelStyle::Secondary,
                });
            }
        }
        self.finish_inference(&ctx);
        self.active_trait_assumptions = previous_assumptions;
    }

    fn function_generic_params(
        &mut self,
        function_id: FunctionId,
        function: &HirFunction,
        outer_generics: &[String],
        outer_const_generics: &[String],
    ) -> HashMap<String, Type> {
        let outer_params = crate::lowering::generic_param_map_with_consts(
            outer_generics.iter().map(String::as_str),
            outer_const_generics.iter().map(String::as_str),
        );
        let mut params = crate::lowering::generic_param_map_with_consts(
            outer_generics
                .iter()
                .map(String::as_str)
                .chain(function.generics.iter().map(|name| name.0.as_str()))
                .chain(
                    function
                        .implicit_generics
                        .iter()
                        .map(|name| name.0.as_str()),
                ),
            outer_const_generics
                .iter()
                .map(String::as_str)
                .chain(function.const_generics.iter().map(|name| name.0.as_str())),
        );
        if let Some(self_ty_ref) = self.impl_self_ty_ref(function_id).cloned() {
            let self_ty_range = self
                .hir
                .item_tree
                .impls
                .iter()
                .find_map(|(_, imp)| {
                    imp.methods
                        .contains(&function_id)
                        .then_some(imp.self_ty_range)
                })
                .unwrap_or(function.name_range);
            let self_ty = self.lower_type_ref_with_params_at(
                &self_ty_ref,
                &outer_params,
                Some(self_ty_range),
            );
            let owner = self
                .hir
                .item_tree
                .impls
                .iter()
                .find_map(|(_, imp)| imp.methods.contains(&function_id).then(|| imp.clone()));
            if let Some((imp, trait_id)) = owner.as_ref().and_then(|imp| {
                imp.trait_ty
                    .as_ref()
                    .and_then(|trait_ty| self.resolve_trait_ref(trait_ty))
                    .map(|trait_id| (imp, trait_id))
            }) {
                params = self.trait_ref_subst(
                    trait_id,
                    imp.trait_ty.as_ref().unwrap(),
                    &self_ty,
                    &params,
                    imp.trait_ty_range,
                );
            } else {
                params.insert("Self".into(), self_ty);
            }
        } else if let Some(trait_id) = self.trait_for_default_method(function_id) {
            params.insert("Self".into(), Type::Param("Self".into()));
            // Associated types stay abstract while checking the default body;
            // each `Self::Item` becomes a `Param` placeholder that MIR
            // monomorphization substitutes with the implementing impl's type.
            // Supertraits may declare them (`Iterator::Item` on an extending
            // trait), so collect aliases across the whole trait family.
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
            for member in &seen {
                for alias in &self.hir.item_tree.traits[*member].type_aliases {
                    params
                        .entry(format!("Self::{}", alias.name.0))
                        .or_insert_with(|| Type::Param(alias.name.0.clone()));
                }
            }
        }
        params
    }

    pub(crate) const fn fresh_infer(&mut self) -> Type {
        let id = self.next_infer;
        self.next_infer += 1;
        Type::InferVar(id)
    }

    pub(crate) fn resolve_type(&self, ty: &Type) -> Type {
        match ty {
            Type::InferVar(id) => self
                .infer_values
                .get(id)
                .map_or_else(|| ty.clone(), |value| self.resolve_type(value)),
            Type::Ref(inner, mutable) => Type::Ref(Box::new(self.resolve_type(inner)), *mutable),
            Type::DynTrait {
                trait_id,
                args,
                assoc_bindings,
            } => Type::DynTrait {
                trait_id: *trait_id,
                args: args.iter().map(|arg| self.resolve_type(arg)).collect(),
                assoc_bindings: assoc_bindings
                    .iter()
                    .map(|(name, ty)| (name.clone(), self.resolve_type(ty)))
                    .collect(),
            },
            Type::OwnedDynTrait {
                trait_id,
                args,
                assoc_bindings,
            } => Type::OwnedDynTrait {
                trait_id: *trait_id,
                args: args.iter().map(|arg| self.resolve_type(arg)).collect(),
                assoc_bindings: assoc_bindings
                    .iter()
                    .map(|(name, ty)| (name.clone(), self.resolve_type(ty)))
                    .collect(),
            },
            Type::Ptr { mutable, inner } => Type::Ptr {
                mutable: *mutable,
                inner: Box::new(self.resolve_type(inner)),
            },
            Type::Tuple(elements) => Type::Tuple(
                elements
                    .iter()
                    .map(|item| self.resolve_type(item))
                    .collect(),
            ),
            Type::Slice(inner) => Type::Slice(Box::new(self.resolve_type(inner))),
            Type::Array(inner, len) => Type::Array(Box::new(self.resolve_type(inner)), len.clone()),
            Type::Struct(id, args) => {
                Type::Struct(*id, args.iter().map(|arg| self.resolve_type(arg)).collect())
            }
            Type::Enum(id, args) => {
                Type::Enum(*id, args.iter().map(|arg| self.resolve_type(arg)).collect())
            }
            Type::FunctionItem { function, args } => Type::FunctionItem {
                function: *function,
                args: args.iter().map(|arg| self.resolve_type(arg)).collect(),
            },
            Type::Closure {
                id,
                generics,
                signature,
            } => Type::Closure {
                id: *id,
                generics: generics.clone(),
                signature: self.resolve_callable_signature(signature),
            },
            Type::OpaqueCallable { id, signature } => Type::OpaqueCallable {
                id: *id,
                signature: self.resolve_callable_signature(signature),
            },
            Type::OpaqueTrait { id, trait_id, args } => Type::OpaqueTrait {
                id: *id,
                trait_id: *trait_id,
                args: args.iter().map(|arg| self.resolve_type(arg)).collect(),
            },
            Type::CallableConstraint(signature) => {
                Type::CallableConstraint(self.resolve_callable_signature(signature))
            }
            _ => ty.clone(),
        }
    }

    pub(crate) fn callable_type(&mut self, ty: &Type) -> Type {
        let ty = self.resolve_type(ty);
        let Some(signature) = self.callable_signature_for_type(&ty) else {
            return ty;
        };
        Type::CallableConstraint(signature)
    }

    fn resolve_callable_signature(&self, signature: &CallableSignature) -> CallableSignature {
        CallableSignature {
            is_unsafe: signature.is_unsafe,
            kind: signature.kind,
            params: signature
                .params
                .iter()
                .map(|param| self.resolve_type(param))
                .collect(),
            ret: Box::new(self.resolve_type(&signature.ret)),
        }
    }

    pub(crate) fn callable_signature_for_type(&mut self, ty: &Type) -> Option<CallableSignature> {
        match ty {
            Type::CallableConstraint(signature) => Some(signature.clone()),
            Type::Closure { signature, .. } | Type::OpaqueCallable { signature, .. } => {
                Some(signature.clone())
            }
            Type::FunctionItem { function, args } => {
                Some(self.function_item_signature(*function, args))
            }
            Type::Ref(inner, _) => self.callable_signature_for_type(inner),
            _ => self
                .callable_impl_for_type(ty)
                .map(|(signature, _)| signature),
        }
    }

    /// Binds the generic parameters appearing inside a callable bound signature
    /// from the concrete signature of the argument value, so calls like
    /// `map(iter, closure)` infer the closure's parameter and return types.
    pub(crate) fn collect_callable_argument_subst(
        &mut self,
        expected: &Type,
        actual: &Type,
        subst: &mut HashMap<String, Type>,
    ) {
        let Type::CallableConstraint(expected_signature) = expected else {
            return;
        };
        let Some(actual_signature) = self.callable_signature_for_type(actual) else {
            return;
        };
        for (expected_param, actual_param) in expected_signature
            .params
            .iter()
            .zip(actual_signature.params.iter())
        {
            let _ = self.unify_types(expected_param, actual_param);
            self.last_occurs_error = None;
            crate::lowering::collect_subst(expected_param, actual_param, subst);
        }
        let _ = self.unify_types(&expected_signature.ret, &actual_signature.ret);
        self.last_occurs_error = None;
        crate::lowering::collect_subst(&expected_signature.ret, &actual_signature.ret, subst);
    }

    /// Binds params that appear only inside where-clause associated-type
    /// bindings: for `I: Iterator<Item = T>` with `I` already substituted to
    /// a concrete type, `T` unifies with that type's `Item`.
    pub(crate) fn collect_bound_assoc_subst(
        &mut self,
        function: &HirFunction,
        impl_generic_names: &[String],
        subst: &mut HashMap<String, Type>,
    ) {
        for bound in &function.generic_bounds {
            let Some(param) = bound_target_param(bound) else {
                continue;
            };
            let Some(target_ty) = subst.get(param) else {
                continue;
            };
            let target_ty = self.resolve_type(target_ty);
            if target_ty.is_unknown_like() {
                continue;
            }
            let Some(trait_id) = self.resolve_trait_ref(&bound.trait_ty) else {
                continue;
            };
            for constraint in &bound.assoc_constraints {
                let Some(actual) =
                    self.result
                        .trait_env
                        .associated_type(&target_ty, trait_id, &constraint.name.0)
                else {
                    continue;
                };
                let actual = self.resolve_type(&actual);
                // Lower the constraint's value type against the callee's own
                // generic names so `Item = T` yields `Param("T")`, which
                // collect_subst can bind in the call's substitution.
                let mut bound_params = HashMap::new();
                for name in impl_generic_names {
                    bound_params.insert(name.clone(), Type::Param(name.clone()));
                }
                for name in function
                    .generics
                    .iter()
                    .chain(function.implicit_generics.iter())
                {
                    bound_params.insert(name.0.clone(), Type::Param(name.0.clone()));
                }
                let pattern = self.lower_type_ref_with_params_at(
                    &constraint.ty,
                    &bound_params,
                    Some(constraint.range),
                );
                // The constraint value is a pattern (`&T`, `(T, i32)`,
                // `Vector<T>`), so each param must be matched against the
                // associated type at its own position instead of binding the
                // whole associated type to every param the pattern mentions.
                // Seeded slots hold fresh inference variables and
                // `collect_subst` never overwrites occupied entries, so drop
                // those first and commit the result only when the pattern
                // matches the associated type structurally.
                let mut matched = subst.clone();
                for name in bound_params.keys() {
                    if !crate::body::type_has_param_where(&pattern, &|candidate| {
                        candidate == name.as_str()
                    }) {
                        continue;
                    }
                    if matched.get(name).is_none_or(|existing| {
                        existing.is_unknown_like() || matches!(existing, Type::InferVar(_))
                    }) {
                        matched.remove(name);
                    }
                }
                if crate::lowering::collect_subst(&pattern, &actual, &mut matched) {
                    *subst = matched;
                }
            }
        }
    }

    pub(crate) fn callable_impl_for_type(
        &mut self,
        ty: &Type,
    ) -> Option<(CallableSignature, FunctionId)> {
        let ty = match ty {
            Type::Ref(inner, _) => inner.as_ref(),
            ty => ty,
        };
        let impls = self
            .hir
            .item_tree
            .impls
            .iter()
            .map(|(_, imp)| imp.clone())
            .collect::<Vec<_>>();
        for imp in impls {
            let Some(trait_ty) = imp.trait_ty.as_ref() else {
                continue;
            };
            let Some(kind) = crate::lowering::builtin_callable_kind(trait_ty) else {
                continue;
            };
            let Some(signature) = imp.callable.as_ref() else {
                continue;
            };
            let Some(subst) = self.impl_subst_from_self_ty(&imp, ty) else {
                continue;
            };
            let Some(fid) = imp
                .methods
                .iter()
                .copied()
                .find(|fid| self.hir.item_tree.functions[*fid].name.0 == "call")
            else {
                continue;
            };
            return Some((
                self.lower_hir_callable_signature(signature, kind, &subst, imp.trait_ty_range),
                fid,
            ));
        }
        None
    }

    fn function_item_signature(&mut self, fid: FunctionId, args: &[Type]) -> CallableSignature {
        let function = self.hir.item_tree.functions[fid].clone();
        let impl_generics = self.impl_generic_names(fid);
        let impl_const_generics = self.impl_const_generic_names(fid);
        let params = crate::lowering::generic_param_map_with_consts(
            impl_generics
                .iter()
                .map(String::as_str)
                .chain(function.generics.iter().map(|name| name.0.as_str()))
                .chain(
                    function
                        .implicit_generics
                        .iter()
                        .map(|name| name.0.as_str()),
                ),
            impl_const_generics
                .iter()
                .map(String::as_str)
                .chain(function.const_generics.iter().map(|name| name.0.as_str())),
        );
        let subst = impl_generics
            .iter()
            .map(String::as_str)
            .chain(function.generics.iter().map(|name| name.0.as_str()))
            .chain(
                function
                    .implicit_generics
                    .iter()
                    .map(|name| name.0.as_str()),
            )
            .chain(impl_const_generics.iter().map(String::as_str))
            .chain(function.const_generics.iter().map(|name| name.0.as_str()))
            .zip(args.iter())
            .map(|(name, ty)| (name.to_string(), ty.clone()))
            .collect::<HashMap<_, _>>();
        let output = function.ret_type.as_ref().map_or(Type::Unit, |ret| {
            crate::lowering::substitute_type(
                &self.lower_type_ref_with_params_at(
                    ret,
                    &params,
                    function.ret_type_range.or(Some(function.name_range)),
                ),
                &subst,
            )
        });
        CallableSignature {
            is_unsafe: function.is_unsafe,
            kind: ClosureKind::Fn,
            params: function
                .params
                .iter()
                .map(|param| {
                    crate::lowering::substitute_type(
                        &self.lower_type_ref_with_params_at(
                            &param.ty,
                            &params,
                            Some(param.ty_range),
                        ),
                        &subst,
                    )
                })
                .collect(),
            ret: Box::new(output),
        }
    }

    pub(crate) fn unify_types(&mut self, lhs: &Type, rhs: &Type) -> bool {
        self.last_occurs_error = None;
        self.unify_types_inner(lhs, rhs)
    }

    fn unify_types_inner(&mut self, lhs: &Type, rhs: &Type) -> bool {
        let lhs = self.resolve_type(lhs);
        let rhs = self.resolve_type(rhs);
        if let Some(result) = self.unify_special_types(&lhs, &rhs) {
            return result;
        }
        match (&lhs, &rhs) {
            (Type::InferVar(id), ty) | (ty, Type::InferVar(id)) => self.bind_infer_var(*id, ty),
            (Type::Ref(a, am), Type::Ref(b, bm))
            | (
                Type::Ptr {
                    mutable: am,
                    inner: a,
                },
                Type::Ptr {
                    mutable: bm,
                    inner: b,
                },
            ) => am == bm && self.unify_types_inner(a, b),
            (
                Type::DynTrait {
                    trait_id: lhs_id,
                    args: lhs_args,
                    assoc_bindings: lhs_assoc,
                },
                Type::DynTrait {
                    trait_id: rhs_id,
                    args: rhs_args,
                    assoc_bindings: rhs_assoc,
                },
            ) => {
                lhs_id == rhs_id
                    && lhs_args.len() == rhs_args.len()
                    && lhs_args
                        .iter()
                        .zip(rhs_args)
                        .all(|(lhs, rhs)| self.unify_types_inner(lhs, rhs))
                    && self.unify_assoc_bindings(lhs_assoc, rhs_assoc)
            }
            (
                Type::OwnedDynTrait {
                    trait_id: lhs_id,
                    args: lhs_args,
                    assoc_bindings: lhs_assoc,
                },
                Type::OwnedDynTrait {
                    trait_id: rhs_id,
                    args: rhs_args,
                    assoc_bindings: rhs_assoc,
                },
            ) => {
                lhs_id == rhs_id
                    && lhs_args.len() == rhs_args.len()
                    && lhs_args
                        .iter()
                        .zip(rhs_args)
                        .all(|(lhs, rhs)| self.unify_types_inner(lhs, rhs))
                    && self.unify_assoc_bindings(lhs_assoc, rhs_assoc)
            }
            (Type::Tuple(a), Type::Tuple(b)) => {
                a.len() == b.len() && a.iter().zip(b).all(|(a, b)| self.unify_types_inner(a, b))
            }
            (Type::Slice(a), Type::Slice(b)) => self.unify_types_inner(a, b),
            (Type::Array(a, al), Type::Array(b, bl)) => al == bl && self.unify_types_inner(a, b),
            (
                Type::FunctionItem {
                    function: af,
                    args: aa,
                },
                Type::FunctionItem {
                    function: bf,
                    args: ba,
                },
            ) => {
                af == bf
                    && aa.len() == ba.len()
                    && aa.iter().zip(ba).all(|(a, b)| self.unify_types_inner(a, b))
            }
            (
                Type::OpaqueTrait {
                    id: aid,
                    trait_id: atrait,
                    args: aa,
                },
                Type::OpaqueTrait {
                    id: bid,
                    trait_id: btrait,
                    args: ba,
                },
            ) => {
                aid == bid
                    && atrait == btrait
                    && aa.len() == ba.len()
                    && aa.iter().zip(ba).all(|(a, b)| self.unify_types_inner(a, b))
            }
            (Type::CallableConstraint(expected), Type::CallableConstraint(actual)) => self
                .unify_callable_signature(
                    expected.is_unsafe,
                    expected.kind,
                    &expected.params,
                    &expected.ret,
                    actual,
                ),
            _ => lhs == rhs || Self::numeric_assignable(&lhs, &rhs),
        }
    }

    fn unify_assoc_bindings(&mut self, lhs: &[(String, Type)], rhs: &[(String, Type)]) -> bool {
        lhs.len() == rhs.len()
            && lhs.iter().all(|(name, lhs)| {
                rhs.iter()
                    .find(|(rhs_name, _)| rhs_name == name)
                    .is_some_and(|(_, rhs)| self.unify_types_inner(lhs, rhs))
            })
    }

    fn unify_special_types(&mut self, lhs: &Type, rhs: &Type) -> Option<bool> {
        if let Type::CallableConstraint(expected) = lhs
            && !matches!(rhs, Type::CallableConstraint(_))
            && let Some(actual) = self.callable_signature_for_type(rhs)
        {
            return Some(self.unify_callable_signature(
                expected.is_unsafe,
                expected.kind,
                &expected.params,
                &expected.ret,
                &actual,
            ));
        }
        if let (
            Some(lhs_nominal),
            Some(rhs_nominal),
            Type::Struct(_, lhs_args) | Type::Enum(_, lhs_args),
            Type::Struct(_, rhs_args) | Type::Enum(_, rhs_args),
        ) = (
            type_nominal_identity(lhs),
            type_nominal_identity(rhs),
            lhs,
            rhs,
        ) {
            return Some(
                lhs_nominal == rhs_nominal
                    && lhs_args.len() == rhs_args.len()
                    && lhs_args
                        .iter()
                        .zip(rhs_args)
                        .all(|(a, b)| self.unify_types_inner(a, b)),
            );
        }
        if let (
            Some(lhs_id),
            Some(rhs_id),
            Type::Closure {
                signature: lhs_signature,
                ..
            }
            | Type::OpaqueCallable {
                signature: lhs_signature,
                ..
            },
            Type::Closure {
                signature: rhs_signature,
                ..
            }
            | Type::OpaqueCallable {
                signature: rhs_signature,
                ..
            },
        ) = (
            type_callable_identity(lhs),
            type_callable_identity(rhs),
            lhs,
            rhs,
        ) {
            return Some(
                lhs_id == rhs_id
                    && self.unify_same_callable_signature(lhs_signature, rhs_signature),
            );
        }
        None
    }

    fn unify_callable_signature(
        &mut self,
        expected_unsafe: bool,
        expected_kind: ClosureKind,
        expected_params: &[Type],
        expected_ret: &Type,
        actual: &CallableSignature,
    ) -> bool {
        (!actual.is_unsafe || expected_unsafe)
            && expected_kind.accepts(actual.kind)
            && expected_params.len() == actual.params.len()
            && expected_params
                .iter()
                .zip(&actual.params)
                .all(|(a, b)| self.unify_types_inner(a, b))
            && self.unify_types_inner(expected_ret, &actual.ret)
    }

    fn unify_same_callable_signature(
        &mut self,
        lhs: &CallableSignature,
        rhs: &CallableSignature,
    ) -> bool {
        lhs.is_unsafe == rhs.is_unsafe
            && lhs.kind == rhs.kind
            && lhs.params.len() == rhs.params.len()
            && lhs
                .params
                .iter()
                .zip(&rhs.params)
                .all(|(a, b)| self.unify_types_inner(a, b))
            && self.unify_types_inner(&lhs.ret, &rhs.ret)
    }

    fn bind_infer_var(&mut self, id: u32, ty: &Type) -> bool {
        if matches!(ty, Type::InferVar(other) if *other == id) {
            return true;
        }
        if type_contains_infer_var(id, ty, &self.infer_values, &mut HashSet::new()) {
            self.last_occurs_error = Some((id, ty.clone()));
            return false;
        }
        self.infer_values.insert(id, ty.clone());
        true
    }

    pub(crate) fn record_lambda(
        &mut self,
        body_id: BodyId,
        expr: ExprId,
        params: Vec<(String, Option<TextRange>, Type)>,
    ) {
        self.pending_lambdas.push(PendingLambda {
            body_id,
            expr,
            params,
        });
    }

    fn finish_inference(&mut self, ctx: &BodyCtx<'_>) {
        let body_id = ctx.body_id;
        self.finish_pending_generic_calls(ctx);
        self.resolve_body_inference_results(body_id);
        self.finish_pending_moves(body_id);
        self.report_delayed_binding_errors(body_id);
        self.report_lambda_inference_errors(body_id);
    }

    fn resolve_body_inference_results(&mut self, body_id: BodyId) {
        let exprs = self
            .result
            .expr_types
            .iter()
            .filter(|((bid, _), _)| *bid == body_id)
            .map(|(key, ty)| (*key, self.resolve_type(ty)))
            .collect::<Vec<_>>();
        for (key, ty) in exprs {
            self.result.expr_types.insert(key, ty);
        }
        let generic_calls = self
            .result
            .generic_calls
            .iter()
            .filter(|((checked_body, _), _)| *checked_body == body_id)
            .map(|(key, call)| {
                let mut call = call.clone();
                call.args = call.args.iter().map(|arg| self.resolve_type(arg)).collect();
                (*key, call)
            })
            .collect::<Vec<_>>();
        for (key, call) in generic_calls {
            self.result.generic_calls.insert(key, call);
        }
        let method_signatures = self
            .result
            .method_signatures
            .iter()
            .filter(|((checked_body, _), _)| *checked_body == body_id)
            .map(|(key, signature)| {
                let mut signature = signature.clone();
                signature.params = signature
                    .params
                    .iter()
                    .map(|param| self.resolve_type(param))
                    .collect();
                *signature.ret = self.resolve_type(&signature.ret);
                (*key, signature)
            })
            .collect::<Vec<_>>();
        for (key, signature) in method_signatures {
            self.result.method_signatures.insert(key, signature);
        }
        let lambdas = self
            .result
            .lambda_infos
            .iter()
            .filter(|((checked_body, _), _)| *checked_body == body_id)
            .map(|(key, info)| {
                let mut info = info.clone();
                for capture in &mut info.captures {
                    capture.ty = self.resolve_type(&capture.ty);
                }
                (*key, info)
            })
            .collect::<Vec<_>>();
        for (key, info) in lambdas {
            self.result.lambda_infos.insert(key, info);
        }
        let patterns = self
            .result
            .pattern_types
            .iter()
            .filter(|((checked_body, _), _)| *checked_body == body_id)
            .map(|(key, ty)| (*key, self.resolve_type(ty)))
            .collect::<Vec<_>>();
        for (key, ty) in patterns {
            self.result.pattern_types.insert(key, ty);
        }
        let pattern_bindings = self
            .result
            .pattern_binding_types
            .iter()
            .filter(|((checked_body, _), _)| *checked_body == body_id)
            .map(|(key, ty)| (*key, self.resolve_type(ty)))
            .collect::<Vec<_>>();
        for (key, ty) in pattern_bindings {
            self.result.pattern_binding_types.insert(key, ty);
        }
    }

    fn finish_pending_moves(&mut self, body_id: BodyId) {
        let pending_moves = self
            .pending_move_uses
            .iter()
            .filter(|(checked_body, _)| *checked_body == body_id)
            .copied()
            .collect::<Vec<_>>();
        self.pending_move_uses
            .retain(|(checked_body, _)| *checked_body != body_id);
        for key in pending_moves {
            if self
                .result
                .expr_types
                .get(&key)
                .is_some_and(|ty| !self.result.trait_env.type_is_copy(ty))
            {
                self.result
                    .value_uses
                    .entry(key)
                    .and_modify(|current| *current = current.merge(crate::result::ValueUse::Move))
                    .or_insert(crate::result::ValueUse::Move);
            }
        }
    }

    fn report_delayed_binding_errors(&mut self, body_id: BodyId) {
        let delayed_bindings = self
            .pending_delayed_bindings
            .iter()
            .filter(|(checked_body, _, _)| *checked_body == body_id)
            .copied()
            .collect::<Vec<_>>();
        for (_, binding, range) in delayed_bindings {
            let ty = self
                .result
                .pattern_binding_types
                .get(&(body_id, binding))
                .cloned()
                .unwrap_or(Type::Unknown);
            if matches!(ty, Type::InferVar(_) | Type::Unknown) {
                self.diagnostic(
                    "E0045",
                    "cannot infer the type of a delayed `let` binding",
                    range,
                );
            }
        }
    }

    fn report_lambda_inference_errors(&mut self, body_id: BodyId) {
        let pending = self
            .pending_lambdas
            .iter()
            .filter(|lambda| lambda.body_id == body_id)
            .map(|lambda| {
                (
                    lambda.expr,
                    lambda
                        .params
                        .iter()
                        .map(|(name, range, ty)| (name.clone(), *range, self.resolve_type(ty)))
                        .collect::<Vec<_>>(),
                )
            })
            .collect::<Vec<_>>();
        for (_, params) in pending {
            for (name, range, ty) in params {
                if matches!(ty, Type::InferVar(_)) {
                    self.diagnostic(
                        "E0045",
                        format!("cannot infer type of anonymous function parameter `{name}`"),
                        range,
                    );
                }
            }
        }
    }

    pub(crate) fn impl_generic_names(&self, function_id: FunctionId) -> Vec<String> {
        self.hir
            .item_tree
            .impls
            .iter()
            .find_map(|(_, imp)| {
                imp.methods
                    .contains(&function_id)
                    .then(|| imp.generics.iter().map(|name| name.0.clone()).collect())
            })
            .or_else(|| {
                self.hir.item_tree.traits.iter().find_map(|(_, tr)| {
                    tr.default_methods
                        .contains(&function_id)
                        .then(|| tr.generics.iter().map(|name| name.0.clone()).collect())
                })
            })
            .unwrap_or_default()
    }

    pub(crate) fn impl_const_generic_names(&self, function_id: FunctionId) -> Vec<String> {
        self.hir
            .item_tree
            .impls
            .iter()
            .find_map(|(_, imp)| {
                imp.methods.contains(&function_id).then(|| {
                    imp.const_generics
                        .iter()
                        .map(|name| name.0.clone())
                        .collect()
                })
            })
            .unwrap_or_default()
    }

    pub(crate) fn impl_self_ty_ref(&self, function_id: FunctionId) -> Option<&HirTypeRef> {
        self.hir
            .item_tree
            .impls
            .iter()
            .find_map(|(_, imp)| imp.methods.contains(&function_id).then_some(&imp.self_ty))
    }

    pub(crate) fn trait_for_default_method(
        &self,
        function_id: FunctionId,
    ) -> Option<hir::item_tree::TraitId> {
        self.hir.item_tree.traits.iter().find_map(|(trait_id, tr)| {
            tr.default_methods
                .contains(&function_id)
                .then_some(trait_id)
        })
    }

    pub(crate) fn check_type_layouts(&mut self) {
        let structs = self
            .hir
            .item_tree
            .structs
            .iter()
            .map(|(id, strukt)| {
                (
                    id,
                    strukt.name.0.clone(),
                    strukt.name_range,
                    strukt.fields.clone(),
                )
            })
            .collect::<Vec<_>>();

        for (id, name, name_range, fields) in structs {
            if let Some(field_range) = fields.iter().find_map(|field| {
                self.type_ref_contains_inline_type(
                    &field.ty,
                    NominalType::Struct(id),
                    &mut Vec::new(),
                )
                .then_some(field.ty_range)
            }) {
                self.infinite_layout_types.insert(NominalType::Struct(id));
                self.diagnostic(
                    "E0072",
                    format!("recursive type `{name}` has infinite size"),
                    Some(name_range),
                );
                self.result
                    .diagnostics
                    .last_mut()
                    .unwrap()
                    .labels
                    .push(SourceLabel {
                        range: field_range,
                        message: "recursive field".into(),
                        style: LabelStyle::Secondary,
                    });
            }
        }

        let enums = self
            .hir
            .item_tree
            .enums
            .iter()
            .map(|(id, item)| {
                (
                    id,
                    item.name.0.clone(),
                    item.name_range,
                    item.variants.clone(),
                )
            })
            .collect::<Vec<_>>();
        for (id, name, name_range, variants) in enums {
            let target = NominalType::Enum(id);
            let recursive_field =
                variants.iter().find_map(|variant| match &variant.kind {
                    HirVariantKind::Unit => None,
                    HirVariantKind::Tuple(fields) => fields
                        .iter()
                        .zip(&variant.field_ranges)
                        .find_map(|(field, range)| {
                            self.type_ref_contains_inline_type(field, target, &mut Vec::new())
                                .then_some(*range)
                        }),
                    HirVariantKind::Struct(fields) => fields.iter().find_map(|field| {
                        self.type_ref_contains_inline_type(&field.ty, target, &mut Vec::new())
                            .then_some(field.ty_range)
                    }),
                });
            if let Some(field_range) = recursive_field {
                self.infinite_layout_types.insert(target);
                self.diagnostic(
                    "E0072",
                    format!("recursive type `{name}` has infinite size"),
                    Some(name_range),
                );
                self.result
                    .diagnostics
                    .last_mut()
                    .unwrap()
                    .labels
                    .push(SourceLabel {
                        range: field_range,
                        message: "recursive field".into(),
                        style: LabelStyle::Secondary,
                    });
            }
        }
    }

    fn type_ref_contains_inline_type(
        &self,
        ty: &HirTypeRef,
        target: NominalType,
        seen: &mut Vec<NominalType>,
    ) -> bool {
        match ty {
            HirTypeRef::Named(path) => {
                let Some(name) = path.as_single_name().map(|name| name.0.as_str()) else {
                    return false;
                };
                let Some(current) = self.find_nominal_type(name) else {
                    return false;
                };
                if current == target {
                    return true;
                }
                if seen.contains(&current) {
                    return false;
                }
                seen.push(current);
                let found = self.nominal_type_contains_inline_target(current, target, seen);
                seen.pop();
                found
            }
            HirTypeRef::Tuple(elements) => elements
                .iter()
                .any(|ty| self.type_ref_contains_inline_type(ty, target, seen)),
            HirTypeRef::Array(_, HirConstArg::Value(0))
            | HirTypeRef::Never
            | HirTypeRef::Const(_)
            | HirTypeRef::Ref(_, _)
            | HirTypeRef::Ptr { .. }
            | HirTypeRef::ImplTrait { .. }
            | HirTypeRef::DynTrait { .. }
            | HirTypeRef::Unknown
            | HirTypeRef::Error => false,
            HirTypeRef::Array(inner, _) | HirTypeRef::Slice(inner) => {
                self.type_ref_contains_inline_type(inner, target, seen)
            }
        }
    }

    fn find_nominal_type(&self, name: &str) -> Option<NominalType> {
        self.hir
            .item_tree
            .structs
            .iter()
            .find_map(|(id, item)| (item.name.0 == name).then_some(NominalType::Struct(id)))
            .or_else(|| {
                self.hir
                    .item_tree
                    .enums
                    .iter()
                    .find_map(|(id, item)| (item.name.0 == name).then_some(NominalType::Enum(id)))
            })
    }

    fn nominal_type_contains_inline_target(
        &self,
        current: NominalType,
        target: NominalType,
        seen: &mut Vec<NominalType>,
    ) -> bool {
        match current {
            NominalType::Struct(id) => self.hir.item_tree.structs[id]
                .fields
                .iter()
                .any(|field| self.type_ref_contains_inline_type(&field.ty, target, seen)),
            NominalType::Enum(id) => {
                self.hir.item_tree.enums[id]
                    .variants
                    .iter()
                    .any(|variant| match &variant.kind {
                        HirVariantKind::Unit => false,
                        HirVariantKind::Tuple(fields) => fields
                            .iter()
                            .any(|field| self.type_ref_contains_inline_type(field, target, seen)),
                        HirVariantKind::Struct(fields) => fields.iter().any(|field| {
                            self.type_ref_contains_inline_type(&field.ty, target, seen)
                        }),
                    })
            }
        }
    }

    pub(crate) fn type_has_infinite_layout(&self, ty: &Type) -> bool {
        match ty {
            Type::Struct(id, _) => self
                .infinite_layout_types
                .contains(&NominalType::Struct(*id)),
            Type::Enum(id, _) => self.infinite_layout_types.contains(&NominalType::Enum(*id)),
            Type::Tuple(fields) => fields
                .iter()
                .any(|field| self.type_has_infinite_layout(field)),
            Type::Array(inner, len) => {
                !matches!(len, crate::ConstArg::Value(0)) && self.type_has_infinite_layout(inner)
            }
            _ => false,
        }
    }

    /// Resolves a name used as a const-generic argument to a constant item's
    /// compile-time value.
    /// Looks up a constant item by name for a reference at `use_range`.
    ///
    /// Resolution rules: a non-`pub` constant only resolves for references
    /// inside its own package; when several same-package constants share a
    /// name (module shadowing), the declaration closest before the use wins.
    pub(crate) fn const_item_visible_at(
        &self,
        name: &str,
        use_range: rowan::TextRange,
    ) -> Option<ConstId> {
        let use_package = self.hir.package_for_range(use_range);
        let mut best: Option<ConstId> = None;
        for (id, konst) in self.hir.item_tree.consts.iter() {
            if konst.name.0 != name {
                continue;
            }
            let declared_package = self.hir.package_for_range(konst.name_range);
            if declared_package != use_package && !konst.visibility.is_public() {
                continue;
            }
            if declared_package == use_package && konst.name_range.start() <= use_range.start() {
                best = Some(id);
            } else if best.is_none() && declared_package != use_package {
                // Cross-package references accept any first match.
                best = Some(id);
            }
        }
        best
    }

    /// Evaluates a constant item to its compile-time integer value so it can
    /// back array lengths and const-generic arguments. Initialization cycles
    /// are rejected as E0060 elsewhere; `active` guards against them here too.
    pub(crate) fn const_item_value(&self, id: ConstId, active: &mut Vec<ConstId>) -> Option<u64> {
        if active.contains(&id) {
            return None;
        }
        let body_id = self.hir.const_bodies.get(&id).copied()?;
        let body = &self.hir.bodies[body_id];
        active.push(id);
        let value = self.const_expr_value(body, body.root_block, active);
        active.pop();
        value
    }

    pub(crate) fn const_expr_value(
        &self,
        body: &Body,
        expr_id: ExprId,
        active: &mut Vec<ConstId>,
    ) -> Option<u64> {
        match &body.exprs[expr_id] {
            Expr::IntLiteral { value, .. } => Some(*value),
            Expr::BoolLiteral { value } => Some(u64::from(*value)),
            Expr::Unary { operand, op } => {
                let value = self.const_expr_value(body, *operand, active)?;
                match op {
                    // The evaluator models non-negative values only (array
                    // lengths and const arguments); negatives stay non-const.
                    UnaryOp::Neg if value == 0 => Some(0),
                    UnaryOp::Pos => Some(value),
                    UnaryOp::Not => Some(!value),
                    _ => None,
                }
            }
            Expr::Binary { lhs, rhs, op } => {
                let lhs_value = self.const_expr_value(body, *lhs, active)?;
                let rhs_value = self.const_expr_value(body, *rhs, active)?;
                const_value_binary(lhs_value, rhs_value, *op)
            }
            Expr::Cast { base, target } => {
                let value = self.const_expr_value(body, *base, active)?;
                Some(match const_target_int_ty(target) {
                    Some(int_ty) => truncate_const_value(int_ty, value),
                    None => value,
                })
            }
            Expr::Block { tail, .. } => {
                tail.and_then(|tail| self.const_expr_value(body, tail, active))
            }
            Expr::Path {
                resolved: Some(ResolvedName::Const(referenced)),
                ..
            } => self.const_item_value(*referenced, active),
            _ => None,
        }
    }

    pub(crate) fn check_generic_recursion(&mut self) {
        for i in 0..self.generic_edges.len() {
            if !self.generic_edges[i].grows {
                continue;
            }
            let caller = self.generic_edges[i].caller;
            let target = self.generic_edges[i].callee;
            if !self.reaches(target, caller) {
                continue;
            }

            let callee_name = self.hir.item_tree.functions[target].name.0.clone();
            self.diagnostic(
                "E0033",
                format!("generic recursion grows type arguments while calling `{callee_name}`"),
                self.generic_edges[i].span,
            );
        }
    }

    fn reaches(&self, from: FunctionId, target: FunctionId) -> bool {
        let mut seen = Vec::new();
        let mut stack = vec![from];

        while let Some(next) = stack.pop() {
            if next == target {
                return true;
            }
            if seen.contains(&next) {
                continue;
            }
            seen.push(next);
            stack.extend(
                self.generic_edges
                    .iter()
                    .filter_map(|edge| (edge.caller == next).then_some(edge.callee)),
            );
        }

        false
    }

    pub(crate) fn join_branch_types(
        &mut self,
        lhs: Type,
        rhs: Type,
        context: &str,
        span: Option<TextRange>,
    ) -> Type {
        if self.unify_types(&lhs, &rhs) {
            return self.resolve_type(&lhs);
        }
        if self.last_occurs_error.take().is_some() {
            self.diagnostic("E0067", "cannot construct an infinite type", span);
            return Type::Error;
        }
        if matches!(
            (&lhs, &rhs),
            (Type::CallableConstraint(_), Type::CallableConstraint(_))
        ) && self.unify_types(&rhs, &lhs)
        {
            return self.resolve_type(&rhs);
        }
        if self.last_occurs_error.take().is_some() {
            self.diagnostic("E0067", "cannot construct an infinite type", span);
            return Type::Error;
        }
        if lhs.is_never() {
            return rhs;
        }
        if rhs.is_never() {
            return lhs;
        }
        if lhs.is_unknown_like() {
            return rhs;
        }
        if rhs.is_unknown_like() {
            return lhs;
        }
        if let Some(ty) = Self::join_numeric_types(&lhs, &rhs) {
            ty
        } else if lhs == rhs {
            lhs
        } else {
            self.diagnostic(
                "E0002",
                format!(
                    "{} have incompatible types: {} and {}",
                    context,
                    lhs.display(self.hir),
                    rhs.display(self.hir)
                ),
                span,
            );
            Type::Error
        }
    }

    pub(crate) fn expect_numeric(&mut self, ty: &Type, context: &str, span: Option<TextRange>) {
        if ty.is_unknown_like() || ty.is_numeric() {
            return;
        }
        self.diagnostic(
            "E0003",
            format!("{} must be numeric, got {}", context, ty.display(self.hir)),
            span,
        );
    }

    pub(crate) fn expect_assignable(
        &mut self,
        expected: &Type,
        actual: &Type,
        context: &str,
        span: Option<TextRange>,
    ) {
        self.expect_assignable_with_occurs_span(expected, actual, context, span, span);
    }

    pub(crate) fn expect_assignable_with_occurs_span(
        &mut self,
        expected: &Type,
        actual: &Type,
        context: &str,
        span: Option<TextRange>,
        occurs_span: Option<TextRange>,
    ) {
        if let Type::OpaqueCallable { id, signature } = self.resolve_type(expected) {
            self.expect_opaque_callable_assignable(id, &signature, actual, span, occurs_span);
            return;
        }
        if let Type::OpaqueTrait { id, trait_id, args } = self.resolve_type(expected) {
            self.expect_opaque_trait_assignable(id, trait_id, &args, actual, span);
            return;
        }
        if self.unify_types(expected, actual) {
            return;
        }
        if self.last_occurs_error.take().is_some() {
            self.diagnostic("E0067", "cannot construct an infinite type", occurs_span);
            return;
        }
        let expected = self.resolve_type(expected);
        let actual = self.resolve_type(actual);
        if expected.is_unknown_like()
            || actual.is_unknown_like()
            || expected == actual
            || Self::numeric_assignable(&expected, &actual)
            || Self::is_slice_coercion(&expected, &actual)
            || self.is_dyn_trait_coercion_allowed(&expected, &actual)
            || self.is_owned_dyn_trait_coercion_allowed(&expected, &actual)
            || Self::structural_assignable(&expected, &actual)
        {
            return;
        }
        if actual.is_never() {
            return;
        }
        self.diagnostic(
            "E0001",
            format!(
                "{} type mismatch: expected {}, got {}",
                context,
                expected.display(self.hir),
                actual.display(self.hir)
            ),
            span,
        );
    }

    fn expect_opaque_callable_assignable(
        &mut self,
        id: OpaqueCallableId,
        signature: &CallableSignature,
        actual: &Type,
        span: Option<TextRange>,
        occurs_span: Option<TextRange>,
    ) {
        let actual = self.resolve_type(actual);
        if actual.is_unknown_like() || actual.is_never() {
            return;
        }
        if matches!(actual, Type::OpaqueCallable { id: actual_id, .. } if actual_id == id) {
            return;
        }

        let required = Type::CallableConstraint(signature.clone());
        if !self.unify_types(&required, &actual) {
            if self.last_occurs_error.take().is_some() {
                self.diagnostic("E0067", "cannot construct an infinite type", occurs_span);
                return;
            }
            let message = self
                .callable_signature_for_type(&actual)
                .filter(|actual| actual.is_unsafe && !signature.is_unsafe)
                .map_or_else(
                    || {
                        format!(
                            "opaque callable return does not satisfy `{}`",
                            required.display(self.hir)
                        )
                    },
                    |_| {
                        format!(
                            "unsafe function does not satisfy opaque callable return bound `{}`",
                            signature.kind.as_str()
                        )
                    },
                );
            self.diagnostic("E0035", message, span);
            return;
        }

        let actual = self.resolve_type(&actual);
        let Some(previous) = self.result.opaque_hidden_types.get(&id).cloned() else {
            self.result.opaque_hidden_types.insert(id, actual);
            return;
        };
        if self.unify_types(&previous, &actual) {
            return;
        }
        self.diagnostic(
            "E0002",
            format!(
                "opaque callable return has incompatible concrete types: {} and {}",
                previous.display(self.hir),
                actual.display(self.hir)
            ),
            span,
        );
    }

    fn expect_opaque_trait_assignable(
        &mut self,
        id: OpaqueCallableId,
        trait_id: hir::item_tree::TraitId,
        args: &[Type],
        actual: &Type,
        span: Option<TextRange>,
    ) {
        let actual = self.resolve_type(actual);
        if actual.is_unknown_like() || actual.is_never() {
            return;
        }
        if matches!(actual, Type::OpaqueTrait { id: actual_id, .. } if actual_id == id) {
            return;
        }
        if !self
            .result
            .trait_env
            .type_implements_with_args_assuming(&actual, trait_id, args, &[])
        {
            self.diagnostic(
                "E0035",
                format!(
                    "opaque return type `{}` does not satisfy `{}`",
                    actual.display(self.hir),
                    self.hir.item_tree.traits[trait_id].name.0
                ),
                span,
            );
            return;
        }
        let Some(previous) = self.result.opaque_hidden_types.get(&id).cloned() else {
            self.result.opaque_hidden_types.insert(id, actual);
            return;
        };
        if !self.unify_types(&previous, &actual) {
            self.diagnostic(
                "E0002",
                format!(
                    "opaque return has incompatible concrete types: {} and {}",
                    previous.display(self.hir),
                    actual.display(self.hir)
                ),
                span,
            );
        }
    }

    pub(crate) fn expect_sized_value(&mut self, ty: &Type, span: Option<TextRange>) {
        if ty.is_valid_value_type() {
            return;
        }
        self.diagnostic(
            "E0043",
            format!(
                "type `{}` contains unsized {} in a position that requires a sized type",
                ty.display(self.hir),
                if type_contains_slice(ty) {
                    "slice `[T]`"
                } else if type_contains_dyn_trait(ty) {
                    "trait object `dyn Trait`"
                } else {
                    "`str`"
                }
            ),
            span,
        );
    }

    pub(crate) fn diagnostic(
        &mut self,
        code: &'static str,
        message: impl Into<String>,
        span: Option<TextRange>,
    ) {
        self.diagnostic_with_help(code, message, span, None);
    }

    /// Emits a diagnostic with an explicit help note, so call sites never
    /// have to key extra guidance on English message substrings.
    pub(crate) fn diagnostic_with_help(
        &mut self,
        code: &'static str,
        message: impl Into<String>,
        span: Option<TextRange>,
        help: Option<String>,
    ) {
        let span = span.expect("type-checker diagnostics require a source range");
        let message = message.into();
        if self.result.diagnostics.iter().any(|diagnostic| {
            diagnostic.code == code
                && diagnostic.message == message
                && diagnostic
                    .labels
                    .first()
                    .is_some_and(|label| label.range == span)
        }) {
            return;
        }
        let notes = if let Some(help) = help {
            vec![help]
        } else {
            match code {
            "E0001" => vec!["expected one type but found another; consider an explicit type annotation or cast".into()],
            "E0002" => vec!["all branches must produce values of the same type; ensure both branches return compatible types".into()],
            "E0003" => vec!["this operation requires a numeric or `char` type".into()],
            "E0004" => vec!["only functions can be called".into()],
            "E0005" => vec!["check the function declaration for the expected parameter count".into()],
            "E0006" => vec!["check the struct definition for available fields".into()],
            "E0007" => vec!["add the missing field to the struct literal".into()],
            "E0008" => vec!["only references can be dereferenced, and only arrays can be indexed".into()],
            "E0009" => vec!["check that the path names a struct definition".into()],
            "E0010" => vec!["ensure tuple element counts match".into()],
            "E0011" => vec!["use a valid numeric suffix and keep the literal within that type's range".into()],
            "E0012" => vec!["this source and target type pair does not support `as` conversion".into()],
            "E0013" => vec!["check the impl block and receiver type".into()],
            "E0020" | "E0024" => vec!["remove the duplicate definition".into()],
            "E0031" if message == "cannot call a mutable closure through an immutable binding" => {
                vec!["add `mut` to the closure binding because calling it may update captured state".into()]
            }
            "E0031" if message.contains("immutable parameter") => {
                vec!["add `mut` to the parameter declaration".into()]
            }
            "E0031" => vec!["add `mut` to the `let` binding if reassignment is intended".into()],
            "E0033" => vec!["recursive generic calls must reuse the same type arguments; wrapping them requires infinitely many instantiations".into()],
            "E0035" => vec!["the inferred type must implement every trait bound on the generic parameter".into()],
            "E0036" => vec!["add the required trait impl for this type".into()],
            "E0037" => vec!["make impl where-clause bounds structurally smaller than the implemented type".into()],
            "E0038" => vec!["use a variant pattern whose shape and fields match the enum declaration".into()],
            "E0039" => vec!["cover every possible case or add a wildcard arm".into()],
            "E0041" => vec!["every field must implement `Copy`; add the required generic bounds or remove the impl".into()],
            "E0042" => vec!["move this statement inside a `while`, `for`, or `loop` loop".into()],
            "E0065" => vec!["use `loop` when the loop should produce a value, or remove the value".into()],
            "E0066" => vec!["diverge with `return`, `break`, `continue`, or an infinite `loop`".into()],
            "E0043" => vec!["put unsized `str` or `[T]` behind a reference or raw pointer".into()],
            "E0044" => vec!["define every supertrait and remove cycles from the trait hierarchy".into()],
            "E0045" => vec!["add an explicit type annotation or use the binding where its type is known".into()],
            "E0047" => vec!["remove the duplicate or overlapping trait implementation".into()],
            "E0048" if message.contains("callable trait names are reserved") => {
                vec!["use another name for a user-defined trait".into()]
            }
            "E0048" => vec!["define the trait or a participating nominal type in the current package".into()],
            "E0057" => vec!["a `let` binding or `for` loop header has no alternative branch, so its pattern must match every value; use `match` to handle the other cases".into()],
            "E0059" => vec!["assign the binding on every path before reading it".into()],
            "E0060" => vec!["use literals, constants, pure operators, casts, or aggregate values in a constant initializer".into()],
            "E0072" => vec!["insert indirection such as `&`, `*const`, or `*mut` to break the cycle".into()],
            "E0391" => vec!["replace the recursive alias with a non-recursive type".into()],
            "E0022" | "E0025" => vec!["remove the duplicate associated type".into()],
            "E0023" => vec!["define the trait or import it into scope".into()],
            "E0026" => vec!["add an implementation for the required method".into()],
            "E0027" => vec!["add a type definition for the required associated type".into()],
            "E0028" | "E0029" | "E0030" => vec!["the method signature must exactly match the trait declaration: check parameter count, types, and return type".into()],
            "E0067" => vec!["add an explicit type annotation or break the self-referential substitution cycle".into()],
            _ => Vec::new(),
            }
        };
        let e0046_help =
            (code == "E0046").then(|| "wrap this operation in `unsafe { ... }`".to_string());
        self.result.diagnostics.push(Diagnostic {
            code,
            severity: Severity::Error,
            message,
            labels: vec![SourceLabel {
                range: span,
                message: String::new(),
                style: LabelStyle::Primary,
            }],
            help: e0046_help,
            notes,
        });
    }

    pub(crate) fn int_literal_type(
        &mut self,
        suffix: Option<&str>,
        expected: Option<&Type>,
        span: Option<TextRange>,
    ) -> Type {
        if let Some(suffix) = suffix {
            return IntTy::parse(suffix).map_or_else(
                || {
                    let (message, help) = integer_suffix_error(suffix);
                    self.diagnostic_with_help("E0011", message, span, help);
                    Type::Error
                },
                Type::Int,
            );
        }
        match expected {
            Some(Type::Int(ty)) => Type::Int(*ty),
            _ => Type::InferInt,
        }
    }

    pub(crate) fn float_literal_type(
        &mut self,
        suffix: Option<&str>,
        expected: Option<&Type>,
        span: Option<TextRange>,
    ) -> Type {
        if let Some(suffix) = suffix {
            return FloatTy::parse(suffix).map_or_else(
                || {
                    let (message, help) = float_suffix_error(suffix);
                    self.diagnostic_with_help("E0011", message, span, help);
                    Type::Error
                },
                Type::Float,
            );
        }
        match expected {
            Some(Type::Float(ty)) => Type::Float(*ty),
            _ => Type::InferFloat,
        }
    }

    pub(crate) const fn numeric_assignable(expected: &Type, actual: &Type) -> bool {
        matches!(
            (expected, actual),
            (Type::Int(_) | Type::InferInt, Type::InferInt)
                | (Type::InferInt, Type::Int(_))
                | (Type::Float(_) | Type::InferFloat, Type::InferFloat)
                | (Type::InferFloat, Type::Float(_))
        )
    }

    fn structural_assignable(expected: &Type, actual: &Type) -> bool {
        match (expected, actual) {
            (Type::Ref(expected_inner, expected_mut), Type::Ref(actual_inner, actual_mut))
            | (
                Type::Ptr {
                    mutable: expected_mut,
                    inner: expected_inner,
                },
                Type::Ptr {
                    mutable: actual_mut,
                    inner: actual_inner,
                },
            ) => {
                expected_mut == actual_mut
                    && (expected_inner == actual_inner
                        || Self::numeric_assignable(expected_inner, actual_inner)
                        || Self::structural_assignable(expected_inner, actual_inner))
            }
            (Type::Array(expected_inner, expected_len), Type::Array(actual_inner, actual_len)) => {
                expected_len == actual_len
                    && (expected_inner == actual_inner
                        || Self::numeric_assignable(expected_inner, actual_inner)
                        || Self::structural_assignable(expected_inner, actual_inner))
            }
            (Type::Slice(expected_inner), Type::Slice(actual_inner)) => {
                expected_inner == actual_inner
                    || Self::numeric_assignable(expected_inner, actual_inner)
                    || Self::structural_assignable(expected_inner, actual_inner)
            }
            _ => false,
        }
    }

    pub(crate) fn is_slice_coercion(expected: &Type, actual: &Type) -> bool {
        matches!(
            (expected, actual),
            (
                Type::Ref(expected, expected_mut),
                Type::Ref(actual, actual_mut),
            ) if expected_mut == actual_mut
                && matches!(
                    (expected.as_ref(), actual.as_ref()),
                    (Type::Slice(expected), Type::Array(actual, _))
                        if expected == actual
                            || Self::numeric_assignable(expected, actual)
                            || Self::structural_assignable(expected, actual)
                )
        )
    }

    pub(crate) fn is_dyn_trait_coercion(expected: &Type, actual: &Type) -> bool {
        matches!(
            (expected, actual),
            (
                Type::Ref(expected, expected_mut),
                Type::Ref(actual, actual_mut),
            ) if expected_mut == actual_mut
                && matches!(expected.as_ref(), Type::DynTrait { .. })
                && !matches!(actual.as_ref(), Type::Param(..))
        )
    }

    pub(crate) fn is_owned_dyn_trait_coercion(expected: &Type, actual: &Type) -> bool {
        matches!(expected, Type::OwnedDynTrait { .. }) && !matches!(actual, Type::DynTrait { .. })
    }

    fn is_dyn_trait_coercion_allowed(&mut self, expected: &Type, actual: &Type) -> bool {
        let (Type::Ref(expected_inner, expected_mut), Type::Ref(actual_inner, actual_mut)) =
            (expected, actual)
        else {
            return false;
        };
        if expected_mut != actual_mut {
            return false;
        }
        let Type::DynTrait {
            trait_id,
            args,
            assoc_bindings: expected_assoc,
        } = expected_inner.as_ref()
        else {
            return false;
        };
        if actual_inner.is_unknown_like() {
            return true;
        }
        match actual_inner.as_ref() {
            Type::DynTrait {
                trait_id: actual_id,
                args: actual_args,
                assoc_bindings: actual_assoc,
            }
            | Type::OwnedDynTrait {
                trait_id: actual_id,
                args: actual_args,
                assoc_bindings: actual_assoc,
            } => {
                return self.dyn_trait_upcast_allowed(
                    *actual_id,
                    actual_args,
                    actual_assoc,
                    *trait_id,
                    args,
                    expected_assoc,
                );
            }
            Type::Param(param) => {
                let assumptions = self.active_trait_assumptions.clone();
                return assumptions.iter().any(|bound| {
                    bound.ty == Type::Param(param.clone())
                        && self.dyn_trait_upcast_allowed(
                            bound.trait_id,
                            &bound.trait_args,
                            &bound
                                .assoc_constraints
                                .iter()
                                .map(|constraint| (constraint.name.clone(), constraint.ty.clone()))
                                .collect::<Vec<_>>(),
                            *trait_id,
                            args,
                            expected_assoc,
                        )
                });
            }
            _ => {}
        }
        self.result
            .trait_env
            .type_implements_with_args_assuming(actual_inner, *trait_id, args, &[])
            && self.dyn_assoc_bindings_match_concrete(actual_inner, *trait_id, args, expected_assoc)
    }

    fn is_owned_dyn_trait_coercion_allowed(&mut self, expected: &Type, actual: &Type) -> bool {
        let Type::OwnedDynTrait {
            trait_id,
            args,
            assoc_bindings: expected_assoc,
        } = expected
        else {
            return false;
        };
        if actual.is_unknown_like() {
            return true;
        }
        match actual {
            Type::OwnedDynTrait {
                trait_id: actual_id,
                args: actual_args,
                assoc_bindings: actual_assoc,
            } => {
                return self.dyn_trait_upcast_allowed(
                    *actual_id,
                    actual_args,
                    actual_assoc,
                    *trait_id,
                    args,
                    expected_assoc,
                );
            }
            Type::Param(param) => {
                let assumptions = self.active_trait_assumptions.clone();
                return assumptions.iter().any(|bound| {
                    bound.ty == Type::Param(param.clone())
                        && self.dyn_trait_upcast_allowed(
                            bound.trait_id,
                            &bound.trait_args,
                            &bound
                                .assoc_constraints
                                .iter()
                                .map(|constraint| (constraint.name.clone(), constraint.ty.clone()))
                                .collect::<Vec<_>>(),
                            *trait_id,
                            args,
                            expected_assoc,
                        )
                });
            }
            Type::DynTrait { .. } => return false,
            _ => {}
        }
        self.result
            .trait_env
            .type_implements_with_args_assuming(actual, *trait_id, args, &[])
            && self.dyn_assoc_bindings_match_concrete(actual, *trait_id, args, expected_assoc)
    }

    fn dyn_assoc_bindings_match_concrete(
        &self,
        actual: &Type,
        trait_id: hir::item_tree::TraitId,
        args: &[Type],
        expected: &[(String, Type)],
    ) -> bool {
        expected.iter().all(|(name, expected_ty)| {
            self.result
                .trait_env
                .associated_type_with_args(actual, trait_id, args, name)
                .is_some_and(|actual_ty| {
                    expected_ty.is_unknown_like()
                        || actual_ty.is_unknown_like()
                        || expected_ty == &actual_ty
                })
        })
    }

    pub(crate) fn join_numeric_types(lhs: &Type, rhs: &Type) -> Option<Type> {
        match (lhs, rhs) {
            (Type::Int(a), Type::Int(b)) if a == b => Some(Type::Int(*a)),
            (Type::Float(a), Type::Float(b)) if a == b => Some(Type::Float(*a)),
            (Type::Int(ty), Type::InferInt) | (Type::InferInt, Type::Int(ty)) => {
                Some(Type::Int(*ty))
            }
            (Type::Float(ty), Type::InferFloat) | (Type::InferFloat, Type::Float(ty)) => {
                Some(Type::Float(*ty))
            }
            (Type::InferInt, Type::InferInt) => Some(Type::InferInt),
            (Type::InferFloat, Type::InferFloat) => Some(Type::InferFloat),
            _ => None,
        }
    }

    /// If `ctx` is not inside `unsafe {}`, emit E0046.
    pub(crate) fn require_unsafe(
        &mut self,
        ctx: &BodyCtx<'_>,
        operation: &str,
        span: Option<rowan::TextRange>,
    ) {
        if ctx.unsafe_depth == 0 {
            self.diagnostic(
                "E0046",
                format!("{operation} requires an unsafe block"),
                span,
            );
        }
    }
}

/// The lexer accepts wide literal suffixes for forward compatibility, but the
/// type system has no such scalars: the C11 backend only maps 8–64-bit
/// integers and `f32`/`f64`.
const RESERVED_WIDE_TYPE_HINT: &str = "scalar types are limited to the C11 portable set: integers `i8`-`i64`/`u8`-`u64` (plus `isize`/`usize`) and floats `f32`/`f64`";

/// Help shown alongside reserved-wide-type suffix errors.
const RESERVED_WIDE_TYPE_HELP: &str = "scalar types are limited to the C11 portable set: integers `i8`–`i64`/`u8`–`u64` (plus `isize`/`usize`) and floats `f32`/`f64`; use the closest supported type";

fn integer_suffix_error(suffix: &str) -> (String, Option<String>) {
    match suffix {
        "i128" | "u128" => (
            format!(
                "integer literal suffix `{suffix}` is not supported; {RESERVED_WIDE_TYPE_HINT}"
            ),
            Some(RESERVED_WIDE_TYPE_HELP.to_string()),
        ),
        _ => (format!("unknown integer literal suffix `{suffix}`"), None),
    }
}

fn float_suffix_error(suffix: &str) -> (String, Option<String>) {
    match suffix {
        "f16" | "f128" => (
            format!("float literal suffix `{suffix}` is not supported; {RESERVED_WIDE_TYPE_HINT}"),
            Some(RESERVED_WIDE_TYPE_HELP.to_string()),
        ),
        _ => (format!("unknown float literal suffix `{suffix}`"), None),
    }
}

fn type_contains_infer_var(
    needle: u32,
    ty: &Type,
    infer_values: &HashMap<u32, Type>,
    visited: &mut HashSet<u32>,
) -> bool {
    match ty {
        Type::InferVar(id) => {
            if *id == needle {
                return true;
            }
            visited.insert(*id)
                && infer_values
                    .get(id)
                    .is_some_and(|ty| type_contains_infer_var(needle, ty, infer_values, visited))
        }
        Type::Ref(inner, _)
        | Type::Slice(inner)
        | Type::Ptr { inner, .. }
        | Type::Array(inner, _) => type_contains_infer_var(needle, inner, infer_values, visited),
        Type::Tuple(elements) | Type::Struct(_, elements) | Type::Enum(_, elements) => elements
            .iter()
            .any(|ty| type_contains_infer_var(needle, ty, infer_values, visited)),
        Type::FunctionItem { args, .. } | Type::OpaqueTrait { args, .. } => args
            .iter()
            .any(|ty| type_contains_infer_var(needle, ty, infer_values, visited)),
        Type::DynTrait {
            args,
            assoc_bindings,
            ..
        }
        | Type::OwnedDynTrait {
            args,
            assoc_bindings,
            ..
        } => args
            .iter()
            .chain(assoc_bindings.iter().map(|(_, ty)| ty))
            .any(|ty| type_contains_infer_var(needle, ty, infer_values, visited)),
        Type::CallableConstraint(signature)
        | Type::Closure { signature, .. }
        | Type::OpaqueCallable { signature, .. } => {
            signature
                .params
                .iter()
                .any(|ty| type_contains_infer_var(needle, ty, infer_values, visited))
                || type_contains_infer_var(needle, &signature.ret, infer_values, visited)
        }
        Type::Int(_)
        | Type::Float(_)
        | Type::InferInt
        | Type::InferFloat
        | Type::Bool
        | Type::Str
        | Type::Char
        | Type::Unit
        | Type::Never
        | Type::Param(_)
        | Type::Const(_)
        | Type::Unknown
        | Type::Error => false,
    }
}

/// Folds a constant binary operation over evaluated integer operands.
fn const_value_binary(lhs: u64, rhs: u64, op: HirBinOp) -> Option<u64> {
    match op {
        // Arithmetic is checked: the evaluator models non-negative values,
        // and an overflow (e.g. `0 - 1`) must leave the constant
        // unevaluated instead of wrapping into a bogus array length.
        HirBinOp::Add => lhs.checked_add(rhs),
        HirBinOp::Sub => lhs.checked_sub(rhs),
        HirBinOp::Mul => lhs.checked_mul(rhs),
        HirBinOp::Div if rhs != 0 => Some(lhs / rhs),
        HirBinOp::Mod if rhs != 0 => Some(lhs % rhs),
        HirBinOp::BitAnd => Some(lhs & rhs),
        HirBinOp::BitOr => Some(lhs | rhs),
        HirBinOp::BitXor => Some(lhs ^ rhs),
        HirBinOp::Shl if rhs < u64::BITS as u64 => lhs.checked_shl(rhs.try_into().ok()?),
        HirBinOp::Shr if rhs < u64::BITS as u64 => lhs.checked_shr(rhs.try_into().ok()?),
        HirBinOp::Eq => Some(u64::from(lhs == rhs)),
        HirBinOp::Neq => Some(u64::from(lhs != rhs)),
        HirBinOp::Lt => Some(u64::from(lhs < rhs)),
        HirBinOp::Gt => Some(u64::from(lhs > rhs)),
        HirBinOp::LtEq => Some(u64::from(lhs <= rhs)),
        HirBinOp::GtEq => Some(u64::from(lhs >= rhs)),
        _ => None,
    }
}

/// The integer type a `as` cast targets, when it is a primitive integer.
fn const_target_int_ty(target: &HirTypeRef) -> Option<IntTy> {
    let HirTypeRef::Named(path) = target else {
        return None;
    };
    let name = path.segments.last()?.0.as_str();
    IntTy::parse(name)
}

/// Applies `as`-cast wrapping truncation to a non-negative const value.
fn truncate_const_value(int_ty: IntTy, value: u64) -> u64 {
    let bits: u32 = match int_ty {
        IntTy::I8 | IntTy::U8 => 8,
        IntTy::I16 | IntTy::U16 => 16,
        IntTy::I32 | IntTy::U32 => 32,
        IntTy::I64 | IntTy::U64 | IntTy::Isize | IntTy::Usize => 64,
    };
    if bits == 64 {
        value
    } else {
        value & ((1u64 << bits) - 1)
    }
}

impl TypeChecker<'_> {
    /// True when `value` (the non-negative magnitude the const evaluator
    /// produces, with negatives reported as `None`) is representable in the
    /// declared constant type.
    fn const_value_fits_type(ty: &Type, value: u64) -> bool {
        match ty {
            Type::Int(int_ty) => Self::const_value_fits_int(*int_ty, value),
            Type::Bool => value <= 1,
            _ => true,
        }
    }

    fn const_value_fits_int(int_ty: IntTy, value: u64) -> bool {
        let max: u128 = match int_ty {
            IntTy::I8 => i8::MAX as u128,
            IntTy::I16 => i16::MAX as u128,
            IntTy::I32 => i32::MAX as u128,
            IntTy::I64 => i64::MAX as u128,
            IntTy::Isize => isize::MAX as u128,
            IntTy::U8 => u8::MAX as u128,
            IntTy::U16 => u16::MAX as u128,
            IntTy::U32 => u32::MAX as u128,
            IntTy::U64 => u64::MAX as u128,
            IntTy::Usize => usize::MAX as u128,
        };
        (value as u128) <= max
    }
}

fn first_non_const_expr(
    body: &Body,
    expr_id: ExprId,
    dependencies: &mut Vec<ConstId>,
) -> Option<TextRange> {
    let span = body.source_map.expr_ranges[&expr_id];
    match &body.exprs[expr_id] {
        Expr::IntLiteral { .. }
        | Expr::FloatLiteral { .. }
        | Expr::StringLiteral { .. }
        | Expr::CharLiteral { .. }
        | Expr::BoolLiteral { .. } => None,
        Expr::Path { resolved, .. } => match resolved {
            Some(ResolvedName::Const(id)) => {
                dependencies.push(*id);
                None
            }
            Some(ResolvedName::EnumVariant(..) | ResolvedName::Unresolved) => None,
            _ => Some(span),
        },
        Expr::Binary { lhs, rhs, op } if !op.is_assignment() => {
            first_non_const_expr(body, *lhs, dependencies)
                .or_else(|| first_non_const_expr(body, *rhs, dependencies))
        }
        Expr::Block { stmts, tail } if stmts.is_empty() => {
            tail.and_then(|tail| first_non_const_expr(body, tail, dependencies))
        }
        Expr::Array { elements } | Expr::Tuple { elements } => elements
            .iter()
            .find_map(|expr| first_non_const_expr(body, *expr, dependencies)),
        Expr::ArrayRepeat { value, len } => first_non_const_expr(body, *value, dependencies)
            .or_else(|| first_non_const_expr(body, *len, dependencies)),
        Expr::Struct { fields, .. } => fields
            .iter()
            .find_map(|field| first_non_const_expr(body, field.value, dependencies)),
        Expr::Unary {
            operand,
            op: UnaryOp::Neg | UnaryOp::Pos | UnaryOp::Not,
        }
        | Expr::FieldAccess { base: operand, .. }
        | Expr::Cast { base: operand, .. }
        | Expr::Try { operand } => first_non_const_expr(body, *operand, dependencies),
        Expr::IndexAccess { base, index } => first_non_const_expr(body, *base, dependencies)
            .or_else(|| first_non_const_expr(body, *index, dependencies)),
        Expr::Missing
        | Expr::Binary { .. }
        | Expr::Unary { .. }
        | Expr::Call { .. }
        | Expr::Lambda { .. }
        | Expr::If { .. }
        | Expr::While { .. }
        | Expr::Loop { .. }
        | Expr::For { .. }
        | Expr::Match { .. }
        | Expr::Unsafe { .. }
        | Expr::Block { .. } => Some(span),
    }
}

fn const_cycle_from(
    const_id: ConstId,
    dependencies: &HashMap<ConstId, Vec<ConstId>>,
    states: &mut HashMap<ConstId, u8>,
) -> bool {
    match states.get(&const_id) {
        Some(1) => return true,
        Some(2) => return false,
        _ => {}
    }
    states.insert(const_id, 1);
    if dependencies
        .get(&const_id)
        .into_iter()
        .flatten()
        .copied()
        .any(|dependency| const_cycle_from(dependency, dependencies, states))
    {
        return true;
    }
    states.insert(const_id, 2);
    false
}

// ── Lang item signature validation ──────────────────────────────────────────
//
// Called from `build_trait_env` before a `#[lang = "..."]` trait is
// registered.  Returns `None` when the trait satisfies the contract, or
// `Some(reason)` when it does not.

pub fn validate_lang_item_signature(
    item: crate::lang_items::LangItem,
    tr: &HirTrait,
) -> Option<String> {
    use crate::lang_items::LangItem;
    let self_ty = HirTypeRef::Named(HirPath {
        anchor: PathAnchor::Plain,
        segments: vec![hir::Name("Self".into())],
        segment_type_args: Vec::new(),
        type_args: Vec::new(),
        range: tr.name_range,
    });

    match item {
        LangItem::Drop => validate_drop_signature(tr, &self_ty),

        // Marker traits — no method/assoc-type contract required.
        LangItem::Copy | LangItem::Eq => {
            (!tr.generics.is_empty()).then(|| "marker trait must not be generic".into())
        }

        // Clone — must have `clone(&self) -> Self`.
        LangItem::Clone => validate_clone_signature(tr, &self_ty),

        // Comparison traits.
        LangItem::PartialEq => {
            if !lang_has_valid_rhs_generic(tr, &self_ty) {
                return Some(
                    "comparison trait must have at most one generic type \
                     parameter `Rhs = Self`"
                        .into(),
                );
            }
            validate_comparison_method_sig(tr, "eq", &self_ty)
        }

        LangItem::PartialOrd => {
            if !lang_has_valid_rhs_generic(tr, &self_ty) {
                return Some(
                    "comparison trait must have at most one generic type \
                     parameter `Rhs = Self`"
                        .into(),
                );
            }
            let mut found = false;
            for name in ["lt", "gt", "le", "ge"] {
                if tr.methods.iter().any(|method| method.name.0 == name) {
                    found = true;
                    if let Some(reason) = validate_comparison_method_sig(tr, name, &self_ty) {
                        return Some(reason);
                    }
                }
            }
            if tr
                .methods
                .iter()
                .any(|method| method.name.0 == "partial_cmp")
            {
                found = true;
                if let Some(reason) = validate_partial_cmp(tr, &self_ty) {
                    return Some(reason);
                }
            }
            (!found).then(|| {
                "must define at least one ordering method \
                 (`lt`, `gt`, `le`, `ge`, or `partial_cmp`)"
                    .into()
            })
        }

        LangItem::Ord => validate_ord_cmp(tr, &self_ty),

        // Binary operators — (self, rhs) → Self::Output.
        LangItem::Add
        | LangItem::Sub
        | LangItem::Mul
        | LangItem::Div
        | LangItem::Rem
        | LangItem::BitAnd
        | LangItem::BitOr
        | LangItem::BitXor
        | LangItem::Shl
        | LangItem::Shr => validate_binary_operator_trait(tr, item.as_str(), &self_ty),

        // Unary operators — (self) → Self::Output.
        LangItem::Neg | LangItem::Not => validate_unary_operator_trait(tr, item.as_str(), &self_ty),

        // Assign operators — (&mut self, rhs) → ().
        LangItem::AddAssign
        | LangItem::SubAssign
        | LangItem::MulAssign
        | LangItem::DivAssign
        | LangItem::RemAssign
        | LangItem::BitAndAssign
        | LangItem::BitOrAssign
        | LangItem::BitXorAssign
        | LangItem::ShlAssign
        | LangItem::ShrAssign => validate_assign_operator_trait(tr, item.as_str(), &self_ty),

        LangItem::Index => validate_index_trait(tr, "index", false, &self_ty),
        LangItem::IndexMut => validate_index_trait(tr, "index_mut", true, &self_ty),

        // Formatting — no method/assoc-type contract beyond what `fmt` uses.
        LangItem::Debug => None,

        // Enum-carried lang items never validate as traits.
        LangItem::Option | LangItem::Result => {
            Some("this lang item is carried by an enum, not a trait".into())
        }
    }
}

fn validate_drop_signature(tr: &HirTrait, self_ty: &HirTypeRef) -> Option<String> {
    if !tr.generics.is_empty() {
        return Some("`Drop` trait must not be generic".into());
    }
    if !tr.supertraits.is_empty() || tr.methods.len() != 1 || !tr.type_aliases.is_empty() {
        return Some("`Drop` must contain only `drop(&mut self)`".into());
    }
    let method = &tr.methods[0];
    if method.name.0 != "drop"
        || method.has_body
        || !method.generics.is_empty()
        || !method.const_generics.is_empty()
        || method.params.len() != 1
        || !matches!(&method.params[0].ty,
            HirTypeRef::Ref(inner, true) if lang_type_is_self(inner, self_ty))
        || method.ret_type.is_some()
    {
        return Some("method must have signature `fun drop(&mut self)`".into());
    }
    None
}

fn validate_clone_signature(tr: &HirTrait, self_ty: &HirTypeRef) -> Option<String> {
    if !tr.generics.is_empty() {
        return Some("`Clone` trait must not be generic".into());
    }
    let Some(method) = tr.methods.iter().find(|method| method.name.0 == "clone") else {
        return Some("must define a method named `clone`".into());
    };
    if !method.generics.is_empty() || !method.const_generics.is_empty() {
        return Some("`clone` must not carry its own generic parameters".into());
    }
    if method.params.len() != 1 {
        return Some("`clone` must take exactly one parameter (`&self`)".into());
    }
    if !matches!(&method.params[0].ty,
        HirTypeRef::Ref(inner, false) if lang_type_is_self(inner, self_ty))
    {
        return Some("first parameter of `clone` must be `&self`".into());
    }
    if !method
        .ret_type
        .as_ref()
        .is_some_and(|ty| lang_type_is_self(ty, self_ty))
    {
        return Some("`clone` must return `Self`".into());
    }
    None
}

fn lang_type_is_self(ty: &HirTypeRef, self_ty: &HirTypeRef) -> bool {
    ty == self_ty
        || matches!(
            ty,
            HirTypeRef::Named(path)
                if path.as_single_name().is_some_and(|name| name.0 == "Self")
        )
}

fn lang_is_self_output(ty: &HirTypeRef) -> bool {
    let HirTypeRef::Named(path) = ty else {
        return false;
    };
    matches!(path.anchor, PathAnchor::Plain)
        && path.segments.len() == 2
        && path.segments[0].0 == "Self"
        && path.segments[1].0 == "Output"
        && path.type_args.is_empty()
}

fn lang_has_output_assoc_type(tr: &HirTrait) -> bool {
    tr.type_aliases
        .iter()
        .any(|alias: &HirTypeAlias| alias.name.0 == "Output" && alias.ty.is_none())
}

fn lang_returns_unit(function: &HirFunction) -> bool {
    function
        .ret_type
        .as_ref()
        .is_none_or(|ty| matches!(ty, HirTypeRef::Tuple(elements) if elements.is_empty()))
}

/// Returns None if the trait's RHS generic setup is valid for an operator
/// lang item (0 or 1 type param, if 1 it must default to `Self`).
fn lang_has_valid_rhs_generic(tr: &HirTrait, self_ty: &HirTypeRef) -> bool {
    tr.generics.len() <= 1
        && tr.generics.first().is_none_or(|_| {
            tr.generic_defaults
                .first()
                .and_then(Option::as_ref)
                .is_some_and(|default| lang_type_is_self(default, self_ty))
        })
}

fn lang_type_is_rhs(tr: &HirTrait, ty: &HirTypeRef) -> bool {
    tr.generics.first().is_none_or(|rhs| {
        matches!(
            ty,
            HirTypeRef::Named(path)
                if path.as_single_name().is_some_and(|name| name == rhs)
        )
    })
}

fn lang_ref_is_rhs(tr: &HirTrait, ty: &HirTypeRef) -> bool {
    matches!(ty, HirTypeRef::Ref(inner, false) if lang_type_is_rhs(tr, inner))
}

fn validate_binary_operator_trait(
    tr: &HirTrait,
    method_name: &str,
    self_ty: &HirTypeRef,
) -> Option<String> {
    let Some(method) = tr.methods.iter().find(|m| m.name.0 == method_name) else {
        return Some(format!("must define a method named `{method_name}`"));
    };
    if !method.generics.is_empty() || !method.const_generics.is_empty() {
        return Some(format!(
            "`{method_name}` must not carry its own generic parameters"
        ));
    }
    if !lang_has_valid_rhs_generic(tr, self_ty) {
        return Some(format!(
            "`#[lang = \"{method_name}\"]` trait must have at most one \
             generic type parameter `Rhs = Self`"
        ));
    }
    if !lang_has_output_assoc_type(tr) {
        return Some(format!(
            "`#[lang = \"{method_name}\"]` trait must declare an \
             associated type `Output` (without a default)"
        ));
    }
    if method.params.len() != 2 {
        return Some(format!(
            "`{method_name}` must take exactly 2 parameters (`self` and `rhs`)"
        ));
    }
    if !lang_type_is_self(&method.params[0].ty, self_ty) {
        return Some(format!("first parameter of `{method_name}` must be `self`"));
    }
    if !lang_type_is_rhs(tr, &method.params[1].ty) {
        return Some(format!(
            "second parameter of `{method_name}` must match `Rhs`"
        ));
    }
    if !method.ret_type.as_ref().is_some_and(lang_is_self_output) {
        return Some(format!("`{method_name}` must return `Self::Output`"));
    }
    None
}

fn validate_unary_operator_trait(
    tr: &HirTrait,
    method_name: &str,
    self_ty: &HirTypeRef,
) -> Option<String> {
    if !tr.generics.is_empty() {
        return Some(format!(
            "`#[lang = \"{method_name}\"]` trait must not be generic"
        ));
    }
    let Some(method) = tr.methods.iter().find(|m| m.name.0 == method_name) else {
        return Some(format!("must define a method named `{method_name}`"));
    };
    if !method.generics.is_empty() || !method.const_generics.is_empty() {
        return Some(format!(
            "`{method_name}` must not carry its own generic parameters"
        ));
    }
    if !lang_has_output_assoc_type(tr) {
        return Some(format!(
            "`#[lang = \"{method_name}\"]` trait must declare an associated type `Output`"
        ));
    }
    if method.params.len() != 1 {
        return Some(format!(
            "`{method_name}` must take exactly 1 parameter (`self`)"
        ));
    }
    if !lang_type_is_self(&method.params[0].ty, self_ty) {
        return Some(format!("first parameter of `{method_name}` must be `self`"));
    }
    if !method.ret_type.as_ref().is_some_and(lang_is_self_output) {
        return Some(format!("`{method_name}` must return `Self::Output`"));
    }
    None
}

fn validate_assign_operator_trait(
    tr: &HirTrait,
    method_name: &str,
    self_ty: &HirTypeRef,
) -> Option<String> {
    let Some(method) = tr.methods.iter().find(|m| m.name.0 == method_name) else {
        return Some(format!("must define a method named `{method_name}`"));
    };
    if !method.generics.is_empty() || !method.const_generics.is_empty() {
        return Some(format!(
            "`{method_name}` must not carry its own generic parameters"
        ));
    }
    if !lang_has_valid_rhs_generic(tr, self_ty) {
        return Some(format!(
            "`#[lang = \"{method_name}\"]` trait must have at most one \
             generic type parameter `Rhs = Self`"
        ));
    }
    if method.params.len() != 2 {
        return Some(format!(
            "`{method_name}` must take exactly 2 parameters (`&mut self` and `rhs`)"
        ));
    }
    if !matches!(&method.params[0].ty,
        HirTypeRef::Ref(inner, true) if lang_type_is_self(inner, self_ty))
    {
        return Some(format!(
            "first parameter of `{method_name}` must be `&mut self`"
        ));
    }
    if !lang_type_is_rhs(tr, &method.params[1].ty) {
        return Some(format!(
            "second parameter of `{method_name}` must match `Rhs`"
        ));
    }
    if !lang_returns_unit(method) {
        return Some(format!("`{method_name}` must return `()` (unit)"));
    }
    None
}

fn validate_index_trait(
    tr: &HirTrait,
    method_name: &str,
    mutable: bool,
    self_ty: &HirTypeRef,
) -> Option<String> {
    if tr.generics.len() != 1 {
        return Some("index trait must have exactly one index type parameter".into());
    }
    let Some(method) = tr
        .methods
        .iter()
        .find(|method| method.name.0 == method_name)
    else {
        return Some(format!("must define a method named `{method_name}`"));
    };
    if !method.generics.is_empty() || !method.const_generics.is_empty() {
        return Some(format!(
            "`{method_name}` must not carry its own generic parameters"
        ));
    }
    if !mutable && !lang_has_output_assoc_type(tr) {
        return Some("`Index` must declare an associated type `Output`".into());
    }
    if mutable
        && !tr.supertraits.iter().any(|bound| {
            matches!(&bound.trait_ty,
                HirTypeRef::Named(path)
                    if path.segments.last().is_some_and(|name| name.0 == "Index")
                        && path.type_args.len() == 1
                        && lang_type_is_rhs(tr, &path.type_args[0]))
        })
    {
        return Some("`IndexMut<Idx>` must extend `Index<Idx>`".into());
    }
    if method.params.len() != 2 {
        return Some(format!("`{method_name}` must take exactly 2 parameters"));
    }
    if !matches!(&method.params[0].ty,
        HirTypeRef::Ref(inner, is_mut) if *is_mut == mutable && lang_type_is_self(inner, self_ty))
    {
        return Some(format!(
            "first parameter of `{method_name}` must be `{}`",
            if mutable { "&mut self" } else { "&self" }
        ));
    }
    if !lang_type_is_rhs(tr, &method.params[1].ty) {
        return Some(format!(
            "second parameter of `{method_name}` must match the index type"
        ));
    }
    if !matches!(method.ret_type.as_ref(),
        Some(HirTypeRef::Ref(inner, is_mut)) if *is_mut == mutable && lang_is_self_output(inner))
    {
        return Some(format!(
            "`{method_name}` must return `{}`",
            if mutable {
                "&mut Self::Output"
            } else {
                "&Self::Output"
            }
        ));
    }
    None
}

fn validate_comparison_method_sig(
    tr: &HirTrait,
    method_name: &str,
    self_ty: &HirTypeRef,
) -> Option<String> {
    let Some(method) = tr.methods.iter().find(|m| m.name.0 == method_name) else {
        return Some(format!("must define a method named `{method_name}`"));
    };
    if !method.generics.is_empty() || !method.const_generics.is_empty() {
        return Some(format!(
            "`{method_name}` must not carry its own generic parameters"
        ));
    }
    if method.params.len() != 2 {
        return Some(format!(
            "`{method_name}` must take exactly 2 parameters (`&self` and `&Rhs`)"
        ));
    }
    if !matches!(&method.params[0].ty,
        HirTypeRef::Ref(inner, false) if lang_type_is_self(inner, self_ty))
    {
        return Some(format!(
            "first parameter of `{method_name}` must be `&self`"
        ));
    }
    if !lang_ref_is_rhs(tr, &method.params[1].ty) {
        return Some(format!(
            "second parameter of `{method_name}` must be `&Rhs`"
        ));
    }
    let is_bool = |ty: &HirTypeRef| {
        matches!(ty, HirTypeRef::Named(path)
            if path.as_single_name().is_some_and(|n| n.0 == "bool")
                && path.type_args.is_empty())
    };
    if !method.ret_type.as_ref().is_some_and(is_bool) {
        return Some(format!("`{method_name}` must return `bool`"));
    }
    None
}

fn validate_partial_cmp(tr: &HirTrait, self_ty: &HirTypeRef) -> Option<String> {
    let method = tr
        .methods
        .iter()
        .find(|method| method.name.0 == "partial_cmp")?;
    if !method.generics.is_empty() || !method.const_generics.is_empty() {
        return Some("`partial_cmp` must not carry its own generic parameters".into());
    }
    if method.params.len() != 2 {
        return Some("`partial_cmp` must take exactly 2 parameters (`&self` and `&Rhs`)".into());
    }
    if !matches!(&method.params[0].ty,
        HirTypeRef::Ref(inner, false) if lang_type_is_self(inner, self_ty))
    {
        return Some("first parameter of `partial_cmp` must be `&self`".into());
    }
    if !lang_ref_is_rhs(tr, &method.params[1].ty) {
        return Some("second parameter of `partial_cmp` must be `&Rhs`".into());
    }
    if !method
        .ret_type
        .as_ref()
        .is_some_and(lang_is_option_ordering)
    {
        return Some("`partial_cmp` must return `Option<Ordering>`".into());
    }
    None
}

fn lang_is_ordering(ty: &HirTypeRef) -> bool {
    matches!(
        ty,
        HirTypeRef::Named(path)
            if path.segments.last().is_some_and(|name| name.0 == "Ordering")
                && path.type_args.is_empty()
    )
}

fn lang_is_option_ordering(ty: &HirTypeRef) -> bool {
    matches!(
        ty,
        HirTypeRef::Named(path)
            if path.segments.last().is_some_and(|name| name.0 == "Option")
                && path.type_args.len() == 1
                && lang_is_ordering(&path.type_args[0])
    )
}

fn validate_ord_cmp(tr: &HirTrait, self_ty: &HirTypeRef) -> Option<String> {
    if !tr.generics.is_empty() {
        return Some("`Ord` trait must not be generic".into());
    }
    let Some(method) = tr.methods.iter().find(|m| m.name.0 == "cmp") else {
        return Some("must define a method named `cmp`".into());
    };
    if !method.generics.is_empty() || !method.const_generics.is_empty() {
        return Some("`cmp` must not carry its own generic parameters".into());
    }
    if method.params.len() != 2 {
        return Some("`cmp` must take exactly 2 parameters (`&self` and `&Self`)".into());
    }
    if !matches!(&method.params[0].ty,
        HirTypeRef::Ref(inner, false) if lang_type_is_self(inner, self_ty))
    {
        return Some("first parameter of `cmp` must be `&self`".into());
    }
    if !matches!(&method.params[1].ty,
        HirTypeRef::Ref(inner, false) if lang_type_is_self(inner, self_ty))
    {
        return Some("second parameter of `cmp` must be `&Self`".into());
    }
    if !method.ret_type.as_ref().is_some_and(lang_is_ordering) {
        return Some("`cmp` must return `Ordering`".into());
    }
    None
}

fn type_contains_slice(ty: &Type) -> bool {
    match ty {
        Type::Slice(_) => true,
        Type::Ref(inner, _) | Type::Ptr { inner, .. } | Type::Array(inner, _) => {
            type_contains_slice(inner)
        }
        Type::Tuple(elements)
        | Type::Struct(_, elements)
        | Type::Enum(_, elements)
        | Type::OpaqueTrait { args: elements, .. } => elements.iter().any(type_contains_slice),
        Type::CallableConstraint(signature)
        | Type::Closure { signature, .. }
        | Type::OpaqueCallable { signature, .. } => {
            signature.params.iter().any(type_contains_slice) || type_contains_slice(&signature.ret)
        }
        Type::FunctionItem { args, .. } => args.iter().any(type_contains_slice),
        _ => false,
    }
}

fn type_contains_dyn_trait(ty: &Type) -> bool {
    match ty {
        Type::DynTrait { .. } | Type::OwnedDynTrait { .. } => true,
        Type::Ref(inner, _) | Type::Ptr { inner, .. } | Type::Array(inner, _) => {
            type_contains_dyn_trait(inner)
        }
        Type::Tuple(elements)
        | Type::Struct(_, elements)
        | Type::Enum(_, elements)
        | Type::OpaqueTrait { args: elements, .. } => elements.iter().any(type_contains_dyn_trait),
        Type::CallableConstraint(signature)
        | Type::Closure { signature, .. }
        | Type::OpaqueCallable { signature, .. } => {
            signature.params.iter().any(type_contains_dyn_trait)
                || type_contains_dyn_trait(&signature.ret)
        }
        Type::FunctionItem { args, .. } => args.iter().any(type_contains_dyn_trait),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use super::type_contains_infer_var;
    use crate::Type;

    #[test]
    fn detects_an_inference_variable_nested_in_a_type() {
        let ty = Type::Tuple(vec![Type::Int(crate::IntTy::I32), Type::InferVar(7)]);
        assert!(type_contains_infer_var(
            7,
            &ty,
            &HashMap::new(),
            &mut HashSet::new()
        ));
    }
}
