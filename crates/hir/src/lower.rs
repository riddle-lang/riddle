use la_arena::Arena;

use ast::{
    self, ExternFnDecl, FuncDecl, Param, StructDecl, StructField, StructFieldList, Type,
    support::{AstNode, trimmed_range},
};
use syntax::{SyntaxKind, SyntaxNode, SyntaxToken};

use super::{
    Name,
    item_tree::{
        ConstId, EnumId, FunctionId, HirAssocTypeConstraint, HirAttr, HirCallableSignature,
        HirConst, HirConstArg, HirEnum, HirEnumVariant, HirFunction, HirGenericBound,
        HirInternalAttr, HirParam, HirPath, HirStruct, HirStructField, HirTrait, HirTypeAlias,
        HirTypeRef, HirUseTree, HirUseTreeKind, HirVariantKind, InternalAttrTarget, PathAnchor,
        StructId, TraitId, TypeAliasId, Visibility,
    },
};

pub trait AstLower {
    type Id;
    type Item;
    fn lower(self, arena: &mut Arena<Self::Item>) -> Self::Id;
}

pub trait Lower {
    type Output;
    fn lower(self) -> Self::Output;
}

#[must_use]
pub fn lower_name(name: Option<SyntaxToken>) -> Name {
    name.map_or_else(|| Name("<missing>".into()), |t| Name(t.text().to_string()))
}

#[must_use]
pub fn lower_generic_params(params: Option<ast::GenericParams>) -> Vec<Name> {
    params
        .map(|g| {
            g.params()
                .filter(|param| !param.is_const)
                .map(|param| Name(param.name))
                .collect::<Vec<_>>()
        })
        .unwrap_or_default()
}

#[must_use]
pub fn lower_const_generic_params(params: Option<ast::GenericParams>) -> Vec<Name> {
    params
        .map(|g| {
            g.params()
                .filter(|param| param.is_const)
                .map(|param| Name(param.name))
                .collect::<Vec<_>>()
        })
        .unwrap_or_default()
}

#[must_use]
pub fn lower_generic_defaults(params: Option<ast::GenericParams>) -> Vec<Option<HirTypeRef>> {
    params
        .map(|g| {
            g.params()
                .filter(|param| !param.is_const)
                .map(|param| param.default.map(Lower::lower))
                .collect()
        })
        .unwrap_or_default()
}

#[must_use]
pub fn lower_generic_bounds(
    params: Option<ast::GenericParams>,
    where_clause: Option<ast::WhereClause>,
) -> Vec<HirGenericBound> {
    let mut bounds: Vec<HirGenericBound> = params
        .map(|g| {
            g.params()
                .flat_map(|param| {
                    if param.is_const {
                        return Vec::new().into_iter();
                    }
                    let name = Name(param.name);
                    param
                        .bounds
                        .into_iter()
                        .map(move |bound| {
                            let callable = bound.callable.as_ref().map(lower_callable_signature);
                            let trait_range = trimmed_range(bound.trait_path.syntax());
                            let mut trait_path = bound.trait_path.lower();
                            trait_path.type_args =
                                bound.type_args.into_iter().map(Lower::lower).collect();
                            let assoc_constraints = bound
                                .assoc_constraints
                                .into_iter()
                                .map(|constraint| {
                                    let range = trimmed_range(constraint.ty.syntax());
                                    HirAssocTypeConstraint {
                                        name: Name(constraint.name),
                                        ty: constraint.ty.lower(),
                                        range,
                                    }
                                })
                                .collect();
                            HirGenericBound {
                                param: name.clone(),
                                target_ty: HirTypeRef::Named(HirPath {
                                    anchor: PathAnchor::Plain,
                                    segments: vec![name.clone()],
                                    segment_type_args: Vec::new(),
                                    type_args: Vec::new(),
                                    range: trait_range,
                                }),
                                target_range: trait_range,
                                trait_ty: HirTypeRef::Named(trait_path),
                                trait_range,
                                callable,
                                assoc_constraints,
                            }
                        })
                        .collect::<Vec<_>>()
                        .into_iter()
                })
                .collect()
        })
        .unwrap_or_default();

    if let Some(where_clause) = where_clause {
        bounds.extend(where_clause.predicates().flat_map(|predicate| {
            let target_range = trimmed_range(predicate.target_ty.syntax());
            let target_ty = predicate.target_ty.lower();
            let param = generic_bound_param_name(&target_ty);
            predicate.bounds.into_iter().map(move |bound| {
                let callable = bound.callable.as_ref().map(lower_callable_signature);
                let trait_range = trimmed_range(bound.trait_path.syntax());
                let mut trait_path = bound.trait_path.lower();
                trait_path.type_args = bound.type_args.into_iter().map(Lower::lower).collect();
                let assoc_constraints = bound
                    .assoc_constraints
                    .into_iter()
                    .map(|constraint| {
                        let range = trimmed_range(constraint.ty.syntax());
                        HirAssocTypeConstraint {
                            name: Name(constraint.name),
                            ty: constraint.ty.lower(),
                            range,
                        }
                    })
                    .collect();
                HirGenericBound {
                    param: param.clone(),
                    target_ty: target_ty.clone(),
                    target_range,
                    trait_ty: HirTypeRef::Named(trait_path),
                    trait_range,
                    callable,
                    assoc_constraints,
                }
            })
        }));
    }

    bounds
}

pub(crate) fn lower_callable_signature(callable: &ast::CallableTraitArgs) -> HirCallableSignature {
    HirCallableSignature {
        params: callable.params().map(Lower::lower).collect(),
        ret: Box::new(
            callable
                .return_type()
                .map_or(HirTypeRef::Error, Lower::lower),
        ),
    }
}

fn assign_implicit_generics(
    params: &mut [HirParam],
    generic_bounds: &mut Vec<HirGenericBound>,
) -> Vec<Name> {
    let mut names = Vec::new();
    for (index, param) in params.iter_mut().enumerate() {
        let HirTypeRef::ImplTrait {
            trait_ty,
            trait_range,
            callable,
            hidden,
        } = &mut param.ty
        else {
            continue;
        };
        let name = Name(format!("#impl{index}"));
        *hidden = Some(name.clone());
        generic_bounds.push(HirGenericBound {
            param: name.clone(),
            target_ty: HirTypeRef::Named(HirPath {
                anchor: PathAnchor::Plain,
                segments: vec![name.clone()],
                segment_type_args: Vec::new(),
                type_args: Vec::new(),
                range: *trait_range,
            }),
            target_range: param.ty_range,
            trait_ty: *trait_ty.clone(),
            trait_range: *trait_range,
            callable: callable.clone(),
            assoc_constraints: Vec::new(),
        });
        names.push(name);
    }
    names
}

fn generic_bound_param_name(ty: &HirTypeRef) -> Name {
    match ty {
        HirTypeRef::Named(path)
            if matches!(path.anchor, PathAnchor::Plain)
                && path.segments.len() == 1
                && path.type_args.is_empty() =>
        {
            path.segments[0].clone()
        }
        _ => Name("<where>".into()),
    }
}

#[must_use]
pub fn lower_attrs(node: &SyntaxNode) -> Vec<HirAttr> {
    ast::attrs_for_node(node)
        .into_iter()
        .map(|attr| lower_attr(&attr))
        .collect()
}

pub fn lower_internal_attrs(node: &SyntaxNode) -> Vec<HirInternalAttr> {
    node.descendants()
        .filter_map(ast::Attribute::cast)
        .filter_map(|attr| {
            let lowered = lower_attr(&attr);
            matches!(lowered.name.0.as_str(), "lang" | "fundamental").then(|| {
                let mut target = attr.syntax().next_sibling();
                while target
                    .as_ref()
                    .is_some_and(|node| node.kind() == SyntaxKind::Attribute)
                {
                    target = target.and_then(|node| node.next_sibling());
                }
                let target = match target.as_ref().map(rowan::SyntaxNode::kind) {
                    Some(SyntaxKind::TraitDecl) => InternalAttrTarget::Trait,
                    Some(SyntaxKind::StructDecl) => InternalAttrTarget::Struct,
                    Some(SyntaxKind::EnumDecl) => InternalAttrTarget::Enum,
                    _ => InternalAttrTarget::Other,
                };
                HirInternalAttr {
                    attr: lowered,
                    target,
                }
            })
        })
        .collect()
}

fn lower_attr(attr: &ast::Attribute) -> HirAttr {
    HirAttr {
        name: lower_name(attr.name()),
        value: attr.string_value(),
        raw: attr.raw_text(),
        range: attr.syntax().text_range(),
    }
}

#[must_use]
pub const fn lower_visibility(is_pub: bool) -> Visibility {
    if is_pub {
        Visibility::Public
    } else {
        Visibility::Private
    }
}

impl Lower for Param {
    type Output = HirParam;
    fn lower(self) -> Self::Output {
        let range = trimmed_range(self.syntax());
        let is_mut = self.is_mut() && !self.is_self_receiver();
        let name_token = self.name();
        let name_range = name_token
            .as_ref()
            .map_or(range, rowan::SyntaxToken::text_range);
        let name = lower_name(name_token);
        let ty_ast = self.ty();
        let ty_range = ty_ast
            .as_ref()
            .map_or(range, |ty| trimmed_range(ty.syntax()));
        let ty = ty_ast.lower();
        let attrs = lower_attrs(self.syntax());
        HirParam {
            name,
            name_range,
            is_receiver: false,
            is_mut,
            ty,
            ty_range,
            attrs,
        }
    }
}

impl AstLower for FuncDecl {
    type Id = FunctionId;
    type Item = HirFunction;
    fn lower(self, arena: &mut Arena<Self::Item>) -> Self::Id {
        let range = trimmed_range(self.syntax());
        let name_token = self.name();
        let name_range = name_token
            .as_ref()
            .map_or(range, rowan::SyntaxToken::text_range);
        let name = lower_name(name_token);
        let generic_params = self.generic_params();
        let generics = lower_generic_params(generic_params.clone());
        let const_generics = lower_const_generic_params(generic_params.clone());
        let mut generic_bounds = lower_generic_bounds(generic_params, self.where_clause());
        let mut params: Vec<HirParam> = self
            .param_list()
            .map(|pl| pl.params().map(Lower::lower).collect())
            .unwrap_or_default();
        let implicit_generics = assign_implicit_generics(&mut params, &mut generic_bounds);
        let ret_type_ast = self.return_type();
        let ret_type_range = ret_type_ast.as_ref().map(|ty| trimmed_range(ty.syntax()));
        let ret_type = ret_type_ast.map(Lower::lower);
        let has_body = self.body().is_some();
        let attrs = lower_attrs(self.syntax());
        let visibility = lower_visibility(self.is_pub());
        let is_unsafe = self.is_unsafe();
        arena.alloc(HirFunction {
            name,
            name_range,
            extent: range,
            visibility,
            is_unsafe,
            generics,
            implicit_generics,
            const_generics,
            generic_bounds,
            params,
            ret_type,
            ret_type_range,
            has_body,
            attrs,
        })
    }
}

impl AstLower for ExternFnDecl {
    type Id = FunctionId;
    type Item = HirFunction;
    fn lower(self, arena: &mut Arena<Self::Item>) -> Self::Id {
        let explicitly_unsafe = self.is_unsafe();
        let func = self
            .func_decl()
            .expect("ExternFnDecl must contain FuncDecl");
        let id = func.lower(arena);
        arena[id].is_unsafe |= explicitly_unsafe;
        id
    }
}

impl AstLower for StructDecl {
    type Id = StructId;
    type Item = HirStruct;
    fn lower(self, arena: &mut Arena<Self::Item>) -> Self::Id {
        let name = lower_name(self.name());
        let name_range = self
            .name()
            .map_or_else(|| self.syntax().text_range(), |name| name.text_range());
        let generic_params = self.generic_params();
        let generics = lower_generic_params(generic_params.clone());
        let const_generics = lower_const_generic_params(generic_params.clone());
        let generic_bounds = lower_generic_bounds(generic_params, self.where_clause());
        let fields = self.field_list().lower();
        let attrs = lower_attrs(self.syntax());
        let visibility = lower_visibility(self.is_pub());
        arena.alloc(HirStruct {
            name,
            visibility,
            name_range,
            extent: trimmed_range(self.syntax()),
            generics,
            const_generics,
            generic_bounds,
            fields,
            attrs,
        })
    }
}

impl AstLower for ast::EnumDecl {
    type Id = EnumId;
    type Item = HirEnum;
    fn lower(self, arena: &mut Arena<Self::Item>) -> Self::Id {
        let range = trimmed_range(self.syntax());
        let name_token = self.name();
        let name_range = name_token
            .as_ref()
            .map_or(range, rowan::SyntaxToken::text_range);
        let name = lower_name(name_token);
        let generic_params = self.generic_params();
        let generics = lower_generic_params(generic_params.clone());
        let const_generics = lower_const_generic_params(generic_params.clone());
        let generic_bounds = lower_generic_bounds(generic_params, self.where_clause());
        let variants = self.variants().map(Lower::lower).collect();
        let attrs = lower_attrs(self.syntax());
        let visibility = lower_visibility(self.is_pub());
        arena.alloc(HirEnum {
            name,
            name_range,
            extent: range,
            visibility,
            generics,
            const_generics,
            generic_bounds,
            variants,
            attrs,
        })
    }
}

impl Lower for ast::EnumVariant {
    type Output = HirEnumVariant;
    fn lower(self) -> Self::Output {
        let range = trimmed_range(self.syntax());
        let name_token = self.name();
        let name_range = name_token
            .as_ref()
            .map_or(range, rowan::SyntaxToken::text_range);
        let name = lower_name(name_token);
        let (tuple, mut field_ranges): (Vec<HirTypeRef>, Vec<_>) = self
            .tuple_types()
            .map(|ty| {
                let range = trimmed_range(ty.syntax());
                (ty.lower(), range)
            })
            .unzip();
        let kind = match (self.field_list(), tuple.is_empty()) {
            (Some(field_list), _) => {
                let fields = Some(field_list).lower();
                field_ranges = fields.iter().map(|field| field.ty_range).collect();
                HirVariantKind::Struct(fields)
            }
            (None, false) => HirVariantKind::Tuple(tuple),
            (None, true) => HirVariantKind::Unit,
        };
        let attrs = lower_attrs(self.syntax());
        HirEnumVariant {
            name,
            name_range,
            kind,
            field_ranges,
            attrs,
        }
    }
}

impl AstLower for ast::TraitDecl {
    type Id = TraitId;
    type Item = HirTrait;
    fn lower(self, arena: &mut Arena<Self::Item>) -> Self::Id {
        let range = trimmed_range(self.syntax());
        let name_token = self.name();
        let name_range = name_token
            .as_ref()
            .map_or(range, rowan::SyntaxToken::text_range);
        let name = lower_name(name_token);
        let visibility = lower_visibility(self.is_pub());
        let generic_params = self.generic_params();
        let generics = lower_generic_params(generic_params.clone());
        let generic_defaults = lower_generic_defaults(generic_params.clone());
        let generic_bounds = lower_generic_bounds(generic_params, None);
        let supertraits = self
            .supertraits()
            .into_iter()
            .map(lower_supertrait)
            .collect();
        let methods = self
            .methods()
            .map(|method| lower_trait_method(&method))
            .collect();
        let type_aliases = self
            .type_aliases()
            .map(|alias| lower_trait_type_alias(&alias))
            .collect();
        let attrs = lower_attrs(self.syntax());
        arena.alloc(HirTrait {
            name,
            name_range,
            extent: trimmed_range(self.syntax()),
            visibility,
            generics,
            generic_defaults,
            generic_bounds,
            supertraits,
            methods,
            default_methods: Vec::new(),
            type_aliases,
            attrs,
        })
    }
}

fn lower_supertrait(bound: ast::GenericBound) -> HirGenericBound {
    let ast::GenericBound {
        trait_path,
        type_args,
        assoc_constraints,
        callable,
    } = bound;
    let callable = callable.as_ref().map(lower_callable_signature);
    let trait_range = trimmed_range(trait_path.syntax());
    let mut trait_path = trait_path.lower();
    trait_path.type_args = type_args.into_iter().map(Lower::lower).collect();
    HirGenericBound {
        param: Name("Self".into()),
        target_ty: HirTypeRef::Named(HirPath {
            anchor: PathAnchor::Plain,
            segments: vec![Name("Self".into())],
            segment_type_args: Vec::new(),
            type_args: Vec::new(),
            range: trait_range,
        }),
        target_range: trait_range,
        trait_ty: HirTypeRef::Named(trait_path),
        trait_range,
        callable,
        assoc_constraints: assoc_constraints
            .into_iter()
            .map(|constraint| {
                let range = trimmed_range(constraint.ty.syntax());
                HirAssocTypeConstraint {
                    name: Name(constraint.name),
                    ty: constraint.ty.lower(),
                    range,
                }
            })
            .collect(),
    }
}

fn lower_trait_method(method: &ast::FuncDecl) -> HirFunction {
    let method_range = trimmed_range(method.syntax());
    let method_name = method.name();
    let method_name_range = method_name
        .as_ref()
        .map_or(method_range, rowan::SyntaxToken::text_range);
    let name = lower_name(method_name);
    let mut params: Vec<HirParam> = method
        .param_list()
        .map(|list| {
            list.params()
                .map(|param| {
                    let is_self = param.is_self_receiver();
                    let is_ref = param.is_ref();
                    let is_mut = param.is_mut();
                    let mut param = param.lower();
                    param.is_receiver = is_self;
                    if is_self {
                        param.ty = self_receiver_type(is_ref, is_mut, param.ty_range);
                    }
                    param
                })
                .collect()
        })
        .unwrap_or_default();
    let mut generic_bounds = lower_generic_bounds(method.generic_params(), method.where_clause());
    let implicit_generics = assign_implicit_generics(&mut params, &mut generic_bounds);
    let ret_type_ast = method.return_type();
    let ret_type_range = ret_type_ast.as_ref().map(|ty| trimmed_range(ty.syntax()));
    let ret_type = ret_type_ast.map(Lower::lower);
    let generic_params = method.generic_params();
    HirFunction {
        name,
        name_range: method_name_range,
        extent: trimmed_range(method.syntax()),
        visibility: lower_visibility(method.is_pub()),
        is_unsafe: method.is_unsafe(),
        generics: lower_generic_params(generic_params.clone()),
        implicit_generics,
        const_generics: lower_const_generic_params(generic_params.clone()),
        generic_bounds,
        params,
        ret_type,
        ret_type_range,
        has_body: method.body().is_some(),
        attrs: lower_attrs(method.syntax()),
    }
}

fn lower_trait_type_alias(alias: &ast::TypeAliasDecl) -> HirTypeAlias {
    let range = trimmed_range(alias.syntax());
    let name_token = alias.name();
    let name_range = name_token
        .as_ref()
        .map_or(range, rowan::SyntaxToken::text_range);
    let ty_ast = alias.ty();
    let ty_range = ty_ast.as_ref().map(|ty| trimmed_range(ty.syntax()));
    HirTypeAlias {
        extent: range,
        name: lower_name(name_token),
        name_range,
        visibility: lower_visibility(alias.is_pub()),
        ty: ty_ast.map(Lower::lower),
        ty_range,
        attrs: lower_attrs(alias.syntax()),
    }
}

fn self_receiver_type(is_ref: bool, is_mut: bool, range: rowan::TextRange) -> HirTypeRef {
    let self_ty = HirTypeRef::Named(HirPath {
        anchor: PathAnchor::Plain,
        segments: vec![Name("Self".into())],
        segment_type_args: Vec::new(),
        type_args: Vec::new(),
        range,
    });
    if is_ref {
        HirTypeRef::Ref(Box::new(self_ty), is_mut)
    } else {
        self_ty
    }
}

impl AstLower for ast::ConstDecl {
    type Id = ConstId;
    type Item = HirConst;
    fn lower(self, arena: &mut Arena<Self::Item>) -> Self::Id {
        let range = trimmed_range(self.syntax());
        let name_token = self.name();
        let name_range = name_token
            .as_ref()
            .map_or(range, rowan::SyntaxToken::text_range);
        let name = lower_name(name_token);
        let ty_ast = self.ty();
        let ty_range = ty_ast
            .as_ref()
            .map_or(range, |ty| trimmed_range(ty.syntax()));
        let ty = ty_ast.lower();
        let has_value = self.value().is_some();
        let attrs = lower_attrs(self.syntax());
        let visibility = lower_visibility(self.is_pub());
        arena.alloc(HirConst {
            name,
            name_range,
            extent: range,
            visibility,
            ty,
            ty_range,
            has_value,
            attrs,
        })
    }
}

impl AstLower for ast::TypeAliasDecl {
    type Id = TypeAliasId;
    type Item = HirTypeAlias;
    fn lower(self, arena: &mut Arena<Self::Item>) -> Self::Id {
        let range = trimmed_range(self.syntax());
        let name_token = self.name();
        let name_range = name_token
            .as_ref()
            .map_or(range, rowan::SyntaxToken::text_range);
        let name = lower_name(name_token);
        let ty_ast = self.ty();
        let ty_range = ty_ast.as_ref().map(|ty| trimmed_range(ty.syntax()));
        let ty = ty_ast.map(Lower::lower);
        let attrs = lower_attrs(self.syntax());
        let visibility = lower_visibility(self.is_pub());
        arena.alloc(HirTypeAlias {
            name,
            name_range,
            extent: range,
            visibility,
            ty,
            ty_range,
            attrs,
        })
    }
}

impl Lower for Option<StructFieldList> {
    type Output = Vec<HirStructField>;
    fn lower(self) -> Self::Output {
        self.map_or_else(Vec::new, |list| list.fields().map(Lower::lower).collect())
    }
}

impl Lower for StructField {
    type Output = HirStructField;
    fn lower(self) -> Self::Output {
        let name_token = self.name();
        let name_range = name_token.as_ref().map_or_else(
            || trimmed_range(self.syntax()),
            rowan::SyntaxToken::text_range,
        );
        let name = lower_name(name_token);
        let ty = self.ty().lower();
        let ty_range = self.ty().map_or_else(
            || trimmed_range(self.syntax()),
            |ty| trimmed_range(ty.syntax()),
        );
        let attrs = lower_attrs(self.syntax());
        HirStructField {
            name,
            name_range,
            visibility: lower_visibility(self.is_pub()),
            is_mut: self.is_mut(),
            ty,
            ty_range,
            attrs,
        }
    }
}

impl Lower for Type {
    type Output = HirTypeRef;
    fn lower(self) -> Self::Output {
        match self {
            Self::Named(node) => {
                let mut path = node.path().lower();
                path.type_args = node.type_args().into_iter().map(Lower::lower).collect();
                HirTypeRef::Named(path)
            }
            Self::Never(_) => HirTypeRef::Never,
            Self::Ref(ref_ty) => ref_ty.inner().map_or(HirTypeRef::Error, |inner| {
                HirTypeRef::Ref(Box::new(inner.lower()), ref_ty.is_mut())
            }),
            Self::Ptr(ptr_ty) => {
                ptr_ty
                    .inner()
                    .map_or(HirTypeRef::Error, |inner| HirTypeRef::Ptr {
                        mutable: ptr_ty.is_mut(),
                        inner: Box::new(inner.lower()),
                    })
            }
            Self::Tuple(tuple) => HirTypeRef::Tuple(tuple.elements().map(Lower::lower).collect()),
            Self::Array(arr) => {
                let Some(inner) = arr.element() else {
                    return HirTypeRef::Error;
                };
                match arr.len_expr() {
                    Some(len_expr) => {
                        HirTypeRef::Array(Box::new(inner.lower()), lower_const_arg(len_expr))
                    }
                    None => HirTypeRef::Slice(Box::new(inner.lower())),
                }
            }
            Self::Const(value) => value.value().map_or(HirTypeRef::Error, |value| {
                HirTypeRef::Const(HirConstArg::Value(value))
            }),
            Self::ImplTrait(impl_trait) => {
                let Some(bound) = impl_trait.bound() else {
                    return HirTypeRef::Error;
                };
                let callable = bound.callable.as_ref().map(lower_callable_signature);
                let trait_range = trimmed_range(bound.trait_path.syntax());
                let mut trait_path = bound.trait_path.lower();
                trait_path.type_args = bound.type_args.into_iter().map(Lower::lower).collect();
                HirTypeRef::ImplTrait {
                    trait_ty: Box::new(HirTypeRef::Named(trait_path)),
                    trait_range,
                    callable,
                    hidden: None,
                }
            }
            Self::DynTrait(dyn_trait) => {
                let Some(bound) = dyn_trait.bound() else {
                    return HirTypeRef::Error;
                };
                let trait_range = trimmed_range(bound.trait_path.syntax());
                let mut trait_path = bound.trait_path.lower();
                trait_path.type_args = bound.type_args.into_iter().map(Lower::lower).collect();
                HirTypeRef::DynTrait {
                    trait_ty: Box::new(HirTypeRef::Named(trait_path)),
                    trait_range,
                    callable: bound.callable.as_ref().map(lower_callable_signature),
                    assoc_constraints: bound
                        .assoc_constraints
                        .into_iter()
                        .map(|constraint| {
                            let range = trimmed_range(constraint.ty.syntax());
                            HirAssocTypeConstraint {
                                name: Name(constraint.name),
                                ty: constraint.ty.lower(),
                                range,
                            }
                        })
                        .collect(),
                }
            }
        }
    }
}

fn lower_const_arg(expr: ast::Expr) -> HirConstArg {
    match expr {
        ast::Expr::Number(n) => n
            .value()
            .and_then(|value| usize::try_from(value).ok())
            .map_or(HirConstArg::Error, HirConstArg::Value),
        ast::Expr::NameRef(name_ref) => name_ref
            .path()
            .and_then(|path| path.segments().next())
            .and_then(|segment| segment.name_token())
            .map_or(HirConstArg::Error, |name| {
                HirConstArg::Param(Name(name.text().to_string()))
            }),
        _ => HirConstArg::Error,
    }
}

impl Lower for Option<Type> {
    type Output = HirTypeRef;
    fn lower(self) -> Self::Output {
        self.map_or(HirTypeRef::Error, Lower::lower)
    }
}

impl Lower for ast::Path {
    type Output = HirPath;
    fn lower(self) -> Self::Output {
        let range = trimmed_range(self.syntax());
        let absolute = self.is_absolute();
        let mut segs: Vec<(SyntaxKind, String, Vec<HirTypeRef>)> = self
            .segments()
            .filter_map(|seg| {
                let t = seg.name_token()?;
                let type_args = seg.type_args().into_iter().map(Lower::lower).collect();
                Some((t.kind(), t.text().to_string(), type_args))
            })
            .collect();

        let anchor = if absolute {
            PathAnchor::Absolute
        } else {
            match segs.first().map(|(k, text, _)| (*k, text.as_str())) {
                Some((SyntaxKind::CrateKw, _) | (_, "crate")) => {
                    segs.remove(0);
                    PathAnchor::Crate
                }
                Some((SyntaxKind::SuperKw, _) | (_, "super")) => {
                    segs.remove(0);
                    PathAnchor::Super
                }
                Some((SyntaxKind::SelfKw, _) | (_, "self")) if segs.len() > 1 => {
                    segs.remove(0);
                    PathAnchor::SelfMod
                }
                _ => PathAnchor::Plain,
            }
        };

        let mut segments = Vec::with_capacity(segs.len());
        let mut segment_type_args = Vec::new();
        for (_, text, type_args) in segs {
            let index = segments.len();
            segments.push(Name(text));
            if !type_args.is_empty() {
                segment_type_args.push((index, type_args));
            }
        }
        HirPath {
            anchor,
            segments,
            segment_type_args,
            type_args: Vec::new(),
            range,
        }
    }
}

impl Lower for Option<ast::Path> {
    type Output = HirPath;
    fn lower(self) -> Self::Output {
        self.map_or_else(
            || HirPath {
                anchor: PathAnchor::Plain,
                segments: vec![Name("<missing>".into())],
                segment_type_args: Vec::new(),
                type_args: Vec::new(),
                range: rowan::TextRange::default(),
            },
            Lower::lower,
        )
    }
}

impl Lower for ast::UseTree {
    type Output = HirUseTree;
    fn lower(self) -> Self::Output {
        let range = trimmed_range(self.syntax());
        let prefix = self.path().lower();
        let kind = if self.is_glob() {
            HirUseTreeKind::Glob
        } else if let Some(list) = self.subtree_list() {
            HirUseTreeKind::List(list.trees().map(Lower::lower).collect())
        } else {
            let alias = self.alias().map(|t| Name(t.text().to_string()));
            HirUseTreeKind::Simple { alias }
        };
        HirUseTree {
            prefix,
            kind,
            range,
        }
    }
}
