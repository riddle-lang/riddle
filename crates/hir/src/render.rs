//! Shared item-signature rendering, used by LSP hovers and `clue doc`.
//!
//! Signatures are reconstructed from the HIR item tree: visibility,
//! generics, parameters, and types print back in Riddle syntax via
//! [`HirTypeRef::display`].

use rowan::TextRange;

use crate::Name;
use crate::item_tree::{
    HirConst, HirEnum, HirEnumVariant, HirFunction, HirGenericBound, HirImpl, HirParam, HirStruct,
    HirStructField, HirTrait, HirTypeAlias, HirTypeRef, HirVariantKind, PathAnchor, Visibility,
};

/// How many fields/variants `format_struct`/`format_enum` render by
/// default; callers may pass a tighter limit for compact hovers.
const DECLARATION_ITEM_LIMIT: usize = 12;

fn visibility_prefix(visibility: &Visibility) -> &'static str {
    if visibility.is_public() { "pub " } else { "" }
}

/// Whether the bound is written directly on its generic parameter (`T: B`
/// or `where T: B`) as opposed to a compound target like `T::Assoc: B`, so
/// it renders inline in the generics list instead of the where clause.
fn is_bare_parameter_bound(bound: &HirGenericBound) -> bool {
    match &bound.target_ty {
        HirTypeRef::Named(path) => {
            matches!(path.anchor, PathAnchor::Plain)
                && path.segments.len() == 1
                && path.type_args.is_empty()
                && path.segments[0] == bound.param
        }
        _ => false,
    }
}

/// Renders generic parameters with their bounds, e.g. `<T: Ord, U>`. Only
/// bounds written directly on a declared parameter appear here; every
/// other bound renders in the where clause (see [`where_clause`]).
fn generics_with_bounds(function: &HirFunction) -> String {
    let all: Vec<String> = function
        .generics
        .iter()
        .map(|name| {
            let bound = function
                .generic_bounds
                .iter()
                .find(|bound| bound.param == *name && is_bare_parameter_bound(bound));
            match bound {
                Some(bound) => format!("{}: {}", name.0, bound.trait_ty.display()),
                None => name.0.clone(),
            }
        })
        .collect();
    if all.is_empty() {
        String::new()
    } else {
        format!("<{}>", all.join(", "))
    }
}

/// Renders `where T: Bound` for the bounds not already shown inline in the
/// generics list. Bounds on hidden (impl-trait) parameters never render:
/// the parameter itself already displays as `impl Trait`.
fn where_clause(bounds: &[HirGenericBound], generics: &[Name], hidden: &[Name]) -> String {
    let rendered = bounds
        .iter()
        .filter(|bound| {
            !hidden.contains(&bound.param)
                && !(is_bare_parameter_bound(bound) && generics.contains(&bound.param))
        })
        .map(|bound| format!("{}: {}", bound.param.0, bound.trait_ty.display()))
        .collect::<Vec<_>>()
        .join(", ");
    if rendered.is_empty() {
        String::new()
    } else {
        format!(" where {rendered}")
    }
}

#[must_use]
pub fn format_function(function: &HirFunction) -> String {
    let visibility = visibility_prefix(&function.visibility);
    let safety = if function.is_unsafe { "unsafe " } else { "" };
    let generics = generics_with_bounds(function);
    let params = function
        .params
        .iter()
        .map(format_param)
        .collect::<Vec<_>>()
        .join(", ");
    let ret = function
        .ret_type
        .as_ref()
        .map_or_else(String::new, |ty| format!(" -> {}", ty.display()));
    let wheres = where_clause(
        &function.generic_bounds,
        &function.generics,
        &function.implicit_generics,
    );
    format!(
        "{visibility}{safety}fun {}{generics}({params}){ret}{wheres}",
        function.name.0
    )
}

fn format_param(parameter: &HirParam) -> String {
    if parameter.name.0 == "self" {
        return match &parameter.ty {
            HirTypeRef::Ref(_, true) => "&mut self".into(),
            HirTypeRef::Ref(_, false) => "&self".into(),
            _ => "self".into(),
        };
    }
    format!("{}: {}", parameter.name.0, parameter.ty.display())
}

#[must_use]
pub fn format_struct(strukt: &HirStruct) -> String {
    format_struct_limited(strukt, DECLARATION_ITEM_LIMIT)
}

/// Like [`format_struct`] but truncating the field list at `limit`.
#[must_use]
pub fn format_struct_limited(strukt: &HirStruct, limit: usize) -> String {
    let visibility = visibility_prefix(&strukt.visibility);
    let mut detail = format!(
        "{visibility}{}",
        format_nominal("struct", &strukt.name, &strukt.generics)
    );
    detail.push_str(&where_clause(&strukt.generic_bounds, &[], &[]));
    if strukt.fields.is_empty() {
        detail.push_str(" {}");
        return detail;
    }

    detail.push_str(" {\n");
    for field in strukt.fields.iter().take(limit) {
        detail.push_str("    ");
        detail.push_str(&format_struct_field(field));
        detail.push_str(",\n");
    }
    if strukt.fields.len() > limit {
        detail.push_str("    /* ... */\n");
    }
    detail.push('}');
    detail
}

#[must_use]
pub fn format_struct_field(field: &HirStructField) -> String {
    let visibility = visibility_prefix(&field.visibility);
    let mutable = if field.is_mut { "mut " } else { "" };
    format!(
        "{visibility}{mutable}{}: {}",
        field.name.0,
        field.ty.display()
    )
}

#[must_use]
pub fn format_enum(enumeration: &HirEnum) -> String {
    format_enum_limited(enumeration, DECLARATION_ITEM_LIMIT)
}

/// Like [`format_enum`] but truncating the variant list at `limit`.
#[must_use]
pub fn format_enum_limited(enumeration: &HirEnum, limit: usize) -> String {
    let visibility = visibility_prefix(&enumeration.visibility);
    let mut detail = format!(
        "{visibility}{}",
        format_nominal("enum", &enumeration.name, &enumeration.generics)
    );
    detail.push_str(&where_clause(&enumeration.generic_bounds, &[], &[]));
    if enumeration.variants.is_empty() {
        detail.push_str(" {}");
        return detail;
    }

    detail.push_str(" {\n");
    for variant in enumeration.variants.iter().take(limit) {
        detail.push_str("    ");
        detail.push_str(&format_enum_variant(variant));
        detail.push_str(",\n");
    }
    if enumeration.variants.len() > limit {
        detail.push_str("    /* ... */\n");
    }
    detail.push('}');
    detail
}

#[must_use]
pub fn format_enum_variant(variant: &HirEnumVariant) -> String {
    match &variant.kind {
        HirVariantKind::Unit => variant.name.0.clone(),
        HirVariantKind::Tuple(fields) => format!(
            "{}({})",
            variant.name.0,
            fields
                .iter()
                .map(HirTypeRef::display)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        HirVariantKind::Struct(fields) => format!(
            "{} {{ {} }}",
            variant.name.0,
            fields
                .iter()
                .map(format_struct_field)
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

#[must_use]
pub fn format_trait(trait_decl: &HirTrait) -> String {
    let visibility = visibility_prefix(&trait_decl.visibility);
    let mut detail = format!(
        "{visibility}{}",
        format_nominal("trait", &trait_decl.name, &trait_decl.generics)
    );
    if !trait_decl.supertraits.is_empty() {
        let supers = trait_decl
            .supertraits
            .iter()
            .map(|bound| bound.trait_ty.display())
            .collect::<Vec<_>>()
            .join(" + ");
        detail.push_str(&format!(": {supers}"));
    }
    detail.push_str(&where_clause(&trait_decl.generic_bounds, &[], &[]));
    detail
}

#[must_use]
pub fn format_const(constant: &HirConst) -> String {
    let visibility = visibility_prefix(&constant.visibility);
    format!(
        "{visibility}const {}: {}",
        constant.name.0,
        constant.ty.display()
    )
}

#[must_use]
pub fn format_type_alias(alias: &HirTypeAlias) -> String {
    let visibility = visibility_prefix(&alias.visibility);
    let target = alias
        .ty
        .as_ref()
        .map_or_else(|| "?".into(), HirTypeRef::display);
    format!("{visibility}type {} = {target}", alias.name.0)
}

/// Renders an impl header, e.g. `impl<T> Ord for Wrapper<T>`.
#[must_use]
pub fn format_impl(imp: &HirImpl) -> String {
    let generics = if imp.generics.is_empty() {
        String::new()
    } else {
        format!(
            "<{}>",
            imp.generics
                .iter()
                .map(|name| name.0.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        )
    };
    match &imp.trait_ty {
        Some(trait_ty) => format!(
            "impl{generics} {} for {}{}",
            trait_ty.display(),
            imp.self_ty.display(),
            where_clause(&imp.generic_bounds, &[], &[])
        ),
        None => format!(
            "impl{generics} {}{}",
            imp.self_ty.display(),
            where_clause(&imp.generic_bounds, &[], &[])
        ),
    }
}

fn format_nominal(kind: &str, name: &Name, generics: &[Name]) -> String {
    if generics.is_empty() {
        return format!("{kind} {}", name.0);
    }
    format!(
        "{kind} {}<{}>",
        name.0,
        generics
            .iter()
            .map(|generic| generic.0.as_str())
            .collect::<Vec<_>>()
            .join(", ")
    )
}

/// Finds the doc comment text attached to the item whose syntax node
/// contains `target` (usually the item's name range), preferring the
/// smallest containing node. Mirrors the LSP hover lookup.
#[must_use]
pub fn doc_comment_for_range(
    doc_comments: &[(TextRange, Vec<String>)],
    target: TextRange,
) -> Option<String> {
    doc_comments
        .iter()
        .filter(|(range, _)| range.contains_range(target))
        .min_by_key(|(range, _)| range.len())
        .map(|(_, comments)| {
            comments
                .iter()
                .map(|comment| normalize_doc_comment(comment))
                .filter(|comment| !comment.is_empty())
                .collect::<Vec<_>>()
                .join("\n")
        })
        .filter(|documentation| !documentation.is_empty())
}

/// Strips doc-comment markers (`///`, `//!`, `//<`, `/** */`, `/*! */`)
/// from one raw comment token.
#[must_use]
pub fn normalize_doc_comment(comment: &str) -> String {
    let comment = comment.trim();
    if let Some(comment) = comment
        .strip_prefix("///")
        .or_else(|| comment.strip_prefix("//!"))
        .or_else(|| comment.strip_prefix("//<"))
    {
        return comment.trim_start().to_string();
    }
    let Some(comment) = comment
        .strip_prefix("/**")
        .or_else(|| comment.strip_prefix("/*!"))
    else {
        return comment.to_string();
    };
    let comment = comment.strip_suffix("*/").unwrap_or(comment);
    comment
        .lines()
        .map(str::trim)
        .map(|line| line.strip_prefix('*').map_or(line, str::trim_start))
        .collect::<Vec<_>>()
        .join("\n")
        .trim()
        .to_string()
}
