use std::{
    collections::{BTreeSet, HashMap},
    hash::BuildHasher,
    path::PathBuf,
};

use lsp_types::{Location, Range, SymbolInformation, SymbolKind};
use riddlec::pipeline::CompileOptions;
use serde::{Deserialize, Serialize};

use crate::{
    analysis::{AnalysisDepth, DocumentAnalysis, analyze_document_cancellable},
    server::Document,
    session::AnalysisSessions,
    text::{LineIndex, normalized_path},
};
use hir::{
    body::{Expr, ResolvedName},
    item_tree::{HirTypeRef, TopLevelItem},
};
use type_checker::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum IndexedSymbolKind {
    Function,
    Struct,
    Field,
    Enum,
    EnumMember,
    Trait,
    TypeAlias,
    Const,
    Module,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct SymbolKey {
    pub project: PathBuf,
    pub source: PathBuf,
    pub start: u32,
    pub end: u32,
    pub kind: IndexedSymbolKind,
}

#[derive(Debug, Clone)]
pub struct IndexedSymbol {
    pub key: SymbolKey,
    pub name: String,
    pub detail: String,
    pub location: Location,
    /// The identifier span, which selectionRange reports to the client.
    pub selection_range: Range,
    /// Byte offsets of the identifier in source.
    ///
    /// Cross-package matching works on raw source ranges, while key holds the
    /// whole item, so the name has to be remembered separately.
    pub name_source_range: std::ops::Range<u32>,
    pub container_name: Option<String>,
    pub is_public: bool,
}

#[derive(Debug, Clone)]
pub struct CallEdge {
    pub caller: SymbolKey,
    pub target: SymbolKey,
    pub sites: Vec<Range>,
}

#[derive(Debug, Clone, Default)]
pub struct TypeRelations {
    pub supertypes: HashMap<SymbolKey, Vec<SymbolKey>>,
    pub subtypes: HashMap<SymbolKey, Vec<SymbolKey>>,
}

#[derive(Debug, Clone)]
pub struct ProjectIndex {
    pub project: PathBuf,
    pub revision: u64,
    pub files: BTreeSet<PathBuf>,
    pub symbols: Vec<IndexedSymbol>,
    pub calls: Vec<CallEdge>,
    pub types: TypeRelations,
    /// `SymbolKey` → position in `symbols`.
    ///
    /// Call and type hierarchy used to find symbols by scanning the whole
    /// vector per edge, which is O(edges × symbols) on every request.
    by_key: HashMap<SymbolKey, usize>,
    /// `(uri, start, end)` → position in `symbols`, for hit-testing a location.
    ///
    /// Positions are flattened into integers because `lsp_types::Position` and
    /// `Range` do not implement `Hash`.
    by_location: HashMap<LocationKey, usize>,
}

impl ProjectIndex {
    /// Builds the lookup tables an index needs to answer hierarchy queries.
    #[must_use]
    pub fn from_parts(
        project: PathBuf,
        revision: u64,
        files: BTreeSet<PathBuf>,
        symbols: Vec<IndexedSymbol>,
        calls: Vec<CallEdge>,
        types: TypeRelations,
    ) -> Self {
        let by_key = symbols
            .iter()
            .enumerate()
            .map(|(position, symbol)| (symbol.key.clone(), position))
            .collect();
        let by_location = symbols
            .iter()
            .enumerate()
            .map(|(position, symbol)| (location_key(symbol), position))
            .collect();
        Self {
            project,
            revision,
            files,
            symbols,
            calls,
            types,
            by_key,
            by_location,
        }
    }

    /// Looks a symbol up by its key.
    #[must_use]
    pub fn symbol_by_key(&self, key: &SymbolKey) -> Option<&IndexedSymbol> {
        self.by_key
            .get(key)
            .and_then(|position| self.symbols.get(*position))
    }

    /// Looks a symbol up by the identifier the client is pointing at.
    #[must_use]
    pub fn symbol_at(&self, location: &Location) -> Option<&IndexedSymbol> {
        self.by_location
            .get(&range_key(&location.uri, &location.range))
            .and_then(|position| self.symbols.get(*position))
    }
}

/// A hashable key for a document range.
///
/// `lsp_types::Range` and `Position` do not implement `Hash`, so the four
/// coordinates are flattened into integers.
type LocationKey = (String, u32, u32, u32, u32);

fn range_key(uri: &lsp_types::Url, range: &Range) -> LocationKey {
    (
        uri.as_str().to_string(),
        range.start.line,
        range.start.character,
        range.end.line,
        range.end.character,
    )
}

fn location_key(symbol: &IndexedSymbol) -> LocationKey {
    range_key(&symbol.location.uri, &symbol.selection_range)
}

#[allow(deprecated)]
#[must_use]
pub fn workspace_symbols_for_index(index: &ProjectIndex, query: &str) -> Vec<SymbolInformation> {
    let query = query.to_lowercase();
    index
        .symbols
        .iter()
        .filter(|symbol| symbol.is_public)
        .filter(|symbol| symbol.name.to_lowercase().contains(&query))
        .map(|symbol| SymbolInformation {
            name: symbol.name.clone(),
            kind: match symbol.key.kind {
                IndexedSymbolKind::Function => SymbolKind::FUNCTION,
                IndexedSymbolKind::Struct => SymbolKind::STRUCT,
                IndexedSymbolKind::Field => SymbolKind::FIELD,
                IndexedSymbolKind::Enum => SymbolKind::ENUM,
                IndexedSymbolKind::EnumMember => SymbolKind::ENUM_MEMBER,
                IndexedSymbolKind::Trait => SymbolKind::INTERFACE,
                IndexedSymbolKind::TypeAlias => SymbolKind::TYPE_PARAMETER,
                IndexedSymbolKind::Const => SymbolKind::CONSTANT,
                IndexedSymbolKind::Module => SymbolKind::MODULE,
            },
            tags: None,
            deprecated: None,
            location: symbol.location.clone(),
            container_name: symbol.container_name.clone(),
        })
        .collect()
}

impl ProjectIndex {
    pub(crate) fn from_analysis(analysis: &DocumentAnalysis) -> Option<Self> {
        let project = analysis.project_root.clone()?;
        let hir = analysis.result.hir.as_ref()?;
        let symbols = collect_symbols(analysis, hir);

        let mut files = analysis
            .files
            .iter()
            .cloned()
            .map(normalized_path)
            .collect::<BTreeSet<_>>();
        files.insert(normalized_path(project.join(clue::CLUE_PROJECT_FILE_NAME)));
        let calls = build_call_edges(analysis, hir, &symbols);
        let types = build_type_relations(analysis, hir, &symbols);
        Some(Self::from_parts(
            project,
            analysis.project_revision,
            files,
            symbols,
            calls,
            types,
        ))
    }

    #[cfg(feature = "test")]
    #[must_use]
    pub fn empty(
        project: PathBuf,
        revision: u64,
        files: impl IntoIterator<Item = PathBuf>,
    ) -> Self {
        Self::from_parts(
            project,
            revision,
            files.into_iter().collect(),
            Vec::new(),
            Vec::new(),
            TypeRelations::default(),
        )
    }
}

fn collect_symbols(analysis: &DocumentAnalysis, hir: &hir::HirFile) -> Vec<IndexedSymbol> {
    let mut symbols = Vec::new();
    collect_items(analysis, hir, &hir.item_tree.top_level, None, &mut symbols);
    collect_unreachable_symbols(analysis, hir, &mut symbols);
    symbols.sort_by(|left, right| left.key.cmp(&right.key));
    symbols.dedup_by(|left, right| left.key == right.key);
    symbols
}

fn collect_unreachable_symbols(
    analysis: &DocumentAnalysis,
    hir: &hir::HirFile,
    symbols: &mut Vec<IndexedSymbol>,
) {
    for (_, item) in hir.item_tree.functions.iter() {
        push_unreachable_symbol(
            symbols,
            analysis,
            IndexedSymbolKind::Function,
            &item.name.0,
            format!("fun {}", item.name.0),
            item.extent,
            item.name_range,
            item.visibility.is_public(),
        );
    }
    for (_, item) in hir.item_tree.structs.iter() {
        push_unreachable_symbol(
            symbols,
            analysis,
            IndexedSymbolKind::Struct,
            &item.name.0,
            format!("struct {}", item.name.0),
            item.extent,
            item.name_range,
            item.visibility.is_public(),
        );
        let container = item.name.0.clone();
        for field in &item.fields {
            push_unreachable_symbol_with_container(
                symbols,
                analysis,
                IndexedSymbolKind::Field,
                &field.name.0,
                format!("{}: {}", field.name.0, field.ty.display()),
                field.ty_range,
                field.name_range,
                Some(&container),
                item.visibility.is_public() && field.visibility.is_public(),
            );
        }
    }
    for (_, item) in hir.item_tree.enums.iter() {
        push_unreachable_symbol(
            symbols,
            analysis,
            IndexedSymbolKind::Enum,
            &item.name.0,
            format!("enum {}", item.name.0),
            item.extent,
            item.name_range,
            item.visibility.is_public(),
        );
        let container = item.name.0.clone();
        for variant in &item.variants {
            push_unreachable_symbol_with_container(
                symbols,
                analysis,
                IndexedSymbolKind::EnumMember,
                &variant.name.0,
                format!("enum member {}", variant.name.0),
                variant.name_range,
                variant.name_range,
                Some(&container),
                item.visibility.is_public(),
            );
        }
    }
    for (_, item) in hir.item_tree.traits.iter() {
        push_unreachable_symbol(
            symbols,
            analysis,
            IndexedSymbolKind::Trait,
            &item.name.0,
            format!("trait {}", item.name.0),
            item.extent,
            item.name_range,
            item.visibility.is_public(),
        );
        for method in &item.methods {
            push_unreachable_symbol(
                symbols,
                analysis,
                IndexedSymbolKind::Function,
                &method.name.0,
                format!("fun {}", method.name.0),
                method.extent,
                method.name_range,
                item.visibility.is_public() && method.visibility.is_public(),
            );
        }
        for fid in &item.default_methods {
            let method = &hir.item_tree.functions[*fid];
            push_unreachable_symbol(
                symbols,
                analysis,
                IndexedSymbolKind::Function,
                &method.name.0,
                format!("fun {}", method.name.0),
                method.extent,
                method.name_range,
                item.visibility.is_public() && method.visibility.is_public(),
            );
        }
    }
    for (_, item) in hir.item_tree.consts.iter() {
        push_unreachable_symbol(
            symbols,
            analysis,
            IndexedSymbolKind::Const,
            &item.name.0,
            format!("const {}", item.name.0),
            item.extent,
            item.name_range,
            item.visibility.is_public(),
        );
    }
    for (_, item) in hir.item_tree.type_aliases.iter() {
        push_unreachable_symbol(
            symbols,
            analysis,
            IndexedSymbolKind::TypeAlias,
            &item.name.0,
            format!("type {}", item.name.0),
            item.extent,
            item.name_range,
            item.visibility.is_public(),
        );
    }
    for (_, item) in hir.item_tree.modules.iter() {
        push_unreachable_symbol(
            symbols,
            analysis,
            IndexedSymbolKind::Module,
            &item.name.0,
            format!("mod {}", item.name.0),
            item.extent,
            item.name_range,
            item.visibility.is_public(),
        );
    }
}

#[allow(clippy::too_many_arguments)]
fn push_unreachable_symbol(
    symbols: &mut Vec<IndexedSymbol>,
    analysis: &DocumentAnalysis,
    kind: IndexedSymbolKind,
    name: &str,
    detail: String,
    extent: rowan::TextRange,
    name_range: rowan::TextRange,
    is_public: bool,
) {
    push_unreachable_symbol_with_container(
        symbols, analysis, kind, name, detail, extent, name_range, None, is_public,
    );
}

#[allow(clippy::too_many_arguments)]
fn push_unreachable_symbol_with_container(
    symbols: &mut Vec<IndexedSymbol>,
    analysis: &DocumentAnalysis,
    kind: IndexedSymbolKind,
    name: &str,
    detail: String,
    extent: rowan::TextRange,
    name_range: rowan::TextRange,
    container: Option<&str>,
    is_public: bool,
) {
    if symbol_key_for_range(analysis, symbols, name_range, kind).is_none() {
        push_symbol(
            symbols, analysis, kind, name, detail, extent, name_range, container, is_public,
        );
    }
}

fn collect_items(
    analysis: &DocumentAnalysis,
    hir: &hir::HirFile,
    items: &[TopLevelItem],
    container: Option<&str>,
    symbols: &mut Vec<IndexedSymbol>,
) {
    for item in items {
        match *item {
            TopLevelItem::Function(id) => {
                let item = &hir.item_tree.functions[id];
                push_symbol(
                    symbols,
                    analysis,
                    IndexedSymbolKind::Function,
                    &item.name.0,
                    format!("fun {}", item.name.0),
                    item.extent,
                    item.name_range,
                    container,
                    item.visibility.is_public(),
                );
            }
            TopLevelItem::Struct(id) => {
                let item = &hir.item_tree.structs[id];
                let is_public = item.visibility.is_public();
                push_symbol(
                    symbols,
                    analysis,
                    IndexedSymbolKind::Struct,
                    &item.name.0,
                    format!("struct {}", item.name.0),
                    item.extent,
                    item.name_range,
                    container,
                    is_public,
                );
                let member_container = joined_container(container, &item.name.0);
                for field in &item.fields {
                    push_symbol(
                        symbols,
                        analysis,
                        IndexedSymbolKind::Field,
                        &field.name.0,
                        format!("{}: {}", field.name.0, field.ty.display()),
                        field.ty_range,
                        field.name_range,
                        Some(&member_container),
                        is_public && field.visibility.is_public(),
                    );
                }
            }
            TopLevelItem::Enum(id) => {
                let item = &hir.item_tree.enums[id];
                let is_public = item.visibility.is_public();
                push_symbol(
                    symbols,
                    analysis,
                    IndexedSymbolKind::Enum,
                    &item.name.0,
                    format!("enum {}", item.name.0),
                    item.extent,
                    item.name_range,
                    container,
                    is_public,
                );
                let member_container = joined_container(container, &item.name.0);
                for variant in &item.variants {
                    push_symbol(
                        symbols,
                        analysis,
                        IndexedSymbolKind::EnumMember,
                        &variant.name.0,
                        format!("enum member {}", variant.name.0),
                        variant.name_range,
                        variant.name_range,
                        Some(&member_container),
                        is_public,
                    );
                    if let hir::item_tree::HirVariantKind::Struct(fields) = &variant.kind {
                        let variant_container =
                            joined_container(Some(&member_container), &variant.name.0);
                        for field in fields {
                            push_symbol(
                                symbols,
                                analysis,
                                IndexedSymbolKind::Field,
                                &field.name.0,
                                format!("{}: {}", field.name.0, field.ty.display()),
                                field.ty_range,
                                field.name_range,
                                Some(&variant_container),
                                is_public && field.visibility.is_public(),
                            );
                        }
                    }
                }
            }
            TopLevelItem::Trait(id) => {
                let item = &hir.item_tree.traits[id];
                let is_public = item.visibility.is_public();
                push_symbol(
                    symbols,
                    analysis,
                    IndexedSymbolKind::Trait,
                    &item.name.0,
                    format!("trait {}", item.name.0),
                    item.extent,
                    item.name_range,
                    container,
                    is_public,
                );
                let member_container = joined_container(container, &item.name.0);
                for method in &item.methods {
                    push_symbol(
                        symbols,
                        analysis,
                        IndexedSymbolKind::Function,
                        &method.name.0,
                        format!("fun {}", method.name.0),
                        method.extent,
                        method.name_range,
                        Some(&member_container),
                        is_public && method.visibility.is_public(),
                    );
                }
                for fid in &item.default_methods {
                    let method = &hir.item_tree.functions[*fid];
                    push_symbol(
                        symbols,
                        analysis,
                        IndexedSymbolKind::Function,
                        &method.name.0,
                        format!("fun {}", method.name.0),
                        method.extent,
                        method.name_range,
                        Some(&member_container),
                        is_public && method.visibility.is_public(),
                    );
                }
            }
            TopLevelItem::Module(id) => {
                let item = &hir.item_tree.modules[id];
                let is_public = item.visibility.is_public();
                push_symbol(
                    symbols,
                    analysis,
                    IndexedSymbolKind::Module,
                    &item.name.0,
                    format!("mod {}", item.name.0),
                    item.extent,
                    item.name_range,
                    container,
                    is_public,
                );
                if let Some(children) = item.items.as_deref() {
                    let member_container = joined_container(container, &item.name.0);
                    collect_items(analysis, hir, children, Some(&member_container), symbols);
                }
            }
            TopLevelItem::Const(id) => {
                let item = &hir.item_tree.consts[id];
                push_symbol(
                    symbols,
                    analysis,
                    IndexedSymbolKind::Const,
                    &item.name.0,
                    format!("const {}", item.name.0),
                    item.extent,
                    item.name_range,
                    container,
                    item.visibility.is_public(),
                );
            }
            TopLevelItem::TypeAlias(id) => {
                let item = &hir.item_tree.type_aliases[id];
                push_symbol(
                    symbols,
                    analysis,
                    IndexedSymbolKind::TypeAlias,
                    &item.name.0,
                    format!("type {}", item.name.0),
                    item.extent,
                    item.name_range,
                    container,
                    item.visibility.is_public(),
                );
            }
            TopLevelItem::Impl(id) => {
                let implementation = &hir.item_tree.impls[id];
                let member_container = joined_container(
                    container,
                    &format!("impl {}", implementation.self_ty.display()),
                );
                for fid in &implementation.methods {
                    let method = &hir.item_tree.functions[*fid];
                    push_symbol(
                        symbols,
                        analysis,
                        IndexedSymbolKind::Function,
                        &method.name.0,
                        format!("fun {}", method.name.0),
                        method.extent,
                        method.name_range,
                        Some(&member_container),
                        method.visibility.is_public(),
                    );
                }
            }
            TopLevelItem::Use(_) => {}
        }
    }
}

fn joined_container(parent: Option<&str>, name: &str) -> String {
    parent.map_or_else(|| name.to_string(), |parent| format!("{parent}::{name}"))
}

fn build_call_edges(
    analysis: &DocumentAnalysis,
    hir: &hir::HirFile,
    symbols: &[IndexedSymbol],
) -> Vec<CallEdge> {
    let mut grouped = HashMap::<(SymbolKey, SymbolKey), Vec<Range>>::new();
    for (function, body) in &hir.function_bodies {
        let Some(caller) = symbol_key_for_range(
            analysis,
            symbols,
            hir.item_tree.functions[*function].name_range,
            IndexedSymbolKind::Function,
        ) else {
            continue;
        };
        for (_, value) in hir.bodies[*body].exprs.iter() {
            let Expr::Call { callee, .. } = value else {
                continue;
            };
            let target_range = call_target_range(analysis, hir, *body, *callee);
            let Some(target) = target_range.and_then(|range| {
                symbol_key_for_range(analysis, symbols, range, IndexedSymbolKind::Function)
            }) else {
                continue;
            };
            let Some(site) = hir.bodies[*body]
                .source_map
                .expr_ranges
                .get(callee)
                .and_then(|range| mapped_lsp_range(analysis, *range))
            else {
                continue;
            };
            grouped
                .entry((caller.clone(), target))
                .or_default()
                .push(site);
        }
    }
    let mut calls = grouped
        .into_iter()
        .map(|((caller, target), mut sites)| {
            sites.sort_by_key(|range| {
                (
                    range.start.line,
                    range.start.character,
                    range.end.line,
                    range.end.character,
                )
            });
            sites.dedup();
            CallEdge {
                caller,
                target,
                sites,
            }
        })
        .collect::<Vec<_>>();
    calls.sort_by(|left, right| {
        left.caller
            .cmp(&right.caller)
            .then_with(|| left.target.cmp(&right.target))
    });
    calls
}

fn call_target_range(
    analysis: &DocumentAnalysis,
    hir: &hir::HirFile,
    body: hir::body::BodyId,
    callee: hir::body::ExprId,
) -> Option<rowan::TextRange> {
    analysis
        .result
        .type_result
        .trait_method_calls
        .get(&(body, callee))
        .map_or_else(
            || match analysis.result.type_result.expr_types.get(&(body, callee)) {
                Some(Type::FunctionItem { function, .. }) => {
                    Some(hir.item_tree.functions[*function].name_range)
                }
                _ => None,
            },
            |call| {
                hir.item_tree.traits[call.trait_id]
                    .methods
                    .iter()
                    .find(|method| method.name.0 == call.method)
                    .map(|method| method.name_range)
            },
        )
}

fn build_type_relations(
    analysis: &DocumentAnalysis,
    hir: &hir::HirFile,
    symbols: &[IndexedSymbol],
) -> TypeRelations {
    let mut supertypes = HashMap::<SymbolKey, BTreeSet<SymbolKey>>::new();
    let mut subtypes = HashMap::<SymbolKey, BTreeSet<SymbolKey>>::new();
    let mut connect = |subtype: SymbolKey, supertype: SymbolKey| {
        supertypes
            .entry(subtype.clone())
            .or_default()
            .insert(supertype.clone());
        subtypes.entry(supertype).or_default().insert(subtype);
    };

    for (_, tr) in hir.item_tree.traits.iter() {
        let Some(child) =
            symbol_key_for_range(analysis, symbols, tr.name_range, IndexedSymbolKind::Trait)
        else {
            continue;
        };
        for bound in &tr.supertraits {
            let Some(parent_range) = resolved_trait_range(hir, &bound.trait_ty) else {
                continue;
            };
            if let Some(parent) =
                symbol_key_for_range(analysis, symbols, parent_range, IndexedSymbolKind::Trait)
            {
                connect(child.clone(), parent);
            }
        }
    }
    for (_, implementation) in hir.item_tree.impls.iter() {
        let Some(trait_range) = implementation
            .trait_ty
            .as_ref()
            .and_then(|ty| resolved_trait_range(hir, ty))
        else {
            continue;
        };
        let Some(supertype) =
            symbol_key_for_range(analysis, symbols, trait_range, IndexedSymbolKind::Trait)
        else {
            continue;
        };
        let Some((kind, range)) = resolved_nominal_range(hir, &implementation.self_ty) else {
            continue;
        };
        if let Some(subtype) = symbol_key_for_range(analysis, symbols, range, kind) {
            connect(subtype, supertype);
        }
    }

    TypeRelations {
        supertypes: supertypes
            .into_iter()
            .map(|(key, values)| (key, values.into_iter().collect()))
            .collect(),
        subtypes: subtypes
            .into_iter()
            .map(|(key, values)| (key, values.into_iter().collect()))
            .collect(),
    }
}

fn resolved_trait_range(hir: &hir::HirFile, ty: &HirTypeRef) -> Option<rowan::TextRange> {
    let HirTypeRef::Named(path) = ty else {
        return None;
    };
    match hir.type_resolutions.get(&path.range) {
        Some(ResolvedName::Trait(id)) => Some(hir.item_tree.traits[*id].name_range),
        _ => None,
    }
}

fn resolved_nominal_range(
    hir: &hir::HirFile,
    ty: &HirTypeRef,
) -> Option<(IndexedSymbolKind, rowan::TextRange)> {
    let HirTypeRef::Named(path) = ty else {
        return None;
    };
    match hir.type_resolutions.get(&path.range) {
        Some(ResolvedName::Struct(id)) => Some((
            IndexedSymbolKind::Struct,
            hir.item_tree.structs[*id].name_range,
        )),
        Some(ResolvedName::Enum(id)) => {
            Some((IndexedSymbolKind::Enum, hir.item_tree.enums[*id].name_range))
        }
        _ => None,
    }
}

fn symbol_key_for_range(
    analysis: &DocumentAnalysis,
    symbols: &[IndexedSymbol],
    range: rowan::TextRange,
    kind: IndexedSymbolKind,
) -> Option<SymbolKey> {
    let mapped = analysis.source_map.as_ref()?.map_range(range)?;
    let source = normalized_path(mapped.path.to_path_buf());
    let start = u32::from(mapped.range.start());
    let end = u32::from(mapped.range.end());
    symbols
        .iter()
        .find(|symbol| {
            symbol.key.source == source
                && symbol.name_source_range.start == start
                && symbol.name_source_range.end == end
                && symbol.key.kind == kind
        })
        .map(|symbol| symbol.key.clone())
}

fn mapped_lsp_range(analysis: &DocumentAnalysis, range: rowan::TextRange) -> Option<Range> {
    let mapped = analysis.source_map.as_ref()?.map_range(range)?;
    LineIndex::new(mapped.source).range(mapped.source, mapped.range)
}

#[allow(clippy::too_many_arguments)]
fn push_symbol(
    symbols: &mut Vec<IndexedSymbol>,
    analysis: &DocumentAnalysis,
    kind: IndexedSymbolKind,
    name: &str,
    detail: String,
    extent: rowan::TextRange,
    name_range: rowan::TextRange,
    container_name: Option<&str>,
    is_public: bool,
) {
    let Some(source_map) = &analysis.source_map else {
        return;
    };
    let Some(mapped) = source_map.map_range(extent) else {
        return;
    };
    let source = normalized_path(mapped.path.to_path_buf());
    let Ok(uri) = lsp_types::Url::from_file_path(&source) else {
        return;
    };
    let index = LineIndex::new(mapped.source);
    let Some(range) = index.range(mapped.source, mapped.range) else {
        return;
    };
    // The name span is what `selectionRange` reports; it is clamped into the
    // item extent, so a name reported outside its own item cannot happen.
    let Some(name_mapped) = source_map.map_range(name_range) else {
        return;
    };
    let Some(selection_range) = index.range(name_mapped.source, name_mapped.range) else {
        return;
    };
    symbols.push(IndexedSymbol {
        key: SymbolKey {
            project: analysis
                .project_root
                .clone()
                .expect("project symbols require a project root"),
            source,
            start: mapped.range.start().into(),
            end: mapped.range.end().into(),
            kind,
        },
        name: name.into(),
        detail,
        location: Location { uri, range },
        selection_range,
        name_source_range: u32::from(name_mapped.range.start())..u32::from(name_mapped.range.end()),
        container_name: container_name.map(str::to_string),
        is_public,
    });
}

pub fn project_index_for_document_cancellable<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> Result<Option<ProjectIndex>, String> {
    let Some(analysis) = analyze_document_cancellable(
        uri,
        docs,
        options,
        sessions,
        AnalysisDepth::Infer,
        cancelled,
    )?
    else {
        return Ok(None);
    };
    Ok(ProjectIndex::from_analysis(&analysis))
}

#[cfg(feature = "test")]
/// Builds a project index for an open test document.
///
/// # Errors
///
/// Returns an error when project analysis fails.
pub fn project_index_for_document<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    options: CompileOptions,
    sessions: &AnalysisSessions,
) -> Result<Option<ProjectIndex>, String> {
    project_index_for_document_cancellable(uri, docs, options, sessions, &|| false)
}

pub fn project_index_for_root_cancellable<S: BuildHasher>(
    root: &std::path::Path,
    overlays: &HashMap<PathBuf, String, S>,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> Result<Option<ProjectIndex>, String> {
    let root = normalized_path(root.to_path_buf());
    let session = sessions.project(&root);
    let mut session = session
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let analysis = clue::infer_project_with_session_cancellable(
        &root,
        overlays,
        options,
        &mut session,
        cancelled,
    )
    .map_err(|error| error.to_string())?;
    let Some(analysis) = analysis else {
        return Ok(None);
    };
    let project_revision = session.revision();
    drop(session);
    let files = analysis.source.files.clone();
    let path = Some(normalized_path(analysis.entry.clone()));
    let document_analysis = DocumentAnalysis {
        result: std::sync::Arc::clone(&analysis.result),
        source: analysis.source.source.clone(),
        source_map: Some(analysis.source.source_map.clone()),
        macro_occurrences: analysis.macro_occurrences.clone(),
        macro_source_map: Some(analysis.macro_source_map.clone()),
        path,
        project_root: Some(root),
        project_revision,
        files,
    };
    Ok(ProjectIndex::from_analysis(&document_analysis))
}

#[cfg(feature = "test")]
/// Builds a project index from a test project root.
///
/// # Errors
///
/// Returns an error when the project cannot be loaded or checked.
pub fn project_index_for_root<S: BuildHasher>(
    root: &std::path::Path,
    overlays: &HashMap<PathBuf, String, S>,
    options: CompileOptions,
    sessions: &AnalysisSessions,
) -> Result<Option<ProjectIndex>, String> {
    project_index_for_root_cancellable(root, overlays, options, sessions, &|| false)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symbol_keys_round_trip_through_protocol_data() {
        let key = SymbolKey {
            project: PathBuf::from("project"),
            source: PathBuf::from("project/src/main.rid"),
            start: 7,
            end: 12,
            kind: IndexedSymbolKind::Function,
        };

        let encoded = serde_json::to_value(&key).unwrap();
        let decoded: SymbolKey = serde_json::from_value(encoded).unwrap();

        assert_eq!(decoded, key);
    }
}
