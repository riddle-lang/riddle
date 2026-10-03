use hir::item_tree::{HirVariantKind, TopLevelItem};
use lsp_types::{
    DocumentSymbol, FoldingRange, FoldingRangeKind, Location, SymbolInformation, SymbolKind,
};
use std::{collections::HashMap, hash::BuildHasher};
use syntax::SyntaxKind;

use crate::text::LineIndex;
use crate::{
    analysis::{AnalysisDepth, analyze_document_cancellable},
    navigation::source_uri,
    server::Document,
    session::AnalysisSessions,
};

#[must_use]
pub fn format_source(source: &str, tab_size: u32, insert_spaces: bool) -> String {
    riddlec::fmt::format_source(
        source,
        riddlec::fmt::FormatOptions {
            tab_size,
            insert_spaces,
        },
    )
}

/// Whether a token is a comment of any flavour.
fn is_comment_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::LineComment
            | SyntaxKind::DocComment
            | SyntaxKind::BlockComment
            | SyntaxKind::DocBlockComment
    )
}

#[must_use]
pub fn folding_ranges(source: &str) -> Vec<FoldingRange> {
    let index = LineIndex::new(source);
    let tokens = frontend::lexer::lex(source);
    let mut ranges = brace_folding_ranges(source, &index, &tokens);
    ranges.extend(comment_folding_ranges(source, &index, &tokens));
    ranges.extend(use_folding_ranges(source, &index));
    ranges.sort_by_key(|range| (range.start_line, range.start_character));
    ranges.dedup_by(|left, right| {
        left.start_line == right.start_line
            && left.end_line == right.end_line
            && left.kind == right.kind
    });
    ranges
}

/// Brace-delimited blocks, the only thing the server used to fold.
///
/// These are genuine regions: a client that collapses one is hiding an
/// implementation, not a comment.
fn brace_folding_ranges(
    source: &str,
    index: &LineIndex,
    tokens: &[frontend::lexer::Token],
) -> Vec<FoldingRange> {
    let mut stack = Vec::new();
    let mut ranges = Vec::new();
    for token in tokens {
        match token.kind {
            SyntaxKind::LBrace => stack.push(token.span.start),
            SyntaxKind::RBrace => {
                let Some(start) = stack.pop() else {
                    continue;
                };
                let Some(start) = index.position(source, start) else {
                    continue;
                };
                let Some(end) = index.position(source, token.span.end.saturating_sub(1)) else {
                    continue;
                };
                if start.line < end.line {
                    ranges.push(FoldingRange {
                        start_line: start.line,
                        start_character: Some(start.character),
                        end_line: end.line,
                        end_character: Some(end.character),
                        kind: Some(FoldingRangeKind::Region),
                        collapsed_text: None,
                    });
                }
            }
            _ => {}
        }
    }
    ranges
}

/// Runs of consecutive comment lines, folded as `comment`.
///
/// The lexer reports a multi-line block comment as one token, so its line span
/// is folded directly; single-line comments are grouped while they stay
/// adjacent.
fn comment_folding_ranges(
    source: &str,
    index: &LineIndex,
    tokens: &[frontend::lexer::Token],
) -> Vec<FoldingRange> {
    let mut ranges = Vec::new();
    let mut run: Option<(u32, u32)> = None;
    let flush = |run: &mut Option<(u32, u32)>, ranges: &mut Vec<FoldingRange>| {
        if let Some((start, end)) = run.take()
            && start < end
        {
            ranges.push(FoldingRange {
                start_line: start,
                start_character: None,
                end_line: end,
                end_character: None,
                kind: Some(FoldingRangeKind::Comment),
                collapsed_text: None,
            });
        }
    };
    for token in tokens {
        if !is_comment_kind(token.kind) {
            flush(&mut run, &mut ranges);
            continue;
        }
        let Some(start) = index.position(source, token.span.start) else {
            continue;
        };
        let Some(end) = index.position(source, token.span.end.saturating_sub(1)) else {
            continue;
        };
        match run {
            // Adjacent to the previous comment line: extend the run.
            Some((run_start, run_end)) if start.line <= run_end + 1 => {
                run = Some((run_start, run_end.max(end.line)));
            }
            _ => {
                flush(&mut run, &mut ranges);
                run = Some((start.line, end.line));
            }
        }
    }
    flush(&mut run, &mut ranges);
    ranges
}

/// A top-level `use` run, folded as `imports`.
fn use_folding_ranges(source: &str, index: &LineIndex) -> Vec<FoldingRange> {
    let mut ranges = Vec::new();
    let mut run: Option<(u32, u32)> = None;
    let flush = |run: &mut Option<(u32, u32)>, ranges: &mut Vec<FoldingRange>| {
        if let Some((start, end)) = run.take()
            && start < end
        {
            ranges.push(FoldingRange {
                start_line: start,
                start_character: None,
                end_line: end,
                end_character: None,
                kind: Some(FoldingRangeKind::Imports),
                collapsed_text: None,
            });
        }
    };
    let mut in_use = false;
    for token in frontend::lexer::lex(source) {
        if token.kind.is_trivia() {
            continue;
        }
        if token.kind == SyntaxKind::Use {
            let Some(start) = index.position(source, token.span.start) else {
                continue;
            };
            // A `use` tree can span lines; fold it as a run with its neighbours.
            let end_line = source[token.span.start..]
                .find(';')
                .and_then(|offset| index.position(source, token.span.start + offset))
                .map_or(start.line, |position| position.line);
            match run {
                Some((run_start, run_end)) if start.line <= run_end + 1 => {
                    run = Some((run_start, run_end.max(end_line)));
                }
                _ => {
                    flush(&mut run, &mut ranges);
                    run = Some((start.line, end_line));
                }
            }
            in_use = true;
            continue;
        }
        if in_use {
            flush(&mut run, &mut ranges);
            in_use = false;
        }
    }
    flush(&mut run, &mut ranges);
    ranges
}

#[cfg(feature = "test")]
#[must_use]
pub fn document_symbols_for_source(source: &str) -> Vec<DocumentSymbol> {
    let result = riddlec::pipeline::resolve_with_options(
        source,
        riddlec::pipeline::CompileOptions { use_std: false },
    );
    let Some(hir) = result.hir.as_ref() else {
        return Vec::new();
    };
    let index = LineIndex::new(source);
    symbols_for_items(hir, &hir.item_tree.top_level, &|range| {
        index.range(source, range)
    })
}

#[cfg(feature = "test")]
#[must_use]
/// Returns workspace symbols from standalone test source.
///
/// # Panics
///
/// Panics if the fixed test document URL cannot be parsed.
pub fn workspace_symbols_for_source(source: &str, query: &str) -> Vec<SymbolInformation> {
    let uri = lsp_types::Url::parse("untitled:riddle-workspace-symbols.rid").unwrap();
    let mut symbols = Vec::new();
    flatten_symbols(
        &uri,
        query,
        None,
        &document_symbols_for_source(source),
        &mut symbols,
    );
    symbols
}

#[allow(deprecated)]
#[cfg(feature = "test")]
fn flatten_symbols(
    uri: &lsp_types::Url,
    query: &str,
    container: Option<&str>,
    nested: &[DocumentSymbol],
    output: &mut Vec<SymbolInformation>,
) {
    for symbol in nested {
        if symbol.name.to_lowercase().contains(&query.to_lowercase()) {
            output.push(SymbolInformation {
                name: symbol.name.clone(),
                kind: symbol.kind,
                tags: symbol.tags.clone(),
                deprecated: None,
                location: Location::new(uri.clone(), symbol.selection_range),
                container_name: container.map(str::to_string),
            });
        }
        if let Some(children) = &symbol.children {
            flatten_symbols(uri, query, Some(&symbol.name), children, output);
        }
    }
}

fn symbols_for_items(
    hir: &hir::HirFile,
    items: &[TopLevelItem],
    range_for: &impl Fn(rowan::TextRange) -> Option<lsp_types::Range>,
) -> Vec<DocumentSymbol> {
    items
        .iter()
        .filter_map(|item| symbol_for_item(hir, *item, range_for))
        .collect()
}

fn symbol_for_item(
    hir: &hir::HirFile,
    item: TopLevelItem,
    range_for: &impl Fn(rowan::TextRange) -> Option<lsp_types::Range>,
) -> Option<DocumentSymbol> {
    match item {
        TopLevelItem::Function(id) => {
            let item = &hir.item_tree.functions[id];
            item_symbol(
                range_for,
                &item.name.0,
                SymbolKind::FUNCTION,
                item.extent,
                item.name_range,
                None,
            )
        }
        TopLevelItem::Struct(id) => {
            let item = &hir.item_tree.structs[id];
            let children = symbols_for_fields(&item.fields, range_for);
            item_symbol(
                range_for,
                &item.name.0,
                SymbolKind::STRUCT,
                item.extent,
                item.name_range,
                Some(children),
            )
        }
        TopLevelItem::Enum(id) => {
            let item = &hir.item_tree.enums[id];
            item_symbol(
                range_for,
                &item.name.0,
                SymbolKind::ENUM,
                item.extent,
                item.name_range,
                Some(symbols_for_enum_children(item, range_for)),
            )
        }
        TopLevelItem::Trait(id) => {
            let item = &hir.item_tree.traits[id];
            item_symbol(
                range_for,
                &item.name.0,
                SymbolKind::INTERFACE,
                item.extent,
                item.name_range,
                Some(symbols_for_methods(&item.methods, range_for)),
            )
        }
        TopLevelItem::Module(id) => {
            let item = &hir.item_tree.modules[id];
            let children = item
                .items
                .as_deref()
                .map(|items| symbols_for_items(hir, items, range_for));
            item_symbol(
                range_for,
                &item.name.0,
                SymbolKind::MODULE,
                item.extent,
                item.name_range,
                children,
            )
        }
        TopLevelItem::Const(id) => {
            let item = &hir.item_tree.consts[id];
            item_symbol(
                range_for,
                &item.name.0,
                SymbolKind::CONSTANT,
                item.extent,
                item.name_range,
                None,
            )
        }
        TopLevelItem::TypeAlias(id) => {
            let item = &hir.item_tree.type_aliases[id];
            item_symbol(
                range_for,
                &item.name.0,
                SymbolKind::TYPE_PARAMETER,
                item.extent,
                item.name_range,
                None,
            )
        }
        TopLevelItem::Impl(id) => symbols_for_impl(hir, id, range_for),
        TopLevelItem::Use(_) => None,
    }
}

fn symbols_for_fields(
    fields: &[hir::item_tree::HirStructField],
    range_for: &impl Fn(rowan::TextRange) -> Option<lsp_types::Range>,
) -> Vec<DocumentSymbol> {
    fields
        .iter()
        .filter_map(|field| {
            symbol(
                range_for,
                &field.name.0,
                SymbolKind::FIELD,
                field.ty_range,
                field.name_range,
                None,
            )
        })
        .collect()
}

fn symbols_for_enum_children(
    item: &hir::item_tree::HirEnum,
    range_for: &impl Fn(rowan::TextRange) -> Option<lsp_types::Range>,
) -> Vec<DocumentSymbol> {
    item.variants
        .iter()
        .filter_map(|variant| {
            let fields = match &variant.kind {
                HirVariantKind::Struct(fields) => symbols_for_fields(fields, range_for),
                _ => Vec::new(),
            };
            symbol(
                range_for,
                &variant.name.0,
                SymbolKind::ENUM_MEMBER,
                variant.name_range,
                variant.name_range,
                Some(fields),
            )
        })
        .collect()
}

fn symbols_for_methods(
    methods: &[hir::item_tree::HirFunction],
    range_for: &impl Fn(rowan::TextRange) -> Option<lsp_types::Range>,
) -> Vec<DocumentSymbol> {
    methods
        .iter()
        .filter_map(|method| {
            symbol(
                range_for,
                &method.name.0,
                SymbolKind::METHOD,
                method.extent,
                method.name_range,
                None,
            )
        })
        .collect()
}

#[allow(deprecated)]
fn symbol(
    range_for: &impl Fn(rowan::TextRange) -> Option<lsp_types::Range>,
    name: &str,
    kind: SymbolKind,
    extent: rowan::TextRange,
    name_range: rowan::TextRange,
    children: Option<Vec<DocumentSymbol>>,
) -> Option<DocumentSymbol> {
    // `range` is the whole item and `selectionRange` the identifier, which is
    // what clients use to highlight the item in the outline and to scroll to
    // it. Reporting the name span for both made peek views and sticky scroll
    // show the identifier alone.
    let range = range_for(extent)?;
    let selection_range = range_for(name_range).unwrap_or(range);
    Some(DocumentSymbol {
        name: name.into(),
        detail: None,
        kind,
        tags: None,
        deprecated: None,
        range,
        selection_range,
        children,
    })
}

/// A symbol whose extent is the whole item, falling back to the name span for
/// items lowered without one.
#[allow(deprecated)]
fn item_symbol(
    range_for: &impl Fn(rowan::TextRange) -> Option<lsp_types::Range>,
    name: &str,
    kind: SymbolKind,
    extent: rowan::TextRange,
    name_range: rowan::TextRange,
    children: Option<Vec<DocumentSymbol>>,
) -> Option<DocumentSymbol> {
    symbol(range_for, name, kind, extent, name_range, children)
}

/// Methods, consts, and associated types of an `impl` block, as outline
/// children of a container symbol.
///
/// The container is named after the implementing type and carries the trait
/// name as detail, so the outline reads like the source does. Impl blocks used
/// to be dropped from the outline entirely, which meant every method defined in
/// one was invisible to breadcrumbs and the symbol list even though
/// `workspace/symbol` found them.
fn symbols_for_impl(
    hir: &hir::HirFile,
    id: hir::item_tree::ImplId,
    range_for: &impl Fn(rowan::TextRange) -> Option<lsp_types::Range>,
) -> Option<DocumentSymbol> {
    let item = &hir.item_tree.impls[id];
    let name = item.self_ty.display();
    let trait_name = item
        .trait_ty
        .as_ref()
        .map(hir::item_tree::HirTypeRef::display);
    let mut children = Vec::new();
    for id in &item.methods {
        let method = &hir.item_tree.functions[*id];
        children.extend(item_symbol(
            range_for,
            &method.name.0,
            SymbolKind::METHOD,
            method.extent,
            method.name_range,
            None,
        ));
    }
    for id in &item.consts {
        let konst = &hir.item_tree.consts[*id];
        children.extend(item_symbol(
            range_for,
            &konst.name.0,
            SymbolKind::CONSTANT,
            konst.extent,
            konst.name_range,
            None,
        ));
    }
    for id in &item.type_aliases {
        let alias = &hir.item_tree.type_aliases[*id];
        children.extend(item_symbol(
            range_for,
            &alias.name.0,
            SymbolKind::TYPE_PARAMETER,
            alias.extent,
            alias.name_range,
            None,
        ));
    }
    let mut symbol = item_symbol(
        range_for,
        &name,
        SymbolKind::NAMESPACE,
        item.extent,
        item.self_ty_range,
        Some(children),
    )?;
    symbol.detail = trait_name;
    Some(symbol)
}

pub fn document_symbols_for_document_cancellable<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    options: riddlec::pipeline::CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> Result<Option<Vec<DocumentSymbol>>, String> {
    let document = docs
        .get(uri)
        .ok_or_else(|| "document is not open".to_string())?;
    let Some(analysis) = analyze_document_cancellable(
        uri,
        docs,
        options,
        sessions,
        AnalysisDepth::Resolve,
        cancelled,
    )?
    else {
        return Ok(None);
    };
    let Some(hir) = analysis.result.hir.as_ref() else {
        return Ok(Some(Vec::new()));
    };
    let index = LineIndex::new(&document.text);
    Ok(Some(symbols_for_items(
        hir,
        &hir.item_tree.top_level,
        &|range| {
            analysis
                .local_range(range)
                .and_then(|range| index.range(&document.text, range))
        },
    )))
}

pub fn workspace_symbols_for_document_cancellable<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    query: &str,
    options: riddlec::pipeline::CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> Result<Option<Vec<SymbolInformation>>, String> {
    let Some(analysis) = analyze_document_cancellable(
        uri,
        docs,
        options,
        sessions,
        AnalysisDepth::Resolve,
        cancelled,
    )?
    else {
        return Ok(None);
    };
    let Some(hir) = analysis.result.hir.as_ref() else {
        return Ok(Some(Vec::new()));
    };
    let mut symbols = Vec::new();
    collect_workspace_items(
        uri,
        &analysis,
        hir,
        &hir.item_tree.top_level,
        &query.to_lowercase(),
        None,
        &mut symbols,
    );
    Ok(Some(symbols))
}

fn collect_workspace_items(
    current_uri: &lsp_types::Url,
    analysis: &crate::analysis::DocumentAnalysis,
    hir: &hir::HirFile,
    items: &[TopLevelItem],
    query: &str,
    container: Option<&str>,
    output: &mut Vec<SymbolInformation>,
) {
    for item in items {
        collect_workspace_item(current_uri, analysis, hir, *item, query, container, output);
    }
}

fn collect_workspace_item(
    current_uri: &lsp_types::Url,
    analysis: &crate::analysis::DocumentAnalysis,
    hir: &hir::HirFile,
    item: TopLevelItem,
    query: &str,
    container: Option<&str>,
    output: &mut Vec<SymbolInformation>,
) {
    match item {
        TopLevelItem::Function(id) => {
            let item = &hir.item_tree.functions[id];
            push_workspace_symbol(
                current_uri,
                analysis,
                &item.name.0,
                SymbolKind::FUNCTION,
                item.name_range,
                query,
                container,
                output,
            );
        }
        TopLevelItem::Struct(id) => {
            let item = &hir.item_tree.structs[id];
            push_workspace_symbol(
                current_uri,
                analysis,
                &item.name.0,
                SymbolKind::STRUCT,
                item.name_range,
                query,
                container,
                output,
            );
        }
        TopLevelItem::Enum(id) => collect_enum_workspace_items(
            current_uri,
            analysis,
            &hir.item_tree.enums[id],
            query,
            container,
            output,
        ),
        TopLevelItem::Trait(id) => collect_trait_workspace_items(
            current_uri,
            analysis,
            &hir.item_tree.traits[id],
            query,
            container,
            output,
        ),
        TopLevelItem::Module(id) => collect_module_workspace_items(
            current_uri,
            analysis,
            hir,
            &hir.item_tree.modules[id],
            query,
            container,
            output,
        ),
        TopLevelItem::Impl(id) => collect_impl_workspace_items(
            current_uri,
            analysis,
            hir,
            &hir.item_tree.impls[id],
            query,
            output,
        ),
        TopLevelItem::Const(id) => {
            let item = &hir.item_tree.consts[id];
            push_workspace_symbol(
                current_uri,
                analysis,
                &item.name.0,
                SymbolKind::CONSTANT,
                item.name_range,
                query,
                container,
                output,
            );
        }
        TopLevelItem::TypeAlias(id) => {
            let item = &hir.item_tree.type_aliases[id];
            push_workspace_symbol(
                current_uri,
                analysis,
                &item.name.0,
                SymbolKind::TYPE_PARAMETER,
                item.name_range,
                query,
                container,
                output,
            );
        }
        TopLevelItem::Use(_) => {}
    }
}

fn collect_enum_workspace_items(
    current_uri: &lsp_types::Url,
    analysis: &crate::analysis::DocumentAnalysis,
    item: &hir::item_tree::HirEnum,
    query: &str,
    container: Option<&str>,
    output: &mut Vec<SymbolInformation>,
) {
    push_workspace_symbol(
        current_uri,
        analysis,
        &item.name.0,
        SymbolKind::ENUM,
        item.name_range,
        query,
        container,
        output,
    );
    for variant in &item.variants {
        push_workspace_symbol(
            current_uri,
            analysis,
            &variant.name.0,
            SymbolKind::ENUM_MEMBER,
            variant.name_range,
            query,
            Some(&item.name.0),
            output,
        );
    }
}

fn collect_trait_workspace_items(
    current_uri: &lsp_types::Url,
    analysis: &crate::analysis::DocumentAnalysis,
    item: &hir::item_tree::HirTrait,
    query: &str,
    container: Option<&str>,
    output: &mut Vec<SymbolInformation>,
) {
    push_workspace_symbol(
        current_uri,
        analysis,
        &item.name.0,
        SymbolKind::INTERFACE,
        item.name_range,
        query,
        container,
        output,
    );
    for method in &item.methods {
        push_workspace_symbol(
            current_uri,
            analysis,
            &method.name.0,
            SymbolKind::METHOD,
            method.name_range,
            query,
            Some(&item.name.0),
            output,
        );
    }
}

fn collect_module_workspace_items(
    current_uri: &lsp_types::Url,
    analysis: &crate::analysis::DocumentAnalysis,
    hir: &hir::HirFile,
    item: &hir::item_tree::HirModule,
    query: &str,
    container: Option<&str>,
    output: &mut Vec<SymbolInformation>,
) {
    push_workspace_symbol(
        current_uri,
        analysis,
        &item.name.0,
        SymbolKind::MODULE,
        item.name_range,
        query,
        container,
        output,
    );
    if let Some(items) = &item.items {
        collect_workspace_items(
            current_uri,
            analysis,
            hir,
            items,
            query,
            Some(&item.name.0),
            output,
        );
    }
}

fn collect_impl_workspace_items(
    current_uri: &lsp_types::Url,
    analysis: &crate::analysis::DocumentAnalysis,
    hir: &hir::HirFile,
    item: &hir::item_tree::HirImpl,
    query: &str,
    output: &mut Vec<SymbolInformation>,
) {
    let container = format!("impl {}", item.self_ty.display());
    for method in &item.methods {
        let method = &hir.item_tree.functions[*method];
        push_workspace_symbol(
            current_uri,
            analysis,
            &method.name.0,
            SymbolKind::METHOD,
            method.name_range,
            query,
            Some(&container),
            output,
        );
    }
}

#[allow(deprecated, clippy::too_many_arguments)]
fn push_workspace_symbol(
    current_uri: &lsp_types::Url,
    analysis: &crate::analysis::DocumentAnalysis,
    name: &str,
    kind: SymbolKind,
    range: rowan::TextRange,
    query: &str,
    container: Option<&str>,
    output: &mut Vec<SymbolInformation>,
) {
    if !name.to_lowercase().contains(query) {
        return;
    }
    let location = if let Some(source_map) = &analysis.source_map {
        let Some(mapped) = source_map.map_range(range) else {
            return;
        };
        let Some(uri) = source_uri(current_uri, mapped.path) else {
            return;
        };
        let Some(range) = LineIndex::new(mapped.source).range(mapped.source, mapped.range) else {
            return;
        };
        Location::new(uri, range)
    } else {
        let Some(range) = LineIndex::new(&analysis.source).range(&analysis.source, range) else {
            return;
        };
        Location::new(current_uri.clone(), range)
    };
    output.push(SymbolInformation {
        name: name.into(),
        kind,
        tags: None,
        deprecated: None,
        location,
        container_name: container.map(str::to_string),
    });
}
