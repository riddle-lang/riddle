use std::{
    collections::{HashMap, HashSet},
    hash::BuildHasher,
    path::{Path, PathBuf},
};

use ast::{ImplDecl, support::AstNode};
use hir::body::{BodyId, Expr, Pattern, ResolvedName, Stmt};
use hir::item_tree::{
    HirAttr, HirFunction, HirTypeRef, HirUseTree, HirUseTreeKind, StructId, TopLevelItem,
    Visibility,
};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionTextEdit,
    InsertTextFormat, TextEdit,
};
use riddlec::{
    pipeline::{CompileOptions, CompileResult},
    proc_macro::{ProcMacroKind, STANDARD_DERIVE_MACROS, STANDARD_FUNCTION_MACROS},
};
use rowan::TextRange;
use scope_graph::resolve::{
    exported_definitions, resolve_path_at_reference, resolve_path_from, visible_definitions,
    visible_definitions_from,
};
use scope_graph::{DefRef, Node, NodeId, RefOrigin, ScopeGraph};
use syntax::SyntaxKind;

use crate::{
    code_actions::trait_method_signature,
    imports::{import_edit, parse_root},
    server::{Document, Phase, measure},
    session::AnalysisSessions,
    text::{LineIndex, is_identifier_continue, offset_for_position, text_range, text_size},
};

const COMPLETION_MARKER: &str = "__riddle_completion";
const COMPLETION_KEYWORDS: &[&str] = &[
    "let", "fun", "struct", "if", "else", "while", "loop", "break", "continue", "return", "as",
    "self", "mod", "use", "mut", "pub", "super", "crate", "enum", "trait", "impl", "match",
    "const", "type", "extern", "unsafe", "safe", "for", "in", "where", "move", "true", "false",
];
pub const BUILTIN_TYPES: &[&str] = &[
    "bool", "char", "str", "i8", "i16", "i32", "i64", "isize", "u8", "u16", "u32", "u64", "usize",
    "f32", "f64",
];

pub fn completion_trigger_characters() -> Vec<String> {
    vec![".".into(), ":".into()]
}

#[must_use]
pub fn completion_trigger_is_active(source: &str, position: lsp_types::Position) -> bool {
    let Some(offset) = offset_for_position(source, position) else {
        return false;
    };
    let start = identifier_start(source, offset);
    let before = &source[..start];
    before.ends_with('.') || before.ends_with("::")
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CompletionContext {
    General,
    Type,
    TypePath,
    Member,
    Associated,
    Import,
    StructField,
    PatternField,
    ImplBody,
}

/// What [`completion_site`] precomputes for an `ImplBody` context: the trait
/// being implemented (by its simple name) and the members the impl already
/// provides, so missing trait members can be offered.
struct ImplBodyInfo {
    trait_name: String,
    method_names: Vec<String>,
    type_alias_names: Vec<String>,
}

struct CompletionSite {
    start: usize,
    end: usize,
    prefix: String,
    context: CompletionContext,
    macro_kind: Option<ProcMacroKind>,
    stmt_start: bool,
    impl_body: Option<ImplBodyInfo>,
}

/// Computes completion items for an open document.
///
/// # Errors
///
/// Returns an error when the document is unavailable or project analysis fails.
pub fn completion_items_for_document<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: lsp_types::Position,
    compile_options: CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: impl Fn() -> bool,
) -> std::result::Result<Option<Vec<CompletionItem>>, String> {
    if cancelled() {
        return Ok(None);
    }
    let document = docs
        .get(uri)
        .ok_or_else(|| "document is not open".to_string())?;
    let _site_phase = Phase::start("completion.site");
    let site = completion_site(&document.text, position)
        .ok_or_else(|| "completion position is outside the document".to_string())?;
    let mut marked = marked_completion_source(&document.text, &site);
    drop(_site_phase);

    if let Some((path, mut overlays)) = project_completion_overlays(uri, docs, &marked)
        && let Some(root) = clue::find_project_root(&path)
    {
        let session = sessions.project(&root);
        // Waiting for the project session is invisible to the phase timings
        // that follow, and it is where a completion lands whenever the
        // diagnostics pass holds the same session.
        let mut session = measure("completion.session_lock_wait", || {
            session
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
        });
        if cancelled() {
            return Ok(None);
        }
        let mut analyze = |overlays: &HashMap<PathBuf, String>| {
            if completion_needs_type_check(site.context) {
                clue::infer_project_with_session_cancellable(
                    &root,
                    overlays,
                    compile_options,
                    &mut session,
                    &cancelled,
                )
            } else {
                clue::resolve_project_with_session_cancellable(
                    &root,
                    overlays,
                    compile_options,
                    &mut session,
                    &cancelled,
                )
            }
        };
        let Some(mut analysis) = measure("completion.primary_analysis", || {
            analyze(&overlays).map_err(|error| error.to_string())
        })?
        else {
            return Ok(None);
        };
        if analysis.result.hir.is_none() && completion_needs_semicolon(&marked, &site) {
            marked.insert(site.start + COMPLETION_MARKER.len(), ';');
            overlays.insert(path.clone(), marked.clone());
            let Some(recovered) = measure("completion.recovery_analysis", || {
                analyze(&overlays).map_err(|error| error.to_string())
            })?
            else {
                return Ok(None);
            };
            analysis = recovered;
        }
        if cancelled() {
            return Ok(None);
        }
        // If the marker wasn't resolved as a scope-graph reference (e.g. the
        // cursor is at the start of a fresh statement), re-analyse the original
        // source to recover global-scope completions.
        let fallback_result = if matches!(
            site.context,
            CompletionContext::General | CompletionContext::Type
        ) && analysis
            .result
            .hir
            .as_ref()
            .zip(analysis.result.scope_graph.as_ref())
            .is_some_and(|(_, graph)| completion_marker_reference(graph).is_none())
        {
            // The fallback runs in the same session as the analysis above, so
            // the session lock has to be released first: a second `lock()` on a
            // guard still held here waits on itself and never returns. Reusing
            // one session is safe because analyses are cached by input
            // fingerprint, so alternating sources do not evict each other.
            drop(session);
            let session = sessions.project(&root);
            let mut fb_session = session
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            // Restore the original (non-marked) overlay so the fallback sees
            // the unmodified source.
            let mut fb_overlays = overlays.clone();
            fb_overlays.insert(path.clone(), document.text.clone());
            measure("completion.fallback_analysis", || {
                clue::resolve_project_with_session_cancellable(
                    &root,
                    &fb_overlays,
                    compile_options,
                    &mut fb_session,
                    &cancelled,
                )
                .ok()
                .flatten()
            })
        } else {
            None
        };
        if cancelled() {
            return Ok(None);
        }
        let mut items = measure("completion.collect", || {
            project_completion_items(
                &analysis,
                &path,
                &document.text,
                &site,
                fallback_result.as_ref(),
            )
        });
        attach_completion_edits(&document.text, &site, &mut items);
        return Ok(Some(items));
    }

    let mut items =
        standalone_completion_items(uri, document, &site, compile_options, sessions, &cancelled);
    if let Some(items) = &mut items {
        attach_completion_edits(&document.text, &site, items);
    }
    Ok(items)
}

fn project_completion_overlays<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    marked: &str,
) -> Option<(PathBuf, HashMap<PathBuf, String>)> {
    let path = uri.to_file_path().ok()?;
    let mut overlays = docs
        .iter()
        .filter_map(|(uri, document)| {
            uri.to_file_path()
                .ok()
                .map(|path| (path, document.text.clone()))
        })
        .collect::<HashMap<_, _>>();
    overlays.insert(path.clone(), marked.to_owned());
    Some((path, overlays))
}

fn project_completion_items(
    analysis: &clue::ProjectAnalysis,
    path: &Path,
    document_source: &str,
    site: &CompletionSite,
    fallback: Option<&clue::ProjectAnalysis>,
) -> Vec<CompletionItem> {
    let mut items = completion_items_from_result(
        &analysis.result,
        site,
        fallback.and_then(|analysis| analysis.result.hir.as_ref()),
    );
    collect_standard_macro_completions(site, &mut items);
    collect_macro_completions(analysis, path, site, &mut items);
    rank_existing_completions(site, &mut items);
    collect_auto_import_completions(&analysis.result, document_source, site, &mut items);
    sort_and_dedup_completion_items(&mut items);
    items
}

fn standalone_completion_items(
    uri: &lsp_types::Url,
    document: &Document,
    site: &CompletionSite,
    compile_options: CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> Option<Vec<CompletionItem>> {
    let mut marked = marked_completion_source(&document.text, site);
    let session = sessions.standalone(uri);
    let mut session = session
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if cancelled() {
        return None;
    }
    let result = if completion_needs_type_check(site.context) {
        session.infer_with_options_cancellable(&marked, compile_options, cancelled)
    } else {
        session.resolve_with_options_cancellable(&marked, compile_options, cancelled)
    };
    let mut result = result?;
    if result.hir.is_none() && completion_needs_semicolon(&marked, site) {
        marked.insert(site.start + COMPLETION_MARKER.len(), ';');
        let recovered = if completion_needs_type_check(site.context) {
            session.infer_with_options_cancellable(&marked, compile_options, cancelled)
        } else {
            session.resolve_with_options_cancellable(&marked, compile_options, cancelled)
        };
        result = recovered?;
    }
    if cancelled() {
        return None;
    }
    // Fallback: if the marker wasn't resolved as a reference in the scope graph
    // (e.g. typing at the start of a statement), re-analyse the original source
    // so that global definitions are still offered. The session lock is dropped
    // first and re-taken for the second pass: locking the guard held above again
    // waits on itself forever.
    let fallback_result = if matches!(
        site.context,
        CompletionContext::General | CompletionContext::Type
    ) && result
        .hir
        .as_ref()
        .zip(result.scope_graph.as_ref())
        .is_some_and(|(_, graph)| completion_marker_reference(graph).is_none())
    {
        drop(session);
        let fallback = sessions.standalone(uri);
        let mut fallback = fallback
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        fallback.resolve_with_options_cancellable(&document.text, compile_options, cancelled)
    } else {
        None
    };
    if cancelled() {
        return None;
    }
    let mut items = completion_items_from_result(
        &result,
        site,
        fallback_result.as_ref().and_then(|r| r.hir.as_ref()),
    );
    collect_standard_macro_completions(site, &mut items);
    sort_and_dedup_completion_items(&mut items);
    Some(items)
}

fn attach_completion_edits(source: &str, site: &CompletionSite, items: &mut [CompletionItem]) {
    let Some(range) = LineIndex::new(source).range(source, text_range(site.start, site.end)) else {
        return;
    };
    for item in items {
        let new_text = item
            .insert_text
            .clone()
            .unwrap_or_else(|| item.label.clone());
        item.text_edit = Some(CompletionTextEdit::Edit(TextEdit::new(range, new_text)));
        if item.insert_text_format != Some(InsertTextFormat::SNIPPET) {
            item.insert_text_format = Some(InsertTextFormat::PLAIN_TEXT);
        }
    }
}

fn sort_and_dedup_completion_items(items: &mut Vec<CompletionItem>) {
    items.sort_by(|left, right| {
        left.label
            .cmp(&right.label)
            .then_with(|| completion_description(left).cmp(completion_description(right)))
    });
    items.dedup_by(|left, right| {
        left.label == right.label
            && left.kind == right.kind
            && completion_description(left) == completion_description(right)
    });
}

fn completion_description(item: &CompletionItem) -> &str {
    item.label_details
        .as_ref()
        .and_then(|details| details.description.as_deref())
        .unwrap_or_default()
}

fn rank_existing_completions(site: &CompletionSite, items: &mut [CompletionItem]) {
    let group = match site.context {
        CompletionContext::TypePath
        | CompletionContext::Member
        | CompletionContext::Associated
        | CompletionContext::Import
        | CompletionContext::StructField
        | CompletionContext::PatternField
        | CompletionContext::ImplBody => 0,
        CompletionContext::General | CompletionContext::Type => 1,
    };
    for item in items {
        item.filter_text.get_or_insert_with(|| item.label.clone());
        item.sort_text.get_or_insert_with(|| {
            let group = if item.kind == Some(CompletionItemKind::KEYWORD) {
                2
            } else {
                group
            };
            format!("{group}:{}", item.label)
        });
    }
}

#[derive(Clone)]
struct AutoImportRoute {
    key: (u8, u32, u32),
    label: String,
    path: String,
    definition: DefRef,
}

fn collect_auto_import_completions(
    result: &CompileResult,
    source: &str,
    site: &CompletionSite,
    out: &mut Vec<CompletionItem>,
) {
    if !matches!(
        site.context,
        CompletionContext::General | CompletionContext::Type
    ) {
        return;
    }
    let Some((hir, graph)) = result.hir.as_ref().zip(result.scope_graph.as_ref()) else {
        return;
    };
    let visible = out
        .iter()
        .map(|item| item.label.clone())
        .collect::<HashSet<_>>();
    let prefix = site.prefix.to_lowercase();

    for route in auto_import_routes(hir, graph) {
        if !definition_is_available_for_completion(hir, graph, &route.definition)
            || visible.contains(&route.label)
            || !route.label.to_lowercase().starts_with(&prefix)
            || (site.context == CompletionContext::Type && !definition_is_type(&route.definition))
        {
            continue;
        }
        let Some(edit) = import_edit(source, &route.path) else {
            continue;
        };
        let mut item = Vec::with_capacity(1);
        push_definition_completion(hir, None, &route.label, &route.definition, &mut item);
        let Some(mut item) = item.pop() else {
            continue;
        };
        let label_detail = item.label_details.take().and_then(|details| details.detail);
        item.label_details = Some(CompletionItemLabelDetails {
            detail: label_detail,
            description: Some(route.path.clone()),
        });
        item.insert_text = Some(route.label.clone());
        item.filter_text = Some(route.label.clone());
        item.sort_text = Some(format!("3:{}:{}", route.label, route.path));
        item.additional_text_edits = Some(vec![edit]);
        out.push(item);
    }
}

/// An import route stripped of completion-only detail, shared with the
/// unresolved-name quick fixes.
pub(crate) struct ImportRoute {
    pub(crate) label: String,
    pub(crate) path: String,
}

/// Import routes that bring an item named `name` into scope, hidden and
/// unavailable items filtered out.
pub(crate) fn import_routes_for_name(
    hir: &hir::HirFile,
    graph: &ScopeGraph,
    name: &str,
) -> Vec<ImportRoute> {
    auto_import_routes(hir, graph)
        .into_iter()
        .filter(|route| route.label == name)
        .filter(|route| definition_is_available_for_completion(hir, graph, &route.definition))
        .take(8)
        .map(|route| ImportRoute {
            label: route.label,
            path: route.path,
        })
        .collect()
}

fn auto_import_routes(hir: &hir::HirFile, graph: &ScopeGraph) -> Vec<AutoImportRoute> {
    let mut routes = Vec::new();
    collect_import_routes(
        hir,
        graph,
        graph.root,
        &mut Vec::new(),
        &mut HashSet::new(),
        &mut routes,
        64,
    );
    routes.sort_by(|left, right| {
        left.key
            .cmp(&right.key)
            .then_with(|| left.label.cmp(&right.label))
            .then_with(|| {
                left.path
                    .split("::")
                    .count()
                    .cmp(&right.path.split("::").count())
            })
            .then_with(|| left.path.cmp(&right.path))
    });
    routes.dedup_by(|left, right| left.key == right.key && left.label == right.label);
    routes.sort_by(|left, right| {
        left.label
            .cmp(&right.label)
            .then_with(|| left.path.cmp(&right.path))
    });
    routes
}

fn collect_import_routes(
    hir: &hir::HirFile,
    graph: &ScopeGraph,
    scope: NodeId,
    path: &mut Vec<String>,
    active: &mut HashSet<NodeId>,
    out: &mut Vec<AutoImportRoute>,
    fuel: u32,
) {
    if fuel == 0 || !active.insert(scope) {
        return;
    }
    for (name, definition) in exported_definitions(graph, scope) {
        path.push(name.0.clone());
        match definition {
            DefRef::Module { enter, .. } => {
                if let Some(key) = definition_key(hir, &definition) {
                    out.push(AutoImportRoute {
                        key,
                        label: name.0,
                        path: path.join("::"),
                        definition: definition.clone(),
                    });
                }
                collect_import_routes(hir, graph, enter, path, active, out, fuel - 1);
            }
            DefRef::UseAlias {
                ref rewrite_to,
                anchor,
                ..
            } => {
                for resolved in resolve_path_from(graph, anchor, rewrite_to) {
                    if let DefRef::Module { enter, .. } = resolved {
                        collect_import_routes(hir, graph, enter, path, active, out, fuel - 1);
                    } else if let Some(key) = definition_key(hir, &resolved) {
                        out.push(AutoImportRoute {
                            key,
                            label: name.0.clone(),
                            path: path.join("::"),
                            definition: resolved,
                        });
                    }
                }
            }
            definition => {
                if let Some(key) = definition_key(hir, &definition) {
                    out.push(AutoImportRoute {
                        key,
                        label: name.0,
                        path: path.join("::"),
                        definition,
                    });
                }
            }
        }
        path.pop();
    }
    active.remove(&scope);
}

fn definition_key(hir: &hir::HirFile, definition: &DefRef) -> Option<(u8, u32, u32)> {
    let (kind, range) = match definition {
        DefRef::Function(id) => (0, hir.item_tree.functions[*id].name_range),
        DefRef::Struct(id) => (1, hir.item_tree.structs[*id].name_range),
        DefRef::Enum(id) => (2, hir.item_tree.enums[*id].name_range),
        DefRef::Trait(id) => (3, hir.item_tree.traits[*id].name_range),
        DefRef::Const(id) => (4, hir.item_tree.consts[*id].name_range),
        DefRef::TypeAlias(id) => (5, hir.item_tree.type_aliases[*id].name_range),
        DefRef::Module { id, .. } => (6, hir.item_tree.modules[*id].name_range),
        DefRef::EnumVariant { enum_id, index } => {
            (7, hir.item_tree.enums[*enum_id].variants[*index].name_range)
        }
        DefRef::PatternBinding { .. }
        | DefRef::Param { .. }
        | DefRef::LambdaParam { .. }
        | DefRef::ConstParam { .. }
        | DefRef::UseAlias { .. } => return None,
    };
    Some((kind, range.start().into(), range.end().into()))
}

const fn definition_is_type(definition: &DefRef) -> bool {
    matches!(
        definition,
        DefRef::Struct(_)
            | DefRef::Enum(_)
            | DefRef::Trait(_)
            | DefRef::TypeAlias(_)
            | DefRef::Module { .. }
    )
}

#[cfg(feature = "test")]
#[must_use]
pub fn completion_items_for_source(
    source: &str,
    position: lsp_types::Position,
    compile_options: CompileOptions,
) -> Vec<CompletionItem> {
    let Some(site) = completion_site(source, position) else {
        return Vec::new();
    };
    let mut analyzed_source = marked_completion_source(source, &site);
    if site.context == CompletionContext::ImplBody {
        // A bare identifier is not a valid impl member, so the marker would
        // break parsing and take the item tree down with it. Substitute a
        // well-formed placeholder method instead; the marker reference is not
        // used in this context.
        let marker_range = site.start..site.start + COMPLETION_MARKER.len();
        analyzed_source.replace_range(marker_range, "fun __riddle_completion() {}");
    }
    let analyze = if completion_needs_type_check(site.context) {
        riddlec::pipeline::check_with_options
    } else {
        riddlec::pipeline::resolve_with_options
    };
    let mut resolved = analyze(&analyzed_source, compile_options);
    let marker_end = site.start + COMPLETION_MARKER.len();
    if resolved.hir.is_none()
        && site.context != CompletionContext::ImplBody
        && !analyzed_source[marker_end..].trim_start().starts_with(';')
    {
        analyzed_source.insert(marker_end, ';');
        resolved = analyze(&analyzed_source, compile_options);
    }
    let fallback = (matches!(
        site.context,
        CompletionContext::General | CompletionContext::Type
    ) && resolved
        .hir
        .as_ref()
        .zip(resolved.scope_graph.as_ref())
        .is_some_and(|(_, graph)| completion_marker_reference(graph).is_none()))
    .then(|| riddlec::pipeline::resolve_with_options(source, compile_options));
    let mut items = completion_items_from_result(
        &resolved,
        &site,
        fallback.as_ref().and_then(|result| result.hir.as_ref()),
    );
    collect_standard_macro_completions(&site, &mut items);
    attach_completion_edits(source, &site, &mut items);
    items
}

fn completion_site(source: &str, position: lsp_types::Position) -> Option<CompletionSite> {
    let offset = offset_for_position(source, position)?;
    let start = identifier_start(source, offset);
    let end = identifier_end(source, offset);
    let before = &source[..start];
    let syntax_context = syntax_completion_context(source, start, end);
    let context = if before.ends_with('.') {
        CompletionContext::Member
    } else if matches!(
        syntax_context,
        Some(CompletionContext::StructField | CompletionContext::PatternField)
    ) {
        syntax_context.unwrap()
    } else if syntax_context == Some(CompletionContext::Import) {
        CompletionContext::Import
    } else if before.ends_with("::") {
        if syntax_context == Some(CompletionContext::Type) {
            CompletionContext::TypePath
        } else {
            CompletionContext::Associated
        }
    } else {
        syntax_context.unwrap_or(CompletionContext::General)
    };
    Some(CompletionSite {
        start,
        end,
        prefix: source[start..offset].into(),
        context,
        macro_kind: macro_completion_kind(source, start, end),
        stmt_start: stmt_start_position(before),
        impl_body: (context == CompletionContext::ImplBody)
            .then(|| impl_body_info(source, start))
            .flatten(),
    })
}

/// Statement and item starts follow a block brace or a semicolon (or begin the
/// file); everywhere else keywords like `let` or `struct` would be noise.
fn stmt_start_position(before: &str) -> bool {
    before
        .trim_end()
        .chars()
        .next_back()
        .is_none_or(|ch| matches!(ch, '{' | '}' | ';'))
}

/// Resolves the enclosing `impl` block for an `ImplBody` completion site.
fn impl_body_info(source: &str, offset: usize) -> Option<ImplBodyInfo> {
    let root = parse_root(source)?;
    let impl_decl = root
        .syntax()
        .descendants()
        .filter_map(ImplDecl::cast)
        .find(|decl| decl.syntax().text_range().contains(text_size(offset)))?;
    // `impl Trait for Target`: the first type child is the trait, the second
    // the implementing type (matching the HIR lowering).
    let trait_ast = impl_decl.has_for().then(|| impl_decl.self_type()).flatten();
    let trait_text = trait_ast?.syntax().text().to_string();
    let trait_name = trait_text
        .rsplit("::")
        .next()?
        .split('<')
        .next()?
        .trim()
        .to_string();
    let method_names = impl_decl
        .methods()
        .filter_map(|method| method.name().map(|name| name.to_string()))
        .collect();
    let type_alias_names = impl_decl
        .type_aliases()
        .filter_map(|alias| alias.name().map(|name| name.to_string()))
        .collect();
    Some(ImplBodyInfo {
        trait_name,
        method_names,
        type_alias_names,
    })
}

const fn completion_needs_type_check(context: CompletionContext) -> bool {
    matches!(
        context,
        CompletionContext::Member | CompletionContext::PatternField
    )
}

fn macro_completion_kind(source: &str, start: usize, end: usize) -> Option<ProcMacroKind> {
    let mut marked = source.to_string();
    marked.replace_range(start..end, COMPLETION_MARKER);
    let tokens = frontend::lexer::lex(&marked);
    let (events, tokens, errors, parsed_source) =
        frontend::parser::Parser::new(&marked, tokens).parse();
    let parse = frontend::tree_builder::build_tree(&events, &tokens, parsed_source, errors);
    let target_range = text_range(start, start + COMPLETION_MARKER.len());
    let marker_token = parse
        .syntax()
        .descendants_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .find(|token| token.text_range() == target_range)?;
    for node in marker_token.parent_ancestors() {
        match node.kind() {
            SyntaxKind::MacroCall => return Some(ProcMacroKind::FunctionLike),
            SyntaxKind::Attribute => {
                let compact = node
                    .text()
                    .to_string()
                    .chars()
                    .filter(|character| !character.is_whitespace())
                    .collect::<String>();
                return Some(if compact.starts_with("#[derive(") {
                    ProcMacroKind::Derive
                } else {
                    ProcMacroKind::Attribute
                });
            }
            _ => {}
        }
    }
    None
}

fn collect_macro_completions(
    analysis: &clue::ProjectAnalysis,
    path: &Path,
    site: &CompletionSite,
    items: &mut Vec<CompletionItem>,
) {
    let needs_bang = site.macro_kind.is_none();
    let kind = match (site.macro_kind, site.context) {
        (Some(kind), _) => kind,
        (None, CompletionContext::General) => ProcMacroKind::FunctionLike,
        _ => return,
    };
    let path = crate::text::normalized_path(path.to_owned());
    for occurrence in analysis
        .macro_occurrences
        .iter()
        .filter(|occurrence| occurrence.is_declaration && occurrence.kind == kind)
    {
        let range = text_range(occurrence.range.start, occurrence.range.end);
        let Some(mapped) = analysis.macro_source_map.map_range(range) else {
            continue;
        };
        if crate::text::normalized_path(mapped.path.to_owned()) != path
            || !occurrence
                .name
                .to_lowercase()
                .starts_with(&site.prefix.to_lowercase())
        {
            continue;
        }
        let detail = match kind {
            ProcMacroKind::Derive => "derive proc macro",
            ProcMacroKind::Attribute => "attribute proc macro",
            ProcMacroKind::FunctionLike => "function-like proc macro",
        };
        items.push(if kind == ProcMacroKind::FunctionLike {
            function_like_macro_completion(&occurrence.name, detail, needs_bang)
        } else {
            completion_item(
                &occurrence.name,
                CompletionItemKind::FUNCTION,
                Some(detail.into()),
            )
        });
    }
}

fn collect_standard_macro_completions(site: &CompletionSite, items: &mut Vec<CompletionItem>) {
    match (site.macro_kind, site.context) {
        (Some(ProcMacroKind::FunctionLike), _) | (None, CompletionContext::General) => {
            for name in STANDARD_FUNCTION_MACROS {
                if name.to_lowercase().starts_with(&site.prefix.to_lowercase()) {
                    items.push(function_like_macro_completion(
                        name,
                        "standard macro",
                        site.macro_kind.is_none(),
                    ));
                }
            }
        }
        (Some(ProcMacroKind::Derive), _) => {
            for name in STANDARD_DERIVE_MACROS {
                if name.to_lowercase().starts_with(&site.prefix.to_lowercase()) {
                    items.push(completion_item(
                        name,
                        CompletionItemKind::FUNCTION,
                        Some("standard derive macro".into()),
                    ));
                }
            }
        }
        _ => {}
    }
}

fn function_like_macro_completion(name: &str, detail: &str, needs_bang: bool) -> CompletionItem {
    let label = if needs_bang {
        format!("{name}!")
    } else {
        name.into()
    };
    let mut item = completion_item(&label, CompletionItemKind::FUNCTION, Some(detail.into()));
    item.filter_text = Some(name.into());
    item.insert_text = Some(label);
    item
}

fn syntax_completion_context(source: &str, start: usize, end: usize) -> Option<CompletionContext> {
    let mut marked = source.to_string();
    marked.replace_range(start..end, COMPLETION_MARKER);
    let tokens = frontend::lexer::lex(&marked);
    let (events, tokens, errors, parsed_source) =
        frontend::parser::Parser::new(&marked, tokens).parse();
    let parse = frontend::tree_builder::build_tree(&events, &tokens, parsed_source, errors);
    let target_range = text_range(start, start + COMPLETION_MARKER.len());
    let marker_token = parse
        .syntax()
        .descendants_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .find(|token| token.text_range() == target_range)?;

    if let Some(parent) = marker_token.parent() {
        match parent.kind() {
            SyntaxKind::StructExprField => return Some(CompletionContext::StructField),
            SyntaxKind::StructPattern => return Some(CompletionContext::PatternField),
            _ => {}
        }
    }

    for node in marker_token.parent_ancestors() {
        match node.kind() {
            SyntaxKind::UseTree => return Some(CompletionContext::Import),
            SyntaxKind::NamedType => return Some(CompletionContext::Type),
            // Completing inside a method of the impl is regular expression
            // completion; only positions directly in the impl body offer the
            // trait's missing members.
            SyntaxKind::FuncDecl => return None,
            SyntaxKind::ImplDecl => return Some(CompletionContext::ImplBody),
            _ => {}
        }
    }
    None
}

fn marked_completion_source(source: &str, site: &CompletionSite) -> String {
    let mut marked = source.to_string();
    marked.replace_range(site.start..site.end, COMPLETION_MARKER);
    marked
}

/// Whether appending a semicolon could make the marked source parse.
///
/// The recovery pass re-analyses the whole project, so its guard has to be
/// exact: a `false` here silently loses every completion item at a statement
/// end, because the recovery analysis is what produces a usable HIR.
fn completion_needs_semicolon(marked: &str, site: &CompletionSite) -> bool {
    !marked[site.start + COMPLETION_MARKER.len()..]
        .trim_start()
        .starts_with(';')
}

fn completion_items_from_result(
    resolved: &CompileResult,
    site: &CompletionSite,
    fallback_hir: Option<&hir::HirFile>,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    if site.context == CompletionContext::General {
        // Declarations (`struct`, `impl`, …) only make sense at statement and
        // item starts; expression positions get the control/value keywords.
        items.extend(
            COMPLETION_KEYWORDS
                .iter()
                .filter(|keyword| site.stmt_start || !DECLARATION_KEYWORDS.contains(keyword))
                .map(|keyword| {
                    completion_item(keyword, CompletionItemKind::KEYWORD, Some("keyword".into()))
                }),
        );
        if site.stmt_start {
            items.extend(statement_snippet_items());
        }
    }
    if matches!(
        site.context,
        CompletionContext::General | CompletionContext::Type
    ) {
        items.extend(BUILTIN_TYPES.iter().map(|ty| {
            completion_item(
                ty,
                CompletionItemKind::TYPE_PARAMETER,
                Some("builtin type".into()),
            )
        }));
    }

    // Missing trait members only need the item tree, not resolved references.
    if site.context == CompletionContext::ImplBody
        && let Some(info) = &site.impl_body
        && let Some(hir) = resolved.hir.as_ref().or(fallback_hir)
    {
        collect_missing_trait_members(hir, info, &mut items);
    }

    if let (Some(hir), Some(scope_graph)) = (resolved.hir.as_ref(), resolved.scope_graph.as_ref()) {
        let marker = completion_marker_reference(scope_graph);
        match site.context {
            CompletionContext::General => {
                if let Some(marker) = marker {
                    collect_visible_completions(
                        hir,
                        scope_graph,
                        marker.reference,
                        marker.body,
                        &mut items,
                    );
                } else {
                    collect_global_completions(fallback_hir.unwrap_or(hir), &mut items);
                }
            }
            CompletionContext::Type => {
                collect_type_completions(hir, scope_graph, marker, fallback_hir, &mut items);
            }
            CompletionContext::TypePath => {
                if let Some(marker) = marker {
                    let mut candidates = Vec::new();
                    collect_resolved_associated_completions(
                        hir,
                        scope_graph,
                        marker.reference,
                        marker.body,
                        marker.segments,
                        &mut candidates,
                    );
                    items.extend(candidates.into_iter().filter(type_completion_item));
                }
            }
            CompletionContext::Member => {
                let checked_types;
                let types = if resolved.type_result.expr_types.is_empty() {
                    checked_types = type_checker::check_hir(hir);
                    &checked_types
                } else {
                    &resolved.type_result
                };
                collect_member_completions(hir, types, &mut items);
            }
            CompletionContext::Associated => {
                if let Some(marker) = marker {
                    collect_resolved_associated_completions(
                        hir,
                        scope_graph,
                        marker.reference,
                        marker.body,
                        marker.segments,
                        &mut items,
                    );
                }
            }
            CompletionContext::Import => {
                collect_import_completions(hir, scope_graph, &mut items);
            }
            CompletionContext::ImplBody => {
                // Handled before this match: it only needs the item tree.
            }
            CompletionContext::StructField | CompletionContext::PatternField => {
                let checked_types;
                let types = if resolved.type_result.pattern_types.is_empty() {
                    checked_types = type_checker::check_hir(hir);
                    &checked_types
                } else {
                    &resolved.type_result
                };
                collect_struct_field_completions(hir, types, &mut items);
            }
        }
    }

    let normalized_prefix = site.prefix.to_lowercase();
    items.retain(|item| item.label.to_lowercase().starts_with(&normalized_prefix));
    items.sort_by(|left, right| left.label.cmp(&right.label));
    items.dedup_by(|left, right| left.label == right.label && left.kind == right.kind);
    items
}

#[derive(Clone, Copy)]
struct CompletionMarker<'a> {
    reference: NodeId,
    body: Option<BodyId>,
    origin: RefOrigin,
    segments: &'a [hir::Name],
}

fn completion_marker_reference(scope_graph: &ScopeGraph) -> Option<CompletionMarker<'_>> {
    scope_graph.nodes.iter().find_map(|(reference, node)| {
        let Node::Reference {
            segments, origin, ..
        } = node
        else {
            return None;
        };
        (segments
            .last()
            .is_some_and(|name| name.0 == COMPLETION_MARKER))
        .then_some(CompletionMarker {
            reference,
            body: match origin {
                RefOrigin::Expr { body, .. } => Some(*body),
                RefOrigin::Type { .. } => None,
            },
            origin: *origin,
            segments,
        })
    })
}

fn collect_visible_completions(
    hir: &hir::HirFile,
    scope_graph: &ScopeGraph,
    reference: NodeId,
    body: Option<BodyId>,
    out: &mut Vec<CompletionItem>,
) {
    for (name, definition) in visible_definitions(scope_graph, reference) {
        if !definition_is_available_for_completion(hir, scope_graph, &definition) {
            continue;
        }
        push_definition_completion(hir, body, &name.0, &definition, out);
    }
}

fn definition_is_available_for_completion(
    hir: &hir::HirFile,
    scope_graph: &ScopeGraph,
    definition: &DefRef,
) -> bool {
    match definition {
        DefRef::UseAlias {
            rewrite_to, anchor, ..
        } => resolve_path_from(scope_graph, *anchor, rewrite_to)
            .iter()
            .any(|resolved| !definition_is_hidden(hir, resolved)),
        definition => !definition_is_hidden(hir, definition),
    }
}

fn definition_is_hidden(hir: &hir::HirFile, definition: &DefRef) -> bool {
    let (name_range, attrs) = match definition {
        DefRef::Function(id) => {
            let item = &hir.item_tree.functions[*id];
            (item.name_range, item.attrs.as_slice())
        }
        DefRef::Struct(id) => {
            let item = &hir.item_tree.structs[*id];
            (item.name_range, item.attrs.as_slice())
        }
        DefRef::Enum(id) => {
            let item = &hir.item_tree.enums[*id];
            (item.name_range, item.attrs.as_slice())
        }
        DefRef::Trait(id) => {
            let item = &hir.item_tree.traits[*id];
            (item.name_range, item.attrs.as_slice())
        }
        DefRef::Const(id) => {
            let item = &hir.item_tree.consts[*id];
            (item.name_range, item.attrs.as_slice())
        }
        DefRef::TypeAlias(id) => {
            let item = &hir.item_tree.type_aliases[*id];
            (item.name_range, item.attrs.as_slice())
        }
        DefRef::Module { id, .. } => {
            let item = &hir.item_tree.modules[*id];
            (item.name_range, item.attrs.as_slice())
        }
        DefRef::EnumVariant { enum_id, index } => {
            let item = &hir.item_tree.enums[*enum_id].variants[*index];
            (item.name_range, item.attrs.as_slice())
        }
        _ => return false,
    };

    standard_item_is_hidden(hir, name_range, attrs)
}

fn standard_item_is_hidden(hir: &hir::HirFile, name_range: TextRange, attrs: &[HirAttr]) -> bool {
    hir.std_loaded
        && hir.package_for_range(name_range).is_none()
        && attrs
            .iter()
            .any(|attr| attr.name.0 == "doc" && attr.value.as_deref() == Some("hidden"))
}

fn top_level_item_is_hidden(hir: &hir::HirFile, item: TopLevelItem) -> bool {
    match item {
        TopLevelItem::Function(id) => definition_is_hidden(hir, &DefRef::Function(id)),
        TopLevelItem::Struct(id) => definition_is_hidden(hir, &DefRef::Struct(id)),
        TopLevelItem::Module(id) => {
            let item = &hir.item_tree.modules[id];
            standard_item_is_hidden(hir, item.name_range, &item.attrs)
        }
        TopLevelItem::Enum(id) => definition_is_hidden(hir, &DefRef::Enum(id)),
        TopLevelItem::Trait(id) => definition_is_hidden(hir, &DefRef::Trait(id)),
        TopLevelItem::Const(id) => definition_is_hidden(hir, &DefRef::Const(id)),
        TopLevelItem::TypeAlias(id) => definition_is_hidden(hir, &DefRef::TypeAlias(id)),
        TopLevelItem::Use(_) | TopLevelItem::Impl(_) => false,
    }
}

fn push_definition_completion(
    hir: &hir::HirFile,
    body: Option<BodyId>,
    label: &str,
    definition: &DefRef,
    out: &mut Vec<CompletionItem>,
) {
    match definition {
        DefRef::Function(id) => out.push(function_completion_named(
            &hir.item_tree.functions[*id],
            CompletionItemKind::FUNCTION,
            label,
        )),
        DefRef::Struct(id) => out.push(completion_item(
            label,
            CompletionItemKind::STRUCT,
            Some(format!("struct {}", hir.item_tree.structs[*id].name.0)),
        )),
        DefRef::Enum(id) => out.push(completion_item(
            label,
            CompletionItemKind::ENUM,
            Some(format!("enum {}", hir.item_tree.enums[*id].name.0)),
        )),
        DefRef::Trait(id) => out.push(completion_item(
            label,
            CompletionItemKind::INTERFACE,
            Some(format!("trait {}", hir.item_tree.traits[*id].name.0)),
        )),
        DefRef::Const(id) => out.push(completion_item(
            label,
            CompletionItemKind::CONSTANT,
            Some(hir.item_tree.consts[*id].ty.display()),
        )),
        DefRef::TypeAlias(id) => out.push(completion_item(
            label,
            CompletionItemKind::TYPE_PARAMETER,
            hir.item_tree.type_aliases[*id]
                .ty
                .as_ref()
                .map(HirTypeRef::display),
        )),
        DefRef::Module { .. } => out.push(completion_item(
            label,
            CompletionItemKind::MODULE,
            Some(format!("mod {label}")),
        )),
        DefRef::PatternBinding { id, .. } if body.is_some() => {
            // A `let` records its annotation on the statement; other patterns
            // (match arms, `for`) only get their type from inference.
            let ty = let_stmt_of_pattern(&hir.bodies[body.unwrap()], id.pattern)
                .filter(|ty| **ty != HirTypeRef::Unknown)
                .map(HirTypeRef::display);
            out.push(completion_item(label, CompletionItemKind::VARIABLE, ty));
        }
        DefRef::PatternBinding { .. } => {}
        DefRef::Param { fn_id, index } => {
            let param = &hir.item_tree.functions[*fn_id].params[*index];
            out.push(completion_item(
                label,
                CompletionItemKind::VARIABLE,
                Some(param.ty.display()),
            ));
        }
        DefRef::LambdaParam {
            body_id,
            lambda,
            index,
        } => {
            if let Expr::Lambda { params, .. } = &hir.bodies[*body_id].exprs[*lambda] {
                out.push(completion_item(
                    label,
                    CompletionItemKind::VARIABLE,
                    Some(params[*index].ty.display()),
                ));
            }
        }
        DefRef::ConstParam { .. } => out.push(completion_item(
            label,
            CompletionItemKind::TYPE_PARAMETER,
            Some("const parameter".into()),
        )),
        DefRef::UseAlias { .. } => out.push(completion_item(
            label,
            CompletionItemKind::REFERENCE,
            Some(format!("use {label}")),
        )),
        DefRef::EnumVariant { enum_id, index } => {
            let enumeration = &hir.item_tree.enums[*enum_id];
            let variant = &enumeration.variants[*index];
            out.push(completion_item(
                label,
                CompletionItemKind::ENUM_MEMBER,
                Some(format!("{}::{}", enumeration.name.0, variant.name.0)),
            ));
        }
    }
}

/// The type annotation of the `let` whose pattern is `pat`, if any.
fn let_stmt_of_pattern(body: &hir::body::Body, pat: hir::body::PatId) -> Option<&HirTypeRef> {
    body.stmts.iter().find_map(|(_, stmt)| match stmt {
        Stmt::Let {
            pat: let_pat, ty, ..
        } if *let_pat == pat => Some(ty),
        _ => None,
    })
}

fn collect_resolved_associated_completions(
    hir: &hir::HirFile,
    scope_graph: &ScopeGraph,
    reference: NodeId,
    body: Option<BodyId>,
    marker_segments: &[hir::Name],
    out: &mut Vec<CompletionItem>,
) {
    let qualifier = &marker_segments[..marker_segments.len().saturating_sub(1)];
    for definition in resolve_path_at_reference(scope_graph, reference, qualifier) {
        let definitions = match definition {
            DefRef::Module { enter, .. } => exported_definitions(scope_graph, enter),
            DefRef::Struct(id) => scope_graph
                .impl_scopes_by_struct
                .get(&id)
                .map(|scope| exported_definitions(scope_graph, *scope))
                .unwrap_or_default(),
            DefRef::Enum(id) => scope_graph
                .variant_scopes_by_enum
                .get(&id)
                .map(|scope| exported_definitions(scope_graph, *scope))
                .unwrap_or_default(),
            _ => Vec::new(),
        };
        for (name, associated) in definitions {
            if !definition_is_available_for_completion(hir, scope_graph, &associated) {
                continue;
            }
            if let DefRef::Function(id) = &associated
                && !function_is_visible(hir, body, *id)
            {
                continue;
            }
            if let DefRef::Function(id) = &associated
                && hir.item_tree.functions[*id]
                    .params
                    .first()
                    .is_some_and(|param| param.name.0 == "self")
            {
                continue;
            }
            push_definition_completion(hir, body, &name.0, &associated, out);
        }
    }
}

fn collect_import_completions(
    hir: &hir::HirFile,
    scope_graph: &ScopeGraph,
    out: &mut Vec<CompletionItem>,
) {
    let Some((anchor, segments)) = completion_import_marker(scope_graph) else {
        return;
    };
    let qualifier = &segments[..segments.len() - 1];
    let definitions = if qualifier.is_empty() {
        visible_definitions_from(scope_graph, anchor)
    } else {
        resolve_path_from(scope_graph, anchor, qualifier)
            .into_iter()
            .flat_map(|definition| match definition {
                DefRef::Module { enter, .. } => exported_definitions(scope_graph, enter),
                DefRef::Enum(id) => scope_graph
                    .variant_scopes_by_enum
                    .get(&id)
                    .map(|scope| exported_definitions(scope_graph, *scope))
                    .unwrap_or_default(),
                _ => Vec::new(),
            })
            .collect()
    };

    for (name, definition) in definitions {
        if name.0 == COMPLETION_MARKER
            || !definition_is_available_for_completion(hir, scope_graph, &definition)
            || matches!(
                definition,
                DefRef::PatternBinding { .. }
                    | DefRef::Param { .. }
                    | DefRef::LambdaParam { .. }
                    | DefRef::ConstParam { .. }
            )
        {
            continue;
        }
        push_definition_completion(hir, None, &name.0, &definition, out);
    }
}

fn completion_import_marker(scope_graph: &ScopeGraph) -> Option<(NodeId, &[hir::Name])> {
    scope_graph.nodes.iter().find_map(|(_, node)| {
        let Node::PopSymbol {
            name,
            define: DefRef::UseAlias {
                rewrite_to, anchor, ..
            },
        } = node
        else {
            return None;
        };
        (name.0 == COMPLETION_MARKER
            && rewrite_to
                .last()
                .is_some_and(|segment| segment.0 == COMPLETION_MARKER))
        .then_some((*anchor, rewrite_to.as_slice()))
    })
}

fn collect_global_completions(hir: &hir::HirFile, out: &mut Vec<CompletionItem>) {
    for item in &hir.item_tree.top_level {
        push_top_level_item(hir, *item, true, out);
    }
    for (_, use_item) in hir.item_tree.uses.iter() {
        if use_item.visibility.is_public() {
            push_use_tree(&use_item.tree, out);
        }
    }
}

fn collect_type_completions(
    hir: &hir::HirFile,
    scope_graph: &ScopeGraph,
    marker: Option<CompletionMarker<'_>>,
    fallback_hir: Option<&hir::HirFile>,
    out: &mut Vec<CompletionItem>,
) {
    let mut candidates = Vec::new();
    if let Some(marker) = marker {
        collect_visible_completions(
            hir,
            scope_graph,
            marker.reference,
            marker.body,
            &mut candidates,
        );
        collect_generic_type_completions(hir, marker, &mut candidates);
    } else {
        collect_global_completions(fallback_hir.unwrap_or(hir), &mut candidates);
    }
    out.extend(candidates.into_iter().filter(type_completion_item));
}

fn type_completion_item(item: &CompletionItem) -> bool {
    matches!(
        item.kind,
        Some(
            CompletionItemKind::STRUCT
                | CompletionItemKind::ENUM
                | CompletionItemKind::INTERFACE
                | CompletionItemKind::TYPE_PARAMETER
                | CompletionItemKind::MODULE
                | CompletionItemKind::REFERENCE
        )
    )
}

fn collect_generic_type_completions(
    hir: &hir::HirFile,
    marker: CompletionMarker<'_>,
    out: &mut Vec<CompletionItem>,
) {
    let RefOrigin::Type { range } = marker.origin else {
        return;
    };
    for (function_id, function) in hir.item_tree.functions.iter() {
        let contains_marker = function
            .params
            .iter()
            .any(|param| type_ref_contains_range(&param.ty, range))
            || function
                .ret_type
                .as_ref()
                .is_some_and(|ty| type_ref_contains_range(ty, range))
            || function.generic_bounds.iter().any(|bound| {
                type_ref_contains_range(&bound.target_ty, range)
                    || type_ref_contains_range(&bound.trait_ty, range)
            });
        if !contains_marker {
            continue;
        }
        let impl_generics = hir.item_tree.impls.iter().find_map(|(_, implementation)| {
            implementation
                .methods
                .contains(&function_id)
                .then_some(implementation.generics.as_slice())
        });
        for generic in impl_generics
            .into_iter()
            .flatten()
            .chain(function.generics.iter())
        {
            out.push(completion_item(
                &generic.0,
                CompletionItemKind::TYPE_PARAMETER,
                Some("type parameter".into()),
            ));
        }
        return;
    }
}

fn type_ref_contains_range(ty: &HirTypeRef, range: TextRange) -> bool {
    match ty {
        HirTypeRef::Named(path) => {
            path.range == range
                || path
                    .type_args
                    .iter()
                    .any(|arg| type_ref_contains_range(arg, range))
        }
        HirTypeRef::Ref(inner, _)
        | HirTypeRef::Ptr { inner, .. }
        | HirTypeRef::Slice(inner)
        | HirTypeRef::Array(inner, _) => type_ref_contains_range(inner, range),
        HirTypeRef::Tuple(elements) => elements
            .iter()
            .any(|element| type_ref_contains_range(element, range)),
        HirTypeRef::ImplTrait {
            trait_ty, callable, ..
        } => {
            type_ref_contains_range(trait_ty, range)
                || callable.as_ref().is_some_and(|signature| {
                    signature
                        .params
                        .iter()
                        .any(|param| type_ref_contains_range(param, range))
                        || type_ref_contains_range(&signature.ret, range)
                })
        }
        HirTypeRef::DynTrait { trait_ty, .. } => type_ref_contains_range(trait_ty, range),
        HirTypeRef::Never | HirTypeRef::Const(_) | HirTypeRef::Unknown | HirTypeRef::Error => false,
    }
}

fn collect_member_completions(
    hir: &hir::HirFile,
    types: &type_checker::TypeCheckResult,
    out: &mut Vec<CompletionItem>,
) {
    let receiver = hir.bodies.iter().find_map(|(body_id, body)| {
        body.exprs.iter().find_map(|(_, expr)| {
            let Expr::FieldAccess { base, field } = expr else {
                return None;
            };
            (field.0 == COMPLETION_MARKER)
                .then(|| {
                    types
                        .expr_types
                        .get(&(body_id, *base))
                        .map(|receiver| (body_id, receiver))
                })
                .flatten()
        })
    });
    let Some((body_id, receiver)) = receiver else {
        return;
    };

    if let Some(struct_id) = receiver_struct_id(receiver) {
        let struct_item = &hir.item_tree.structs[struct_id];
        for field in &struct_item.fields {
            if !type_checker::struct_field_is_visible(hir, body_id, struct_id, &field.visibility) {
                continue;
            }
            out.push(completion_item(
                &field.name.0,
                CompletionItemKind::FIELD,
                Some(field.ty.display()),
            ));
        }
    }

    for (_, impl_item) in hir.item_tree.impls.iter() {
        if !type_ref_matches_type(hir, &impl_item.generics, &impl_item.self_ty, receiver) {
            continue;
        }
        for function_id in &impl_item.methods {
            let function = &hir.item_tree.functions[*function_id];
            if function
                .params
                .first()
                .is_some_and(|param| param.name.0 == "self")
                && (function_is_visible(hir, Some(body_id), *function_id)
                    || impl_item.trait_ty.is_some())
            {
                out.push(function_completion(function, CompletionItemKind::METHOD));
            }
        }
    }
}

fn collect_struct_field_completions(
    hir: &hir::HirFile,
    types: &type_checker::TypeCheckResult,
    out: &mut Vec<CompletionItem>,
) {
    for (body_id, body) in hir.bodies.iter() {
        for (_, expr) in body.exprs.iter() {
            let Expr::Struct {
                fields, resolved, ..
            } = expr
            else {
                continue;
            };
            if !fields.iter().any(|field| field.name.0 == COMPLETION_MARKER) {
                continue;
            }
            let Some(definitions) = crate::navigation::fields_for_struct_expression(hir, expr)
            else {
                return;
            };
            let struct_id = match resolved {
                Some(ResolvedName::Struct(id)) => Some(*id),
                _ => None,
            };
            push_missing_field_completions(
                hir,
                body_id,
                struct_id,
                definitions,
                fields.iter().map(|field| &field.name),
                out,
            );
            return;
        }

        for (pat_id, pattern) in body.pats.iter() {
            let Pattern::Struct { fields, path } = pattern else {
                continue;
            };
            if !fields.iter().any(|field| field.name.0 == COMPLETION_MARKER) {
                continue;
            }
            let Some(definitions) =
                crate::navigation::fields_for_struct_pattern(hir, types, body_id, pat_id, path)
            else {
                return;
            };
            let struct_id = types
                .pattern_types
                .get(&(body_id, pat_id))
                .and_then(receiver_struct_id);
            push_missing_field_completions(
                hir,
                body_id,
                struct_id,
                definitions,
                fields.iter().map(|field| &field.name),
                out,
            );
            return;
        }
    }
}

fn push_missing_field_completions<'a>(
    hir: &hir::HirFile,
    body_id: BodyId,
    struct_id: Option<StructId>,
    definitions: &[hir::item_tree::HirStructField],
    existing: impl Iterator<Item = &'a hir::Name>,
    out: &mut Vec<CompletionItem>,
) {
    let existing = existing.collect::<HashSet<_>>();
    for field in definitions {
        if existing.contains(&field.name)
            || struct_id.is_some_and(|id| {
                !type_checker::struct_field_is_visible(hir, body_id, id, &field.visibility)
            })
        {
            continue;
        }
        out.push(completion_item(
            &field.name.0,
            CompletionItemKind::FIELD,
            Some(field.ty.display()),
        ));
    }
}

fn push_top_level_item(
    hir: &hir::HirFile,
    item: TopLevelItem,
    allow_private_user_item: bool,
    out: &mut Vec<CompletionItem>,
) {
    if top_level_item_is_hidden(hir, item) {
        return;
    }

    match item {
        TopLevelItem::Function(id) => {
            let item = &hir.item_tree.functions[id];
            if visible_for_completion(
                hir,
                &item.visibility,
                item.name_range,
                allow_private_user_item,
            ) {
                out.push(function_completion(item, CompletionItemKind::FUNCTION));
            }
        }
        TopLevelItem::Struct(id) => {
            let item = &hir.item_tree.structs[id];
            if visible_for_completion(
                hir,
                &item.visibility,
                item.name_range,
                allow_private_user_item,
            ) {
                out.push(completion_item(
                    &item.name.0,
                    CompletionItemKind::STRUCT,
                    Some(format!("struct {}", item.name.0)),
                ));
            }
        }
        TopLevelItem::Module(id) => {
            let item = &hir.item_tree.modules[id];
            if allow_private_user_item || item.visibility.is_public() {
                out.push(completion_item(
                    &item.name.0,
                    CompletionItemKind::MODULE,
                    Some(format!("mod {}", item.name.0)),
                ));
            }
        }
        TopLevelItem::Enum(id) => {
            let item = &hir.item_tree.enums[id];
            if visible_for_completion(
                hir,
                &item.visibility,
                item.name_range,
                allow_private_user_item,
            ) {
                out.push(completion_item(
                    &item.name.0,
                    CompletionItemKind::ENUM,
                    Some(format!("enum {}", item.name.0)),
                ));
            }
        }
        TopLevelItem::Trait(id) => {
            let item = &hir.item_tree.traits[id];
            if allow_private_user_item || item.visibility.is_public() {
                out.push(completion_item(
                    &item.name.0,
                    CompletionItemKind::INTERFACE,
                    Some(format!("trait {}", item.name.0)),
                ));
            }
        }
        TopLevelItem::Const(id) => {
            let item = &hir.item_tree.consts[id];
            if visible_for_completion(
                hir,
                &item.visibility,
                item.name_range,
                allow_private_user_item,
            ) {
                out.push(completion_item(
                    &item.name.0,
                    CompletionItemKind::CONSTANT,
                    Some(item.ty.display()),
                ));
            }
        }
        TopLevelItem::TypeAlias(id) => {
            let item = &hir.item_tree.type_aliases[id];
            if visible_for_completion(
                hir,
                &item.visibility,
                item.name_range,
                allow_private_user_item,
            ) {
                out.push(completion_item(
                    &item.name.0,
                    CompletionItemKind::TYPE_PARAMETER,
                    item.ty.as_ref().map(HirTypeRef::display),
                ));
            }
        }
        TopLevelItem::Use(_) | TopLevelItem::Impl(_) => {}
    }
}

fn push_use_tree(tree: &HirUseTree, out: &mut Vec<CompletionItem>) {
    match &tree.kind {
        HirUseTreeKind::Simple { alias } => {
            let name = alias
                .as_ref()
                .or_else(|| tree.prefix.segments.last())
                .map(|name| name.0.as_str());
            if let Some(name) = name {
                out.push(completion_item(
                    name,
                    CompletionItemKind::REFERENCE,
                    Some(format!("use {}", tree.prefix.display())),
                ));
            }
        }
        HirUseTreeKind::List(items) => {
            for item in items {
                push_use_tree(item, out);
            }
        }
        HirUseTreeKind::Glob => {}
    }
}

fn function_completion(function: &HirFunction, kind: CompletionItemKind) -> CompletionItem {
    function_completion_named(function, kind, &function.name.0)
}

fn function_completion_named(
    function: &HirFunction,
    kind: CompletionItemKind,
    label: &str,
) -> CompletionItem {
    let params = function
        .params
        .iter()
        .map(|param| {
            if param.name.0 == "self" {
                match &param.ty {
                    HirTypeRef::Ref(_, true) => "&mut self".into(),
                    HirTypeRef::Ref(_, false) => "&self".into(),
                    _ => "self".into(),
                }
            } else {
                format!("{}: {}", param.name.0, param.ty.display())
            }
        })
        .collect::<Vec<_>>()
        .join(", ");
    let ret = function
        .ret_type
        .as_ref()
        .map_or_else(|| "()".into(), HirTypeRef::display);
    let mut item = completion_item(
        label,
        kind,
        Some(format!("fun {}({params}) -> {ret}", function.name.0)),
    );
    item.label_details = Some(CompletionItemLabelDetails {
        detail: Some(format!("({params})")),
        description: Some(ret),
    });
    item.insert_text = Some(label.into());
    item
}

fn completion_item(
    label: &str,
    kind: CompletionItemKind,
    detail: Option<String>,
) -> CompletionItem {
    CompletionItem {
        label: label.into(),
        kind: Some(kind),
        detail,
        ..CompletionItem::default()
    }
}

/// Keywords that introduce declarations and bindings; offered only at
/// statement and item starts.
const DECLARATION_KEYWORDS: &[&str] = &[
    "let", "fun", "struct", "mod", "use", "mut", "pub", "enum", "trait", "impl", "const", "type",
    "extern", "where", "else",
];

/// Common statement templates offered alongside the keywords at statement
/// starts. Snippet placeholders follow the LSP snippet grammar.
fn statement_snippet_items() -> Vec<CompletionItem> {
    let templates: &[(&str, &str, &str)] = &[
        (
            "match",
            "match ${1:value} {\n    ${2:pattern} => ${3:todo!()},\n}$0",
            "snippet: match expression",
        ),
        (
            "ifelse",
            "if ${1:cond} {\n    ${2}\n} else {\n    ${3}\n}$0",
            "snippet: if / else",
        ),
        (
            "forin",
            "for ${1:item} in ${2:iterable} {\n    ${3}\n}$0",
            "snippet: for loop",
        ),
    ];
    templates
        .iter()
        .map(|(label, insert, detail)| {
            let mut item =
                completion_item(label, CompletionItemKind::SNIPPET, Some((*detail).into()));
            item.insert_text = Some((*insert).into());
            item.insert_text_format = Some(InsertTextFormat::SNIPPET);
            item
        })
        .collect()
}

/// Offers every required trait member the impl does not provide yet as a
/// snippet completion carrying the trait's declared signature.
fn collect_missing_trait_members(
    hir: &hir::HirFile,
    info: &ImplBodyInfo,
    items: &mut Vec<CompletionItem>,
) {
    let Some((_, tr)) = hir
        .item_tree
        .traits
        .iter()
        .find(|(_, candidate)| candidate.name.0 == info.trait_name)
    else {
        return;
    };
    for method in &tr.methods {
        if method.has_body || info.method_names.contains(&method.name.0) {
            continue;
        }
        let signature = trait_method_signature(method);
        let mut item = completion_item(
            &method.name.0,
            CompletionItemKind::METHOD,
            Some(format!("{signature} {{ … }}")),
        );
        item.label_details = Some(CompletionItemLabelDetails {
            detail: Some("implement missing trait method".into()),
            description: Some(format!("trait {}", tr.name.0)),
        });
        item.insert_text = Some(format!("{signature} {{\n    $0\n}}"));
        item.insert_text_format = Some(InsertTextFormat::SNIPPET);
        items.push(item);
    }
    for alias in &tr.type_aliases {
        if alias.ty.is_some() || info.type_alias_names.contains(&alias.name.0) {
            continue;
        }
        items.push(completion_item(
            &alias.name.0,
            CompletionItemKind::TYPE_PARAMETER,
            Some(format!("associated type from trait {}", tr.name.0)),
        ));
    }
}

fn visible_for_completion(
    hir: &hir::HirFile,
    visibility: &Visibility,
    range: TextRange,
    allow_private_user_item: bool,
) -> bool {
    visibility.is_public() || (allow_private_user_item && hir.package_for_range(range).is_some())
}

fn function_is_visible(
    hir: &hir::HirFile,
    body: Option<BodyId>,
    function_id: hir::item_tree::FunctionId,
) -> bool {
    let function = &hir.item_tree.functions[function_id];
    function.visibility.is_public()
        || hir.item_tree.impls.iter().any(|(_, implementation)| {
            implementation.trait_ty.is_some() && implementation.methods.contains(&function_id)
        })
        || body.is_some_and(|body| {
            type_checker::method_is_visible(hir, body, function_id, &function.visibility)
        })
}

fn receiver_struct_id(ty: &type_checker::Type) -> Option<StructId> {
    match ty {
        type_checker::Type::Struct(id, _) => Some(*id),
        type_checker::Type::Ref(inner, _) | type_checker::Type::Ptr { inner, .. } => {
            receiver_struct_id(inner)
        }
        _ => None,
    }
}

fn type_ref_matches_type(
    hir: &hir::HirFile,
    generics: &[hir::Name],
    expected: &HirTypeRef,
    actual: &type_checker::Type,
) -> bool {
    match (expected, actual) {
        (HirTypeRef::Ref(expected, _), type_checker::Type::Ref(actual, _))
        | (
            HirTypeRef::Ref(expected, _)
            | HirTypeRef::Ptr {
                inner: expected, ..
            },
            type_checker::Type::Ptr { inner: actual, .. },
        ) => type_ref_matches_type(hir, generics, expected, actual),
        (HirTypeRef::Array(expected, _), type_checker::Type::Array(actual, _))
        | (HirTypeRef::Slice(expected), type_checker::Type::Slice(actual)) => {
            type_ref_matches_type(hir, generics, expected, actual)
        }
        (
            expected @ HirTypeRef::Slice(_),
            type_checker::Type::Ref(actual, _) | type_checker::Type::Ptr { inner: actual, .. },
        ) => type_ref_matches_type(hir, generics, expected, actual),
        (
            HirTypeRef::Named(_),
            type_checker::Type::Ref(actual, _) | type_checker::Type::Ptr { inner: actual, .. },
        ) => type_ref_matches_type(hir, generics, expected, actual),
        (HirTypeRef::Named(path), _)
            if path.segments.len() == 1
                && path.type_args.is_empty()
                && generics.contains(&path.segments[0]) =>
        {
            true
        }
        (HirTypeRef::Named(_), _) => type_ref_name(expected).is_some_and(|expected| {
            let actual = actual.display(hir);
            actual.split('<').next() == Some(expected)
        }),
        _ => false,
    }
}

fn type_ref_name(ty: &HirTypeRef) -> Option<&str> {
    match ty {
        HirTypeRef::Named(path) => path.segments.last().map(|name| name.0.as_str()),
        HirTypeRef::Ref(inner, _) | HirTypeRef::Ptr { inner, .. } => type_ref_name(inner),
        _ => None,
    }
}

fn identifier_start(source: &str, offset: usize) -> usize {
    source[..offset]
        .char_indices()
        .rev()
        .find(|(_, ch)| !is_identifier_continue(*ch))
        .map_or(0, |(index, ch)| index + ch.len_utf8())
}

fn identifier_end(source: &str, offset: usize) -> usize {
    source[offset..]
        .char_indices()
        .find(|(_, ch)| !is_identifier_continue(*ch))
        .map_or(source.len(), |(index, _)| offset + index)
}
