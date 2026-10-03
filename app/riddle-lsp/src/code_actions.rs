use std::collections::{HashMap, HashSet};
use std::hash::BuildHasher;

use ast::{
    ImplDecl, Root, Stmt, UseDecl, VarDecl, attrs_for_node, support::AstNode,
    support::trimmed_range,
};
use frontend::lexer::lex;
use hir::item_tree::{HirFunction, HirImpl, HirTypeRef};
use lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionResponse, Diagnostic,
    DocumentChanges, NumberOrString, OneOf, OptionalVersionedTextDocumentIdentifier,
    TextDocumentEdit, TextEdit, Url, WorkspaceEdit,
};
use riddlec::pipeline::CompileOptions;
use rowan::{TextRange, TextSize};
use scope_graph::{Node, NodeId, RefOrigin, ScopeGraph};
use syntax::SyntaxKind;

use crate::{
    analysis::{AnalysisDepth, DocumentAnalysis, analyze_document_cancellable},
    completion::import_routes_for_name,
    imports::{import_edit, parse_root},
    server::Document,
    session::AnalysisSessions,
    suggest::closest_name,
    text::{LineIndex, offset_for_position, text_range, text_size},
};

const CLOSURE_MUT_MESSAGE: &str =
    "cannot call a mutable closure through an immutable binding\nimmutable closure binding";
const ASSIGN_NOT_MUTABLE_PREFIX: &str = "cannot assign to `";
const UNSAFE_MESSAGE_SUFFIX: &str = "requires an unsafe block";
const MISSING_FIELD_PREFIX: &str = "missing field `";
const MISSING_PATTERN_PREFIX: &str = "missing pattern `";
const EMPTY_USE_MESSAGE: &str = "empty use declaration";
const DROP_MESSAGE_HINT: &str = "use `drop(value)` instead";
const UNRESOLVED_NAME_CODE: &str = "E0050";
const UNKNOWN_METHOD_CODE: &str = "E0013";
const UNKNOWN_METHOD_PREFIX: &str = "unknown method `";
const MISSING_IMPL_METHOD_CODE: &str = "E0026";
const MISSING_IMPL_TRAIT_PREFIX: &str = "of trait `";
const MISSING_IMPL_METHOD_PREFIX: &str = "missing method `";
const TODO_PLACEHOLDER: &str = "todo!()";

/// Syntax-only quick fixes: one action per (diagnostic, fix) pair.
#[must_use]
pub fn quick_fixes(
    uri: &Url,
    version: Option<i32>,
    source: &str,
    diagnostics: &[Diagnostic],
) -> CodeActionResponse {
    diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.source.as_deref() == Some("riddle"))
        .flat_map(|diagnostic| {
            diagnostic_fixes(source, diagnostic)
                .into_iter()
                .map(|(title, edits, preferred)| {
                    quickfix_action(uri, version, &title, diagnostic, edits, preferred)
                })
        })
        .collect()
}

/// Quick fixes that need an up-to-date [`DocumentAnalysis`] of the same
/// document snapshot the diagnostics were produced from: unresolved names
/// ("did you mean" and import suggestions), unknown methods, and missing
/// trait-method implementations.
#[must_use]
pub fn analysis_fixes(
    uri: &Url,
    version: Option<i32>,
    source: &str,
    diagnostics: &[Diagnostic],
    analysis: &DocumentAnalysis,
) -> CodeActionResponse {
    diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.source.as_deref() == Some("riddle"))
        .flat_map(|diagnostic| match diagnostic_code(diagnostic) {
            Some(UNRESOLVED_NAME_CODE) => {
                unresolved_name_actions(uri, version, source, diagnostic, analysis)
            }
            Some(UNKNOWN_METHOD_CODE) => {
                unknown_method_actions(uri, version, source, diagnostic, analysis)
            }
            Some(MISSING_IMPL_METHOD_CODE) => {
                implement_missing_method_actions(uri, version, source, diagnostic, analysis)
            }
            _ => Vec::new(),
        })
        .collect()
}

/// Server entry point: analyzes the document and returns the analysis-based
/// fixes, cooperating with request cancellation.
pub fn analysis_fixes_for_document_cancellable<S: BuildHasher>(
    uri: &Url,
    docs: &HashMap<Url, Document, S>,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    version: Option<i32>,
    diagnostics: &[Diagnostic],
    cancelled: &impl Fn() -> bool,
) -> Result<CodeActionResponse, String> {
    let Some(text) = docs.get(uri).map(|document| document.text.clone()) else {
        return Ok(Vec::new());
    };
    let Some(analysis) = analyze_document_cancellable(
        uri,
        docs,
        options,
        sessions,
        AnalysisDepth::Resolve,
        cancelled,
    )?
    else {
        return Ok(Vec::new());
    };
    Ok(analysis_fixes(uri, version, &text, diagnostics, &analysis))
}

/// `source.organizeImports`: sorts, deduplicates, and drops empty top-level
/// `use` declarations. Returns `None` when there is nothing to change.
pub fn organize_imports_action(
    uri: &Url,
    version: Option<i32>,
    source: &str,
) -> Option<CodeActionOrCommand> {
    let edits = organize_import_edits(source)?;
    Some(action_with(
        "Organize imports",
        CodeActionKind::new(ORGANIZE_IMPORTS_KIND),
        single_file_edit(uri, version, edits),
        false,
    ))
}

/// The `source.organizeImports` kind, shared with the request filter.
pub const ORGANIZE_IMPORTS_KIND: &str = "source.organizeImports";
/// Adds the imports needed by names the document already uses.
pub const ADD_MISSING_IMPORTS_KIND: &str = "source.addMissingImports";
/// Applies every fix the server can prove is safe.
pub const FIX_ALL_KIND: &str = "source.fixAll";

/// Merges the auto-import edits of several actions into one `source.fixAll`
/// action.
///
/// Only the *additional* text edits are taken: those carry the `use` lines the
/// unresolved names need. The primary edits belong to individual quick fixes,
/// which may overlap and would corrupt the file when applied together.
#[must_use]
pub fn fix_all_action(
    uri: &Url,
    version: Option<i32>,
    fixes: &CodeActionResponse,
) -> Option<CodeActionOrCommand> {
    let mut edits = Vec::new();
    for action in fixes {
        let CodeActionOrCommand::CodeAction(action) = action else {
            continue;
        };
        let Some(edit) = &action.edit else {
            continue;
        };
        for change in edit
            .document_changes
            .iter()
            .flat_map(|changes| match changes {
                DocumentChanges::Edits(edits) => edits.iter().collect::<Vec<_>>(),
                DocumentChanges::Operations(_) => Vec::new(),
            })
        {
            if change.text_document.uri != *uri {
                continue;
            }
            for edit in &change.edits {
                if let OneOf::Left(text_edit) = edit {
                    edits.push(text_edit.clone());
                }
            }
        }
    }
    if edits.is_empty() {
        return None;
    }
    Some(action_with(
        "Fix all auto-fixable problems",
        CodeActionKind::new(FIX_ALL_KIND),
        single_file_edit(uri, version, edits),
        false,
    ))
}

/// Turns the auto-import quick fixes into one `source.addMissingImports` action.
#[must_use]
pub fn add_missing_imports_action(
    uri: &Url,
    version: Option<i32>,
    fixes: &CodeActionResponse,
) -> Option<CodeActionOrCommand> {
    let mut edits = Vec::new();
    for action in fixes {
        let CodeActionOrCommand::CodeAction(action) = action else {
            continue;
        };
        if !action.title.starts_with("Import `") {
            continue;
        }
        let Some(edit) = &action.edit else {
            continue;
        };
        for change in edit
            .document_changes
            .iter()
            .flat_map(|changes| match changes {
                DocumentChanges::Edits(edits) => edits.iter().collect::<Vec<_>>(),
                DocumentChanges::Operations(_) => Vec::new(),
            })
        {
            if change.text_document.uri != *uri {
                continue;
            }
            for edit in &change.edits {
                if let OneOf::Left(text_edit) = edit {
                    edits.push(text_edit.clone());
                }
            }
        }
    }
    if edits.is_empty() {
        return None;
    }
    Some(action_with(
        "Add missing imports",
        CodeActionKind::new(ADD_MISSING_IMPORTS_KIND),
        single_file_edit(uri, version, edits),
        false,
    ))
}

fn action_with(
    title: &str,
    kind: CodeActionKind,
    edit: WorkspaceEdit,
    preferred: bool,
) -> CodeActionOrCommand {
    CodeActionOrCommand::CodeAction(CodeAction {
        title: title.into(),
        kind: Some(kind),
        edit: Some(edit),
        is_preferred: Some(preferred),
        ..CodeAction::default()
    })
}

/// The text edits behind [`organize_imports_action`], exposed for tests.
pub(crate) fn organize_import_edits(source: &str) -> Option<Vec<TextEdit>> {
    let root = parse_root(source)?;
    let stmts: Vec<Stmt> = root.stmts().collect();
    let first = stmts
        .iter()
        .position(|stmt| matches!(stmt, Stmt::UseDecl(_)))?;
    let last = stmts
        .iter()
        .rposition(|stmt| matches!(stmt, Stmt::UseDecl(_)))?;
    if stmts[first..=last]
        .iter()
        .any(|stmt| !matches!(stmt, Stmt::UseDecl(_)))
    {
        // Items interleaved with the imports: reordering across them is not
        // worth the risk, so leave the block alone.
        return None;
    }
    let decls: Vec<UseDecl> = stmts[first..=last]
        .iter()
        .map(|stmt| match stmt {
            Stmt::UseDecl(decl) => decl.clone(),
            _ => unreachable!("filtered above"),
        })
        .collect();

    let newline = if source.contains("\r\n") {
        "\r\n"
    } else {
        "\n"
    };
    // Node ranges can absorb preceding trivia (comments, blank lines), so
    // every comparison and edit uses the trimmed extent.
    let block_start = trimmed_range(decls.first()?.syntax()).start();
    let block_end = trimmed_range(decls.last()?.syntax()).end();
    let block = source
        .get(usize::from(block_start)..usize::from(block_end))?
        .to_string();
    let entry = |decl: &UseDecl| -> Option<ImportEntry> {
        let range = trimmed_range(decl.syntax());
        Some(ImportEntry {
            is_pub: decl.is_pub(),
            text: source
                .get(usize::from(range.start())..usize::from(range.end()))?
                .trim()
                .to_string(),
            is_empty: decl.use_tree().is_none(),
        })
    };
    let entries: Vec<ImportEntry> = decls.iter().filter_map(entry).collect();

    let reorder_allowed = !block_contains_comment(&block) && !decls.iter().any(decl_has_attributes);
    if reorder_allowed {
        let current = entries
            .iter()
            .map(|entry| entry.text.as_str())
            .collect::<Vec<_>>()
            .join(newline);
        let mut entries: Vec<ImportEntry> = entries
            .into_iter()
            .filter(|entry| !entry.is_empty)
            .collect();
        // Plain imports before re-exports, then by path text.
        entries.sort_by(|left, right| {
            left.is_pub
                .cmp(&right.is_pub)
                .then_with(|| left.text.cmp(&right.text))
        });
        entries.dedup_by(|left, right| left.text == right.text);
        let organized = entries
            .iter()
            .map(|entry| entry.text.as_str())
            .collect::<Vec<_>>()
            .join(newline);
        if organized == current {
            return None;
        }
        let edit = edit_at(
            source,
            text_range(usize::from(block_start), usize::from(block_end)),
            organized,
        )?;
        Some(vec![edit])
    } else {
        // Comments or attributes pin the imports in place: only delete
        // duplicate and empty declarations.
        let mut seen = HashSet::new();
        let mut edits = Vec::new();
        for decl in &decls {
            let range = trimmed_range(decl.syntax());
            let text = source
                .get(usize::from(range.start())..usize::from(range.end()))?
                .trim();
            let redundant = decl.use_tree().is_none() || !seen.insert(text.to_string());
            if redundant
                && let Some(line) = full_line_range(source, range)
                && let Some(edit) = edit_at(source, line, String::new())
            {
                edits.push(edit);
            }
        }
        (!edits.is_empty()).then_some(edits)
    }
}

struct ImportEntry {
    is_pub: bool,
    text: String,
    is_empty: bool,
}

fn decl_has_attributes(decl: &UseDecl) -> bool {
    !attrs_for_node(decl.syntax()).is_empty()
}

fn block_contains_comment(block: &str) -> bool {
    lex(block).iter().any(|token| {
        matches!(
            token.kind,
            SyntaxKind::LineComment
                | SyntaxKind::BlockComment
                | SyntaxKind::DocComment
                | SyntaxKind::DocBlockComment
        )
    })
}

fn diagnostic_fixes(source: &str, diagnostic: &Diagnostic) -> Vec<(String, Vec<TextEdit>, bool)> {
    let Some(code) = diagnostic_code(diagnostic) else {
        return Vec::new();
    };
    match code {
        "E0031" if diagnostic.message.starts_with(CLOSURE_MUT_MESSAGE) => {
            insert_at_diagnostic_start(source, diagnostic, "Add `mut` to closure binding", "mut ")
                .into_iter()
                .collect()
        }
        "E0031" if diagnostic.message.starts_with(ASSIGN_NOT_MUTABLE_PREFIX) => {
            add_mut_binding_fix(source, diagnostic)
                .into_iter()
                .collect()
        }
        "E0046" if diagnostic.message.ends_with(UNSAFE_MESSAGE_SUFFIX) => {
            wrap_in_unsafe_fix(source, diagnostic).into_iter().collect()
        }
        "E0007" if diagnostic.message.starts_with(MISSING_FIELD_PREFIX) => {
            add_missing_field_fix(source, diagnostic)
                .into_iter()
                .collect()
        }
        "E0039" if diagnostic.message.contains(MISSING_PATTERN_PREFIX) => {
            add_match_arm_fix(source, diagnostic).into_iter().collect()
        }
        "E0051" if diagnostic.message == EMPTY_USE_MESSAGE => {
            remove_empty_use_fix(source, diagnostic)
                .into_iter()
                .collect()
        }
        "E0056" if diagnostic.message.contains(DROP_MESSAGE_HINT) => {
            replace_with_drop_fix(source, diagnostic)
                .into_iter()
                .collect()
        }
        _ => Vec::new(),
    }
}

fn add_mut_binding_fix(
    source: &str,
    diagnostic: &Diagnostic,
) -> Option<(String, Vec<TextEdit>, bool)> {
    let (start, _) = diagnostic_span_offsets(source, diagnostic)?;
    let name = quoted_after(&diagnostic.message, ASSIGN_NOT_MUTABLE_PREFIX)?;
    let root = parse_root(source)?;
    let insert_at = binding_before(&root, name, start)?;
    let edit = edit_at(source, TextRange::empty(insert_at), "mut ".to_string())?;
    Some(("Add `mut` to binding".into(), vec![edit], true))
}

/// Finds where to add `mut` for the binding a diagnostic refers to: the
/// closest `let name` / `let mut name` declared before `offset`, preferring
/// ones in the same function. Patterns beyond a simple binding (tuples,
/// struct patterns, …) are left alone.
fn binding_before(root: &Root, name: &str, offset: usize) -> Option<TextSize> {
    let offset = text_size(offset);
    let enclosing_function = root
        .syntax()
        .descendants()
        .find(|node| node.kind() == SyntaxKind::FuncDecl && node.text_range().contains(offset));
    let search_root = enclosing_function.unwrap_or_else(|| root.syntax().clone());
    search_root
        .descendants()
        .filter_map(VarDecl::cast)
        .filter(|decl| decl.syntax().text_range().end() <= offset)
        .filter_map(|decl| simple_binding_insertion(&decl, name))
        .max_by_key(|insert_at| *insert_at)
}

/// `let name = …` / `let mut name = …` → the offset just before `name` where
/// `mut ` belongs. Returns `None` for everything else.
fn simple_binding_insertion(decl: &VarDecl, name: &str) -> Option<TextSize> {
    let mut tokens = decl
        .syntax()
        .descendants_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .filter(|token| !token.kind().is_trivia());
    tokens.find(|token| token.kind() == SyntaxKind::Let)?;
    let mut mutable = false;
    let name_token = loop {
        let token = tokens.next()?;
        match token.kind() {
            SyntaxKind::Mut => mutable = true,
            SyntaxKind::Ident => break token,
            _ => return None,
        }
    };
    if mutable || name_token.text() != name {
        return None;
    }
    // The identifier must be the whole pattern: the next token starts the
    // type annotation or the initializer.
    let next = tokens.next()?.kind();
    matches!(next, SyntaxKind::Colon | SyntaxKind::Eq).then_some(name_token.text_range().start())
}

fn add_missing_field_fix(
    source: &str,
    diagnostic: &Diagnostic,
) -> Option<(String, Vec<TextEdit>, bool)> {
    let (start, end) = diagnostic_span_offsets(source, diagnostic)?;
    let field = quoted_after(&diagnostic.message, MISSING_FIELD_PREFIX)?;
    let (offset, text) =
        insert_before_closing_brace(source, start, end, &format!("{field}: {TODO_PLACEHOLDER}"))?;
    let edit = edit_at(source, TextRange::empty(text_size(offset)), text)?;
    Some((format!("Add field `{field}`"), vec![edit], true))
}

fn add_match_arm_fix(
    source: &str,
    diagnostic: &Diagnostic,
) -> Option<(String, Vec<TextEdit>, bool)> {
    let (start, end) = diagnostic_span_offsets(source, diagnostic)?;
    let pattern = quoted_after(&diagnostic.message, MISSING_PATTERN_PREFIX)?;
    if pattern.is_empty() {
        return None;
    }
    let (offset, text) = insert_before_closing_brace(
        source,
        start,
        end,
        &format!("{pattern} => {TODO_PLACEHOLDER}"),
    )?;
    let edit = edit_at(source, TextRange::empty(text_size(offset)), text)?;
    let title = if pattern == "_" {
        "Add wildcard arm".to_string()
    } else {
        format!("Add `{pattern}` arm")
    };
    Some((title, vec![edit], true))
}

/// Builds an insertion just before the `}` that closes the literal or match
/// covered by `[start, end)`, keeping single-line and multi-line shapes
/// readable. The inserted fragment gets a trailing comma in multi-line
/// layouts and a leading comma when the previous entry lacks one.
fn insert_before_closing_brace(
    source: &str,
    start: usize,
    end: usize,
    fragment: &str,
) -> Option<(usize, String)> {
    let covered = source.get(start..end)?;
    let open = covered.find('{')?;
    let close = covered.rfind('}')?;
    if close <= open {
        return None;
    }
    let inner_start = start + open + 1;
    let inner_end = start + close;
    let inner = source.get(inner_start..inner_end)?;
    if inner.trim().is_empty() {
        return Some((inner_start, fragment.to_string()));
    }
    let multiline = inner.contains('\n');
    let content_end = inner_start + inner.trim_end().len();
    let last_char = source[..content_end].chars().next_back()?;
    let needs_comma = last_char != ',';
    if multiline {
        let indent = first_content_line_indent(inner)?;
        let comma = if needs_comma { "," } else { "" };
        Some((content_end, format!("{comma}\n{indent}{fragment},")))
    } else if needs_comma {
        Some((content_end, format!(", {fragment}")))
    } else {
        Some((content_end, format!(" {fragment}")))
    }
}

fn first_content_line_indent(inner: &str) -> Option<String> {
    inner
        .lines()
        .find(|line| !line.trim().is_empty())
        .map(|line| line[..line.len() - line.trim_start().len()].to_string())
}

fn remove_empty_use_fix(
    source: &str,
    diagnostic: &Diagnostic,
) -> Option<(String, Vec<TextEdit>, bool)> {
    let (start, end) = diagnostic_span_offsets(source, diagnostic)?;
    let root = parse_root(source)?;
    let span = TextRange::new(text_size(start), text_size(end));
    let decl = root
        .stmts()
        .filter_map(|stmt| match stmt {
            Stmt::UseDecl(decl) => Some(decl),
            _ => None,
        })
        .find(|decl| decl.syntax().text_range().contains_range(span))?;
    // Mid-edit error trees can make the declaration node swallow following
    // items, so the deletion never runs past the statement's own `;`.
    let decl_range = trimmed_range(decl.syntax());
    let decl_end = source[usize::from(decl_range.start())..usize::from(decl_range.end())]
        .find(';')
        .map_or(decl_range.end(), |pos| {
            decl_range.start() + text_size(pos + 1)
        });
    let line = full_line_range(source, TextRange::new(decl_range.start(), decl_end))?;
    let edit = edit_at(source, line, String::new())?;
    Some(("Remove empty `use` declaration".into(), vec![edit], true))
}

/// Expands `range` to swallow surrounding indentation and the trailing
/// newline, but only while nothing else shares those lines.
fn full_line_range(source: &str, range: TextRange) -> Option<TextRange> {
    let start = usize::from(range.start());
    let end = usize::from(range.end());
    let line_start = source[..start].rfind('\n').map_or(0, |pos| pos + 1);
    let newline_end = source[end..]
        .find('\n')
        .map_or(source.len(), |pos| end + pos + 1);
    let delete_start = if source[line_start..start].trim().is_empty() {
        line_start
    } else {
        start
    };
    let delete_end = if source[end..newline_end].trim().is_empty() {
        newline_end
    } else {
        end
    };
    Some(text_range(delete_start, delete_end))
}

fn replace_with_drop_fix(
    source: &str,
    diagnostic: &Diagnostic,
) -> Option<(String, Vec<TextEdit>, bool)> {
    let (start, end) = diagnostic_span_offsets(source, diagnostic)?;
    let text = source.get(start..end)?;
    let dot = text.rfind(".drop")?;
    if !text[dot + ".drop".len()..].trim().starts_with('(') {
        return None;
    }
    let receiver = text[..dot].trim();
    if receiver.is_empty() {
        return None;
    }
    let edit = edit_at(source, text_range(start, end), format!("drop({receiver})"))?;
    Some(("Replace with `drop(...)`".into(), vec![edit], true))
}

fn wrap_in_unsafe_fix(
    source: &str,
    diagnostic: &Diagnostic,
) -> Option<(String, Vec<TextEdit>, bool)> {
    let (start, end) = diagnostic_span_offsets(source, diagnostic)?;
    Some((
        "Wrap in `unsafe` block".into(),
        vec![
            edit_at(
                source,
                TextRange::empty(text_size(start)),
                "unsafe { ".into(),
            )?,
            edit_at(source, TextRange::empty(text_size(end)), " }".into())?,
        ],
        true,
    ))
}

fn insert_at_diagnostic_start(
    source: &str,
    diagnostic: &Diagnostic,
    title: &str,
    text: &str,
) -> Option<(String, Vec<TextEdit>, bool)> {
    let (start, _) = diagnostic_span_offsets(source, diagnostic)?;
    let edit = edit_at(source, TextRange::empty(text_size(start)), text.to_string())?;
    Some((title.to_string(), vec![edit], true))
}

fn unresolved_name_actions(
    uri: &Url,
    version: Option<i32>,
    source: &str,
    diagnostic: &Diagnostic,
    analysis: &DocumentAnalysis,
) -> CodeActionResponse {
    let Some(hir) = analysis.result.hir.as_ref() else {
        return Vec::new();
    };
    let Some(graph) = analysis.result.scope_graph.as_ref() else {
        return Vec::new();
    };
    let Some((start, end)) = diagnostic_span_offsets(source, diagnostic) else {
        return Vec::new();
    };
    let Some(path_text) = source.get(start..end).map(str::trim) else {
        return Vec::new();
    };
    let mut actions = Vec::new();

    // A close in-scope name means the user likely typo'd it.
    if !path_text.contains("::")
        && let Some(suggestion) =
            similar_name_in_scope(analysis, hir, graph, text_range(start, end), path_text)
        && let Some(edit) = edit_at(source, text_range(start, end), suggestion.to_string())
    {
        actions.push(quickfix_action(
            uri,
            version,
            &format!("Did you mean `{suggestion}`?"),
            diagnostic,
            vec![edit],
            true,
        ));
    }

    // Import suggestions cover the whole path and its first segment, so both
    // `foo::bar` and a typo'd module head get candidates.
    let mut candidates: Vec<&str> = vec![path_text];
    if let Some((first, _)) = path_text.split_once("::") {
        candidates.push(first);
    }
    let mut offered = 0;
    for candidate in candidates {
        for route in import_routes_for_name(hir, graph, candidate) {
            if offered >= 5 {
                return actions;
            }
            let Some(edit) = import_edit(source, &route.path) else {
                continue;
            };
            offered += 1;
            actions.push(quickfix_action(
                uri,
                version,
                &format!("Import `{}` from `{}`", route.label, route.path),
                diagnostic,
                vec![edit],
                false,
            ));
        }
    }
    actions
}

fn similar_name_in_scope(
    analysis: &DocumentAnalysis,
    hir: &hir::HirFile,
    graph: &ScopeGraph,
    origin: TextRange,
    target: &str,
) -> Option<String> {
    let reference = reference_at(analysis, hir, graph, origin)?;
    let names: Vec<String> = scope_graph::resolve::visible_definitions(graph, reference)
        .into_iter()
        .map(|(name, _)| name.0)
        .collect();
    closest_name(names.iter().map(String::as_str), target).map(str::to_string)
}

/// `E0013` unknown method: offers a "did you mean" rename when the receiver
/// type (or any trait in scope) declares a similarly named method.
fn unknown_method_actions(
    uri: &Url,
    version: Option<i32>,
    source: &str,
    diagnostic: &Diagnostic,
    analysis: &DocumentAnalysis,
) -> CodeActionResponse {
    let Some(method) = quoted_after(&diagnostic.message, UNKNOWN_METHOD_PREFIX) else {
        return Vec::new();
    };
    let Some(receiver) = receiver_type_name(&diagnostic.message) else {
        return Vec::new();
    };
    let Some(hir) = analysis.result.hir.as_ref() else {
        return Vec::new();
    };
    let (start, end) = match diagnostic_span_offsets(source, diagnostic) {
        Some(span) => span,
        None => return Vec::new(),
    };

    let mut candidates: HashSet<&str> = HashSet::new();
    for (_, imp) in hir.item_tree.impls.iter() {
        if impl_self_type_name(imp) == Some(receiver) {
            for function_id in &imp.methods {
                candidates.insert(hir.item_tree.functions[*function_id].name.0.as_str());
            }
        }
    }
    for (_, tr) in hir.item_tree.traits.iter() {
        for method in &tr.methods {
            candidates.insert(method.name.0.as_str());
        }
    }

    let Some(suggestion) = closest_name(candidates.iter().copied(), method) else {
        return Vec::new();
    };
    let Some(name_range) = called_method_name_range(source, start, end) else {
        return Vec::new();
    };
    let Some(edit) = edit_at(source, name_range, suggestion.to_string()) else {
        return Vec::new();
    };
    vec![quickfix_action(
        uri,
        version,
        &format!("Did you mean `{suggestion}`?"),
        diagnostic,
        vec![edit],
        true,
    )]
}

/// `E0026` missing trait method: offers to insert a `todo!()` stub with the
/// trait's declared signature into the impl block.
fn implement_missing_method_actions(
    uri: &Url,
    version: Option<i32>,
    source: &str,
    diagnostic: &Diagnostic,
    analysis: &DocumentAnalysis,
) -> CodeActionResponse {
    let Some(trait_name) = quoted_after(&diagnostic.message, MISSING_IMPL_TRAIT_PREFIX) else {
        return Vec::new();
    };
    let Some(method_name) = quoted_after(&diagnostic.message, MISSING_IMPL_METHOD_PREFIX) else {
        return Vec::new();
    };
    let Some(hir) = analysis.result.hir.as_ref() else {
        return Vec::new();
    };
    let Some((_, tr)) = hir
        .item_tree
        .traits
        .iter()
        .find(|(_, candidate)| candidate.name.0 == trait_name)
    else {
        return Vec::new();
    };
    let Some(method) = tr
        .methods
        .iter()
        .find(|method| method.name.0 == method_name)
    else {
        return Vec::new();
    };
    if method.has_body {
        return Vec::new();
    }
    let (start, _) = match diagnostic_span_offsets(source, diagnostic) {
        Some(span) => span,
        None => return Vec::new(),
    };
    let root = match parse_root(source) {
        Some(root) => root,
        None => return Vec::new(),
    };
    let Some(impl_decl) = root
        .syntax()
        .descendants()
        .filter_map(ImplDecl::cast)
        .find(|decl| decl.syntax().text_range().contains(text_size(start)))
    else {
        return Vec::new();
    };
    let Some((range, text)) = impl_method_insertion(source, &impl_decl, method) else {
        return Vec::new();
    };
    let Some(edit) = edit_at(source, range, text) else {
        return Vec::new();
    };
    vec![quickfix_action(
        uri,
        version,
        &format!("Implement `{}` from `{trait_name}`", method.name.0),
        diagnostic,
        vec![edit],
        true,
    )]
}

/// `on type Vector<i32>` / `on type &Point` from an `E0013` message → the
/// plain type name `Vector` / `Point`. Published diagnostics append notes on
/// their own lines, so only the first line is considered.
fn receiver_type_name(message: &str) -> Option<&str> {
    let type_text = message.split_once("on type ")?.1.lines().next()?.trim();
    let mut name = type_text.split('<').next()?.trim();
    while let Some(stripped) = name.strip_prefix('&') {
        name = stripped.trim_start();
    }
    let name = name.strip_prefix("mut ").unwrap_or(name).trim();
    (!name.is_empty()).then_some(name)
}

fn impl_self_type_name(imp: &HirImpl) -> Option<&str> {
    match &imp.self_ty {
        HirTypeRef::Named(path) => path.segments.last().map(|name| name.0.as_str()),
        _ => None,
    }
}

/// The identifier immediately before a call's `(` inside `[start, end)` — the
/// method name a rename fix should replace.
fn called_method_name_range(source: &str, start: usize, end: usize) -> Option<TextRange> {
    let covered = source.get(start..end)?;
    let tokens = lex(covered);
    let mut found = None;
    for (index, token) in tokens.iter().enumerate() {
        if token.kind != SyntaxKind::Ident {
            continue;
        }
        let next = tokens[index + 1..]
            .iter()
            .find(|candidate| candidate.kind != SyntaxKind::Whitespace)?;
        if next.kind == SyntaxKind::LParen {
            let offset = start + token.span.start;
            found = Some(text_range(
                offset,
                offset + (token.span.end - token.span.start),
            ));
        }
    }
    found
}

/// The edit for a stub of `method` at the impl block's closing brace,
/// replacing the block's trailing whitespace so single-line and multi-line
/// impls both end up with one method per line.
fn impl_method_insertion(
    source: &str,
    impl_decl: &ImplDecl,
    method: &HirFunction,
) -> Option<(TextRange, String)> {
    let range = trimmed_range(impl_decl.syntax());
    let start = usize::from(range.start());
    let end = usize::from(range.end());
    let covered = source.get(start..end)?;
    let open = covered.find('{')?;
    let close = covered.rfind('}')?;
    if close <= open {
        return None;
    }
    let inner = source.get(start + open + 1..start + close)?;
    let content_end = start + open + 1 + inner.trim_end().len();
    let newline = if source.contains("\r\n") {
        "\r\n"
    } else {
        "\n"
    };
    let line_start = source[..start].rfind('\n').map_or(0, |pos| pos + 1);
    let indent = &source[line_start..start];
    let body_indent = format!("{indent}    ");
    let signature = trait_method_signature(method);
    let text = format!(
        "{newline}{body_indent}{signature} {{{newline}{body_indent}    {TODO_PLACEHOLDER}{newline}{body_indent}}}{newline}{indent}"
    );
    Some((text_range(content_end, start + close), text))
}

/// Renders a trait method declaration without its body, substituting nothing:
/// declared types are shown as written in the trait.
pub(crate) fn trait_method_signature(function: &HirFunction) -> String {
    let safety = if function.is_unsafe { "unsafe " } else { "" };
    let generics = if function.generics.is_empty() {
        String::new()
    } else {
        format!(
            "<{}>",
            function
                .generics
                .iter()
                .map(|name| name.0.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        )
    };
    let params = function
        .params
        .iter()
        .map(|parameter| {
            if parameter.is_receiver {
                match &parameter.ty {
                    HirTypeRef::Ref(_, true) => "&mut self".to_string(),
                    HirTypeRef::Ref(_, false) => "&self".to_string(),
                    _ => "self".to_string(),
                }
            } else {
                format!("{}: {}", parameter.name.0, parameter.ty.display())
            }
        })
        .collect::<Vec<_>>()
        .join(", ");
    let ret = function
        .ret_type
        .as_ref()
        .map_or_else(String::new, |ty| format!(" -> {}", ty.display()));
    format!("{safety}fun {}{generics}({params}){ret}", function.name.0)
}

/// Finds the scope-graph reference node covering `origin` (document
/// coordinates), in expression position first and type position second.
fn reference_at(
    analysis: &DocumentAnalysis,
    hir: &hir::HirFile,
    graph: &ScopeGraph,
    origin: TextRange,
) -> Option<NodeId> {
    let covers = |local: TextRange| {
        (local.contains_range(origin) || origin.contains_range(local)).then_some(())
    };
    graph
        .nodes
        .iter()
        .find_map(|(node_id, node)| {
            let Node::Reference {
                origin: RefOrigin::Expr { body, expr },
                ..
            } = node
            else {
                return None;
            };
            let range = hir.bodies[*body]
                .source_map
                .expr_ranges
                .get(expr)
                .copied()?;
            let local = analysis.local_range(range)?;
            covers(local)?;
            Some(node_id)
        })
        .or_else(|| {
            graph.nodes.iter().find_map(|(node_id, node)| {
                let Node::Reference {
                    origin: RefOrigin::Type { range },
                    ..
                } = node
                else {
                    return None;
                };
                let local = analysis.local_range(*range)?;
                covers(local)?;
                Some(node_id)
            })
        })
}

fn quickfix_action(
    uri: &Url,
    version: Option<i32>,
    title: &str,
    diagnostic: &Diagnostic,
    edits: Vec<TextEdit>,
    preferred: bool,
) -> CodeActionOrCommand {
    CodeActionOrCommand::CodeAction(CodeAction {
        title: title.to_string(),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![diagnostic.clone()]),
        edit: Some(single_file_edit(uri, version, edits)),
        is_preferred: Some(preferred),
        ..CodeAction::default()
    })
}

fn single_file_edit(uri: &Url, version: Option<i32>, edits: Vec<TextEdit>) -> WorkspaceEdit {
    WorkspaceEdit {
        document_changes: Some(DocumentChanges::Edits(vec![TextDocumentEdit {
            text_document: OptionalVersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version,
            },
            edits: edits.into_iter().map(OneOf::Left).collect(),
        }])),
        ..WorkspaceEdit::default()
    }
}

fn diagnostic_code(diagnostic: &Diagnostic) -> Option<&str> {
    match &diagnostic.code {
        Some(NumberOrString::String(code)) => Some(code.as_str()),
        _ => None,
    }
}

/// The diagnostics the LSP publishes that this module can fix through the
/// analysis-based path, so gate the (relatively costly) analysis run on this
/// check.
pub(crate) fn has_analysis_fix_diagnostics(diagnostics: &[Diagnostic]) -> bool {
    diagnostics.iter().any(|diagnostic| {
        matches!(
            diagnostic_code(diagnostic),
            Some(UNRESOLVED_NAME_CODE) | Some(UNKNOWN_METHOD_CODE) | Some(MISSING_IMPL_METHOD_CODE)
        )
    })
}

fn diagnostic_span_offsets(source: &str, diagnostic: &Diagnostic) -> Option<(usize, usize)> {
    let start = offset_for_position(source, diagnostic.range.start)?;
    let end = offset_for_position(source, diagnostic.range.end)?;
    (start <= end).then_some((start, end))
}

/// Extracts the text between the backticks that follow `marker`.
fn quoted_after<'a>(message: &'a str, marker: &str) -> Option<&'a str> {
    let start = message.find(marker)? + marker.len();
    let rest = message.get(start..)?;
    let end = rest.find('`')?;
    rest.get(..end)
}

fn edit_at(source: &str, range: TextRange, new_text: String) -> Option<TextEdit> {
    LineIndex::new(source)
        .range(source, range)
        .map(|range| TextEdit { range, new_text })
}

/// Test-only helpers: fixes against a standalone analysis of `source`.
#[cfg(feature = "test")]
pub mod fixtures {
    use super::*;

    fn standalone_analysis(source: &str) -> DocumentAnalysis {
        let mut session = riddlec::pipeline::CheckSession::new();
        let result = session
            .infer_with_options_cancellable(source, CompileOptions::default(), || false)
            .expect("inference should not be cancelled");
        DocumentAnalysis {
            result: std::sync::Arc::new(result),
            source: source.into(),
            source_map: None,
            macro_occurrences: Vec::new(),
            macro_source_map: None,
            path: None,
            project_root: None,
            project_revision: 0,
            files: Vec::new(),
        }
    }

    /// Quick fixes plus analysis-based fixes, with the analysis derived from
    /// `source` itself (single-file, no project).
    #[must_use]
    pub fn quick_fixes_for_source(source: &str, diagnostics: &[Diagnostic]) -> CodeActionResponse {
        let uri = Url::parse("untitled:quick-fixes.rid").expect("static URI");
        let mut fixes = quick_fixes(&uri, None, source, diagnostics);
        if has_analysis_fix_diagnostics(diagnostics) {
            let analysis = standalone_analysis(source);
            fixes.extend(analysis_fixes(&uri, None, source, diagnostics, &analysis));
        }
        fixes
    }

    #[must_use]
    pub fn organize_imports_for_source(source: &str) -> Vec<TextEdit> {
        organize_import_edits(source).unwrap_or_default()
    }
}
