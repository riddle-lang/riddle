mod code_actions;
mod completion;
mod completion_hang;
mod diagnostics;
mod editing;
mod editor_features;
mod hierarchy;
mod manifest;
mod navigation;
mod project_features;
mod semantic_tokens;
mod workspace_and_cli;
mod workspace_diagnostics;
mod workspace_index;

use lsp_types::{
    CompletionItemKind, DiagnosticSeverity, DocumentChanges, GotoDefinitionResponse, HoverContents,
    InlayHintLabel, InsertTextFormat, ParameterLabel, Position, PrepareRenameResponse, Range,
    SemanticToken, SemanticTokens, TextDocumentContentChangeEvent,
};
use riddle_lsp::{
    parse_args,
    test_support::{
        AnalysisSessions, ApplyError, DiagnosticSessions, Document, MOD_DECLARATION,
        MOD_DEFAULT_LIBRARY, MOD_MUTABLE, MOD_STATIC, ProjectIndex, RequestRevisions,
        TOKEN_COMMENT, TOKEN_ENUM, TOKEN_FUNCTION, TOKEN_INTERFACE, TOKEN_KEYWORD, TOKEN_MACRO,
        TOKEN_METHOD, TOKEN_PARAMETER, TOKEN_STRING, TOKEN_STRUCT, TOKEN_TYPE, TOKEN_VARIABLE,
        WorkspaceState, apply_content_changes, collect_diagnostics, collect_document_diagnostics,
        collect_workspace_diagnostics, collect_workspace_diagnostics_cancellable,
        collect_workspace_diagnostics_with_sessions, completion_items_for_document,
        completion_items_for_source, definition_for_document, definition_for_source,
        discover_projects, document_highlights_for_source, document_links_for_source,
        document_symbols_for_source, documents_for_uri, folding_ranges, format_source,
        hover_for_document, hover_for_document_cancellable, hover_for_source,
        implementation_for_source, inlay_hints_for_document, inlay_hints_for_source,
        manifest_completions, manifest_diagnostics, manifest_document_symbols, manifest_hover,
        offset_for_position, offset_for_position_clamped, organize_imports_for_source,
        prepare_rename_for_document, prepare_rename_for_source, project_index_for_document,
        project_index_for_root, quick_fixes, quick_fixes_for_source, references_for_document,
        references_for_source, rename_for_document, rename_for_source, selection_ranges_for_source,
        semantic_token_delta, semantic_tokens_for_document, semantic_tokens_for_source,
        semantic_tokens_for_source_with_options, signature_help_for_source, to_lsp, to_lsp_mapped,
        type_definition_for_source, workspace_symbols_for_index, workspace_symbols_for_source,
    },
};
use riddlec::pipeline::{CompileOptions, IntoDiagnosticExt};
use rowan::TextRange;
use std::{
    cell::Cell,
    collections::HashMap,
    env,
    ffi::OsString,
    fs,
    path::PathBuf,
    process::{self, Command},
    time::{Duration, SystemTime, UNIX_EPOCH},
};

const DOCUMENTED_ERROR_CODES: &[&str] = &[
    "E0001", "E0002", "E0003", "E0004", "E0005", "E0006", "E0007", "E0008", "E0009", "E0010",
    "E0011", "E0012", "E0013", "E0020", "E0021", "E0022", "E0023", "E0024", "E0025", "E0026",
    "E0027", "E0028", "E0029", "E0030", "E0031", "E0032", "E0033", "E0034", "E0035", "E0036",
    "E0037", "E0038", "E0039", "E0040", "E0041", "E0042", "E0043", "E0044", "E0045", "E0046",
    "E0047", "E0048", "E0049", "E0050", "E0051", "E0052", "E0053", "E0054", "E0055", "E0056",
    "E0057", "E0058", "E0059", "E0060", "E0061", "E0062", "E0063", "E0064", "E0065", "E0066",
    "E0067", "E0072", "E0100", "E0200", "E0300", "E0301", "E0302", "E0303", "E0304", "E0305",
    "E0306", "E0307", "E0308", "E0310", "E0391", "E0400",
];
// E0400 is emitted by the proc-macro driver rather than from user source spans.
const SOURCE_UNREACHABLE_CODES: &[&str] = &["E0048", "E0049", "E0200", "E0400"];

fn temp_root(name: &str) -> PathBuf {
    env::temp_dir().join(format!(
        "riddle-lsp-{name}-{}-{}",
        process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ))
}

fn c_compiler_available() -> bool {
    env::var_os("CC")
        .into_iter()
        .chain(
            ["cc", "gcc", "clang", "clang-cl", "cl"]
                .into_iter()
                .map(OsString::from),
        )
        .any(|compiler| Command::new(compiler).arg("--version").output().is_ok())
}

fn semantic_tokens(source: &str) -> SemanticTokens {
    semantic_tokens_for_source(source)
}

fn source_label(
    range: TextRange,
    message: &str,
    style: type_checker::LabelStyle,
) -> type_checker::SourceLabel {
    type_checker::SourceLabel {
        range,
        message: message.into(),
        style,
    }
}

fn diagnostic_ext(
    code: &'static str,
    severity: type_checker::Severity,
    labels: Vec<type_checker::SourceLabel>,
) -> riddlec::pipeline::DiagnosticExt {
    riddlec::pipeline::DiagnosticExt {
        code,
        severity,
        message: "message".into(),
        labels,
        help: None,
        notes: Vec::new(),
    }
}

fn position(source: &str, offset: usize) -> Position {
    let prefix = &source[..offset];
    let line_start = prefix.rfind('\n').map_or(0, |newline| newline + 1);
    Position::new(
        u32::try_from(prefix.bytes().filter(|byte| *byte == b'\n').count())
            .expect("test line should fit in u32"),
        u32::try_from(source[line_start..offset].encode_utf16().count())
            .expect("test column should fit in u32"),
    )
}

fn text_size(value: usize) -> rowan::TextSize {
    rowan::TextSize::from(u32::try_from(value).expect("test offset should fit in u32"))
}

fn text_range(start: usize, end: usize) -> TextRange {
    TextRange::new(text_size(start), text_size(end))
}

fn range(source: &str, range: TextRange) -> Range {
    Range::new(
        position(source, range.start().into()),
        position(source, range.end().into()),
    )
}

#[derive(Debug, PartialEq, Eq)]
struct SemanticTokenPosition {
    line: u32,
    start: u32,
    length: u32,
    token_type: u32,
    token_modifiers_bitset: u32,
}

fn semantic_token_positions(tokens: &SemanticTokens) -> Vec<SemanticTokenPosition> {
    let mut line = 0;
    let mut start = 0;
    tokens
        .data
        .iter()
        .map(|token| {
            line += token.delta_line;
            if token.delta_line == 0 {
                start += token.delta_start;
            } else {
                start = token.delta_start;
            }
            SemanticTokenPosition {
                line,
                start,
                length: token.length,
                token_type: token.token_type,
                token_modifiers_bitset: token.token_modifiers_bitset,
            }
        })
        .collect()
}
