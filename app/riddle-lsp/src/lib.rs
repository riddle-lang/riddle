mod analysis;
mod cli;
mod code_actions;
mod completion;
mod diagnostics;
mod document_link;
mod editor_features;
mod hierarchy;
mod imports;
mod index;
mod inlay_hints;
mod manifest_lsp;
mod navigation;
mod selection_range;
mod semantic_tokens;
mod server;
mod session;
mod suggest;
mod text;
mod workspace;

pub use cli::{Options, parse_args};

/// Turns per-phase latency tracing on or off.
///
/// Enabled by `--trace-latency` (or `RIDDLE_LSP_TRACE_LATENCY`). With it off,
/// each phase costs one relaxed atomic load.
pub fn set_latency_tracing(enabled: bool) {
    server::set_latency_tracing(enabled);
}

#[cfg(feature = "test")]
#[doc(hidden)]
pub mod test_support {
    pub use crate::code_actions::fixtures::{organize_imports_for_source, quick_fixes_for_source};
    pub use crate::code_actions::quick_fixes;
    pub use crate::completion::{completion_items_for_document, completion_items_for_source};
    pub use crate::diagnostics::{
        DiagnosticSessions, PublishedDiagnostics, collect_diagnostics,
        collect_document_diagnostics, collect_workspace_diagnostics,
        collect_workspace_diagnostics_cancellable, collect_workspace_diagnostics_with_sessions,
        to_lsp, to_lsp_mapped,
    };
    pub use crate::document_link::document_links_for_source;
    pub use crate::editor_features::{
        document_symbols_for_source, folding_ranges, format_source, workspace_symbols_for_source,
    };
    pub use crate::index::{
        IndexedSymbolKind, ProjectIndex, SymbolKey, project_index_for_document,
        project_index_for_root, workspace_symbols_for_index,
    };
    pub use crate::inlay_hints::{inlay_hints_for_document, inlay_hints_for_source};
    pub use crate::manifest_lsp::{
        manifest_completions, manifest_diagnostics, manifest_document_symbols, manifest_hover,
    };
    pub use crate::navigation::{
        definition_for_document, definition_for_source, document_highlights_for_source,
        hover_for_document, hover_for_document_cancellable, hover_for_source,
        implementation_for_source, prepare_rename_for_document, prepare_rename_for_source,
        references_for_document, references_for_source, rename_for_document, rename_for_source,
        signature_help_for_source, type_definition_for_source,
    };
    pub use crate::selection_range::selection_ranges_for_source;
    pub use crate::semantic_tokens::{
        MOD_DECLARATION, MOD_DEFAULT_LIBRARY, MOD_MUTABLE, MOD_STATIC, TOKEN_COMMENT, TOKEN_ENUM,
        TOKEN_FUNCTION, TOKEN_INTERFACE, TOKEN_KEYWORD, TOKEN_MACRO, TOKEN_METHOD, TOKEN_PARAMETER,
        TOKEN_STRING, TOKEN_STRUCT, TOKEN_TYPE, TOKEN_VARIABLE, semantic_token_delta,
        semantic_tokens_for_document, semantic_tokens_for_source,
        semantic_tokens_for_source_with_options,
    };
    pub use crate::server::{Document, RequestRevisions, documents_for_uri};
    pub use crate::session::AnalysisSessions;
    pub use crate::text::offset_for_position;
    pub use crate::text::offset_for_position_clamped;
    pub use crate::text::{ApplyError, PositionEncoding, apply_content_changes};
    pub use crate::workspace::{WorkspaceState, discover_projects};
}

use server::Backend;
use tower_lsp::{LspService, Server};

pub async fn serve(options: Options) {
    server::set_latency_tracing(options.trace_latency);
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(|client| {
        Backend::new(client, options.compile_options, options.completion_delay)
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
