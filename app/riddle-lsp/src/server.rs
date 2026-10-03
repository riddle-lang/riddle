use std::{
    collections::{HashMap, HashSet},
    hash::BuildHasher,
    path::PathBuf,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, AtomicU64, Ordering},
    },
    time::Duration,
};

use lsp_types::request::{
    GotoDeclarationParams, GotoDeclarationResponse, GotoImplementationParams,
    GotoImplementationResponse, GotoTypeDefinitionParams, GotoTypeDefinitionResponse,
};
use lsp_types::{
    CallHierarchyIncomingCall, CallHierarchyIncomingCallsParams, CallHierarchyItem,
    CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsParams, CallHierarchyPrepareParams,
    CallHierarchyServerCapability, CodeActionKind, CodeActionOptions, CodeActionParams,
    CodeActionProviderCapability, CodeActionResponse, CompletionList, CompletionOptions,
    CompletionParams, CompletionResponse, CompletionTriggerKind, DeclarationCapability,
    DiagnosticOptions, DiagnosticServerCapabilities, DidChangeTextDocumentParams,
    DidChangeWatchedFilesParams, DidChangeWatchedFilesRegistrationOptions,
    DidChangeWorkspaceFoldersParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, DocumentDiagnosticParams, DocumentDiagnosticReport,
    DocumentDiagnosticReportResult, DocumentFormattingParams, DocumentHighlight,
    DocumentHighlightParams, DocumentLink, DocumentLinkOptions, DocumentLinkParams,
    DocumentRangeFormattingParams, DocumentSymbolParams, DocumentSymbolResponse, FileSystemWatcher,
    FoldingRange, FoldingRangeParams, FoldingRangeProviderCapability, FullDocumentDiagnosticReport,
    GlobPattern, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverParams,
    HoverProviderCapability, ImplementationProviderCapability, InitializeParams, InitializeResult,
    InitializedParams, InlayHint, InlayHintParams, MessageType, OneOf, PrepareRenameResponse,
    ReferenceParams, Registration, RelatedFullDocumentDiagnosticReport, RenameOptions,
    RenameParams, SelectionRange, SelectionRangeParams, SelectionRangeProviderCapability,
    SemanticTokens, SemanticTokensDeltaParams, SemanticTokensFullDeltaResult,
    SemanticTokensFullOptions, SemanticTokensOptions, SemanticTokensParams,
    SemanticTokensRangeParams, SemanticTokensRangeResult, SemanticTokensResult,
    SemanticTokensServerCapabilities, ServerCapabilities, ServerInfo, SignatureHelp,
    SignatureHelpOptions, SignatureHelpParams, SymbolInformation, TextDocumentPositionParams,
    TextDocumentRegistrationOptions, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TextDocumentSyncSaveOptions, TextEdit,
    TypeDefinitionProviderCapability, TypeHierarchyItem, TypeHierarchyPrepareParams,
    TypeHierarchyRegistrationOptions, TypeHierarchySubtypesParams, TypeHierarchySupertypesParams,
    WorkspaceDiagnosticParams, WorkspaceDiagnosticReport, WorkspaceDiagnosticReportResult,
    WorkspaceDocumentDiagnosticReport, WorkspaceEdit, WorkspaceFoldersServerCapabilities,
    WorkspaceFullDocumentDiagnosticReport, WorkspaceServerCapabilities, WorkspaceSymbolParams,
};
use riddlec::pipeline::CompileOptions;
use tower_lsp::jsonrpc::Result;
use tower_lsp::{Client, LanguageServer};

use crate::{
    code_actions::{
        self, add_missing_imports_action, analysis_fixes_for_document_cancellable, fix_all_action,
        has_analysis_fix_diagnostics, organize_imports_action, quick_fixes,
    },
    completion::{
        completion_items_for_document, completion_trigger_characters, completion_trigger_is_active,
    },
    diagnostics::{self, DiagnosticSessions, collect_workspace_diagnostics_cancellable},
    document_link::document_links_for_text,
    editor_features::{
        document_symbols_for_document_cancellable, folding_ranges, format_source,
        workspace_symbols_for_document_cancellable,
    },
    hierarchy::{
        incoming_calls as hierarchy_incoming_calls, outgoing_calls as hierarchy_outgoing_calls,
        prepare_call_hierarchy as hierarchy_prepare_call,
        prepare_type_hierarchy as hierarchy_prepare_type, subtypes as hierarchy_subtypes,
        supertypes as hierarchy_supertypes,
    },
    index::{project_index_for_root_cancellable, workspace_symbols_for_index},
    inlay_hints::inlay_hints_for_document_cancellable,
    manifest_lsp::{
        is_manifest_uri, manifest_completions, manifest_document_symbols, manifest_hover,
    },
    navigation::{
        RenameError, definition_for_document_cancellable,
        document_highlights_for_document_cancellable, hover_for_document_cancellable,
        implementation_for_document_cancellable, prepare_rename_for_document_cancellable,
        references_for_document_cancellable, rename_for_document_cancellable,
        signature_help_for_document_cancellable, type_definition_for_document_cancellable,
        validate_identifier,
    },
    selection_range::selection_ranges_for_text,
    semantic_tokens::{
        semantic_token_delta, semantic_tokens_for_document_cancellable,
        semantic_tokens_for_document_range_cancellable, semantic_tokens_legend,
    },
    session::AnalysisSessions,
    text::{LineIndex, apply_content_changes},
    workspace::WorkspaceState,
};

pub struct Backend {
    client: Client,
    docs: Arc<Mutex<HashMap<lsp_types::Url, Document>>>,
    published: Arc<Mutex<HashMap<lsp_types::Url, diagnostics::PublishedDiagnostics>>>,
    publish_gate: Arc<tokio::sync::Mutex<()>>,
    diagnostic_revision: Arc<AtomicU64>,
    diagnostic_sessions: Arc<Mutex<DiagnosticSessions>>,
    /// Sessions shared by every analysis, including the marker-source variant
    /// completion uses.
    ///
    /// Completion used to have a second set of its own, so that the marked and
    /// unmarked sources would not evict each other's parser. That isolation cost
    /// a second full build of the standard library's ~827 checked bodies, and
    /// each session set then re-checked them again whenever its variant came
    /// back. Both variants are cached by input fingerprint now, so one set
    /// serves both and the standard library is built once per process.
    sessions: Arc<AnalysisSessions>,
    analysis_revisions: Arc<RequestRevisions>,
    completion_revisions: Arc<RequestRevisions>,
    semantic_tokens: Arc<Mutex<HashMap<lsp_types::Url, CachedSemanticTokens>>>,
    semantic_token_revision: Arc<AtomicU64>,
    supports_watched_files: AtomicBool,
    supports_type_hierarchy: AtomicBool,
    workspace: Arc<WorkspaceState>,
    compile_options: CompileOptions,
    completion_delay: Duration,
}

const DIAGNOSTICS_DEBOUNCE: Duration = Duration::from_millis(150);
const INDEX_DEBOUNCE: Duration = Duration::from_millis(300);

/// Latency instrumentation, enabled by `--trace-latency`.
///
/// The crate had no way to measure where a request's time went, which made every
/// ordering decision about performance a guess. With the flag off this costs one
/// relaxed atomic load per sample and no allocation.
mod telemetry {
    use std::{
        sync::atomic::{AtomicBool, AtomicU64, Ordering},
        time::Duration,
    };

    static ENABLED: AtomicBool = AtomicBool::new(false);
    static TOTAL_MICROS: AtomicU64 = AtomicU64::new(0);
    static SAMPLES: AtomicU64 = AtomicU64::new(0);

    pub(crate) fn set_enabled(enabled: bool) {
        ENABLED.store(enabled, Ordering::SeqCst);
    }

    #[must_use]
    pub(super) fn enabled() -> bool {
        ENABLED.load(Ordering::Relaxed)
    }

    /// Registers a completed phase, returning its formatted log line.
    #[must_use]
    pub(super) fn report(phase: &str, elapsed: Duration) -> String {
        let total = TOTAL_MICROS.fetch_add(
            u64::try_from(elapsed.as_micros()).unwrap_or(u64::MAX),
            Ordering::Relaxed,
        );
        let samples = SAMPLES.fetch_add(1, Ordering::Relaxed);
        format!(
            "[latency] {phase}: {:.1} ms (cumulative {:.1} ms over {} phases)",
            elapsed.as_secs_f64() * 1000.0,
            Duration::from_micros(total).as_secs_f64() * 1000.0,
            samples + 1,
        )
    }
}

pub(crate) use telemetry::set_enabled as set_latency_tracing;

/// Times one phase of a request and logs it when `--trace-latency` is on.
pub(crate) struct Phase {
    name: &'static str,
    started: Option<std::time::Instant>,
}

impl Phase {
    pub(crate) fn start(name: &'static str) -> Self {
        Self {
            name,
            started: telemetry::enabled().then(std::time::Instant::now),
        }
    }
}

/// Runs `body`, recording how long it took once it returns.
///
/// A closure form is needed wherever the guarded value is returned from the
/// enclosing scope, since `Phase` reports on drop.
pub(crate) fn measure<T>(name: &'static str, body: impl FnOnce() -> T) -> T {
    let phase = Phase::start(name);
    let value = body();
    drop(phase);
    value
}

impl Drop for Phase {
    fn drop(&mut self) {
        if let Some(started) = self.started {
            eprintln!("{}", telemetry::report(self.name, started.elapsed()));
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Document {
    pub text: String,
    pub version: Option<i32>,
    /// Set when a content-change batch could not be applied, so the buffered
    /// text may no longer match the editor's.
    ///
    /// The document stays in the map (so `did_close` still cleans up and a
    /// later full-text change can recover) but is excluded from analysis: an
    /// unanalysable buffer must never be published as "no diagnostics".
    pub out_of_sync: bool,
}

impl Document {
    /// A document that is synchronised with its editor.
    #[must_use]
    pub fn new(text: impl Into<String>, version: Option<i32>) -> Self {
        Self {
            text: text.into(),
            version,
            out_of_sync: false,
        }
    }
}

/// What `did_change` decided, carried out of the `docs` lock so no guard is
/// held across an `.await`.
enum ChangeOutcome {
    /// The buffer was updated and the document is in sync.
    Applied,
    /// Nothing changed; a warning explains why.
    Report(String),
    /// The document disappeared between the two lookups.
    Skipped,
}

#[derive(Clone)]
struct CachedSemanticTokens {
    text: String,
    project_revision: u64,
    tokens: SemanticTokens,
}

#[derive(Default)]
pub struct RequestRevisions(Mutex<HashMap<lsp_types::Url, u64>>);

impl RequestRevisions {
    /// Starts a new request revision for a document.
    ///
    /// # Panics
    ///
    /// Panics if the revision mutex is poisoned.
    pub fn begin(&self, uri: &lsp_types::Url) -> u64 {
        let mut revisions = self.0.lock().unwrap();
        let revision = revisions.entry(uri.clone()).or_default();
        *revision += 1;
        let current = *revision;
        drop(revisions);
        current
    }

    /// Returns whether a request revision is still current.
    ///
    /// # Panics
    ///
    /// Panics if the revision mutex is poisoned.
    pub fn is_current(&self, uri: &lsp_types::Url, revision: u64) -> bool {
        self.current(uri) == revision
    }

    /// Returns the current request revision for a document.
    ///
    /// # Panics
    ///
    /// Panics if the revision mutex is poisoned.
    pub fn current(&self, uri: &lsp_types::Url) -> u64 {
        self.0.lock().unwrap().get(uri).copied().unwrap_or(0)
    }

    /// Removes the request revision for a document.
    ///
    /// # Panics
    ///
    /// Panics if the revision mutex is poisoned.
    pub fn remove(&self, uri: &lsp_types::Url) {
        self.0.lock().unwrap().remove(uri);
    }
}

const ORGANIZE_IMPORTS_KIND: &str = code_actions::ORGANIZE_IMPORTS_KIND;
const ADD_MISSING_IMPORTS_KIND: &str = code_actions::ADD_MISSING_IMPORTS_KIND;
const FIX_ALL_KIND: &str = code_actions::FIX_ALL_KIND;

/// The code-action kinds the server can actually produce.
///
/// Advertising them matters as much as producing them: a client that sees no
/// `codeActionKinds` cannot offer the action in its "Source Action…" menu, so
/// `source.organizeImports` was unreachable from VS Code even though the server
/// had implemented it.
fn code_action_kinds() -> Vec<CodeActionKind> {
    vec![
        CodeActionKind::QUICKFIX,
        CodeActionKind::new(ORGANIZE_IMPORTS_KIND),
        CodeActionKind::new(ADD_MISSING_IMPORTS_KIND),
        CodeActionKind::new(FIX_ALL_KIND),
    ]
}

/// LSP code action kinds are hierarchical: a request for `source` also covers
/// `source.organizeImports`, and an empty kind string means "everything".
fn code_action_kind_requested(only: Option<&[CodeActionKind]>, kind: &str) -> bool {
    let Some(only) = only else {
        return true;
    };
    only.iter().any(|requested| {
        let requested = requested.as_str();
        requested.is_empty()
            || requested == kind
            || kind.starts_with(&format!("{requested}."))
            || requested.starts_with(&format!("{kind}."))
    })
}

fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        position_encoding: Some(crate::text::position_encoding().as_lsp_kind()),
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::INCREMENTAL),
                will_save: None,
                will_save_wait_until: None,
                // Saving is what triggers a workspace-index rebuild.
                save: Some(TextDocumentSyncSaveOptions::Supported(true)),
            },
        )),
        code_action_provider: Some(CodeActionProviderCapability::Options(CodeActionOptions {
            code_action_kinds: Some(code_action_kinds()),
            work_done_progress_options: lsp_types::WorkDoneProgressOptions::default(),
            resolve_provider: Some(false),
        })),
        document_formatting_provider: Some(OneOf::Left(true)),
        document_range_formatting_provider: Some(OneOf::Left(true)),
        document_highlight_provider: Some(OneOf::Left(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        declaration_provider: Some(DeclarationCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
        implementation_provider: Some(ImplementationProviderCapability::Simple(true)),
        call_hierarchy_provider: Some(CallHierarchyServerCapability::Simple(true)),
        // `lsp-types 0.94` models `TypeHierarchyClientCapabilities` but has no
        // `ServerCapabilities.type_hierarchy_provider` field, so static
        // announcement is impossible with this dependency version: dynamic
        // registration in `initialized` is the only channel available. See the
        // note there for how clients without dynamic registration are handled.
        references_provider: Some(OneOf::Left(true)),
        rename_provider: Some(OneOf::Right(RenameOptions {
            prepare_provider: Some(true),
            work_done_progress_options: lsp_types::WorkDoneProgressOptions::default(),
        })),
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(false),
            trigger_characters: Some(completion_trigger_characters()),
            ..CompletionOptions::default()
        }),
        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".into(), ",".into()]),
            retrigger_characters: Some(vec![",".into()]),
            ..SignatureHelpOptions::default()
        }),
        inlay_hint_provider: Some(OneOf::Left(true)),
        selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
        document_link_provider: Some(DocumentLinkOptions {
            resolve_provider: Some(false),
            work_done_progress_options: lsp_types::WorkDoneProgressOptions::default(),
        }),
        diagnostic_provider: Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
            identifier: None,
            inter_file_dependencies: true,
            // The workspace engine already computes diagnostics for unopened
            // modules and local dependencies; only the request was missing.
            workspace_diagnostics: true,
            work_done_progress_options: lsp_types::WorkDoneProgressOptions::default(),
        })),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::from(
            SemanticTokensOptions {
                legend: semantic_tokens_legend(),
                full: Some(SemanticTokensFullOptions::Delta { delta: Some(true) }),
                // Let the client ask for one visible window instead of the
                // whole file.
                range: Some(true),
                ..SemanticTokensOptions::default()
            },
        )),
        workspace: Some(WorkspaceServerCapabilities {
            workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                supported: Some(true),
                change_notifications: Some(OneOf::Left(true)),
            }),
            file_operations: Some(lsp_types::WorkspaceFileOperationsServerCapabilities {
                // Rewriting `use` paths when a module file is renamed needs to
                // happen *before* the rename, while the old paths still resolve.
                will_rename: Some(lsp_types::FileOperationRegistrationOptions {
                    filters: vec![lsp_types::FileOperationFilter {
                        scheme: Some("file".into()),
                        pattern: lsp_types::FileOperationPattern {
                            glob: "**/*.rid".into(),
                            matches: Some(lsp_types::FileOperationPatternKind::File),
                            options: None,
                        },
                    }],
                }),
                ..lsp_types::WorkspaceFileOperationsServerCapabilities::default()
            }),
        }),
        ..ServerCapabilities::default()
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // The position encoding has to be settled before any document exists:
        // every offset the server sends or receives afterwards is expressed in
        // it. Answering in UTF-16 to a client that only speaks UTF-8 or UTF-32
        // would silently shift every position on any line with a multi-byte
        // character.
        let Some(encoding) = crate::text::negotiate_position_encoding(
            params
                .capabilities
                .general
                .as_ref()
                .and_then(|general| general.position_encodings.as_deref()),
        ) else {
            let offered = params
                .capabilities
                .general
                .as_ref()
                .and_then(|general| general.position_encodings.as_deref())
                .unwrap_or_default()
                .iter()
                .map(lsp_types::PositionEncodingKind::as_str)
                .collect::<Vec<_>>()
                .join(", ");
            return Err(tower_lsp::jsonrpc::Error::invalid_params(format!(
                "riddle-lsp needs one of the position encodings utf-16, utf-8 or utf-32; \
                 the client offered only: {offered}"
            )));
        };
        crate::text::set_position_encoding(encoding);
        self.supports_watched_files.store(
            params
                .capabilities
                .workspace
                .as_ref()
                .and_then(|workspace| workspace.did_change_watched_files.as_ref())
                .and_then(|capabilities| capabilities.dynamic_registration)
                .unwrap_or(false),
            Ordering::SeqCst,
        );
        self.supports_type_hierarchy.store(
            params
                .capabilities
                .text_document
                .as_ref()
                .and_then(|capabilities| capabilities.type_hierarchy.as_ref())
                .and_then(|capabilities| capabilities.dynamic_registration)
                .unwrap_or(false),
            Ordering::SeqCst,
        );
        let workspace_roots = params
            .workspace_folders
            .as_deref()
            .unwrap_or_default()
            .iter()
            .filter_map(|folder| folder.uri.to_file_path().ok())
            .chain(
                params
                    .workspace_folders
                    .is_none()
                    .then_some(params.root_uri.as_ref())
                    .flatten()
                    .and_then(|uri| uri.to_file_path().ok()),
            );
        if let Err(error) = self.workspace.set_roots(workspace_roots) {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("failed to discover workspace projects: {error}"),
                )
                .await;
        }
        Ok(InitializeResult {
            capabilities: server_capabilities(),
            server_info: Some(ServerInfo {
                name: "riddle-lsp".into(),
                version: Some(format!(
                    "{} ({})",
                    env!("CARGO_PKG_VERSION"),
                    riddlec::GIT_HASH
                )),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        if self.supports_watched_files.load(Ordering::SeqCst) {
            let options = DidChangeWatchedFilesRegistrationOptions {
                // `Clue.lock` is watched too: a lockfile rewrite changes which
                // dependency versions the project resolves to.
                watchers: ["**/*.rid", "**/Clue.toml", "**/Clue.lock"]
                    .into_iter()
                    .map(|pattern| FileSystemWatcher {
                        glob_pattern: GlobPattern::String(pattern.into()),
                        kind: None,
                    })
                    .collect(),
            };
            let registration = Registration {
                id: "riddle-watched-files".into(),
                method: "workspace/didChangeWatchedFiles".into(),
                register_options: Some(
                    serde_json::to_value(options)
                        .expect("watched-file registration options must serialize"),
                ),
            };
            if let Err(error) = self.client.register_capability(vec![registration]).await {
                self.client
                    .log_message(
                        MessageType::WARNING,
                        format!("failed to register file watchers: {error}"),
                    )
                    .await;
            }
        }
        if self.supports_type_hierarchy.load(Ordering::SeqCst) {
            let options = TypeHierarchyRegistrationOptions {
                text_document_registration_options: TextDocumentRegistrationOptions {
                    document_selector: None,
                },
                ..TypeHierarchyRegistrationOptions::default()
            };
            let registration = Registration {
                id: "riddle-type-hierarchy".into(),
                method: "textDocument/prepareTypeHierarchy".into(),
                register_options: Some(
                    serde_json::to_value(options)
                        .expect("type hierarchy registration options must serialize"),
                ),
            };
            if let Err(error) = self.client.register_capability(vec![registration]).await {
                self.client
                    .log_message(
                        MessageType::WARNING,
                        format!("failed to register type hierarchy: {error}"),
                    )
                    .await;
            }
        }
        self.client
            .log_message(MessageType::INFO, "riddle-lsp initialized")
            .await;
        self.schedule_workspace_indexing();
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let doc = Document::new(
            params.text_document.text,
            Some(params.text_document.version),
        );
        let mut docs = self.docs.lock().unwrap();
        docs.insert(uri.clone(), doc);
        bump_related_revisions(&self.analysis_revisions, &docs, &uri);
        drop(docs);
        self.schedule_diagnostics();
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        // A save is the natural moment to rebuild the workspace index: the
        // buffer is settled, so the `Infer`-depth pass is not thrown away by
        // the next keystroke.
        self.schedule_document_indexing(&params.text_document.uri);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let outcome = self.apply_change(&params);
        self.report_change(outcome).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        // Remove the document and capture the current open set in a single
        // critical section to avoid a TOCTOU race between remove() and
        // the retain_open() calls below.
        let open_docs = {
            let mut docs = self.docs.lock().unwrap();
            let related = related_document_uris(&docs, &uri);
            docs.remove(&uri);
            for related_uri in related {
                self.analysis_revisions.begin(&related_uri);
            }
            docs.clone()
        };
        self.semantic_tokens.lock().unwrap().remove(&uri);
        self.analysis_revisions.remove(&uri);
        self.completion_revisions.remove(&uri);
        self.sessions.retain_open(&open_docs);
        self.sessions.retain_open(&open_docs);
        self.schedule_diagnostics();
        self.schedule_document_indexing(&uri);
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let Some(original) = self.document_text(&params.text_document.uri) else {
            return Ok(None);
        };
        // Formatting reparses the whole file, so it runs on the blocking pool:
        // a synchronous call here would stall every other request, because
        // tower-lsp polls its handlers inside a single driver task.
        let options = params.options;
        let source = original.clone();
        let formatted = tokio::task::spawn_blocking(move || {
            let _phase = Phase::start("formatting");
            format_source(&source, options.tab_size, options.insert_spaces)
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        if formatted == original {
            return Ok(Some(Vec::new()));
        }
        let end = LineIndex::new(&original)
            .position(&original, original.len())
            .unwrap_or_default();
        Ok(Some(vec![TextEdit::new(
            lsp_types::Range::new(lsp_types::Position::new(0, 0), end),
            formatted,
        )]))
    }

    async fn range_formatting(
        &self,
        params: DocumentRangeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let Some(original) = self.document_text(&params.text_document.uri) else {
            return Ok(None);
        };
        let options = params.options;
        let range = params.range;
        let source = original.clone();
        // The formatter is whole-document, so format everything and then keep
        // only the line-level edits that intersect the requested range. That
        // reuses one well-tested formatter instead of teaching it about ranges,
        // and it cannot corrupt text outside the selection.
        let edits = tokio::task::spawn_blocking(move || {
            let _phase = Phase::start("range_formatting");
            let formatted = format_source(&source, options.tab_size, options.insert_spaces);
            line_edits_within(&source, &formatted, range)
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        Ok(Some(edits))
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let Some(document) = self.document(&params.text_document.uri) else {
            return Ok(None);
        };
        let ranges = tokio::task::spawn_blocking(move || {
            let _phase = Phase::start("folding_range");
            folding_ranges(&document.text)
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        Ok(Some(ranges))
    }

    async fn selection_range(
        &self,
        params: SelectionRangeParams,
    ) -> Result<Option<Vec<SelectionRange>>> {
        let Some(text) = self.document_text(&params.text_document.uri) else {
            return Ok(None);
        };
        let positions = params.positions;
        let ranges = tokio::task::spawn_blocking(move || {
            let _phase = Phase::start("selection_range");
            selection_ranges_for_text(&text, &positions)
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        Ok(Some(ranges))
    }

    async fn document_link(&self, params: DocumentLinkParams) -> Result<Option<Vec<DocumentLink>>> {
        let uri = params.text_document.uri;
        let Some(text) = self.document_text(&uri) else {
            return Ok(None);
        };
        let module_dir = uri
            .to_file_path()
            .ok()
            .and_then(|path| path.parent().map(std::path::Path::to_path_buf));
        let links = tokio::task::spawn_blocking(move || {
            let _phase = Phase::start("document_link");
            document_links_for_text(&text, module_dir.as_deref())
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        Ok(Some(links))
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Result<DocumentDiagnosticReportResult> {
        let uri = params.text_document.uri;
        let document_version = {
            let docs = self.docs.lock().unwrap();
            docs.get(&uri).and_then(|document| document.version)
        };
        // Fast path: the push pipeline keeps this set current (debounced), so a
        // pull for the already-synced version is served from the cache. This
        // matters because clients that see the pull capability stop listening
        // to pushes and pull after every change.
        if let Some(version) = document_version {
            let published = self.published.lock().unwrap();
            if let Some(entry) = published.get(&uri)
                && entry.version == Some(version)
            {
                return Ok(full_diagnostic_report(
                    Some(format!("v{version}")),
                    entry.diagnostics.clone(),
                ));
            }
        }
        // Stale or missing: recompute through the shared cached sessions — a
        // fresh session set here would recompile the whole project per pull.
        let docs = self.docs.lock().unwrap().clone();
        let options = self.compile_options;
        let diagnostic_sessions = Arc::clone(&self.diagnostic_sessions);
        let revisions = Arc::clone(&self.analysis_revisions);
        let revision = self.analysis_revisions.current(&uri);
        let target = uri.clone();
        let cancelled_uri = uri.clone();
        let collected = tokio::task::spawn_blocking(move || {
            // Cancelling a pull has to stop the CPU, not just the response: the
            // session lock is held for the whole workspace analysis, so a
            // superseded pull would otherwise block every other request.
            let cancelled = || !revisions.is_current(&cancelled_uri, revision);
            let mut sessions = diagnostic_sessions
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            collect_workspace_diagnostics_cancellable(&docs, options, &mut sessions, cancelled)
        })
        .await;
        let diagnostics = match collected {
            Ok(Some(published)) => published
                .into_iter()
                .find(|entry| entry.uri == target)
                .map(|entry| entry.diagnostics)
                .unwrap_or_default(),
            // The analysis was superseded by a newer edit: the client will ask
            // again, and answering "no problems" would be a lie.
            Ok(None) => return Ok(stale_diagnostic_report()),
            Err(error) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("pull diagnostics failed: {error}"),
                    )
                    .await;
                return Ok(stale_diagnostic_report());
            }
        };
        Ok(full_diagnostic_report(
            document_version.map(|version| format!("v{version}")),
            diagnostics,
        ))
    }

    /// Whole-workspace pull diagnostics.
    ///
    /// The engine already produced diagnostics for unopened modules and local
    /// dependencies; only the request was missing, so a client that prefers
    /// pulling had to open every file to see them.
    async fn workspace_diagnostic(
        &self,
        _params: WorkspaceDiagnosticParams,
    ) -> Result<WorkspaceDiagnosticReportResult> {
        let docs = self.docs.lock().unwrap().clone();
        let options = self.compile_options;
        let diagnostic_sessions = Arc::clone(&self.diagnostic_sessions);
        let collected = tokio::task::spawn_blocking(move || {
            let _phase = Phase::start("workspace_diagnostic");
            let mut sessions = diagnostic_sessions
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            collect_workspace_diagnostics_cancellable(&docs, options, &mut sessions, || false)
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        let Some(published) = collected else {
            return Ok(WorkspaceDiagnosticReportResult::Report(
                WorkspaceDiagnosticReport { items: Vec::new() },
            ));
        };
        let items = published
            .into_iter()
            .map(|entry| {
                WorkspaceDocumentDiagnosticReport::Full(WorkspaceFullDocumentDiagnosticReport {
                    uri: entry.uri,
                    version: entry.version.map(i64::from),
                    full_document_diagnostic_report: FullDocumentDiagnosticReport {
                        result_id: entry.version.map(|version| format!("v{version}")),
                        items: entry.diagnostics,
                    },
                })
            })
            .collect();
        Ok(WorkspaceDiagnosticReportResult::Report(
            WorkspaceDiagnosticReport { items },
        ))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;
        if is_manifest_uri(&uri) {
            let text = {
                let docs = self.docs.lock().unwrap();
                docs.get(&uri).map(|document| document.text.clone())
            };
            return Ok(text.map(|text| manifest_document_symbols(&text)));
        }
        let Some((docs, text, revision)) = self.analysis_snapshot(&uri) else {
            return Ok(None);
        };
        let analysis_uri = uri.clone();
        let options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let revisions = Arc::clone(&self.analysis_revisions);
        let result = tokio::task::spawn_blocking(move || {
            let cancelled = || !revisions.is_current(&analysis_uri, revision);
            document_symbols_for_document_cancellable(
                &analysis_uri,
                &docs,
                options,
                &sessions,
                &cancelled,
            )
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        let symbols = match result {
            Ok(Some(symbols)) => symbols,
            Ok(None) => return Ok(None),
            Err(error) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("document symbols failed: {error}"),
                    )
                    .await;
                return Err(tower_lsp::jsonrpc::Error::internal_error());
            }
        };
        if !self.analysis_is_current(&uri, &text, revision) {
            return Ok(None);
        }
        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
    }

    #[allow(deprecated)]
    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let docs = self.docs.lock().unwrap().clone();
        let uris = docs.keys().cloned().collect::<Vec<_>>();
        let open_uris = uris.iter().cloned().collect::<HashSet<_>>();
        let projects = self.workspace.projects();
        let workspace = Arc::clone(&self.workspace);
        let options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let revisions = Arc::clone(&self.analysis_revisions);
        let query = params.query;
        let result = tokio::task::spawn_blocking(move || {
            let mut symbols = Vec::new();
            for project in projects {
                if let Some(index) = workspace.snapshot(&project) {
                    symbols.extend(
                        workspace_symbols_for_index(&index, &query)
                            .into_iter()
                            .filter(|symbol| !open_uris.contains(&symbol.location.uri)),
                    );
                }
            }
            for uri in uris {
                if is_manifest_uri(&uri) {
                    continue;
                }
                let revision = revisions.current(&uri);
                let cancelled = || !revisions.is_current(&uri, revision);
                if let Some(found) = workspace_symbols_for_document_cancellable(
                    &uri, &docs, &query, options, &sessions, &cancelled,
                )? {
                    symbols.extend(found);
                }
            }
            symbols.sort_by(|left, right| {
                left.name
                    .cmp(&right.name)
                    .then_with(|| left.location.uri.as_str().cmp(right.location.uri.as_str()))
                    .then_with(|| {
                        left.location
                            .range
                            .start
                            .line
                            .cmp(&right.location.range.start.line)
                    })
                    .then_with(|| {
                        left.location
                            .range
                            .start
                            .character
                            .cmp(&right.location.range.start.character)
                    })
            });
            symbols
                .dedup_by(|left, right| left.name == right.name && left.location == right.location);
            Ok::<_, String>(symbols)
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        match result {
            Ok(symbols) => Ok(Some(symbols)),
            Err(error) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("workspace symbols failed: {error}"),
                    )
                    .await;
                Err(tower_lsp::jsonrpc::Error::internal_error())
            }
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        let Some((docs, text, analysis_revision)) = self.analysis_snapshot(&uri) else {
            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: Vec::new(),
            })));
        };
        let project_revision = self.sessions.current_revision(&uri, &docs);
        if let Some(cached) = self
            .semantic_tokens
            .lock()
            .unwrap()
            .get(&uri)
            .filter(|cached| {
                cached.text == text && project_revision == Some(cached.project_revision)
            })
        {
            return Ok(Some(SemanticTokensResult::Tokens(cached.tokens.clone())));
        }

        let compile_options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let analysis_revisions = Arc::clone(&self.analysis_revisions);
        let analysis_uri = uri.clone();
        let analyzed = tokio::task::spawn_blocking(move || {
            let cancelled = || !analysis_revisions.is_current(&analysis_uri, analysis_revision);
            semantic_tokens_for_document_cancellable(
                &analysis_uri,
                &docs,
                compile_options,
                &sessions,
                &cancelled,
            )
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        let mut tokens = match analyzed {
            Ok(Some(tokens)) => tokens,
            Ok(None) => return Ok(None),
            Err(error) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("semantic tokens failed: {error}"),
                    )
                    .await;
                return Err(tower_lsp::jsonrpc::Error::internal_error());
            }
        };
        tokens.result_id = Some(
            self.semantic_token_revision
                .fetch_add(1, Ordering::SeqCst)
                .to_string(),
        );
        let project_revision = self.sessions.revision(&uri);
        if !self.analysis_is_current(&uri, &text, analysis_revision) {
            return Ok(None);
        }
        self.semantic_tokens.lock().unwrap().insert(
            uri,
            CachedSemanticTokens {
                text,
                project_revision,
                tokens: tokens.clone(),
            },
        );

        Ok(Some(SemanticTokensResult::Tokens(tokens)))
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let uri = params.text_document.uri;
        let Some((docs, _text, analysis_revision)) = self.analysis_snapshot(&uri) else {
            return Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
                result_id: None,
                data: Vec::new(),
            })));
        };
        let compile_options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let analysis_revisions = Arc::clone(&self.analysis_revisions);
        let analysis_uri = uri.clone();
        let range = params.range;
        let analyzed = tokio::task::spawn_blocking(move || {
            let cancelled = || !analysis_revisions.is_current(&analysis_uri, analysis_revision);
            semantic_tokens_for_document_range_cancellable(
                &analysis_uri,
                &docs,
                range,
                compile_options,
                &sessions,
                &cancelled,
            )
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        match analyzed {
            Ok(Some(tokens)) => Ok(Some(SemanticTokensRangeResult::Tokens(tokens))),
            Ok(None) => Ok(None),
            Err(error) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("semantic tokens range failed: {error}"),
                    )
                    .await;
                Err(tower_lsp::jsonrpc::Error::internal_error())
            }
        }
    }

    async fn semantic_tokens_full_delta(
        &self,
        params: SemanticTokensDeltaParams,
    ) -> Result<Option<SemanticTokensFullDeltaResult>> {
        let uri = params.text_document.uri;
        let previous = self.semantic_tokens.lock().unwrap().get(&uri).cloned();
        let full = self
            .semantic_tokens_full(SemanticTokensParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                work_done_progress_params: params.work_done_progress_params,
                partial_result_params: params.partial_result_params,
            })
            .await?;
        let Some(SemanticTokensResult::Tokens(tokens)) = full else {
            return Ok(None);
        };
        let Some(previous) = previous.filter(|cached| {
            cached.tokens.result_id.as_deref() == Some(params.previous_result_id.as_str())
        }) else {
            return Ok(Some(SemanticTokensFullDeltaResult::Tokens(tokens)));
        };
        Ok(Some(SemanticTokensFullDeltaResult::TokensDelta(
            semantic_token_delta(
                &previous.tokens.data,
                &tokens.data,
                tokens.result_id.clone().unwrap_or_default(),
            ),
        )))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = params.text_document.uri;
        let Some((docs, text, analysis_revision)) = self.analysis_snapshot(&uri) else {
            return Ok(Some(Vec::new()));
        };
        let compile_options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let analysis_revisions = Arc::clone(&self.analysis_revisions);
        let analysis_uri = uri.clone();
        let analyzed = tokio::task::spawn_blocking(move || {
            let cancelled = || !analysis_revisions.is_current(&analysis_uri, analysis_revision);
            inlay_hints_for_document_cancellable(
                &analysis_uri,
                &docs,
                params.range,
                compile_options,
                &sessions,
                &cancelled,
            )
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        let hints = match analyzed {
            Ok(Some(hints)) => hints,
            Ok(None) => return Ok(None),
            Err(error) => {
                self.client
                    .log_message(MessageType::ERROR, format!("inlay hints failed: {error}"))
                    .await;
                return Err(tower_lsp::jsonrpc::Error::internal_error());
            }
        };
        if !self.analysis_is_current(&uri, &text, analysis_revision) {
            return Ok(None);
        }

        Ok(Some(hints))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let retriggered = params.context.as_ref().is_some_and(|context| {
            context.trigger_kind == CompletionTriggerKind::TRIGGER_FOR_INCOMPLETE_COMPLETIONS
        });
        let request_revision = self.completion_revisions.begin(&uri);
        if !self.completion_delay.is_zero() {
            tokio::time::sleep(self.completion_delay).await;
            if !self.completion_revisions.is_current(&uri, request_revision) {
                return Ok(None);
            }
        }
        if is_manifest_uri(&uri) {
            let text = {
                let docs = self.docs.lock().unwrap();
                docs.get(&uri).map(|document| document.text.clone())
            };
            let items = text.map(|text| manifest_completions(&text, position));
            return Ok(items.map(CompletionResponse::Array));
        }
        let Some((docs, text, analysis_revision)) = self.analysis_snapshot(&uri) else {
            return Ok(Some(CompletionResponse::Array(Vec::new())));
        };
        let trigger_is_active = completion_trigger_is_active(&text, position);
        if retriggered && !trigger_is_active {
            return Ok(Some(CompletionResponse::List(CompletionList {
                is_incomplete: false,
                items: Vec::new(),
            })));
        }
        let compile_options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let completion_revisions = Arc::clone(&self.completion_revisions);
        let current_analysis_revisions = Arc::clone(&self.analysis_revisions);
        let completion_uri = uri.clone();
        let analysis = tokio::task::spawn_blocking(move || {
            completion_items_for_document(
                &completion_uri,
                &docs,
                position,
                compile_options,
                &sessions,
                || {
                    !completion_revisions.is_current(&completion_uri, request_revision)
                        || !current_analysis_revisions
                            .is_current(&completion_uri, analysis_revision)
                },
            )
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        let items = match analysis {
            Ok(Some(items)) => items,
            Ok(None) => return Ok(None),
            Err(error) => {
                self.client
                    .log_message(MessageType::ERROR, format!("completion failed: {error}"))
                    .await;
                return Err(tower_lsp::jsonrpc::Error::internal_error());
            }
        };
        if !self.analysis_is_current(&uri, &text, analysis_revision)
            || !self.completion_revisions.is_current(&uri, request_revision)
        {
            return Ok(None);
        }

        Ok(Some(if trigger_is_active && !items.is_empty() {
            CompletionResponse::List(CompletionList {
                is_incomplete: true,
                items,
            })
        } else {
            CompletionResponse::Array(items)
        }))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        if is_manifest_uri(&uri) {
            let text = {
                let docs = self.docs.lock().unwrap();
                docs.get(&uri).map(|document| document.text.clone())
            };
            return Ok(text.and_then(|text| manifest_hover(&text, position)));
        }
        let Some((docs, text, revision)) = self.analysis_snapshot(&uri) else {
            return Ok(None);
        };
        let analysis_uri = uri.clone();
        let options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let revisions = Arc::clone(&self.analysis_revisions);
        let result = tokio::task::spawn_blocking(move || {
            let cancelled = || !revisions.is_current(&analysis_uri, revision);
            hover_for_document_cancellable(
                &analysis_uri,
                &docs,
                position,
                options,
                &sessions,
                &cancelled,
            )
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        let hover = match result {
            Ok(hover) => hover,
            Err(error) => {
                self.client
                    .log_message(MessageType::ERROR, format!("hover failed: {error}"))
                    .await;
                return Err(tower_lsp::jsonrpc::Error::internal_error());
            }
        };
        if !self.analysis_is_current(&uri, &text, revision) {
            return Ok(None);
        }
        Ok(hover)
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some((docs, text, revision)) = self.analysis_snapshot(&uri) else {
            return Ok(None);
        };
        let analysis_uri = uri.clone();
        let options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let revisions = Arc::clone(&self.analysis_revisions);
        let result = tokio::task::spawn_blocking(move || {
            let cancelled = || !revisions.is_current(&analysis_uri, revision);
            signature_help_for_document_cancellable(
                &analysis_uri,
                &docs,
                position,
                options,
                &sessions,
                &cancelled,
            )
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        let help = match result {
            Ok(help) => help,
            Err(error) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("signature help failed: {error}"),
                    )
                    .await;
                return Err(tower_lsp::jsonrpc::Error::internal_error());
            }
        };
        if !self.analysis_is_current(&uri, &text, revision) {
            return Ok(None);
        }
        Ok(help)
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some((docs, text, revision)) = self.analysis_snapshot(&uri) else {
            return Ok(None);
        };
        let analysis_uri = uri.clone();
        let options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let revisions = Arc::clone(&self.analysis_revisions);
        let result = tokio::task::spawn_blocking(move || {
            let cancelled = || !revisions.is_current(&analysis_uri, revision);
            document_highlights_for_document_cancellable(
                &analysis_uri,
                &docs,
                position,
                options,
                &sessions,
                &cancelled,
            )
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        let highlights = match result {
            Ok(highlights) => highlights,
            Err(error) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("document highlight failed: {error}"),
                    )
                    .await;
                return Err(tower_lsp::jsonrpc::Error::internal_error());
            }
        };
        if !self.analysis_is_current(&uri, &text, revision) {
            return Ok(None);
        }
        Ok(highlights)
    }

    async fn goto_declaration(
        &self,
        params: GotoDeclarationParams,
    ) -> Result<Option<GotoDeclarationResponse>> {
        self.goto_definition(params).await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some((docs, text, revision)) = self.analysis_snapshot(&uri) else {
            return Ok(None);
        };
        let analysis_uri = uri.clone();
        let options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let revisions = Arc::clone(&self.analysis_revisions);
        let result = tokio::task::spawn_blocking(move || {
            let cancelled = || !revisions.is_current(&analysis_uri, revision);
            definition_for_document_cancellable(
                &analysis_uri,
                &docs,
                position,
                options,
                &sessions,
                &cancelled,
            )
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        let definition = match result {
            Ok(definition) => definition,
            Err(error) => {
                self.client
                    .log_message(MessageType::ERROR, format!("definition failed: {error}"))
                    .await;
                return Err(tower_lsp::jsonrpc::Error::internal_error());
            }
        };
        if !self.analysis_is_current(&uri, &text, revision) {
            return Ok(None);
        }
        Ok(definition)
    }

    async fn goto_type_definition(
        &self,
        params: GotoTypeDefinitionParams,
    ) -> Result<Option<GotoTypeDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some((docs, text, revision)) = self.analysis_snapshot(&uri) else {
            return Ok(None);
        };
        let analysis_uri = uri.clone();
        let options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let revisions = Arc::clone(&self.analysis_revisions);
        let result = tokio::task::spawn_blocking(move || {
            let cancelled = || !revisions.is_current(&analysis_uri, revision);
            type_definition_for_document_cancellable(
                &analysis_uri,
                &docs,
                position,
                options,
                &sessions,
                &cancelled,
            )
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        let definition = match result {
            Ok(definition) => definition,
            Err(error) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("type definition failed: {error}"),
                    )
                    .await;
                return Err(tower_lsp::jsonrpc::Error::internal_error());
            }
        };
        if !self.analysis_is_current(&uri, &text, revision) {
            return Ok(None);
        }
        Ok(definition)
    }

    async fn prepare_call_hierarchy(
        &self,
        params: CallHierarchyPrepareParams,
    ) -> Result<Option<Vec<CallHierarchyItem>>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some((docs, text, revision)) = self.analysis_snapshot(&uri) else {
            return Ok(None);
        };
        let analysis_uri = uri.clone();
        let options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let revisions = Arc::clone(&self.analysis_revisions);
        let workspace = Arc::clone(&self.workspace);
        let result = tokio::task::spawn_blocking(move || {
            let cancelled = || !revisions.is_current(&analysis_uri, revision);
            hierarchy_prepare_call(
                &analysis_uri,
                &docs,
                position,
                options,
                &sessions,
                &workspace,
                &cancelled,
            )
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        let items = result.map_err(|error| {
            tower_lsp::jsonrpc::Error::invalid_params(format!(
                "prepare call hierarchy failed: {error}"
            ))
        })?;
        if !self.analysis_is_current(&uri, &text, revision) {
            return Ok(None);
        }
        Ok(items)
    }

    async fn incoming_calls(
        &self,
        params: CallHierarchyIncomingCallsParams,
    ) -> Result<Option<Vec<CallHierarchyIncomingCall>>> {
        hierarchy_incoming_calls(&params.item, &self.workspace)
    }

    async fn outgoing_calls(
        &self,
        params: CallHierarchyOutgoingCallsParams,
    ) -> Result<Option<Vec<CallHierarchyOutgoingCall>>> {
        hierarchy_outgoing_calls(&params.item, &self.workspace)
    }

    async fn prepare_type_hierarchy(
        &self,
        params: TypeHierarchyPrepareParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some((docs, text, revision)) = self.analysis_snapshot(&uri) else {
            return Ok(None);
        };
        let analysis_uri = uri.clone();
        let options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let revisions = Arc::clone(&self.analysis_revisions);
        let workspace = Arc::clone(&self.workspace);
        let result = tokio::task::spawn_blocking(move || {
            let cancelled = || !revisions.is_current(&analysis_uri, revision);
            hierarchy_prepare_type(
                &analysis_uri,
                &docs,
                position,
                options,
                &sessions,
                &workspace,
                &cancelled,
            )
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        let items = result.map_err(|error| {
            tower_lsp::jsonrpc::Error::invalid_params(format!(
                "prepare type hierarchy failed: {error}"
            ))
        })?;
        if !self.analysis_is_current(&uri, &text, revision) {
            return Ok(None);
        }
        Ok(items)
    }

    async fn supertypes(
        &self,
        params: TypeHierarchySupertypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        hierarchy_supertypes(&params.item, &self.workspace)
    }

    async fn subtypes(
        &self,
        params: TypeHierarchySubtypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        hierarchy_subtypes(&params.item, &self.workspace)
    }

    async fn goto_implementation(
        &self,
        params: GotoImplementationParams,
    ) -> Result<Option<GotoImplementationResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some((docs, text, revision)) = self.analysis_snapshot(&uri) else {
            return Ok(None);
        };
        let analysis_uri = uri.clone();
        let options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let revisions = Arc::clone(&self.analysis_revisions);
        let result = tokio::task::spawn_blocking(move || {
            let cancelled = || !revisions.is_current(&analysis_uri, revision);
            implementation_for_document_cancellable(
                &analysis_uri,
                &docs,
                position,
                options,
                &sessions,
                &cancelled,
            )
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        let implementation = match result {
            Ok(implementation) => implementation,
            Err(error) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("implementation failed: {error}"),
                    )
                    .await;
                return Err(tower_lsp::jsonrpc::Error::internal_error());
            }
        };
        if !self.analysis_is_current(&uri, &text, revision) {
            return Ok(None);
        }
        Ok(implementation)
    }

    async fn references(
        &self,
        params: ReferenceParams,
    ) -> Result<Option<Vec<lsp_types::Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let include_declaration = params.context.include_declaration;
        let Some((docs, text, revision)) = self.analysis_snapshot(&uri) else {
            return Ok(None);
        };
        let analysis_uri = uri.clone();
        let options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let revisions = Arc::clone(&self.analysis_revisions);
        let result = tokio::task::spawn_blocking(move || {
            let cancelled = || !revisions.is_current(&analysis_uri, revision);
            references_for_document_cancellable(
                &analysis_uri,
                &docs,
                position,
                include_declaration,
                options,
                &sessions,
                &cancelled,
            )
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        let references = match result {
            Ok(references) => references,
            Err(error) => {
                self.client
                    .log_message(MessageType::ERROR, format!("references failed: {error}"))
                    .await;
                return Err(tower_lsp::jsonrpc::Error::internal_error());
            }
        };
        if !self.analysis_is_current(&uri, &text, revision) {
            return Ok(None);
        }
        Ok(references)
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = params.text_document.uri;
        let position = params.position;
        let Some((docs, text, revision)) = self.analysis_snapshot(&uri) else {
            return Ok(None);
        };
        let analysis_uri = uri.clone();
        let options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let revisions = Arc::clone(&self.analysis_revisions);
        let result = tokio::task::spawn_blocking(move || {
            let cancelled = || !revisions.is_current(&analysis_uri, revision);
            prepare_rename_for_document_cancellable(
                &analysis_uri,
                &docs,
                position,
                options,
                &sessions,
                &cancelled,
            )
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        let prepared = match result {
            Ok(prepared) => prepared,
            Err(error) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("prepare rename failed: {error}"),
                    )
                    .await;
                return Err(tower_lsp::jsonrpc::Error::internal_error());
            }
        };
        if !self.analysis_is_current(&uri, &text, revision) {
            return Ok(None);
        }
        Ok(prepared)
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        // Validated up front so an illegal name is rejected without paying for
        // the project analysis the rename would otherwise trigger.
        if let Err(message) = validate_identifier(&params.new_name) {
            return Err(tower_lsp::jsonrpc::Error::invalid_params(message));
        }
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = params.new_name;
        let Some((docs, text, revision)) = self.analysis_snapshot(&uri) else {
            return Ok(None);
        };
        let analysis_uri = uri.clone();
        let options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let revisions = Arc::clone(&self.analysis_revisions);
        let result = tokio::task::spawn_blocking(move || {
            let cancelled = || !revisions.is_current(&analysis_uri, revision);
            rename_for_document_cancellable(
                &analysis_uri,
                &docs,
                position,
                &new_name,
                options,
                &sessions,
                &cancelled,
            )
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;
        let edit = match result {
            Ok(edit) => edit,
            Err(RenameError::InvalidName(message) | RenameError::Unavailable(message)) => {
                return Err(tower_lsp::jsonrpc::Error::invalid_params(message));
            }
            // A target the client was allowed to start renaming on but that the
            // server will not rewrite. Returning `null` here would leave the
            // user with a rename that silently does nothing.
            Err(RenameError::Rejected(rejection)) => {
                return Err(tower_lsp::jsonrpc::Error::invalid_params(
                    rejection.to_string(),
                ));
            }
        };
        if !self.analysis_is_current(&uri, &text, revision) {
            return Ok(None);
        }
        Ok(edit)
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let only = params.context.only.as_deref();
        let quickfixes_requested =
            code_action_kind_requested(only, CodeActionKind::QUICKFIX.as_str());
        let organize_requested = code_action_kind_requested(only, ORGANIZE_IMPORTS_KIND);
        let add_imports_requested = code_action_kind_requested(only, ADD_MISSING_IMPORTS_KIND);
        let fix_all_requested = code_action_kind_requested(only, FIX_ALL_KIND);
        if !quickfixes_requested
            && !organize_requested
            && !add_imports_requested
            && !fix_all_requested
        {
            return Ok(Some(Vec::new()));
        }
        let uri = params.text_document.uri;
        let Some(document) = self.document(&uri) else {
            return Ok(Some(Vec::new()));
        };
        let Some(published) = self.published.lock().unwrap().get(&uri).cloned() else {
            return Ok(Some(Vec::new()));
        };
        if published.version != document.version {
            return Ok(Some(Vec::new()));
        }
        // The analysis-backed fixes are computed once and then projected onto
        // whichever kinds the client asked for, so a request for `source.fixAll`
        // does not pay for the project analysis twice.
        let needs_analysis = quickfixes_requested || add_imports_requested || fix_all_requested;
        let mut analysis_fixes: CodeActionResponse = Vec::new();
        if needs_analysis {
            let diagnostics = params
                .context
                .diagnostics
                .iter()
                .filter(|diagnostic| published.diagnostics.contains(diagnostic))
                .cloned()
                .collect::<Vec<_>>();
            if has_analysis_fix_diagnostics(&diagnostics)
                && let Some(fixes) = self
                    .analysis_quick_fixes(&uri, document.version, diagnostics)
                    .await
            {
                analysis_fixes = fixes;
            }
        }
        let mut actions: CodeActionResponse = Vec::new();
        if quickfixes_requested {
            let diagnostics = params
                .context
                .diagnostics
                .iter()
                .filter(|diagnostic| published.diagnostics.contains(diagnostic))
                .cloned()
                .collect::<Vec<_>>();
            actions.extend(quick_fixes(
                &uri,
                document.version,
                &document.text,
                &diagnostics,
            ));
            actions.extend(analysis_fixes.iter().cloned());
        }
        if add_imports_requested
            && let Some(action) =
                add_missing_imports_action(&uri, document.version, &analysis_fixes)
        {
            actions.push(action);
        }
        if organize_requested
            && let Some(action) = organize_imports_action(&uri, document.version, &document.text)
        {
            actions.push(action);
        }
        if fix_all_requested
            && let Some(action) = fix_all_action(&uri, document.version, &analysis_fixes)
        {
            actions.push(action);
        }
        Ok(Some(actions))
    }

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        let reset_all = params.changes.iter().any(|change| is_manifest(&change.uri));
        let mut invalidated = params
            .changes
            .iter()
            .filter_map(|change| change.uri.to_file_path().ok())
            .flat_map(|path| self.workspace.invalidate_path(&path))
            .collect::<std::collections::BTreeSet<_>>();
        invalidated.extend(
            params
                .changes
                .iter()
                .filter_map(|change| change.uri.to_file_path().ok())
                .filter_map(|path| clue::find_project_root(&path)),
        );
        {
            let docs = self.docs.lock().unwrap();
            for change in &params.changes {
                bump_related_revisions(&self.analysis_revisions, &docs, &change.uri);
            }
        }
        if reset_all {
            // A manifest change invalidates the cached diagnostics results, but
            // not the incremental checkers behind them: those are keyed on the
            // inputs they were built from and revalidate themselves. Dropping
            // them here forced a full re-type-check of the standard library on
            // the next keystroke.
            self.diagnostic_sessions.lock().unwrap().invalidate_caches();
            self.sessions.clear_projects();
            self.sessions.clear_projects();
            if let Err(error) = self.workspace.set_roots(self.workspace.roots()) {
                self.client
                    .log_message(
                        MessageType::WARNING,
                        format!("failed to refresh workspace projects: {error}"),
                    )
                    .await;
            }
        } else {
            for change in &params.changes {
                self.diagnostic_sessions
                    .lock()
                    .unwrap()
                    .invalidate_project(&change.uri);
                self.sessions.invalidate_project(&change.uri);
                self.sessions.invalidate_project(&change.uri);
            }
            self.sessions.invalidate_roots(&invalidated);
            self.sessions.invalidate_roots(&invalidated);
        }
        self.schedule_diagnostics();
        if reset_all {
            self.schedule_workspace_indexing();
        } else {
            self.schedule_project_indexing(invalidated.into_iter().collect());
        }
    }

    async fn did_change_workspace_folders(&self, params: DidChangeWorkspaceFoldersParams) {
        self.workspace.remove_roots(
            params
                .event
                .removed
                .iter()
                .filter_map(|folder| folder.uri.to_file_path().ok()),
        );
        if let Err(error) = self.workspace.add_roots(
            params
                .event
                .added
                .iter()
                .filter_map(|folder| folder.uri.to_file_path().ok()),
        ) {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("failed to discover workspace projects: {error}"),
                )
                .await;
        }
        self.schedule_workspace_indexing();
    }
}

fn is_manifest(uri: &lsp_types::Url) -> bool {
    uri.path_segments()
        .and_then(|mut segments| segments.next_back())
        == Some("Clue.toml")
}

#[must_use]
pub fn documents_for_uri<S: BuildHasher>(
    docs: &HashMap<lsp_types::Url, Document, S>,
    uri: &lsp_types::Url,
) -> HashMap<lsp_types::Url, Document> {
    let Some(root) = project_root(uri) else {
        return docs
            .get(uri)
            .cloned()
            .map(|document| HashMap::from([(uri.clone(), document)]))
            .unwrap_or_default();
    };
    docs.iter()
        .filter(|(candidate, _)| project_root(candidate).as_ref() == Some(&root))
        .map(|(uri, document)| (uri.clone(), document.clone()))
        .collect()
}

fn project_root(uri: &lsp_types::Url) -> Option<std::path::PathBuf> {
    uri.to_file_path()
        .ok()
        .and_then(|path| clue::find_project_root(&path))
}

fn related_document_uris(
    docs: &HashMap<lsp_types::Url, Document>,
    uri: &lsp_types::Url,
) -> Vec<lsp_types::Url> {
    documents_for_uri(docs, uri).into_keys().collect()
}

fn bump_related_revisions(
    revisions: &RequestRevisions,
    docs: &HashMap<lsp_types::Url, Document>,
    uri: &lsp_types::Url,
) {
    for related in related_document_uris(docs, uri) {
        revisions.begin(&related);
    }
}

impl Backend {
    /// Analysis-based quick fixes (unresolved names, unknown methods, missing
    /// trait members); `None` means the analysis went stale and the response
    /// is dropped.
    async fn analysis_quick_fixes(
        &self,
        uri: &lsp_types::Url,
        version: Option<i32>,
        diagnostics: Vec<lsp_types::Diagnostic>,
    ) -> Option<CodeActionResponse> {
        let Some((docs, text, revision)) = self.analysis_snapshot(uri) else {
            return Some(Vec::new());
        };
        let analysis_uri = uri.clone();
        let options = self.compile_options;
        let sessions = Arc::clone(&self.sessions);
        let revisions = Arc::clone(&self.analysis_revisions);
        let result = tokio::task::spawn_blocking(move || {
            let cancelled = || !revisions.is_current(&analysis_uri, revision);
            analysis_fixes_for_document_cancellable(
                &analysis_uri,
                &docs,
                options,
                &sessions,
                version,
                &diagnostics,
                &cancelled,
            )
        })
        .await
        .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())
        .ok()?;
        match result {
            Ok(fixes) if self.analysis_is_current(uri, &text, revision) => Some(fixes),
            // The document changed while analyzing: drop the stale response.
            Ok(_) => None,
            Err(error) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("unresolved-name fixes failed: {error}"),
                    )
                    .await;
                None
            }
        }
    }

    pub(crate) fn new(
        client: Client,
        compile_options: CompileOptions,
        completion_delay: Duration,
    ) -> Self {
        let sessions = Arc::new(AnalysisSessions::default());
        Self {
            client,
            docs: Arc::new(Mutex::new(HashMap::new())),
            published: Arc::new(Mutex::new(HashMap::new())),
            publish_gate: Arc::new(tokio::sync::Mutex::new(())),
            diagnostic_revision: Arc::new(AtomicU64::new(0)),
            diagnostic_sessions: Arc::new(Mutex::new(DiagnosticSessions::new(Arc::clone(
                &sessions,
            )))),
            sessions,
            analysis_revisions: Arc::new(RequestRevisions::default()),
            completion_revisions: Arc::new(RequestRevisions::default()),
            semantic_tokens: Arc::new(Mutex::new(HashMap::new())),
            semantic_token_revision: Arc::new(AtomicU64::new(1)),
            supports_watched_files: AtomicBool::new(false),
            supports_type_hierarchy: AtomicBool::new(false),
            workspace: Arc::new(WorkspaceState::default()),
            compile_options,
            completion_delay,
        }
    }

    /// Clones an open document's text.
    fn document_text(&self, uri: &lsp_types::Url) -> Option<String> {
        self.docs
            .lock()
            .unwrap()
            .get(uri)
            .map(|document| document.text.clone())
    }

    /// Clones an open document.
    fn document(&self, uri: &lsp_types::Url) -> Option<Document> {
        self.docs.lock().unwrap().get(uri).cloned()
    }

    /// Applies a change batch under the `docs` lock and reports what to do.
    ///
    /// Deliberately synchronous so no `MutexGuard` is ever held across an
    /// `.await`, which would make the caller's future non-`Send`.
    fn apply_change(&self, params: &DidChangeTextDocumentParams) -> ChangeOutcome {
        let uri = &params.text_document.uri;
        let mut docs = self.docs.lock().unwrap();
        if !docs.contains_key(uri) {
            // Unknown document: recover when the batch carries full text.
            let full_text = params
                .content_changes
                .iter()
                .rev()
                .find(|change| change.range.is_none())
                .map(|change| change.text.clone());
            match full_text {
                Some(text) => {
                    docs.insert(
                        uri.clone(),
                        Document::new(text, Some(params.text_document.version)),
                    );
                }
                None => {
                    return ChangeOutcome::Report(format!(
                        "ignoring content change for a document the server has not opened: {uri}"
                    ));
                }
            }
        }
        let Some(document) = docs.get_mut(uri) else {
            return ChangeOutcome::Skipped;
        };
        if let Some(previous) = document.version
            && params.text_document.version <= previous
        {
            return ChangeOutcome::Report(format!(
                "ignoring out-of-order change for {uri}: version {} after {previous}",
                params.text_document.version
            ));
        }
        document.version = Some(params.text_document.version);
        match apply_content_changes(&mut document.text, params.content_changes.clone()) {
            Ok(()) => {
                document.out_of_sync = false;
                bump_related_revisions(&self.analysis_revisions, &docs, uri);
                ChangeOutcome::Applied
            }
            Err(error) => {
                // The buffered text can no longer be reconstructed from this
                // batch. Mark the document instead of dropping it: dropping it
                // would make the next diagnostics round omit the URI, and an
                // omitted URI is published to the client as an empty (clean)
                // diagnostic list.
                document.out_of_sync = true;
                for related_uri in related_document_uris(&docs, uri) {
                    self.analysis_revisions.begin(&related_uri);
                }
                ChangeOutcome::Report(format!("{uri} is out of sync with the editor: {error}"))
            }
        }
    }

    /// Carries out what `did_change` decided, outside the `docs` lock.
    async fn report_change(&self, outcome: ChangeOutcome) {
        match outcome {
            ChangeOutcome::Applied => {
                // Indexing is deliberately *not* scheduled here. The workspace
                // index runs at `Infer` depth over the whole project; doing it
                // per keystroke doubled the analysis cost of typing while the
                // diagnostics pass had already produced the same information.
                // It is rebuilt on save, on external file changes, and at
                // startup instead.
                self.schedule_diagnostics();
            }
            ChangeOutcome::Report(message) => {
                self.client.log_message(MessageType::WARNING, message).await;
                // Republish either way: a newly out-of-sync document has to
                // show its error immediately, not on the next unrelated edit.
                self.schedule_diagnostics();
            }
            ChangeOutcome::Skipped => {}
        }
    }

    fn analysis_snapshot(
        &self,
        uri: &lsp_types::Url,
    ) -> Option<(HashMap<lsp_types::Url, Document>, String, u64)> {
        let (docs, text) = {
            let all_docs = self.docs.lock().unwrap();
            let text = all_docs.get(uri)?.text.clone();
            let docs = documents_for_uri(&all_docs, uri);
            drop(all_docs);
            (docs, text)
        };
        let revision = self.analysis_revisions.current(uri);
        Some((docs, text, revision))
    }

    fn analysis_is_current(&self, uri: &lsp_types::Url, text: &str, revision: u64) -> bool {
        if !self.analysis_revisions.is_current(uri, revision) {
            return false;
        }
        let unchanged = self
            .docs
            .lock()
            .unwrap()
            .get(uri)
            .is_some_and(|document| document.text == text);
        unchanged && self.analysis_revisions.is_current(uri, revision)
    }

    fn schedule_diagnostics(&self) {
        let revision = self.diagnostic_revision.fetch_add(1, Ordering::SeqCst) + 1;
        let client = self.client.clone();
        let docs = Arc::clone(&self.docs);
        let published_state = Arc::clone(&self.published);
        let publish_gate = Arc::clone(&self.publish_gate);
        let diagnostic_revision = Arc::clone(&self.diagnostic_revision);
        let diagnostic_sessions = Arc::clone(&self.diagnostic_sessions);
        let analysis = Arc::clone(&self.sessions);
        let compile_options = self.compile_options;

        tokio::spawn(async move {
            tokio::time::sleep(DIAGNOSTICS_DEBOUNCE).await;
            if diagnostic_revision.load(Ordering::SeqCst) != revision {
                return;
            }

            let docs = docs.lock().unwrap().clone();
            let analysis_revision = Arc::clone(&diagnostic_revision);
            let published = tokio::task::spawn_blocking(move || {
                let phase = Phase::start("diagnostics.collect");
                let mut sessions = diagnostic_sessions
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                if analysis_revision.load(Ordering::SeqCst) != revision {
                    drop(phase);
                    return Ok(None);
                }
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    collect_workspace_diagnostics_cancellable(
                        &docs,
                        compile_options,
                        &mut sessions,
                        || analysis_revision.load(Ordering::SeqCst) != revision,
                    )
                }));
                if let Ok(published) = result {
                    drop(sessions);
                    drop(phase);
                    Ok(published)
                } else {
                    eprintln!("[session] diagnostics: PANIC recovered, sessions replaced");
                    *sessions = DiagnosticSessions::new(analysis);
                    drop(sessions);
                    drop(phase);
                    Err(())
                }
            })
            .await;
            let published = match published {
                Ok(Ok(Some(published))) => published,
                Ok(Ok(None)) => return,
                Ok(Err(())) | Err(_) => {
                    client
                        .log_message(MessageType::ERROR, "riddle-lsp analysis failed")
                        .await;
                    return;
                }
            };
            if diagnostic_revision.load(Ordering::SeqCst) != revision {
                return;
            }

            let _publish_guard = publish_gate.lock().await;
            if diagnostic_revision.load(Ordering::SeqCst) != revision {
                return;
            }
            publish_diagnostics(
                &client,
                &published_state,
                &diagnostic_revision,
                revision,
                published,
            )
            .await;
        });
    }

    fn schedule_workspace_indexing(&self) {
        self.schedule_project_indexing(self.workspace.projects());
    }

    fn schedule_document_indexing(&self, uri: &lsp_types::Url) {
        let Some(project) = uri
            .to_file_path()
            .ok()
            .and_then(|path| clue::find_project_root(&path))
        else {
            return;
        };
        self.schedule_project_indexing(vec![project]);
    }

    fn schedule_project_indexing(&self, mut projects: Vec<PathBuf>) {
        projects.sort();
        projects.dedup();
        if projects.is_empty() {
            return;
        }
        let overlays = Arc::new(
            self.docs
                .lock()
                .unwrap()
                .iter()
                .filter_map(|(uri, document)| {
                    uri.to_file_path()
                        .ok()
                        .map(|path| (path, document.text.clone()))
                })
                .collect::<HashMap<_, _>>(),
        );
        for project in projects {
            let token = self.workspace.begin_rebuild(&project);
            let workspace = Arc::clone(&self.workspace);
            let sessions = Arc::clone(&self.sessions);
            let overlays = Arc::clone(&overlays);
            let client = self.client.clone();
            let options = self.compile_options;
            tokio::spawn(async move {
                tokio::time::sleep(INDEX_DEBOUNCE).await;
                if !workspace.is_current(&token) {
                    return;
                }
                let workspace_for_build = Arc::clone(&workspace);
                let token_for_build = token.clone();
                let result = tokio::task::spawn_blocking(move || {
                    project_index_for_root_cancellable(
                        &project,
                        overlays.as_ref(),
                        options,
                        sessions.as_ref(),
                        &|| !workspace_for_build.is_current(&token_for_build),
                    )
                })
                .await;
                match result {
                    Ok(Ok(Some(index))) => {
                        workspace.install(token, index);
                    }
                    Ok(Ok(None)) => {}
                    Ok(Err(error)) => {
                        client
                            .log_message(
                                MessageType::WARNING,
                                format!("failed to index workspace project: {error}"),
                            )
                            .await;
                    }
                    Err(error) => {
                        client
                            .log_message(
                                MessageType::ERROR,
                                format!("workspace indexing task failed: {error}"),
                            )
                            .await;
                    }
                }
            });
        }
    }
}

/// Line-level edits that turn `original` into `formatted`, restricted to lines
/// intersecting `range`.
///
/// A line whose content changed is replaced whole, which keeps the edits
/// independent of each other and of any offset arithmetic.
fn line_edits_within(original: &str, formatted: &str, range: lsp_types::Range) -> Vec<TextEdit> {
    let original_lines = original.lines().collect::<Vec<_>>();
    let formatted_lines = formatted.lines().collect::<Vec<_>>();
    let index = LineIndex::new(original);
    let mut edits = Vec::new();
    for (line, (before, after)) in original_lines
        .iter()
        .zip(formatted_lines.iter())
        .enumerate()
    {
        if before == after {
            continue;
        }
        let line = u32::try_from(line).unwrap_or(u32::MAX);
        if line < range.start.line || line > range.end.line {
            continue;
        }
        let Some(start) = index.position(original, line_start_offset(original, line as usize))
        else {
            continue;
        };
        let Some(end) = index.position(original, line_end_offset(original, line as usize)) else {
            continue;
        };
        edits.push(TextEdit::new(
            lsp_types::Range::new(start, end),
            (*after).into(),
        ));
    }
    // Differing line counts mean the formatter changed the shape of the file;
    // a per-line diff would be wrong, so fall back to replacing the lines the
    // formatter produced for the requested window.
    if original_lines.len() != formatted_lines.len() {
        return whole_range_edit(original, formatted, range, &index);
    }
    edits
}

fn whole_range_edit(
    original: &str,
    formatted: &str,
    range: lsp_types::Range,
    index: &LineIndex,
) -> Vec<TextEdit> {
    let start_line = range.start.line as usize;
    let end_line = range.end.line as usize;
    let original_lines = original.lines().collect::<Vec<_>>();
    let formatted_lines = formatted.lines().collect::<Vec<_>>();
    if start_line > original_lines.len() || end_line > original_lines.len() {
        return Vec::new();
    }
    let replacement = formatted_lines
        .get(start_line..=end_line.min(formatted_lines.len().saturating_sub(1)))
        .unwrap_or_default()
        .join("\n");
    let Some(start) = index.position(original, line_start_offset(original, start_line)) else {
        return Vec::new();
    };
    let Some(end) = index.position(original, line_end_offset(original, end_line)) else {
        return Vec::new();
    };
    vec![TextEdit::new(
        lsp_types::Range::new(start, end),
        replacement,
    )]
}

fn line_start_offset(source: &str, line: usize) -> usize {
    let mut offset = 0;
    for _ in 0..line {
        match source[offset..].find('\n') {
            Some(found) => offset += found + 1,
            None => return source.len(),
        }
    }
    offset
}

fn line_end_offset(source: &str, line: usize) -> usize {
    let start = line_start_offset(source, line);
    source[start..]
        .find('\n')
        .map_or(source.len(), |offset| start + offset)
}

fn full_diagnostic_report(
    result_id: Option<String>,
    items: Vec<lsp_types::Diagnostic>,
) -> DocumentDiagnosticReportResult {
    DocumentDiagnosticReportResult::Report(DocumentDiagnosticReport::Full(
        RelatedFullDocumentDiagnosticReport {
            related_documents: None,
            full_document_diagnostic_report: FullDocumentDiagnosticReport { result_id, items },
        },
    ))
}

/// The report returned when an analysis was superseded before it finished.
///
/// A client that asked for diagnostics and gets an empty list concludes the
/// file is clean, so a cancelled or failed analysis must never answer that way.
fn stale_diagnostic_report() -> DocumentDiagnosticReportResult {
    DocumentDiagnosticReportResult::Report(DocumentDiagnosticReport::Unchanged(
        lsp_types::RelatedUnchangedDocumentDiagnosticReport {
            related_documents: None,
            unchanged_document_diagnostic_report: lsp_types::UnchangedDocumentDiagnosticReport {
                result_id: String::new(),
            },
        },
    ))
}

async fn publish_diagnostics(
    client: &Client,
    published_state: &Mutex<HashMap<lsp_types::Url, diagnostics::PublishedDiagnostics>>,
    diagnostic_revision: &AtomicU64,
    revision: u64,
    published: Vec<diagnostics::PublishedDiagnostics>,
) {
    let current = published
        .into_iter()
        .map(|published| (published.uri.clone(), published))
        .collect::<HashMap<_, _>>();
    let (previous, uris) = {
        let previous = published_state.lock().unwrap();
        let mut uris = previous
            .keys()
            .chain(current.keys())
            .cloned()
            .collect::<Vec<_>>();
        uris.sort_by(|left, right| left.as_str().cmp(right.as_str()));
        uris.dedup();
        (previous.clone(), uris)
    };

    for uri in uris {
        if diagnostic_revision.load(Ordering::SeqCst) != revision {
            return;
        }
        if previous.get(&uri) == current.get(&uri) {
            continue;
        }
        let (diagnostics, version) = current
            .get(&uri)
            .map(|published| (published.diagnostics.clone(), published.version))
            .unwrap_or_default();
        client
            .publish_diagnostics(uri.clone(), diagnostics, version)
            .await;
        let mut actual = published_state.lock().unwrap();
        if let Some(published) = current.get(&uri) {
            actual.insert(uri, published.clone());
        } else {
            actual.remove(&uri);
        }
    }
}
