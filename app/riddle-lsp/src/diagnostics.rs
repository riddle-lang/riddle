use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashMap, HashSet},
    fs,
    hash::BuildHasher,
    path::{Path, PathBuf},
    sync::Arc,
};

use lsp_types::{
    CodeDescription, Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location,
    NumberOrString, Position, Range, Url,
};
use riddlec::pipeline::{
    CheckSession, CompileOptions, CompileResult, DiagnosticExt, IntoDiagnosticExt, SourceMap,
};
use rowan::TextRange;
use type_checker::{LabelStyle, SourceLabel};

use crate::{
    server::Document,
    session::AnalysisSessions,
    text::{LineIndex, normalized_path, text_range, text_size},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PublishedDiagnostics {
    pub uri: Url,
    pub version: Option<i32>,
    pub diagnostics: Vec<Diagnostic>,
}

/// Code reported when the server's buffer for a document cannot be trusted.
pub const OUT_OF_SYNC_CODE: &str = "LSP0001";

/// The single diagnostic published for a document whose buffered text could
/// not be reconstructed from the editor's changes.
///
/// The point is to be loud: the alternative — publishing nothing — renders as
/// "this file has no problems", which is indistinguishable from a clean file.
fn out_of_sync_diagnostic() -> Diagnostic {
    Diagnostic {
        range: Range::new(Position::new(0, 0), Position::new(0, 0)),
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(NumberOrString::String(OUT_OF_SYNC_CODE.into())),
        code_description: None,
        source: Some("riddle".into()),
        message: "this file is out of sync with the editor, so it was not analysed; \
                  reopen it or undo recent edits to restore diagnostics"
            .into(),
        related_information: None,
        tags: None,
        data: None,
    }
}

struct ResolvedLabel {
    uri: Url,
    range: Range,
}

#[derive(Default)]
pub struct DiagnosticSessions {
    standalone: HashMap<Url, StandaloneDiagnosticSession>,
    projects: HashMap<PathBuf, ProjectDiagnosticSession>,
    analysis: Arc<AnalysisSessions>,
}

/// Whether session-lifecycle tracing is on, under `RIDDLEC_PHASE_TIMING`.
///
/// Read once per call rather than per event, so the tracing costs nothing on
/// the path that runs for every document.
fn tracing_sessions() -> bool {
    std::env::var_os("RIDDLEC_PHASE_TIMING").is_some()
}

/// Logs session-lifecycle events when `RIDDLEC_PHASE_TIMING` is set.
///
/// Which session object a request lands on decides whether the standard
/// library's ~827 checked bodies are reused (about 20 ms) or recomputed (about
/// 700 ms), and nothing else in the system reveals when that object is dropped.
fn trace_sessions(event: &str, key: impl std::fmt::Display) {
    if tracing_sessions() {
        let name = key.to_string();
        let name = name.rsplit('/').next().unwrap_or(&name);
        eprintln!("[session] {event:<28} {name}");
    }
}

impl DiagnosticSessions {
    pub(crate) fn new(analysis: Arc<AnalysisSessions>) -> Self {
        Self {
            analysis,
            ..Self::default()
        }
    }

    /// How many times the session for `uri` has run its checker, if it has one.
    ///
    /// Used by tests to distinguish a session that is being reused from one
    /// that is quietly recreated; a process-wide counter would be shared with
    /// every other test running in parallel.
    #[cfg(feature = "test")]
    #[must_use]
    pub fn standalone_checks(&self, uri: &Url) -> Option<usize> {
        self.standalone.get(uri).map(|session| session.checks)
    }

    /// Returns the session for a standalone document, creating it if needed.
    fn standalone_session(&mut self, uri: &Url) -> &mut StandaloneDiagnosticSession {
        let tracing = tracing_sessions();
        let total = if tracing { self.standalone.len() } else { 0 };
        let session = self.standalone.entry(uri.clone()).or_default();
        if tracing {
            if session.id == 0 {
                static NEXT_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);
                session.id = NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                trace_sessions(
                    "standalone: create",
                    format!("sid{} {uri} (map {total})", session.id),
                );
            }
            let id = session.id;
            trace_sessions(&format!("standalone: use sid{id}"), uri);
        }
        session
    }

    pub(crate) fn invalidate_project(&mut self, uri: &Url) {
        let Some(root) = uri
            .to_file_path()
            .ok()
            .and_then(|path| clue::find_project_root(&path))
        else {
            return;
        };
        self.projects.remove(&root);
    }

    /// Drops every cached diagnostics result while keeping the incremental
    /// checkers and project sessions that produced them.
    ///
    /// Replacing the whole `DiagnosticSessions` value throws away each
    /// `CheckSession`'s incremental type checker, and rebuilding that costs a
    /// full re-type-check of the standard library — around 700 ms on the
    /// reference machine, paid on the keystroke after any manifest or watched
    /// file event. The cached *results* are what a reset is for; the incremental
    /// state is keyed on the inputs themselves and stays valid.
    pub(crate) fn invalidate_caches(&mut self) {
        for session in self.standalone.values_mut() {
            session.cached = None;
        }
        for session in self.projects.values_mut() {
            session.cached = None;
        }
    }
}

#[derive(Default)]
struct StandaloneDiagnosticSession {
    checker: CheckSession,
    cached: Option<CachedStandaloneDiagnostics>,
    /// How many times this session's incremental checker has been used.
    ///
    /// A session that is being reused keeps this low while its checkers still
    /// hold the standard library's cached bodies; a session that is being
    /// recreated starts at zero every time. Tests assert on it directly rather
    /// than on a process-wide counter, which parallel tests share.
    checks: usize,
    /// Process-unique identity, assigned on first use, so an address that moved
    /// with a rehash is not mistaken for a new session in a trace.
    id: u64,
}

struct CachedStandaloneDiagnostics {
    options: CompileOptions,
    source: String,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Default)]
struct ProjectDiagnosticSession {
    cached: Option<CachedProjectDiagnostics>,
}

struct CachedProjectDiagnostics {
    options: CompileOptions,
    revision: u64,
    files: HashSet<PathBuf>,
    diagnostics: BTreeMap<Url, Vec<Diagnostic>>,
}

struct ProjectDiagnostics {
    by_uri: BTreeMap<Url, Vec<Diagnostic>>,
    files: HashSet<PathBuf>,
}

struct DocumentGroups {
    overlays: HashMap<PathBuf, String>,
    projects: BTreeMap<PathBuf, Vec<Url>>,
    standalone: Vec<Url>,
}

#[cfg(feature = "test")]
#[must_use]
pub fn collect_workspace_diagnostics<S: BuildHasher>(
    docs: &HashMap<Url, Document, S>,
    options: CompileOptions,
) -> Vec<PublishedDiagnostics> {
    collect_workspace_diagnostics_with_sessions(docs, options, &mut DiagnosticSessions::default())
}

#[cfg(feature = "test")]
/// Collects workspace diagnostics using reusable analysis sessions.
///
/// # Panics
///
/// Panics if the non-cancellable analysis unexpectedly reports cancellation.
pub fn collect_workspace_diagnostics_with_sessions<S: BuildHasher>(
    docs: &HashMap<Url, Document, S>,
    options: CompileOptions,
    sessions: &mut DiagnosticSessions,
) -> Vec<PublishedDiagnostics> {
    collect_workspace_diagnostics_cancellable(docs, options, sessions, || false)
        .expect("non-cancellable analysis cannot be cancelled")
}

pub fn collect_workspace_diagnostics_cancellable<S: BuildHasher>(
    docs: &HashMap<Url, Document, S>,
    options: CompileOptions,
    sessions: &mut DiagnosticSessions,
    cancelled: impl Fn() -> bool,
) -> Option<Vec<PublishedDiagnostics>> {
    let DocumentGroups {
        overlays,
        projects,
        standalone,
    } = document_groups(docs);

    let mut by_uri = BTreeMap::<Url, Vec<Diagnostic>>::new();
    // A document whose buffered text could not be reconstructed gets one
    // explicit error instead of an empty list. Omitting the URI here would make
    // the publish step report the file as clean, which is the single most
    // misleading thing a language server can do.
    for (uri, document) in docs {
        if document.out_of_sync {
            by_uri.insert(uri.clone(), vec![out_of_sync_diagnostic()]);
        }
    }
    // Manifest buffers get schema diagnostics computed from the open text
    // (the project pipeline reads `Clue.toml` from disk and reports deep
    // errors as CLUE0001 on the same document).
    for (uri, document) in docs {
        if !document.out_of_sync && crate::manifest_lsp::is_manifest_uri(uri) {
            by_uri.insert(
                uri.clone(),
                crate::manifest_lsp::manifest_diagnostics(&document.text),
            );
        }
    }
    let mut live_standalone = HashSet::new();
    let mut live_projects = HashSet::new();
    let analysis_sessions = Arc::clone(&sessions.analysis);
    for uri in standalone {
        if cancelled() {
            return None;
        }
        // Manifest buffers only carry schema diagnostics (pre-pass above);
        // they are not riddle sources.
        if crate::manifest_lsp::is_manifest_uri(&uri) {
            continue;
        }
        let Some(document) = docs.get(&uri) else {
            continue;
        };
        live_standalone.insert(uri.clone());
        let diagnostics =
            standalone_diagnostics(&uri, document, options, sessions.standalone_session(&uri));
        by_uri.entry(uri.clone()).or_default().extend(diagnostics);
    }

    for (root, project_docs) in projects {
        if cancelled() {
            return None;
        }
        live_projects.insert(root.clone());
        if !sessions.projects.contains_key(&root) {
            trace_sessions("project: create", root.display());
        }
        for uri in &project_docs {
            by_uri.entry(uri.clone()).or_default();
        }

        let project_diagnostics = match project_diagnostics(
            &root,
            &overlays,
            options,
            sessions.projects.entry(root.clone()).or_default(),
            &analysis_sessions,
        ) {
            Ok(cached) => cached,
            Err(error) => {
                let manifest = root.join("Clue.toml");
                let uri = Url::from_file_path(&manifest)
                    .ok()
                    .map(|uri| published_uri(&uri, docs))
                    .or_else(|| project_docs.first().cloned());
                if let Some(uri) = uri {
                    let source = fs::read_to_string(&manifest).unwrap_or_default();
                    by_uri
                        .entry(uri.clone())
                        .or_default()
                        .push(project_error(&source, error));
                }
                continue;
            }
        };
        if cancelled() {
            return None;
        }

        for (uri, mut diagnostics) in project_diagnostics.by_uri {
            let uri = published_uri(&uri, docs);
            for related in diagnostics
                .iter_mut()
                .flat_map(|diagnostic| diagnostic.related_information.iter_mut().flatten())
            {
                related.location.uri = published_uri(&related.location.uri, docs);
            }
            by_uri.entry(uri).or_default().extend(diagnostics);
        }

        for uri in project_docs {
            let Some(document) = docs.get(&uri) else {
                continue;
            };
            let Ok(path) = uri.to_file_path() else {
                continue;
            };
            let path = normalized_path(path);
            if project_diagnostics.files.contains(&path) {
                continue;
            }
            live_standalone.insert(uri.clone());
            let diagnostics =
                standalone_diagnostics(&uri, document, options, sessions.standalone_session(&uri));
            by_uri.entry(uri.clone()).or_default().extend(diagnostics);
        }
    }

    // Standalone sessions whose document is gone are dropped: their cached
    // result can never be served again, and a new document re-analyses from
    // scratch anyway.
    let before = sessions.standalone.len();
    sessions.standalone.retain(|uri, _| {
        let keep = live_standalone.contains(uri);
        if !keep {
            trace_sessions("standalone: drop (closed)", uri);
        }
        keep
    });
    if before != sessions.standalone.len() {
        trace_sessions(
            "standalone: total",
            format!("{} -> {}", before, sessions.standalone.len()),
        );
    }
    sessions
        .projects
        .retain(|root, _| live_projects.contains(root));

    Some(publish_diagnostics(by_uri, docs))
}

fn document_groups<S: BuildHasher>(docs: &HashMap<Url, Document, S>) -> DocumentGroups {
    // Out-of-sync buffers are excluded from every group: their text may not
    // match the editor, so feeding it to the project as an overlay would
    // corrupt the diagnostics of every *other* file in the same project.
    let current = || docs.iter().filter(|(_, document)| !document.out_of_sync);
    let overlays = current()
        .filter_map(|(uri, document)| {
            uri.to_file_path()
                .ok()
                .map(|path| (path, document.text.clone()))
        })
        .collect();
    let mut projects = BTreeMap::<PathBuf, Vec<Url>>::new();
    let mut standalone = Vec::new();

    for (uri, _) in current() {
        let Ok(path) = uri.to_file_path() else {
            standalone.push(uri.clone());
            continue;
        };
        if let Some(root) = clue::find_project_root(&path) {
            projects.entry(root).or_default().push(uri.clone());
        } else {
            standalone.push(uri.clone());
        }
    }
    standalone.sort_by(|left, right| left.as_str().cmp(right.as_str()));
    for project_docs in projects.values_mut() {
        project_docs.sort_by(|left, right| left.as_str().cmp(right.as_str()));
    }

    DocumentGroups {
        overlays,
        projects,
        standalone,
    }
}

fn publish_diagnostics<S: BuildHasher>(
    by_uri: BTreeMap<Url, Vec<Diagnostic>>,
    docs: &HashMap<Url, Document, S>,
) -> Vec<PublishedDiagnostics> {
    by_uri
        .into_iter()
        .map(|(uri, mut diagnostics)| {
            sort_and_dedup(&mut diagnostics);
            PublishedDiagnostics {
                version: document_version(&uri, docs),
                uri,
                diagnostics,
            }
        })
        .collect()
}

fn standalone_diagnostics(
    uri: &Url,
    document: &Document,
    options: CompileOptions,
    session: &mut StandaloneDiagnosticSession,
) -> Vec<Diagnostic> {
    if let Some(cached) = &session.cached
        && cached.options == options
        && cached.source == document.text
    {
        return cached.diagnostics.clone();
    }

    let result = session.checker.check_with_options(&document.text, options);
    session.checks += 1;
    if std::env::var_os("RIDDLEC_PHASE_TIMING").is_some() {
        let name = uri.as_str().rsplit('/').next().unwrap_or_default();
        eprintln!(
            "[session] standalone check #{} of {name}: checker {} holds {} bodies",
            session.checks,
            session.checker.identity(),
            session.checker.known_bodies(),
        );
    }
    let diagnostics = collect_diagnostics(uri, &document.text, &result);
    session.cached = Some(CachedStandaloneDiagnostics {
        options,
        source: document.text.clone(),
        diagnostics: diagnostics.clone(),
    });
    diagnostics
}

fn project_diagnostics(
    root: &Path,
    overlays: &HashMap<PathBuf, String>,
    options: CompileOptions,
    session: &mut ProjectDiagnosticSession,
    analysis_sessions: &AnalysisSessions,
) -> Result<ProjectDiagnostics, String> {
    let checker = analysis_sessions.project(root);
    let mut checker = checker
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let analysis = clue::check_project_with_session(root, overlays, options, &mut checker)
        .map_err(|error| error.to_string())?;
    let revision = checker.revision();
    drop(checker);
    if let Some(cached) = &session.cached
        && cached.options == options
        && cached.revision == revision
    {
        return Ok(ProjectDiagnostics {
            by_uri: cached.diagnostics.clone(),
            files: cached.files.clone(),
        });
    }
    let files = analysis
        .source
        .files
        .iter()
        .cloned()
        .collect::<HashSet<_>>();
    let diagnostics = collect_mapped_diagnostics(&analysis.source.source_map, &analysis.result);
    session.cached = Some(CachedProjectDiagnostics {
        options,
        revision,
        files: files.clone(),
        diagnostics: diagnostics.clone(),
    });
    Ok(ProjectDiagnostics {
        by_uri: diagnostics,
        files,
    })
}

#[cfg(feature = "test")]
#[must_use]
pub fn collect_document_diagnostics<S: BuildHasher>(
    uri: &Url,
    _source: &str,
    docs: &HashMap<Url, Document, S>,
    options: CompileOptions,
) -> Vec<Diagnostic> {
    let target = published_uri(uri, docs);
    collect_workspace_diagnostics(docs, options)
        .into_iter()
        .find(|published| published.uri == target)
        .map(|published| published.diagnostics)
        .unwrap_or_default()
}

#[must_use]
pub fn collect_diagnostics(uri: &Url, source: &str, result: &CompileResult) -> Vec<Diagnostic> {
    let line_index = LineIndex::new(source);
    let mut diagnostics = diagnostic_exts(result)
        .filter_map(|diagnostic| to_lsp_with_index(uri, source, &line_index, diagnostic))
        .collect::<Vec<_>>();
    sort_and_dedup(&mut diagnostics);
    diagnostics
}

fn collect_mapped_diagnostics(
    source_map: &SourceMap,
    result: &CompileResult,
) -> BTreeMap<Url, Vec<Diagnostic>> {
    let mut by_uri = BTreeMap::<Url, Vec<Diagnostic>>::new();
    let mut line_indexes = HashMap::new();
    for diagnostic in diagnostic_exts(result) {
        let Some((uri, diagnostic)) =
            to_lsp_mapped_with_indexes(source_map, &mut line_indexes, diagnostic)
        else {
            continue;
        };
        by_uri.entry(uri).or_default().push(diagnostic);
    }
    for diagnostics in by_uri.values_mut() {
        sort_and_dedup(diagnostics);
    }
    by_uri
}

fn diagnostic_exts(result: &CompileResult) -> impl Iterator<Item = DiagnosticExt> + '_ {
    result
        .parse_errors
        .iter()
        .map(IntoDiagnosticExt::to_ext)
        .chain(result.hir_diagnostics.iter().map(IntoDiagnosticExt::to_ext))
        .chain(
            result
                .type_result
                .diagnostics
                .iter()
                .map(IntoDiagnosticExt::to_ext),
        )
        .chain(
            result
                .analysis_diagnostics
                .iter()
                .map(IntoDiagnosticExt::to_ext),
        )
}

#[cfg(feature = "test")]
#[must_use]
pub fn to_lsp(uri: &Url, source: &str, diagnostic: DiagnosticExt) -> Option<Diagnostic> {
    to_lsp_with_index(uri, source, &LineIndex::new(source), diagnostic)
}

fn to_lsp_with_index(
    uri: &Url,
    source: &str,
    line_index: &LineIndex,
    diagnostic: DiagnosticExt,
) -> Option<Diagnostic> {
    convert_diagnostic(diagnostic, |label| {
        Some(ResolvedLabel {
            uri: uri.clone(),
            range: line_index.range(source, normalize_range(source, label.range)?)?,
        })
    })
    .map(|(_, diagnostic)| diagnostic)
}

#[cfg(feature = "test")]
#[must_use]
pub fn to_lsp_mapped(
    source_map: &SourceMap,
    diagnostic: DiagnosticExt,
) -> Option<(Url, Diagnostic)> {
    to_lsp_mapped_with_indexes(source_map, &mut HashMap::new(), diagnostic)
}

fn to_lsp_mapped_with_indexes(
    source_map: &SourceMap,
    line_indexes: &mut HashMap<PathBuf, LineIndex>,
    diagnostic: DiagnosticExt,
) -> Option<(Url, Diagnostic)> {
    convert_diagnostic(diagnostic, |label| {
        let mapped = source_map.map_range(label.range)?;
        let path = mapped.path.to_path_buf();
        let line_index = line_indexes
            .entry(path.clone())
            .or_insert_with(|| LineIndex::new(mapped.source));
        Some(ResolvedLabel {
            uri: Url::from_file_path(path).ok()?,
            range: line_index
                .range(mapped.source, normalize_range(mapped.source, mapped.range)?)?,
        })
    })
}

fn convert_diagnostic(
    diagnostic: DiagnosticExt,
    mut resolve: impl FnMut(&SourceLabel) -> Option<ResolvedLabel>,
) -> Option<(Url, Diagnostic)> {
    let primary_index = diagnostic
        .labels
        .iter()
        .position(|label| label.style == LabelStyle::Primary)?;
    let mut resolved = diagnostic
        .labels
        .iter()
        .map(&mut resolve)
        .collect::<Vec<_>>();
    let primary = resolved.get_mut(primary_index)?.take()?;

    let mut message = diagnostic.message;
    let primary_message = diagnostic.labels[primary_index].message.trim();
    if !primary_message.is_empty() {
        message.push('\n');
        message.push_str(primary_message);
    }
    if let Some(help) = diagnostic.help {
        message.push_str("\nhelp: ");
        message.push_str(&help);
    }
    for note in diagnostic.notes {
        message.push_str("\nnote: ");
        message.push_str(&note);
    }

    let mut related_information = resolved
        .into_iter()
        .enumerate()
        .filter(|(index, _)| *index != primary_index)
        .filter_map(|(index, location)| {
            let location = location?;
            let label = &diagnostic.labels[index];
            Some(DiagnosticRelatedInformation {
                location: Location::new(location.uri, location.range),
                message: if label.message.trim().is_empty() {
                    "related location".into()
                } else {
                    label.message.clone()
                },
            })
        })
        .collect::<Vec<_>>();
    related_information.sort_by(compare_related);
    related_information.dedup();

    let uri = primary.uri;
    let code =
        (!diagnostic.code.is_empty()).then(|| NumberOrString::String(diagnostic.code.into()));
    Some((
        uri,
        Diagnostic {
            range: primary.range,
            severity: Some(severity(diagnostic.severity)),
            code,
            code_description: code_description(diagnostic.code),
            source: Some("riddle".into()),
            message,
            related_information: (!related_information.is_empty()).then_some(related_information),
            ..Diagnostic::default()
        },
    ))
}

fn code_description(code: &str) -> Option<CodeDescription> {
    (!code.is_empty()).then(|| CodeDescription {
        href: Url::parse(&format!(
            "https://riddle-lang.github.io/docs/errorcode.html#{}",
            code.to_ascii_lowercase()
        ))
        .expect("diagnostic code must form a valid documentation URL"),
    })
}

fn project_error(source: &str, error: impl std::fmt::Display) -> Diagnostic {
    Diagnostic {
        range: anchor_range(source),
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(NumberOrString::String("CLUE0001".into())),
        source: Some("clue".into()),
        message: error.to_string(),
        ..Diagnostic::default()
    }
}

fn anchor_range(source: &str) -> Range {
    let start = source
        .char_indices()
        .find_map(|(offset, ch)| (!ch.is_whitespace()).then_some(offset))
        .unwrap_or(0);
    let end = source[start..]
        .find(['\r', '\n'])
        .map_or(source.len(), |offset| start + offset);
    let range = text_range(start, end);
    try_range(source, normalize_range(source, range).unwrap_or(range)).unwrap_or_default()
}

fn normalize_range(source: &str, range: TextRange) -> Option<TextRange> {
    let start = usize::from(range.start());
    let end = usize::from(range.end());
    if start > end
        || end > source.len()
        || !source.is_char_boundary(start)
        || !source.is_char_boundary(end)
    {
        return None;
    }
    if start == end {
        return Some(range);
    }

    let text = source.get(start..end)?;
    let trimmed_start = start + text.len() - text.trim_start().len();
    let trimmed_end = end - (text.len() - text.trim_end().len());
    if trimmed_start >= trimmed_end {
        return Some(TextRange::empty(text_size(trimmed_start)));
    }
    Some(text_range(trimmed_start, trimmed_end))
}

fn try_range(source: &str, range: TextRange) -> Option<Range> {
    LineIndex::new(source).range(source, range)
}

fn try_position(source: &str, offset: usize) -> Option<Position> {
    LineIndex::new(source).position(source, offset)
}

pub fn position(source: &str, offset: usize) -> Position {
    let mut offset = offset.min(source.len());
    while !source.is_char_boundary(offset) {
        offset -= 1;
    }
    try_position(source, offset).unwrap_or_default()
}

const fn severity(severity: type_checker::Severity) -> DiagnosticSeverity {
    match severity {
        type_checker::Severity::Error => DiagnosticSeverity::ERROR,
        type_checker::Severity::Warning => DiagnosticSeverity::WARNING,
        type_checker::Severity::Note => DiagnosticSeverity::INFORMATION,
        type_checker::Severity::Help => DiagnosticSeverity::HINT,
    }
}

fn normalized_uri(uri: &Url) -> Url {
    uri.to_file_path()
        .ok()
        .map(normalized_path)
        .and_then(|path| Url::from_file_path(path).ok())
        .unwrap_or_else(|| uri.clone())
}

fn published_uri<S: BuildHasher>(uri: &Url, docs: &HashMap<Url, Document, S>) -> Url {
    if docs.contains_key(uri) {
        return uri.clone();
    }

    let normalized = normalized_uri(uri);
    docs.keys()
        .filter(|candidate| normalized_uri(candidate) == normalized)
        .min_by(|left, right| left.as_str().cmp(right.as_str()))
        .cloned()
        .unwrap_or(normalized)
}

fn document_version<S: BuildHasher>(uri: &Url, docs: &HashMap<Url, Document, S>) -> Option<i32> {
    docs.get(&published_uri(uri, docs))
        .and_then(|document| document.version)
}

fn sort_and_dedup(diagnostics: &mut Vec<Diagnostic>) {
    diagnostics.sort_by(compare_diagnostics);
    diagnostics.dedup();
}

fn compare_diagnostics(left: &Diagnostic, right: &Diagnostic) -> Ordering {
    diagnostic_range_key(left)
        .cmp(&diagnostic_range_key(right))
        .then_with(|| severity_key(left.severity).cmp(&severity_key(right.severity)))
        .then_with(|| code_key(left.code.as_ref()).cmp(&code_key(right.code.as_ref())))
        .then_with(|| left.source.cmp(&right.source))
        .then_with(|| left.message.cmp(&right.message))
        .then_with(|| related_key(left).cmp(&related_key(right)))
}

fn compare_related(
    left: &DiagnosticRelatedInformation,
    right: &DiagnosticRelatedInformation,
) -> Ordering {
    related_location_key(left)
        .cmp(&related_location_key(right))
        .then_with(|| left.message.cmp(&right.message))
}

const fn diagnostic_range_key(diagnostic: &Diagnostic) -> (u32, u32, u32, u32) {
    let range = diagnostic.range;
    (
        range.start.line,
        range.start.character,
        range.end.line,
        range.end.character,
    )
}

fn related_location_key(related: &DiagnosticRelatedInformation) -> (String, u32, u32, u32, u32) {
    let range = related.location.range;
    (
        related.location.uri.as_str().to_owned(),
        range.start.line,
        range.start.character,
        range.end.line,
        range.end.character,
    )
}

fn related_key(diagnostic: &Diagnostic) -> Vec<(String, u32, u32, u32, u32, String)> {
    diagnostic
        .related_information
        .iter()
        .flatten()
        .map(|related| {
            let (uri, start_line, start_character, end_line, end_character) =
                related_location_key(related);
            (
                uri,
                start_line,
                start_character,
                end_line,
                end_character,
                related.message.clone(),
            )
        })
        .collect()
}

const fn severity_key(severity: Option<DiagnosticSeverity>) -> u8 {
    match severity {
        Some(DiagnosticSeverity::ERROR) => 0,
        Some(DiagnosticSeverity::WARNING) => 1,
        Some(DiagnosticSeverity::INFORMATION) => 2,
        Some(DiagnosticSeverity::HINT) => 3,
        _ => 4,
    }
}

fn code_key(code: Option<&NumberOrString>) -> (u8, String) {
    match code {
        Some(NumberOrString::Number(number)) => (0, number.to_string()),
        Some(NumberOrString::String(code)) => (1, code.clone()),
        None => (2, String::new()),
    }
}
