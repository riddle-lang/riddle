use std::{
    collections::{HashMap, HashSet},
    fs,
    hash::BuildHasher,
    io,
    ops::Range,
    path::{Path, PathBuf},
    sync::{Arc, OnceLock},
    time::Instant,
};

use ast::{self, support::AstNode};
use frontend::incremental::IncrementalParser;
use frontend::{ParseError, tree_builder::Parse};
use hir::lower_root;
use mir::backend::{Backend, c::CBackend};
use mir::{self, Module};
use scope_graph::{builder::build_scope_graph, resolve::resolve_hir};
use syntax::SyntaxNode;
use type_checker::{self, IncrementalTypeChecker, TypeCheckResult, check_hir};

use crate::text_range;

/// Where compilation time goes, printed when `RIDDLEC_PHASE_TIMING` is set.
///
/// A whole project check is the dominant cost behind every language-server
/// request, and the phase boundaries already exist as cancellation checks. This
/// only adds an `Instant` read per boundary, and nothing at all when the
/// variable is unset. The enabled flag is read once per pipeline run rather
/// than per phase.
struct PhaseTimer {
    total: Instant,
    started: Option<Instant>,
}

impl PhaseTimer {
    fn new(enabled: bool) -> Self {
        let total = Instant::now();
        Self {
            total,
            started: enabled.then_some(total),
        }
    }

    /// Whether timing is being collected at all.
    const fn enabled(&self) -> bool {
        self.started.is_some()
    }

    /// Reports the phase that just ended.
    fn phase(&mut self, name: &str) {
        self.phase_with_counts(name, 0, 0);
    }

    /// Reports a phase along with the body-reuse counters it produced.
    fn phase_with_counts(&mut self, name: &str, checked: usize, reused: usize) {
        let Some(started) = self.started else {
            return;
        };
        let now = Instant::now();
        let elapsed = now.duration_since(started);
        let counts = if checked == 0 && reused == 0 {
            String::new()
        } else {
            format!("   [checked {checked}, reused {reused}]")
        };
        eprintln!(
            "[phase] {name:<22} {:>8.2} ms   (cumulative {:>8.2} ms){counts}",
            elapsed.as_secs_f64() * 1000.0,
            now.duration_since(self.total).as_secs_f64() * 1000.0,
        );
        self.started = Some(now);
    }
}

pub use type_checker::Diagnostic;

const RAW_STD_PRELUDE: &str = include_str!(concat!(env!("OUT_DIR"), "/std.rid"));

fn std_prelude() -> &'static str {
    static EXPANDED: OnceLock<String> = OnceLock::new();
    EXPANDED
        .get_or_init(|| {
            let expanded = crate::proc_macro::expand_standard_macros(RAW_STD_PRELUDE);
            assert!(
                expanded.diagnostics.is_empty(),
                "bundled standard library macro expansion failed: {:?}",
                expanded.diagnostics
            );
            expanded.source
        })
        .as_str()
}

/// Test-only access to the expanded bundled std prelude, so diagnostics can
/// slice the exact text their spans refer to.
#[doc(hidden)]
pub fn expanded_std_prelude() -> &'static str {
    std_prelude()
}

/// Placeholder path for the user region of [`CompileResult::source_files`];
/// [`compile_for_interpretation`] renames it to the caller's file name.
const USER_SOURCE_PATH: &str = "source";
/// Path reported for panics inside the appended bundled-std region.
const STD_SOURCE_PATH: &str = "std";

#[derive(Default)]
pub struct CompileResult {
    pub hir: Option<hir::HirFile>,
    pub scope_graph: Option<scope_graph::ScopeGraph>,
    pub type_result: TypeCheckResult,
    pub macro_diagnostics: Vec<type_checker::Diagnostic>,
    pub hir_diagnostics: Vec<type_checker::Diagnostic>,
    pub analysis_diagnostics: Vec<type_checker::Diagnostic>,
    pub analysis: move_checker::AnalysisResult,
    pub mir_module: Option<Module>,
    pub parse_errors: Vec<ParseError>,
    /// Panic-site origin map for interpreted runs: the user region
    /// (rewritten through standard-macro expansion) plus the appended std
    /// region. Empty unless a standalone compile produced a MIR module.
    pub source_files: Vec<mir::SourceFile>,
}

#[derive(Debug, Clone)]
pub struct LoadedSource {
    pub source: String,
    pub files: Vec<PathBuf>,
    pub source_map: SourceMap,
}

#[derive(Debug, Clone, Default)]
pub struct SourceMap {
    segments: Vec<SourceSegment>,
}

#[derive(Debug, Clone)]
struct SourceSegment {
    generated: Range<usize>,
    path: PathBuf,
    source: Arc<str>,
    original_start: usize,
    synthetic: Option<Range<usize>>,
}

pub struct MappedSource<'a> {
    pub path: &'a Path,
    pub source: &'a str,
    pub range: rowan::TextRange,
    pub synthetic: bool,
}

impl SourceMap {
    #[must_use]
    pub fn map_range(&self, range: rowan::TextRange) -> Option<MappedSource<'_>> {
        let start = usize::from(range.start());
        let end = usize::from(range.end());
        let (segment, mapped_range, synthetic) = self
            .segments
            .iter()
            .filter(|segment| segment.generated.contains(&start) && end <= segment.generated.end)
            .min_by_key(|segment| segment.generated.len())
            .map(|segment| (segment, None, segment.synthetic.is_some()))
            .or_else(|| {
                if end == start {
                    self.segments
                        .iter()
                        .find(|segment| segment.generated.end == start)
                        .map(|segment| (segment, None, segment.synthetic.is_some()))
                } else {
                    None
                }
            })
            .or_else(|| {
                // A diagnostic can span copied tokens and synthetic macro output.
                let first = self
                    .segments
                    .iter()
                    .filter(|segment| segment.generated.contains(&start))
                    .min_by_key(|segment| segment.generated.len())?;
                let last_offset = end.checked_sub(1)?;
                let last = self
                    .segments
                    .iter()
                    .filter(|segment| segment.generated.contains(&last_offset))
                    .min_by_key(|segment| segment.generated.len())?;
                if first.path != last.path || first.source != last.source {
                    return None;
                }

                let mut segments = self.segments.iter().filter(|segment| {
                    segment.generated.start < end && start < segment.generated.end
                });
                let mut original_start = usize::MAX;
                let mut original_end = 0;
                let mut synthetic = false;
                for segment in &mut segments {
                    if segment.path != first.path || segment.source != first.source {
                        return None;
                    }
                    synthetic |= segment.synthetic.is_some();
                    let mapped = segment.synthetic.clone().unwrap_or_else(|| {
                        let overlap_start = start.max(segment.generated.start);
                        let overlap_end = end.min(segment.generated.end);
                        let original_start =
                            segment.original_start + overlap_start - segment.generated.start;
                        original_start..original_start + overlap_end - overlap_start
                    });
                    original_start = original_start.min(mapped.start);
                    original_end = original_end.max(mapped.end);
                }
                Some((first, Some(original_start..original_end), synthetic))
            })?;
        let original = mapped_range.unwrap_or_else(|| {
            segment.synthetic.clone().unwrap_or_else(|| {
                let original_start = segment.original_start + start - segment.generated.start;
                original_start..original_start + end - start
            })
        });
        Some(MappedSource {
            path: &segment.path,
            source: &segment.source,
            range: text_range(original.start, original.end),
            synthetic,
        })
    }

    #[must_use]
    pub fn contains_file(&self, path: &Path) -> bool {
        self.segments.iter().any(|segment| segment.path == path)
    }

    pub fn extend(&mut self, mut other: Self, generated_start: usize) {
        for segment in &mut other.segments {
            segment.generated.start += generated_start;
            segment.generated.end += generated_start;
        }
        self.segments.extend(other.segments);
    }

    /// Converts the segments into the source files the MIR C backend uses to
    /// resolve panic locations back to module files.
    #[must_use]
    pub fn to_mir_files(&self) -> Vec<mir::SourceFile> {
        self.segments
            .iter()
            .map(|segment| {
                let mut original_start = segment.original_start;
                if let (true, Some(original)) = (segment.synthetic.is_some(), &segment.synthetic) {
                    // The call-site range may start on surrounding trivia;
                    // anchor generated output at its first code character.
                    let text = segment.source.as_ref();
                    let end = original.end.min(text.len());
                    for (index, ch) in text[original_start..end].char_indices() {
                        if !ch.is_whitespace() {
                            original_start += index;
                            break;
                        }
                    }
                }
                mir::SourceFile {
                    generated: segment.generated.clone(),
                    original_start,
                    synthetic: segment.synthetic.is_some(),
                    path: segment.path.display().to_string(),
                    text: segment.source.to_string(),
                }
            })
            .collect()
    }

    fn push(
        &mut self,
        generated: Range<usize>,
        path: &Path,
        source: Arc<str>,
        original_start: usize,
    ) {
        if !generated.is_empty() || source.is_empty() {
            self.segments.push(SourceSegment {
                generated,
                path: path.to_path_buf(),
                source,
                original_start,
                synthetic: None,
            });
        }
    }

    fn apply_generated_insertions(&mut self, insertions: &[crate::proc_macro::GeneratedInsertion]) {
        if insertions.is_empty() {
            return;
        }
        let sources = insertions
            .iter()
            .map(|insertion| {
                let range = text_range(insertion.call_site.start, insertion.call_site.end);
                let anchor = self.map_range(range).map(|mapped| {
                    (
                        mapped.path.to_path_buf(),
                        Arc::<str>::from(mapped.source),
                        usize::from(mapped.range.start())..usize::from(mapped.range.end()),
                    )
                });
                let spans = insertion
                    .spans
                    .iter()
                    .filter(|span| u32::try_from(span.original.end).is_ok())
                    .filter_map(|span| {
                        let range = text_range(span.original.start, span.original.end);
                        self.map_range(range).map(|mapped| {
                            (
                                span.generated.clone(),
                                mapped.path.to_path_buf(),
                                Arc::<str>::from(mapped.source),
                                usize::from(mapped.range.start())..usize::from(mapped.range.end()),
                            )
                        })
                    })
                    .collect::<Vec<_>>();
                (anchor, spans)
            })
            .collect::<Vec<_>>();

        let mut shifted = Vec::new();
        for segment in std::mem::take(&mut self.segments) {
            let mut cursor = segment.generated.start;
            let mut shift = insertions
                .iter()
                .filter(|insertion| insertion.at <= cursor)
                .map(|insertion| insertion.text.len())
                .sum::<usize>();
            for insertion in insertions.iter().filter(|insertion| {
                segment.generated.start < insertion.at && insertion.at < segment.generated.end
            }) {
                if cursor < insertion.at {
                    shifted.push(SourceSegment {
                        generated: cursor + shift..insertion.at + shift,
                        path: segment.path.clone(),
                        source: segment.source.clone(),
                        original_start: segment.original_start + cursor - segment.generated.start,
                        synthetic: None,
                    });
                }
                cursor = insertion.at;
                shift += insertion.text.len();
            }
            if cursor < segment.generated.end {
                shifted.push(SourceSegment {
                    generated: cursor + shift..segment.generated.end + shift,
                    path: segment.path,
                    source: segment.source,
                    original_start: segment.original_start + cursor - segment.generated.start,
                    synthetic: None,
                });
            }
        }

        let mut shift = 0usize;
        for (insertion, (anchor, token_spans)) in insertions.iter().zip(sources) {
            if let Some((path, source, original)) = anchor {
                shifted.push(SourceSegment {
                    generated: insertion.at + shift..insertion.at + shift + insertion.text.len(),
                    path,
                    source,
                    original_start: original.start,
                    synthetic: Some(original),
                });
            }
            for (generated, path, source, original) in token_spans {
                if generated.end <= insertion.text.len() && generated.start < generated.end {
                    shifted.push(SourceSegment {
                        generated: insertion.at + shift + generated.start
                            ..insertion.at + shift + generated.end,
                        path,
                        source,
                        original_start: original.start,
                        synthetic: Some(original),
                    });
                }
            }
            shift += insertion.text.len();
        }
        shifted.sort_by_key(|segment| segment.generated.start);
        self.segments = shifted;
    }

    fn apply_expansion(&mut self, mappings: &[crate::proc_macro::ExpandedTokenMapping]) {
        let mut segments: Vec<SourceSegment> = Vec::new();
        for mapping in mappings {
            let original = text_range(mapping.original.start, mapping.original.end);
            let Some(mapped) = self.map_range(original) else {
                continue;
            };
            let segment = SourceSegment {
                generated: mapping.generated.clone(),
                path: mapped.path.to_path_buf(),
                source: Arc::<str>::from(mapped.source),
                original_start: usize::from(mapped.range.start()),
                synthetic: mapping
                    .synthetic
                    .then_some(usize::from(mapped.range.start())..usize::from(mapped.range.end())),
            };
            let generated_is_contiguous = segments
                .last()
                .is_some_and(|previous| previous.generated.end == segment.generated.start);
            let original_is_contiguous = segments.last().is_some_and(|previous| {
                previous.original_start + previous.generated.len() == segment.original_start
            });
            if let Some(previous) = segments.last_mut()
                && previous.synthetic.is_none()
                && segment.synthetic.is_none()
                && previous.path == segment.path
                && previous.source == segment.source
                && generated_is_contiguous
                && original_is_contiguous
            {
                previous.generated.end = segment.generated.end;
            } else {
                segments.push(segment);
            }
        }
        self.segments = segments;
    }
}

impl LoadedSource {
    pub fn single_file(path: impl AsRef<Path>, source: impl Into<String>) -> Self {
        let path = path.as_ref().to_path_buf();
        let source = source.into();
        let mut source_map = SourceMap::default();
        source_map.push(0..source.len(), &path, Arc::from(source.as_str()), 0);
        Self {
            source,
            files: vec![path],
            source_map,
        }
    }

    pub fn apply_generated_insertions(
        &mut self,
        source: String,
        insertions: &[crate::proc_macro::GeneratedInsertion],
    ) {
        self.source_map.apply_generated_insertions(insertions);
        self.source = source;
    }

    pub fn apply_expansion(
        &mut self,
        source: String,
        mappings: &[crate::proc_macro::ExpandedTokenMapping],
    ) {
        self.source_map.apply_expansion(mappings);
        self.source = source;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CompileOptions {
    pub use_std: bool,
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self { use_std: true }
    }
}

pub struct CheckSession {
    parser: IncrementalParser,
    type_checker: IncrementalTypeChecker,
    /// Process-unique identity, so a trace can tell a reused session from a
    /// recreated one that happens to hold the same number of bodies.
    id: usize,
}

/// Assigns process-unique identities to `CheckSession`s, for tracing.
///
/// A reused session keeps its identity across requests while a recreated one
/// gets a new number, which is how a trace tells "the entries were dropped"
/// apart from "the checker was replaced" — identical from the outside, and
/// ~700 ms apart in cost.
static CHECK_SESSIONS_CREATED: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

impl Default for CheckSession {
    /// Counted, because both `CheckSession::new` and `or_default` reach here.
    fn default() -> Self {
        let id = CHECK_SESSIONS_CREATED.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;
        Self {
            parser: IncrementalParser::default(),
            type_checker: IncrementalTypeChecker::default(),
            id,
        }
    }
}

impl CheckSession {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// How many function bodies this session's incremental checker holds.
    ///
    /// Exposed so a caller can tell "the checker was replaced" apart from "the
    /// entries were dropped", which look identical from the outside and differ
    /// by roughly 700 ms on the next check.
    #[must_use]
    pub fn known_bodies(&self) -> usize {
        self.type_checker.known_bodies()
    }

    /// Reuse counters from this session's most recent check.
    #[must_use]
    pub const fn last_stats(&self) -> type_checker::IncrementalStats {
        self.type_checker.last_stats()
    }

    /// Process-unique identity of this session, for traces.
    #[must_use]
    pub const fn identity(&self) -> usize {
        self.id
    }

    /// Checks a source buffer while reusing this session's incremental state.
    ///
    /// # Panics
    ///
    /// Panics if the non-cancellable pipeline unexpectedly reports cancellation.
    pub fn check_with_options(&mut self, source: &str, options: CompileOptions) -> CompileResult {
        run_standalone_pipeline_with_state_cancellable(
            source,
            options,
            true,
            PipelineDepth::Check,
            &mut self.parser,
            Some(&mut self.type_checker),
            &|| false,
        )
        .expect("non-cancellable pipeline cannot be cancelled")
    }

    pub fn check_with_options_cancellable(
        &mut self,
        source: &str,
        options: CompileOptions,
        cancelled: impl Fn() -> bool,
    ) -> Option<CompileResult> {
        run_standalone_pipeline_with_state_cancellable(
            source,
            options,
            true,
            PipelineDepth::Check,
            &mut self.parser,
            Some(&mut self.type_checker),
            &cancelled,
        )
    }

    pub fn infer_with_options_cancellable(
        &mut self,
        source: &str,
        options: CompileOptions,
        cancelled: impl Fn() -> bool,
    ) -> Option<CompileResult> {
        run_standalone_pipeline_with_state_cancellable(
            source,
            options,
            true,
            PipelineDepth::Infer,
            &mut self.parser,
            Some(&mut self.type_checker),
            &cancelled,
        )
    }

    pub fn resolve_package_with_options(
        &mut self,
        source: &str,
        package_ranges: &[Range<usize>],
        options: CompileOptions,
    ) -> CompileResult {
        run_pipeline_with_state(
            source,
            package_ranges,
            options,
            true,
            PipelineDepth::Resolve,
            PipelineState {
                preparsed: None,
                parser: &mut self.parser,
                incremental_type_checker: None,
            },
        )
    }

    pub fn resolve_package_with_options_cancellable(
        &mut self,
        source: &str,
        package_ranges: &[Range<usize>],
        options: CompileOptions,
        cancelled: impl Fn() -> bool,
    ) -> Option<CompileResult> {
        self.resolve_package_with_options_and_gc_cancellable(
            source,
            package_ranges,
            options,
            true,
            cancelled,
        )
    }

    pub fn resolve_package_with_options_and_gc_cancellable(
        &mut self,
        source: &str,
        package_ranges: &[Range<usize>],
        options: CompileOptions,
        gc_enabled: bool,
        cancelled: impl Fn() -> bool,
    ) -> Option<CompileResult> {
        run_pipeline_with_state_cancellable(
            source,
            package_ranges,
            options,
            gc_enabled,
            PipelineDepth::Resolve,
            PipelineState {
                preparsed: None,
                parser: &mut self.parser,
                incremental_type_checker: None,
            },
            &cancelled,
        )
    }

    pub fn resolve_parsed_package_with_options_cancellable(
        &mut self,
        source: &str,
        parse: &Parse,
        package_ranges: &[Range<usize>],
        options: CompileOptions,
        cancelled: impl Fn() -> bool,
    ) -> Option<CompileResult> {
        self.resolve_parsed_package_with_options_and_gc_cancellable(
            source,
            parse,
            package_ranges,
            options,
            true,
            cancelled,
        )
    }

    pub fn resolve_parsed_package_with_options_and_gc_cancellable(
        &mut self,
        source: &str,
        parse: &Parse,
        package_ranges: &[Range<usize>],
        options: CompileOptions,
        gc_enabled: bool,
        cancelled: impl Fn() -> bool,
    ) -> Option<CompileResult> {
        run_pipeline_with_state_cancellable(
            source,
            package_ranges,
            options,
            gc_enabled,
            PipelineDepth::Resolve,
            PipelineState {
                preparsed: Some(parse),
                parser: &mut self.parser,
                incremental_type_checker: None,
            },
            &cancelled,
        )
    }

    pub fn infer_package_with_options_and_gc_cancellable(
        &mut self,
        source: &str,
        package_ranges: &[Range<usize>],
        options: CompileOptions,
        gc_enabled: bool,
        cancelled: impl Fn() -> bool,
    ) -> Option<CompileResult> {
        run_pipeline_with_state_cancellable(
            source,
            package_ranges,
            options,
            gc_enabled,
            PipelineDepth::Infer,
            PipelineState {
                preparsed: None,
                parser: &mut self.parser,
                incremental_type_checker: Some(&mut self.type_checker),
            },
            &cancelled,
        )
    }

    pub fn infer_parsed_package_with_options_and_gc_cancellable(
        &mut self,
        source: &str,
        parse: &Parse,
        package_ranges: &[Range<usize>],
        options: CompileOptions,
        gc_enabled: bool,
        cancelled: impl Fn() -> bool,
    ) -> Option<CompileResult> {
        run_pipeline_with_state_cancellable(
            source,
            package_ranges,
            options,
            gc_enabled,
            PipelineDepth::Infer,
            PipelineState {
                preparsed: Some(parse),
                parser: &mut self.parser,
                incremental_type_checker: Some(&mut self.type_checker),
            },
            &cancelled,
        )
    }

    /// Resolves a source buffer while reusing this session's parser state.
    ///
    /// # Panics
    ///
    /// Panics if the non-cancellable pipeline unexpectedly reports cancellation.
    pub fn resolve_with_options(&mut self, source: &str, options: CompileOptions) -> CompileResult {
        run_standalone_pipeline_with_state_cancellable(
            source,
            options,
            true,
            PipelineDepth::Resolve,
            &mut self.parser,
            None,
            &|| false,
        )
        .expect("non-cancellable pipeline cannot be cancelled")
    }

    pub fn resolve_with_options_cancellable(
        &mut self,
        source: &str,
        options: CompileOptions,
        cancelled: impl Fn() -> bool,
    ) -> Option<CompileResult> {
        run_standalone_pipeline_with_state_cancellable(
            source,
            options,
            true,
            PipelineDepth::Resolve,
            &mut self.parser,
            None,
            &cancelled,
        )
    }

    pub fn check_package_with_options(
        &mut self,
        source: &str,
        package_ranges: &[Range<usize>],
        options: CompileOptions,
    ) -> CompileResult {
        run_pipeline_with_state(
            source,
            package_ranges,
            options,
            true,
            PipelineDepth::Check,
            PipelineState {
                preparsed: None,
                parser: &mut self.parser,
                incremental_type_checker: Some(&mut self.type_checker),
            },
        )
    }

    pub fn check_package_with_options_cancellable(
        &mut self,
        source: &str,
        package_ranges: &[Range<usize>],
        options: CompileOptions,
        cancelled: impl Fn() -> bool,
    ) -> Option<CompileResult> {
        self.check_package_with_options_and_gc_cancellable(
            source,
            package_ranges,
            options,
            true,
            cancelled,
        )
    }

    pub fn check_package_with_options_and_gc_cancellable(
        &mut self,
        source: &str,
        package_ranges: &[Range<usize>],
        options: CompileOptions,
        gc_enabled: bool,
        cancelled: impl Fn() -> bool,
    ) -> Option<CompileResult> {
        run_pipeline_with_state_cancellable(
            source,
            package_ranges,
            options,
            gc_enabled,
            PipelineDepth::Check,
            PipelineState {
                preparsed: None,
                parser: &mut self.parser,
                incremental_type_checker: Some(&mut self.type_checker),
            },
            &cancelled,
        )
    }

    pub fn check_parsed_package_with_options_cancellable(
        &mut self,
        source: &str,
        parse: &Parse,
        package_ranges: &[Range<usize>],
        options: CompileOptions,
        cancelled: impl Fn() -> bool,
    ) -> Option<CompileResult> {
        self.check_parsed_package_with_options_and_gc_cancellable(
            source,
            parse,
            package_ranges,
            options,
            true,
            cancelled,
        )
    }

    pub fn check_parsed_package_with_options_and_gc_cancellable(
        &mut self,
        source: &str,
        parse: &Parse,
        package_ranges: &[Range<usize>],
        options: CompileOptions,
        gc_enabled: bool,
        cancelled: impl Fn() -> bool,
    ) -> Option<CompileResult> {
        run_pipeline_with_state_cancellable(
            source,
            package_ranges,
            options,
            gc_enabled,
            PipelineDepth::Check,
            PipelineState {
                preparsed: Some(parse),
                parser: &mut self.parser,
                incremental_type_checker: Some(&mut self.type_checker),
            },
            &cancelled,
        )
    }
}

/// An adapter that lets the diagnostics printer handle diagnostics from
/// different sources (parse errors, type errors, move errors) uniformly.
///
/// This is also used as the bridge type in the diagnostics printer.
#[derive(Debug, Clone)]
pub struct DiagnosticExt {
    pub code: &'static str,
    pub severity: type_checker::Severity,
    pub message: String,
    pub labels: Vec<type_checker::SourceLabel>,
    pub help: Option<String>,
    pub notes: Vec<String>,
}

pub trait IntoDiagnosticExt {
    fn to_ext(&self) -> DiagnosticExt;
}

impl IntoDiagnosticExt for type_checker::Diagnostic {
    fn to_ext(&self) -> DiagnosticExt {
        DiagnosticExt {
            code: self.code,
            severity: self.severity,
            message: self.message.clone(),
            labels: self.labels.clone(),
            help: self.help.clone(),
            notes: self.notes.clone(),
        }
    }
}

impl IntoDiagnosticExt for ParseError {
    fn to_ext(&self) -> DiagnosticExt {
        DiagnosticExt {
            code: "",
            severity: type_checker::Severity::Error,
            message: self.message.clone(),
            labels: vec![type_checker::SourceLabel {
                range: self.span,
                message: String::new(),
                style: type_checker::LabelStyle::Primary,
            }],
            help: None,
            notes: Vec::new(),
        }
    }
}

/// Loads a source file and recursively expands external modules.
///
/// # Errors
///
/// Returns an I/O error when a source or module cannot be read or resolved.
pub fn load_source_file(path: impl AsRef<Path>) -> io::Result<LoadedSource> {
    load_source_file_with_overlays(path, &HashMap::new())
}

/// Loads a source file, preferring the supplied in-memory overlays.
///
/// # Errors
///
/// Returns an I/O error when a source or module cannot be read or resolved.
pub fn load_source_file_with_overlays<S: BuildHasher>(
    path: impl AsRef<Path>,
    overlays: &HashMap<PathBuf, String, S>,
) -> io::Result<LoadedSource> {
    let mut files = Vec::new();
    let mut stack = HashSet::new();
    let overlays = overlays
        .iter()
        .map(|(path, source)| (normalized_path(path), source.clone()))
        .collect::<HashMap<_, _>>();
    let path = normalized_path(path.as_ref());
    let module_dir = path
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf();
    let expanded = load_source_file_inner(&path, &module_dir, &overlays, &mut stack, &mut files)?;
    Ok(LoadedSource {
        source: expanded.source,
        files,
        source_map: expanded.source_map,
    })
}

fn load_source_file_inner(
    path: &Path,
    module_dir: &Path,
    overlays: &HashMap<PathBuf, String>,
    stack: &mut HashSet<PathBuf>,
    files: &mut Vec<PathBuf>,
) -> io::Result<ExpandedSource> {
    let path = normalized_path(path);
    if !stack.insert(path.clone()) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("cyclic module import involving `{}`", path.display()),
        ));
    }

    let source: Arc<str> = overlays
        .get(&path)
        .cloned()
        .map(Into::into)
        .map_or_else(|| fs::read_to_string(&path).map(Into::into), Ok)?;
    files.push(path.clone());
    let expanded = expand_external_mods(&source, &path, module_dir, overlays, stack, files)?;
    stack.remove(&path);
    Ok(expanded)
}

fn expand_external_mods(
    source: &Arc<str>,
    path: &Path,
    module_dir: &Path,
    overlays: &HashMap<PathBuf, String>,
    stack: &mut HashSet<PathBuf>,
    files: &mut Vec<PathBuf>,
) -> io::Result<ExpandedSource> {
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source(source);
    if !parse.errors.is_empty() {
        return Ok(ExpandedSource::original(path, source));
    }

    let mut mods = Vec::new();
    collect_external_mods(&parse.syntax(), module_dir, &mut mods);
    if mods.is_empty() {
        return Ok(ExpandedSource::original(path, source));
    }

    let mut replacements = Vec::new();
    for ExternalMod { module, module_dir } in mods {
        let Some(name) = module.name().map(|token| token.text().to_string()) else {
            continue;
        };
        let child = find_module_file(&module_dir, &name, overlays)?;
        let child_dir = module_dir.join(&name);
        let child_source = load_source_file_inner(&child, &child_dir, overlays, stack, files)?;
        let range = module.syntax().text_range();
        let visibility = if module.is_pub() { "pub " } else { "" };
        replacements.push((
            usize::from(range.start()),
            usize::from(range.end()),
            format!("{visibility}mod {name} {{\n"),
            child_source,
        ));
    }

    replacements.sort_by_key(|(start, _, _, _)| *start);
    let mut out = String::with_capacity(source.len());
    let mut source_map = SourceMap::default();
    let mut cursor = 0;
    for (start, end, prefix, child) in replacements {
        append_original(&mut out, &mut source_map, path, source, cursor..start);
        out.push_str(&prefix);
        let child_start = out.len();
        out.push_str(&child.source);
        source_map.extend(child.source_map, child_start);
        out.push_str("\n}");
        cursor = end;
    }
    append_original(
        &mut out,
        &mut source_map,
        path,
        source,
        cursor..source.len(),
    );
    Ok(ExpandedSource {
        source: out,
        source_map,
    })
}

struct ExpandedSource {
    source: String,
    source_map: SourceMap,
}

impl ExpandedSource {
    fn original(path: &Path, source: &Arc<str>) -> Self {
        let mut source_map = SourceMap::default();
        source_map.push(0..source.len(), path, Arc::clone(source), 0);
        Self {
            source: source.to_string(),
            source_map,
        }
    }
}

fn append_original(
    out: &mut String,
    source_map: &mut SourceMap,
    path: &Path,
    source: &Arc<str>,
    original: Range<usize>,
) {
    let generated_start = out.len();
    out.push_str(&source[original.clone()]);
    source_map.push(
        generated_start..out.len(),
        path,
        Arc::clone(source),
        original.start,
    );
}

struct ExternalMod {
    module: ast::ModDecl,
    module_dir: PathBuf,
}

fn collect_external_mods(node: &SyntaxNode, module_dir: &Path, out: &mut Vec<ExternalMod>) {
    for child in node.children() {
        if let Some(module) = ast::ModDecl::cast(child.clone()) {
            if module.items().is_none() {
                out.push(ExternalMod {
                    module,
                    module_dir: module_dir.to_path_buf(),
                });
                continue;
            }
            let Some(name) = module.name().map(|token| token.text().to_string()) else {
                collect_external_mods(&child, module_dir, out);
                continue;
            };
            collect_external_mods(&child, &module_dir.join(name), out);
            continue;
        }
        collect_external_mods(&child, module_dir, out);
    }
}

fn find_module_file(
    module_dir: &Path,
    name: &str,
    overlays: &HashMap<PathBuf, String>,
) -> io::Result<PathBuf> {
    let flat = module_dir.join(format!("{name}.rid"));
    let nested = module_dir.join(name).join("mod.rid");
    let flat_exists = flat.is_file() || overlays.contains_key(&normalized_path(&flat));
    let nested_exists = nested.is_file() || overlays.contains_key(&normalized_path(&nested));
    match (flat_exists, nested_exists) {
        (true, false) => Ok(flat),
        (false, true) => Ok(nested),
        (true, true) => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "module `{name}` is ambiguous; both `{}` and `{}` exist",
                flat.display(),
                nested.display()
            ),
        )),
        (false, false) => Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!(
                "module `{name}` not found; expected `{}` or `{}`",
                flat.display(),
                nested.display()
            ),
        )),
    }
}

fn normalized_path(path: &Path) -> PathBuf {
    fs::canonicalize(path).unwrap_or_else(|_| {
        path.parent()
            .and_then(|parent| fs::canonicalize(parent).ok())
            .and_then(|parent| path.file_name().map(|name| parent.join(name)))
            .unwrap_or_else(|| path.to_path_buf())
    })
}

/// Generates C for a MIR module using the default GC runtime.
///
/// # Errors
///
/// Returns an error when the module cannot be represented by the C backend.
pub fn generate_c(module: &Module) -> Result<String, String> {
    generate_c_with_gc_and_source(module, true, "<unknown>")
}

/// Generates C for a MIR module with an explicit GC setting.
///
/// # Errors
///
/// Returns an error when the module cannot be represented by the C backend.
pub fn generate_c_with_gc(module: &Module, gc_enabled: bool) -> Result<String, String> {
    generate_c_with_gc_and_source(module, gc_enabled, "<unknown>")
}

pub fn generate_c_with_gc_and_source(
    module: &Module,
    gc_enabled: bool,
    source_name: &str,
) -> Result<String, String> {
    let mut backend = if gc_enabled {
        CBackend::new()
    } else {
        CBackend::without_gc()
    }
    .with_source_name(source_name);
    backend.compile(module)
}

pub fn generate_c_for_package_with_gc_and_source(
    module: &Module,
    package: usize,
    gc_enabled: bool,
    source_name: &str,
) -> Result<String, String> {
    let mut backend = if gc_enabled {
        CBackend::new()
    } else {
        CBackend::without_gc()
    }
    .with_source_name(source_name)
    .for_package(package);
    backend.compile(module)
}

/// Generates C for one package of a MIR module, resolving panic locations
/// against the combined-source map of the loaded package.
///
/// # Errors
///
/// Returns an error when the module cannot be represented by the C backend.
pub fn generate_c_for_package_with_source_map(
    module: &Module,
    package: usize,
    gc_enabled: bool,
    source_map: &SourceMap,
    source_name: &str,
) -> Result<String, String> {
    let mut backend = if gc_enabled {
        CBackend::new()
    } else {
        CBackend::without_gc()
    }
    .with_source_name(source_name)
    .with_source_files(source_map.to_mir_files())
    .for_package(package);
    backend.compile(module)
}

/// Run the full frontend pipeline on `source`.
#[must_use]
pub fn compile(source: &str) -> CompileResult {
    compile_with_options(source, CompileOptions::default())
}

/// Compiles like [`compile`] and, when a MIR module is produced, maps panic
/// sites back to `user_path` (rewritten through standard-macro expansion)
/// and the appended std region — the source files the interpreter needs to
/// render panic locations for `riddle run` and `riddle repl`.
pub fn compile_for_interpretation(source: &str, user_path: &str) -> CompileResult {
    let mut result = compile(source);
    for file in &mut result.source_files {
        if file.path == USER_SOURCE_PATH {
            file.path = user_path.to_string();
        }
    }
    result
}

#[must_use]
pub fn compile_with_options(source: &str, options: CompileOptions) -> CompileResult {
    compile_with_options_and_gc(source, options, true)
}

#[must_use]
pub fn compile_with_options_and_gc(
    source: &str,
    options: CompileOptions,
    gc_enabled: bool,
) -> CompileResult {
    run_pipeline(source, options, gc_enabled, PipelineDepth::Build)
}

#[must_use]
pub fn check_with_options(source: &str, options: CompileOptions) -> CompileResult {
    check_with_options_and_gc(source, options, true)
}

#[must_use]
pub fn check_with_options_and_gc(
    source: &str,
    options: CompileOptions,
    gc_enabled: bool,
) -> CompileResult {
    run_pipeline(source, options, gc_enabled, PipelineDepth::Check)
}

#[must_use]
pub fn compile_package_with_options(
    source: &str,
    package_ranges: &[Range<usize>],
    options: CompileOptions,
) -> CompileResult {
    compile_package_with_options_and_gc(source, package_ranges, options, true)
}

#[must_use]
pub fn compile_package_with_options_and_gc(
    source: &str,
    package_ranges: &[Range<usize>],
    options: CompileOptions,
    gc_enabled: bool,
) -> CompileResult {
    compile_package_with_options_and_gc_and_names(source, package_ranges, &[], options, gc_enabled)
}

#[must_use]
pub fn compile_package_with_options_and_gc_and_names(
    source: &str,
    package_ranges: &[Range<usize>],
    package_names: &[String],
    options: CompileOptions,
    gc_enabled: bool,
) -> CompileResult {
    run_pipeline_with_state_and_names(
        source,
        package_ranges,
        package_names,
        options,
        gc_enabled,
        PipelineDepth::Build,
        PipelineState {
            preparsed: None,
            parser: &mut IncrementalParser::new(),
            incremental_type_checker: None,
        },
    )
}

#[must_use]
pub fn compile_parsed_package_with_options(
    source: &str,
    parse: &Parse,
    package_ranges: &[Range<usize>],
    options: CompileOptions,
) -> CompileResult {
    compile_parsed_package_with_options_and_gc(source, parse, package_ranges, options, true)
}

#[must_use]
pub fn compile_parsed_package_with_options_and_gc(
    source: &str,
    parse: &Parse,
    package_ranges: &[Range<usize>],
    options: CompileOptions,
    gc_enabled: bool,
) -> CompileResult {
    compile_parsed_package_with_options_and_gc_and_names(
        source,
        parse,
        package_ranges,
        &[],
        options,
        gc_enabled,
    )
}

#[must_use]
pub fn compile_parsed_package_with_options_and_gc_and_names(
    source: &str,
    parse: &Parse,
    package_ranges: &[Range<usize>],
    package_names: &[String],
    options: CompileOptions,
    gc_enabled: bool,
) -> CompileResult {
    run_pipeline_with_state_and_names(
        source,
        package_ranges,
        package_names,
        options,
        gc_enabled,
        PipelineDepth::Build,
        PipelineState {
            preparsed: Some(parse),
            parser: &mut IncrementalParser::new(),
            incremental_type_checker: None,
        },
    )
}

#[must_use]
pub fn check_package_with_options(
    source: &str,
    package_ranges: &[Range<usize>],
    options: CompileOptions,
) -> CompileResult {
    check_package_with_options_and_gc(source, package_ranges, options, true)
}

#[must_use]
pub fn check_package_with_options_and_gc(
    source: &str,
    package_ranges: &[Range<usize>],
    options: CompileOptions,
    gc_enabled: bool,
) -> CompileResult {
    run_pipeline_with_state(
        source,
        package_ranges,
        options,
        gc_enabled,
        PipelineDepth::Check,
        PipelineState {
            preparsed: None,
            parser: &mut IncrementalParser::new(),
            incremental_type_checker: None,
        },
    )
}

#[must_use]
pub fn check_parsed_package_with_options(
    source: &str,
    parse: &Parse,
    package_ranges: &[Range<usize>],
    options: CompileOptions,
) -> CompileResult {
    check_parsed_package_with_options_and_gc(source, parse, package_ranges, options, true)
}

#[must_use]
pub fn check_parsed_package_with_options_and_gc(
    source: &str,
    parse: &Parse,
    package_ranges: &[Range<usize>],
    options: CompileOptions,
    gc_enabled: bool,
) -> CompileResult {
    run_pipeline_with_state(
        source,
        package_ranges,
        options,
        gc_enabled,
        PipelineDepth::Check,
        PipelineState {
            preparsed: Some(parse),
            parser: &mut IncrementalParser::new(),
            incremental_type_checker: None,
        },
    )
}

#[must_use]
pub fn resolve_with_options(source: &str, options: CompileOptions) -> CompileResult {
    run_pipeline(source, options, true, PipelineDepth::Resolve)
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum PipelineDepth {
    Resolve,
    Infer,
    Check,
    Build,
}

struct PackageInputs<'a> {
    ranges: &'a [Range<usize>],
    names: &'a [String],
}

struct PipelineState<'a> {
    preparsed: Option<&'a Parse>,
    parser: &'a mut IncrementalParser,
    incremental_type_checker: Option<&'a mut IncrementalTypeChecker>,
}

fn run_pipeline(
    source: &str,
    options: CompileOptions,
    gc_enabled: bool,
    depth: PipelineDepth,
) -> CompileResult {
    run_standalone_pipeline_with_state_cancellable(
        source,
        options,
        gc_enabled,
        depth,
        &mut IncrementalParser::new(),
        None,
        &|| false,
    )
    .expect("non-cancellable pipeline cannot be cancelled")
}

fn run_standalone_pipeline_with_state_cancellable(
    source: &str,
    options: CompileOptions,
    gc_enabled: bool,
    depth: PipelineDepth,
    parser: &mut IncrementalParser,
    incremental_type_checker: Option<&mut IncrementalTypeChecker>,
    cancelled: &dyn Fn() -> bool,
) -> Option<CompileResult> {
    if cancelled() {
        return None;
    }
    let expansion = crate::proc_macro::expand_standard_macros(source);
    let package_range = 0..expansion.source.len();
    let mut result = run_pipeline_with_state_cancellable(
        &expansion.source,
        std::slice::from_ref(&package_range),
        options,
        gc_enabled,
        depth,
        PipelineState {
            preparsed: expansion.parse.as_ref(),
            parser,
            incremental_type_checker,
        },
        cancelled,
    )?;
    result.macro_diagnostics = expansion.diagnostics;
    if result.mir_module.is_some() {
        result.source_files = mir_source_files(
            source,
            &expansion.mappings,
            expansion.source.len(),
            options.use_std,
        );
    }
    Some(result)
}

/// Builds the panic-location map for interpreted runs. The user region's
/// offsets refer to the standard-macro-expanded text, so sites map back
/// through the expansion; the std region is appended verbatim after two
/// separator bytes (matching the deep pipeline's `format!("{source}\n\n{}")`).
fn mir_source_files(
    source: &str,
    mappings: &[crate::proc_macro::ExpandedTokenMapping],
    expanded_len: usize,
    use_std: bool,
) -> Vec<mir::SourceFile> {
    let mut map = SourceMap::default();
    map.push(
        0..source.len(),
        Path::new(USER_SOURCE_PATH),
        Arc::from(source),
        0,
    );
    map.apply_expansion(mappings);
    if use_std {
        let std_start = expanded_len + 2;
        map.push(
            std_start..std_start + std_prelude().len(),
            Path::new(STD_SOURCE_PATH),
            Arc::from(std_prelude()),
            0,
        );
    }
    map.to_mir_files()
}

fn run_pipeline_with_state(
    source: &str,
    package_ranges: &[Range<usize>],
    options: CompileOptions,
    gc_enabled: bool,
    depth: PipelineDepth,
    state: PipelineState<'_>,
) -> CompileResult {
    run_pipeline_with_state_and_names(
        source,
        package_ranges,
        &[],
        options,
        gc_enabled,
        depth,
        state,
    )
}

fn run_pipeline_with_state_and_names(
    source: &str,
    package_ranges: &[Range<usize>],
    package_names: &[String],
    options: CompileOptions,
    gc_enabled: bool,
    depth: PipelineDepth,
    state: PipelineState<'_>,
) -> CompileResult {
    run_pipeline_with_state_cancellable_and_names(
        source,
        PackageInputs {
            ranges: package_ranges,
            names: package_names,
        },
        options,
        gc_enabled,
        depth,
        state,
        &|| false,
    )
    .expect("non-cancellable pipeline cannot be cancelled")
}

fn convert_hir_diag(diagnostic: &hir::body::Diagnostic) -> type_checker::Diagnostic {
    type_checker::Diagnostic {
        code: diagnostic.code,
        severity: match diagnostic.severity {
            hir::body::Severity::Error => type_checker::Severity::Error,
            hir::body::Severity::Warning => type_checker::Severity::Warning,
            hir::body::Severity::Note => type_checker::Severity::Note,
            hir::body::Severity::Help => type_checker::Severity::Help,
        },
        message: diagnostic.message.clone(),
        labels: diagnostic
            .labels
            .iter()
            .map(|label| type_checker::SourceLabel {
                range: label.range,
                message: label.message.clone(),
                style: match label.style {
                    hir::body::LabelStyle::Primary => type_checker::LabelStyle::Primary,
                    hir::body::LabelStyle::Secondary => type_checker::LabelStyle::Secondary,
                },
            })
            .collect(),
        help: diagnostic.help.clone(),
        notes: diagnostic.notes.clone(),
    }
}

fn parse_errors_for_source(parse: &Parse, use_std: bool, user_source: &str) -> Vec<ParseError> {
    let mut errors = parse.errors.clone();
    if use_std
        && errors
            .iter()
            .any(|error| usize::from(error.span.end()) > user_source.len())
    {
        let mut parser = IncrementalParser::new();
        let user_errors = parser.set_source(user_source).errors.clone();
        if !user_errors.is_empty() {
            errors = user_errors;
        }
    }
    errors
}

fn collect_hir_diagnostics(
    hir: &hir::HirFile,
    scope_diagnostics: &[hir::body::Diagnostic],
) -> Vec<type_checker::Diagnostic> {
    hir.bodies
        .iter()
        .flat_map(|(_, body)| body.diagnostics.iter())
        .chain(scope_diagnostics)
        .map(convert_hir_diag)
        .collect()
}

fn pipeline_succeeded(
    parse_errors: &[ParseError],
    hir_diagnostics: &[type_checker::Diagnostic],
    type_result: &TypeCheckResult,
    analysis_diagnostics: &[type_checker::Diagnostic],
) -> bool {
    let has_error = |diagnostics: &[type_checker::Diagnostic]| {
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.severity == type_checker::Severity::Error)
    };
    parse_errors.is_empty()
        && !has_error(hir_diagnostics)
        && !has_error(&type_result.diagnostics)
        && !has_error(analysis_diagnostics)
}

fn resolve_result(
    hir: hir::HirFile,
    scope_graph: scope_graph::ScopeGraph,
    hir_diagnostics: Vec<type_checker::Diagnostic>,
    parse_errors: Vec<ParseError>,
) -> CompileResult {
    CompileResult {
        hir: Some(hir),
        scope_graph: Some(scope_graph),
        type_result: TypeCheckResult::default(),
        macro_diagnostics: Vec::new(),
        hir_diagnostics,
        analysis_diagnostics: Vec::new(),
        analysis: move_checker::AnalysisResult::default(),
        mir_module: None,
        parse_errors,
        source_files: Vec::new(),
    }
}

fn run_pipeline_with_state_cancellable(
    source: &str,
    package_ranges: &[Range<usize>],
    options: CompileOptions,
    gc_enabled: bool,
    depth: PipelineDepth,
    state: PipelineState<'_>,
    cancelled: &dyn Fn() -> bool,
) -> Option<CompileResult> {
    run_pipeline_with_state_cancellable_and_names(
        source,
        PackageInputs {
            ranges: package_ranges,
            names: &[],
        },
        options,
        gc_enabled,
        depth,
        state,
        cancelled,
    )
}

fn run_pipeline_with_state_cancellable_and_names(
    source: &str,
    package: PackageInputs<'_>,
    options: CompileOptions,
    gc_enabled: bool,
    depth: PipelineDepth,
    state: PipelineState<'_>,
    cancelled: &dyn Fn() -> bool,
) -> Option<CompileResult> {
    let PipelineState {
        preparsed,
        parser,
        incremental_type_checker,
    } = state;
    let mut timer = PhaseTimer::new(std::env::var_os("RIDDLEC_PHASE_TIMING").is_some());
    if cancelled() {
        return None;
    }
    let user_source = source;
    let owned_source = options
        .use_std
        .then(|| format!("{source}\n\n{}", std_prelude()));
    let source = owned_source.as_deref().unwrap_or(source);

    // 1. Parse. Process macro output already carries token kinds, so keep its
    // green tree and only parse the bundled standard library suffix here.
    let prepared_parse = preparsed.map(|user_parse| {
        if options.use_std {
            let mut std_parser = IncrementalParser::new();
            frontend::tree_builder::append_parse(
                user_parse,
                "\n\n",
                std_parser.set_source(std_prelude()),
            )
        } else {
            user_parse.clone()
        }
    });
    let parse = prepared_parse
        .as_ref()
        .unwrap_or_else(|| update_parse(parser, source));
    timer.phase("1 parse");
    if cancelled() {
        return None;
    }

    let parse_errors = parse_errors_for_source(parse, options.use_std, user_source);

    if !parse_errors.is_empty() {
        return Some(parse_failure(parse_errors));
    }

    // 2. Lower AST → HIR
    let syntax = parse.syntax();
    let root = ast::Root::cast(syntax.clone()).unwrap();
    let mut hir = lower_root(&root);
    timer.phase("2 lower HIR");
    if cancelled() {
        return None;
    }
    hir.package_ranges = package
        .ranges
        .iter()
        .map(|range| text_range(range.start, range.end))
        .collect();
    hir.std_loaded = options.use_std;

    // 3. Build scope graph + resolve names
    let (sg, scope_diagnostics) = build_scope_graph(&hir, &syntax);
    resolve_hir(&mut hir, &sg);
    timer.phase("3 scope graph");
    if cancelled() {
        return None;
    }

    // Collect HIR diagnostics (lowering, resolution, and scope-graph validation).
    let hir_diagnostics = collect_hir_diagnostics(&hir, &scope_diagnostics);

    if depth == PipelineDepth::Resolve {
        return Some(resolve_result(hir, sg, hir_diagnostics, parse_errors));
    }

    // 4. Type check
    let type_result = match incremental_type_checker {
        Some(checker) => {
            let checked = checker.check_with_syntax(&hir, &syntax);
            // Reuse is the whole point of the incremental checker: if the
            // standard library is being re-checked on every keystroke, the
            // counters say so.
            timer.phase_with_counts(
                "4a type check",
                checked.stats.checked_bodies,
                checked.stats.reused_bodies,
            );
            if timer.enabled() {
                eprintln!(
                    "[phase]    reuse detail: absent {} missed_context {} missed_body {} \
                     missed_replay {} globals_rebuilt {}",
                    checked.stats.missed_absent,
                    checked.stats.missed_context,
                    checked.stats.missed_body,
                    checked.stats.missed_replay,
                    checked.stats.rebuilt_globals,
                );
                // Which part of the context moved, when it did.
                let _ = type_checker::incremental::context_fingerprint_parts(&hir.item_tree);
            }
            checked.result
        }
        None => check_hir(&hir),
    };
    if cancelled() {
        return None;
    }

    if depth == PipelineDepth::Infer {
        return Some(CompileResult {
            hir: Some(hir),
            scope_graph: Some(sg),
            type_result,
            macro_diagnostics: Vec::new(),
            hir_diagnostics,
            analysis_diagnostics: Vec::new(),
            analysis: move_checker::AnalysisResult::default(),
            mir_module: None,
            parse_errors,
            source_files: Vec::new(),
        });
    }

    // 5. Escape analysis (determines which locals need heap allocation)
    let escape_result = escape_analysis::analyze_escapes(&hir, &type_result);
    timer.phase("5 escape analysis");
    if cancelled() {
        return None;
    }

    // 6. Move and borrow checking is independent of storage placement.
    let analysis = move_checker::analyze(&hir, &type_result);
    timer.phase("6 move/borrow");
    if cancelled() {
        return None;
    }
    let mut analysis_diagnostics = analysis.diagnostics.clone();
    if !gc_enabled {
        analysis_diagnostics.extend(escape_result.reference_escape_diagnostics(&hir));
    }

    // Only Error-severity diagnostics block compilation.
    // Notes (like E0200 heap promotion) and warnings are informational.
    let success = pipeline_succeeded(
        &parse_errors,
        &hir_diagnostics,
        &type_result,
        &analysis_diagnostics,
    );

    // 7. Lower HIR → MIR
    let mut mir_module = (success && depth == PipelineDepth::Build).then(|| {
        mir::lower_hir(
            &hir,
            source,
            &type_result,
            &escape_result,
            &analysis.moved_exprs,
            gc_enabled,
            package.names,
        )
    });
    if let Some(module) = &mir_module {
        analysis_diagnostics.extend(module.diagnostics.clone());
        if module
            .diagnostics
            .iter()
            .any(|d| d.severity == type_checker::Severity::Error)
        {
            mir_module = None;
        }
    }

    Some(CompileResult {
        hir: Some(hir),
        scope_graph: Some(sg),
        type_result,
        macro_diagnostics: Vec::new(),
        hir_diagnostics,
        analysis_diagnostics,
        analysis,
        mir_module,
        parse_errors,
        source_files: Vec::new(),
    })
}

fn update_parse<'a>(
    parser: &'a mut IncrementalParser,
    source: &str,
) -> &'a frontend::tree_builder::Parse {
    if parser.current_parse().is_none() {
        return parser.set_source(source);
    }
    if parser.source() == source {
        return parser.current_parse().expect("parse was initialized");
    }

    let (offset, delete_len, insert) = replacement(parser.source(), source);
    if parser.try_apply_edit(offset, delete_len, insert).is_err() {
        parser.set_source(source)
    } else {
        parser.current_parse().expect("edit updated the parse")
    }
}

fn replacement<'a>(old: &str, new: &'a str) -> (usize, usize, &'a str) {
    let mut prefix = 0;
    for (old_char, new_char) in old.chars().zip(new.chars()) {
        if old_char != new_char {
            break;
        }
        prefix += old_char.len_utf8();
    }

    let mut suffix = 0;
    for (old_char, new_char) in old[prefix..].chars().rev().zip(new[prefix..].chars().rev()) {
        if old_char != new_char {
            break;
        }
        suffix += old_char.len_utf8();
    }

    (
        prefix,
        old.len() - prefix - suffix,
        &new[prefix..new.len() - suffix],
    )
}

fn parse_failure(parse_errors: Vec<ParseError>) -> CompileResult {
    CompileResult {
        hir: None,
        scope_graph: None,
        type_result: TypeCheckResult::default(),
        macro_diagnostics: Vec::new(),
        hir_diagnostics: Vec::new(),
        analysis_diagnostics: Vec::new(),
        analysis: move_checker::AnalysisResult::default(),
        mir_module: None,
        parse_errors,
        source_files: Vec::new(),
    }
}

impl CompileResult {
    #[must_use]
    pub fn success(&self) -> bool {
        self.parse_errors.is_empty()
            && !self
                .macro_diagnostics
                .iter()
                .any(|d| d.severity == type_checker::Severity::Error)
            && !self
                .hir_diagnostics
                .iter()
                .any(|d| d.severity == type_checker::Severity::Error)
            && !self
                .type_result
                .diagnostics
                .iter()
                .any(|d| d.severity == type_checker::Severity::Error)
            && !self
                .analysis_diagnostics
                .iter()
                .any(|d| d.severity == type_checker::Severity::Error)
    }
}
