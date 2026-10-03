//! Editor-latency benchmarks for `riddle-lsp`.
//!
//! A completion request is the most latency-sensitive thing the server does,
//! and it is the one that analyses the whole project. These benchmarks build a
//! real Clue project on disk and then measure what one keystroke costs, split
//! into the parts the server's `--trace-latency` phases report:
//!
//! - `completion_keystroke`: the whole `completion_items_for_document` call for
//!   a request arriving after a one-character edit.
//! - `completion_steady_state`: the same call with no edit in between, which is
//!   what a client that re-requests without typing pays.
//! - `diagnostics_keystroke`: the `Check`-depth pass a change schedules.
//!
//! Run with `cargo bench --bench lsp_completion`. The numbers are only
//! meaningful for a release build; a debug run is dominated by the
//! compiler's own unoptimised code.
//!
//! `TEMP` must point somewhere writable: the benchmark creates a project there.

use std::{
    collections::HashMap,
    env, fs,
    path::{Path, PathBuf},
    process,
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};

use lsp_types::{Position, Url};
use riddle_lsp::test_support::{
    AnalysisSessions, DiagnosticSessions, Document, collect_workspace_diagnostics_with_sessions,
    completion_items_for_document,
};
use riddlec::pipeline::CompileOptions;

/// How many warm requests to average over.
const SAMPLES: usize = 30;

fn module_source(name: &str) -> String {
    let mut source = String::new();
    // Enough items that resolution has real work to do in every module.
    for item in 0..12 {
        source.push_str(&format!(
            "pub struct {name}Item{item} {{ value: i32, label: str }}\n\
             impl {name}Item{item} {{\n\
             \x20   pub fun get(&self) -> i32 {{ self.value }}\n\
             \x20   pub fun label(&self) -> &str {{ self.label }}\n\
             }}\n\
             pub fun {name}_make_{item}(value: i32) -> {name}Item{item} {{\n\
             \x20   {name}Item{item} {{ value: value, label: \"item\" }}\n\
             }}\n\n"
        ));
    }
    source
}

struct Project {
    root: PathBuf,
    main: PathBuf,
    main_source: String,
}

impl Project {
    /// Builds a project with `modules` modules of ~36 items each.
    fn create(modules: usize) -> Self {
        let root = env::temp_dir().join(format!(
            "riddle-lsp-bench-{}-{}",
            process::id(),
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map_or(0, |elapsed| elapsed.as_nanos())
        ));
        fs::create_dir_all(root.join("src")).expect("create project");
        fs::write(
            root.join("Clue.toml"),
            "[package]\nname = \"bench\"\nversion = \"0.1.0\"\n\n[dependencies]\n",
        )
        .expect("write manifest");

        let mut declaration = String::new();
        let mut main = String::new();
        for index in 0..modules {
            let name = format!("mod{index}");
            fs::write(
                root.join("src").join(format!("{name}.rid")),
                module_source(&name),
            )
            .expect("write module");
            declaration.push_str(&format!("mod {name};\n"));
        }
        main.push_str(&declaration);
        main.push_str("use std::string::String;\nfun main() {\n    let text = String::new();\n    let len = text.\n}\n");
        let main_path = root.join("src/main.rid");
        fs::write(&main_path, &main).expect("write main");
        Self {
            root,
            main: main_path,
            main_source: main,
        }
    }

    fn docs(&self) -> HashMap<Url, Document> {
        let uri = Url::from_file_path(&self.main).expect("main uri");
        HashMap::from([(uri, Document::new(self.main_source.clone(), Some(1)))])
    }
}

impl Drop for Project {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.root);
    }
}

fn completion_position(source: &str) -> Position {
    let offset = source.find("text.\n").expect("completion site") + "text.".len();
    let prefix = &source[..offset];
    let line = prefix.bytes().filter(|byte| *byte == b'\n').count();
    let line_start = prefix.rfind('\n').map_or(0, |newline| newline + 1);
    Position::new(
        u32::try_from(line).expect("line fits"),
        u32::try_from(source[line_start..offset].encode_utf16().count()).expect("column fits"),
    )
}

/// Median and worst of `runs` timed executions of `body`.
fn timed(runs: usize, mut body: impl FnMut()) -> (Duration, Duration) {
    let mut samples = Vec::with_capacity(runs);
    for _ in 0..runs {
        let started = Instant::now();
        body();
        samples.push(started.elapsed());
    }
    samples.sort_unstable();
    (
        samples[samples.len() / 2],
        *samples.last().expect("at least one sample"),
    )
}

fn report(label: &str, median: Duration, worst: Duration) {
    println!(
        "{label:<44} median {:>8.2} ms   worst {:>8.2} ms",
        median.as_secs_f64() * 1000.0,
        worst.as_secs_f64() * 1000.0
    );
}

fn bench_module_count(modules: usize) {
    let project = Project::create(modules);
    let position = completion_position(&project.main_source);
    let docs = project.docs();
    let uri = Url::from_file_path(&project.main).expect("main uri");
    let options = CompileOptions::default();
    let lines = project.main_source.lines().count();

    println!(
        "\n=== {modules} modules, {} lines in main, project root {} ===",
        lines,
        project.root.display()
    );

    // Cold: the first request has to load and analyse the project from scratch.
    let sessions = AnalysisSessions::default();
    let started = Instant::now();
    let cold = completion_items_for_document(&uri, &docs, position, options, &sessions, || false)
        .expect("completion succeeds");
    report(
        "cold (first request after startup)",
        started.elapsed(),
        started.elapsed(),
    );
    println!(
        "{:<44} {} items",
        "  items returned",
        cold.map_or(0, |items| items.len())
    );

    // Warm: repeated requests with an unchanged buffer. Any cost here is pure
    // overhead, because the client only re-requests when something changed.
    let (median, worst) = timed(SAMPLES, || {
        completion_items_for_document(&uri, &docs, position, options, &sessions, || false)
            .expect("completion succeeds");
    });
    report("warm (no edit between requests)", median, worst);

    // Keystroke: each request arrives after a one-character edit, which is what
    // typing actually does. The marker source changes every time.
    let (median, worst) = timed(SAMPLES, || {
        // Appending a character after the dot and then requesting completion at
        // the new offset mirrors "user types, client asks".
        let mut text = project.main_source.clone();
        let offset = text.find("text.\n").expect("site") + "text.".len();
        text.insert(offset, 'g');
        let typed_position = Position::new(position.line, position.character + 1);
        let mut docs = docs.clone();
        docs.insert(uri.clone(), Document::new(text, Some(2)));
        completion_items_for_document(&uri, &docs, typed_position, options, &sessions, || false)
            .expect("completion succeeds");
    });
    report("keystroke (one-char edit per request)", median, worst);

    // The diagnostics pass the same change schedules, at `Check` depth.
    let mut diagnostic_sessions = DiagnosticSessions::default();
    let (median, worst) = timed(SAMPLES, || {
        collect_workspace_diagnostics_with_sessions(&docs, options, &mut diagnostic_sessions);
    });
    report("diagnostics (Check depth, per change)", median, worst);

    // Per-phase breakdown of the request that actually blocks typing. The
    // phases print to stderr through `Phase`, so they only appear with
    // `RIDDLE_LSP_TRACE_LATENCY=true`.
    println!("  --- phase breakdown of one member completion ---");
    for _ in 0..3 {
        let _ = completion_items_for_document(&uri, &docs, position, options, &sessions, || false)
            .expect("completion succeeds");
    }

    let _ = Path::new(&project.root);
}

fn main() {
    if env::var_os("RIDDLE_LSP_TRACE_LATENCY").is_some_and(|value| value == "true") {
        riddle_lsp::set_latency_tracing(true);
    }
    println!(
        "riddle-lsp completion latency ({} samples per figure)",
        SAMPLES
    );
    for modules in [1usize, 8, 24] {
        bench_module_count(modules);
        bench_index(modules);
    }
}

/// The `Infer`-depth pass the workspace index builds, which the server also
/// schedules whenever it decides an index is stale.
fn bench_index(modules: usize) {
    let project = Project::create(modules);
    let docs = project.docs();
    let overlays = docs
        .iter()
        .filter_map(|(uri, document)| {
            uri.to_file_path()
                .ok()
                .map(|path| (path, document.text.clone()))
        })
        .collect::<HashMap<_, _>>();
    let options = CompileOptions::default();
    let sessions = AnalysisSessions::default();
    let mut samples = Vec::with_capacity(SAMPLES);
    for _ in 0..SAMPLES {
        let started = Instant::now();
        let _ = riddle_lsp::test_support::project_index_for_root(
            &project.root,
            &overlays,
            options,
            &sessions,
        );
        samples.push(started.elapsed());
    }
    samples.sort_unstable();
    report(
        "workspace index (Infer depth)",
        samples[samples.len() / 2],
        *samples.last().expect("sample"),
    );
}
