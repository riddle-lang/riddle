use clap::{ArgAction, Parser};
use std::env;
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use riddlec::{diagnostics, pipeline, target::TargetTriple};

#[derive(Clone, Copy, Debug, clap::ValueEnum)]
enum BackendKind {
    C,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, clap::ValueEnum)]
enum EmitKind {
    /// Generated C source (default with `--backend c`).
    C,
    /// Pretty-printed MIR of the whole program.
    Mir,
}

#[derive(Debug, Parser)]
#[command(
    name = "riddlec",
    about = "The Riddle compiler (frontend)",
    disable_version_flag = true
)]
struct Opts {
    /// Print pass status for each file.
    #[arg(short, long)]
    verbose: bool,

    /// Compile without the bundled standard library.
    #[arg(long = "no-std", action = ArgAction::SetFalse, default_value_t = true)]
    use_std: bool,

    /// Generate code for the C backend (the only available backend).
    #[arg(short, long, value_enum)]
    backend: Option<BackendKind>,

    /// What to emit: `c` (generated C) or `mir` (pretty-printed MIR).
    #[arg(long, value_enum, conflicts_with = "backend")]
    emit: Option<EmitKind>,

    /// Select the target platform triple, which also sets the width `usize` and
    /// `isize` are checked at.
    #[arg(long, value_name = "TRIPLE")]
    target: Option<TargetTriple>,

    /// Write generated code to a file.
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Print the version and git commit hash.
    #[arg(short = 'V', long)]
    version: bool,

    files: Vec<PathBuf>,
}

fn main() {
    // The whole pipeline (parser, HIR lowering, move checking, MIR lowering,
    // codegen) recurses per nesting level of the input; run on a large stack
    // so legitimate deep inputs compile and pathological ones hit the
    // parser's nesting diagnostic instead of a stack overflow.
    let worker = std::thread::Builder::new()
        .stack_size(256 * 1024 * 1024)
        .spawn(run)
        .expect("spawn compiler worker thread");
    let code = worker.join().unwrap_or(1);
    process::exit(code);
}

fn run() -> i32 {
    let opts = match parse_args(env::args_os()) {
        Ok(opts) => opts,
        Err(msg) => {
            let exit_code = msg.exit_code();
            let _ = msg.print();
            return exit_code;
        }
    };

    if opts.version {
        println!(
            "riddlec {} ({})",
            env!("CARGO_PKG_VERSION"),
            riddlec::GIT_HASH
        );
        return 0;
    }

    if opts.files.is_empty() {
        eprintln!("riddlec: no input files");
        return 1;
    }
    let target = match selected_target(opts.target) {
        Ok(target) => target,
        Err(error) => {
            eprintln!("riddlec: {error}");
            return 1;
        }
    };

    // Every input is one program: files are concatenated into a single
    // package so they can reference each other's items, with source maps
    // preserved for panic locations. This holds with and without a backend —
    // checking and codegen must see the same program.
    let errors = compile_program(&opts.files, &opts, target);
    if errors > 0 {
        return 1;
    }
    0
}

/// A macro-expansion diagnostic report for one input file.
///
/// The diagnostics carry spans in the file's pre-expansion coordinates, so
/// they are reported against a snapshot of the loaded source taken before
/// expansion rewrote it.
struct MacroReport {
    loaded: pipeline::LoadedSource,
    diagnostics: Vec<riddlec::pipeline::Diagnostic>,
    name: String,
}

/// Loads and macro-expands every input file, merging them into one package.
///
/// Returns the combined source plus per-file source maps so diagnostics and
/// generated panic locations keep pointing at the original files, along with
/// any macro-expansion diagnostics per file.
fn load_program_sources(
    files: &[PathBuf],
) -> Result<(pipeline::LoadedSource, Vec<MacroReport>), PathBuf> {
    let mut combined = String::new();
    let mut files_loaded = Vec::new();
    let mut source_map = pipeline::SourceMap::default();
    let mut macro_reports = Vec::new();
    for file in files {
        let mut loaded = pipeline::load_source_file(file).map_err(|error| {
            eprintln!("riddlec: cannot read `{}`: {error}", file.display());
            file.clone()
        })?;
        let expansion = riddlec::proc_macro::expand_standard_macros(&loaded.source);
        if !expansion.diagnostics.is_empty() {
            macro_reports.push(MacroReport {
                loaded: loaded.clone(),
                diagnostics: expansion.diagnostics,
                name: file.display().to_string(),
            });
        }
        loaded.apply_expansion(expansion.source, &expansion.mappings);
        if !combined.is_empty() {
            combined.push('\n');
        }
        let offset = combined.len();
        combined.push_str(&loaded.source);
        source_map.extend(loaded.source_map, offset);
        files_loaded.extend(loaded.files);
    }
    Ok((
        pipeline::LoadedSource {
            source: combined,
            files: files_loaded,
            source_map,
        },
        macro_reports,
    ))
}

/// Prints per-file macro-expansion diagnostics and returns the error count.
///
/// Errors abort the program build: an unexpanded macro call lowers to a
/// missing expression, and compiling past it would silently drop the call
/// (and every not-yet-expanded macro after it) from the program.
fn report_macro_reports(reports: &[MacroReport]) -> usize {
    let mut errors = 0;
    for report in reports {
        let result = pipeline::CompileResult {
            macro_diagnostics: report.diagnostics.clone(),
            ..pipeline::CompileResult::default()
        };
        errors += diagnostics::report_mapped(&result, &report.loaded, &report.name);
    }
    errors
}

fn compile_program(files: &[PathBuf], opts: &Opts, target: TargetTriple) -> usize {
    let (loaded, macro_reports) = match load_program_sources(files) {
        Ok(loaded) => loaded,
        Err(_) => return 1,
    };
    let macro_errors = report_macro_reports(&macro_reports);
    if macro_errors > 0 {
        return macro_errors;
    }
    let options = pipeline::CompileOptions {
        use_std: opts.use_std,
        pointer_width_bits: target.pointer_width_bits(),
    };
    let package_range = 0..loaded.source.len();
    let package_ranges = std::slice::from_ref(&package_range);
    // Both C codegen and MIR emission need the lowered module; plain check
    // stops before MIR.
    let needs_mir = opts.backend.is_some() || opts.emit.is_some();
    let result = if needs_mir {
        pipeline::compile_package_with_options(&loaded.source, package_ranges, options)
    } else {
        pipeline::check_package_with_options(&loaded.source, package_ranges, options)
    };

    let entry_name = files.first().map_or_else(
        || "<unknown>".to_string(),
        |file| file.display().to_string(),
    );
    if opts.verbose {
        println!("target: {target}");
        diagnostics::report_verbose(&result, Some(&loaded.source), &entry_name);
        println!();
    }

    let mut errors = diagnostics::report_mapped(&result, &loaded, &entry_name);
    if result.success()
        && let Some(ref module) = result.mir_module
    {
        if opts.emit == Some(EmitKind::Mir) {
            print!("{module}");
            return errors;
        }
        // `--emit c` selects the same codegen as `--backend c`; the two flags
        // conflict, so the backend option alone is never set here.
        if opts.backend.is_some() || opts.emit == Some(EmitKind::C) {
            // A program without `main` fails only at C link time with an opaque
            // `WinMain` error; report it here instead.
            if !module
                .functions
                .values()
                .any(|function| function.name == "main")
            {
                eprintln!(
                    "error[E0401]: no `main` function found in the entry package
  = help: define `fun main() -> i32 {{ ... }}` as the program entry"
                );
                errors += 1;
            }
            match pipeline::generate_c_for_package_with_source_map(
                module,
                0,
                true,
                &loaded.source_map,
                &entry_name,
            ) {
                Ok(code) => errors += write_c(&code, opts.output.as_deref(), files),
                Err(error) => {
                    eprintln!("riddlec: code generation error: {error:?}");
                    errors += 1;
                }
            }
        }
    }
    errors
}

fn parse_args<I, T>(args: I) -> Result<Opts, clap::Error>
where
    I: IntoIterator<Item = T>,
    T: Into<OsString> + Clone,
{
    Opts::try_parse_from(args)
}

fn selected_target(explicit: Option<TargetTriple>) -> Result<TargetTriple, String> {
    if let Some(target) = explicit {
        return Ok(target);
    }
    if let Some(target) = env::var_os("RIDDLE_TARGET") {
        return target
            .to_string_lossy()
            .parse()
            .map_err(|error| format!("invalid RIDDLE_TARGET: {error}"));
    }
    TargetTriple::host().map_err(|error| error.to_string())
}

/// Write generated C code to a `.c` source file.
fn write_c(c_code: &str, output: Option<&Path>, input_files: &[PathBuf]) -> usize {
    let c_path = match output {
        Some(path) if path.extension().is_some_and(|ext| ext == "c") => path.to_path_buf(),
        Some(path) => append_c_suffix(path),
        None => input_files
            .first()
            .and_then(|f| f.file_stem())
            .filter(|stem| !stem.is_empty())
            .map_or_else(
                || PathBuf::from("riddle_out.c"),
                |stem| {
                    let mut output = stem.to_os_string();
                    output.push(".c");
                    PathBuf::from(output)
                },
            ),
    };

    if let Err(e) = fs::write(&c_path, c_code) {
        eprintln!("riddlec: cannot write to `{}`: {e}", c_path.display());
        1
    } else {
        0
    }
}

fn append_c_suffix(path: &Path) -> PathBuf {
    let mut output = path.as_os_str().to_os_string();
    output.push(".c");
    PathBuf::from(output)
}
