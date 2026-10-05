use std::time::Duration;

use clap::Parser;
use riddlec::pipeline::CompileOptions;

pub struct Options {
    pub compile_options: CompileOptions,
    pub completion_delay: Duration,
    pub trace_latency: bool,
}

#[derive(Parser)]
#[command(
    name = "riddle-lsp",
    version = format!("{} ({})", env!("CARGO_PKG_VERSION"), riddlec::GIT_HASH)
)]
struct CliArgs {
    #[arg(long = "no-std", help = "Disable standard library loading")]
    no_std: bool,
    #[arg(
        long = "completion-delay-ms",
        value_name = "MS",
        default_value_t = 40,
        help = "Coalesce completion requests arriving within this window"
    )]
    completion_delay_ms: u64,
    #[arg(
        long = "trace-latency",
        env = "RIDDLE_LSP_TRACE_LATENCY",
        help = "Print per-phase latency (formatting, folding, analysis) to stderr"
    )]
    trace_latency: bool,
}

/// Parses command-line options for the language server.
///
/// # Errors
///
/// Returns an error when an argument is invalid or a required value is missing.
pub fn parse_args(args: &[String]) -> Result<Options, clap::Error> {
    let args = CliArgs::try_parse_from(args.iter().map(String::as_str))?;
    Ok(Options {
        compile_options: CompileOptions {
            use_std: !args.no_std,
            ..Default::default()
        },
        completion_delay: Duration::from_millis(args.completion_delay_ms),
        trace_latency: args.trace_latency,
    })
}
