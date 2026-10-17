pub mod compile;
pub mod diagnosis;

use crate::compile::compile;
use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "riddlec", about = "Riddle complier")]
struct Args {
    #[arg(required = true)]
    inputs: Vec<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    compile(args.inputs)?;

    Ok(())
}
