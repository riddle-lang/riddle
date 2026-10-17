use crate::diagnosis::print_diagnosis;
use riddle_syntax::parse;
use std::fs::read;
use std::path::PathBuf;

pub fn compile(inputs: Vec<PathBuf>) -> anyhow::Result<()> {
    let sources = inputs
        .iter()
        .map(|input| Ok::<_, anyhow::Error>(String::from_utf8(read(input)?)?))
        .collect::<anyhow::Result<Vec<String>>>()?;

    for (input, source) in inputs.iter().zip(&sources) {
        let parse = parse(source);
        print_diagnosis(input, source, &parse);
    }

    Ok(())
}
