use annotate_snippets::{AnnotationKind, Group, Level, Renderer, Snippet};
use riddle_syntax::{Parse, ParseError};
use std::path::Path;

pub fn print_diagnosis(path: &Path, source: &str, parse: &Parse) {
    if parse.ok() {
        return;
    }

    let file = path.display().to_string();
    let groups: Vec<Group> = parse
        .errors()
        .iter()
        .map(|err| render_error(&file, source, err))
        .collect();

    let renderer = Renderer::styled();
    eprintln!("{}", renderer.render(&groups));
}

/// Render a single error as a Group
fn render_error<'a>(file: &'a str, source: &'a str, error: &'a ParseError) -> Group<'a> {
    Group::with_title(Level::ERROR.primary_title(&error.message)).element(
        Snippet::source(source)
            .path(file)
            .annotation(AnnotationKind::Primary.span(error.range.clone())),
    )
}
