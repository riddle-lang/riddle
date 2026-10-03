use super::{
    AnalysisSessions, CompileOptions, Document, Position, RequestRevisions,
    completion_items_for_document, completion_items_for_source,
};
use std::collections::HashMap;
use std::time::{Duration, Instant};

/// Reproduces what the server does for one `textDocument/completion` request.
///
/// A fresh `AnalysisSessions` per call, because the deadlock this guards
/// against needs two analyses in one session: the marker analysis holds the
/// session lock, and the fallback analysis used to lock the same session again.
fn serve_completion(text: &str, line: u32, character: u32) -> Duration {
    let uri = lsp_types::Url::parse("file:///completion-hang.rid").unwrap();
    let docs = HashMap::from([(uri.clone(), Document::new(text, Some(1)))]);
    let sessions = AnalysisSessions::default();
    let revisions = RequestRevisions::default();
    let request_revision = revisions.begin(&uri);

    let started = Instant::now();
    let result = completion_items_for_document(
        &uri,
        &docs,
        Position::new(line, character),
        CompileOptions::default(),
        &sessions,
        || !revisions.is_current(&uri, request_revision),
    );
    let elapsed = started.elapsed();
    let _ = result;
    elapsed
}

// A completion request must always come back. Reported from an editor: with a
// buffer that is not yet a parseable program — an empty file, or a single
// identifier — the language server never answered `textDocument/completion`,
// so the editor sat waiting on the request and then stopped answering anything.
//
// The cause was a re-entrant lock: the completion analysed the marked source
// while holding the session, and the fallback analysis — reached whenever the
// marker was not resolved as a scope reference, which is the normal case for a
// barely-typed buffer — locked that same session again.
#[test]
fn completion_on_an_unparseable_buffer_terminates() {
    for (source, line, character) in [
        ("", 0, 0),
        ("f", 0, 1),
        ("fun main() {\n    p\n", 1, 5),
        ("let x = ", 0, 8),
        ("fun main() {", 0, 12),
    ] {
        let elapsed = serve_completion(source, line, character);
        assert!(
            elapsed < Duration::from_secs(20),
            "completion on {source:?} took {elapsed:?}"
        );
    }
}

#[test]
fn the_plain_source_helper_matches() {
    // Keeps the simpler entry point covered too: it must not be the slow one.
    for source in ["", "f"] {
        let started = Instant::now();
        let _ = completion_items_for_source(source, Position::new(0, 0), CompileOptions::default());
        assert!(started.elapsed() < Duration::from_secs(20), "{source:?}");
    }
}
