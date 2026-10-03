use super::*;

#[test]
fn full_text_uses_latest_full_sync_change() {
    let old = TextDocumentContentChangeEvent {
        range: None,
        range_length: None,
        text: "old".into(),
    };
    let new = TextDocumentContentChangeEvent {
        range: None,
        range_length: None,
        text: "new".into(),
    };

    let mut text = "initial".to_string();
    assert!(apply_content_changes(&mut text, vec![old, new]).is_ok());
    assert_eq!(text, "new");
}

#[test]
fn incremental_changes_apply_sequentially_with_utf16_ranges() {
    let mut text = "a😀c\nlast".to_string();
    let changes = vec![
        TextDocumentContentChangeEvent {
            range: Some(Range::new(Position::new(0, 1), Position::new(0, 3))),
            range_length: Some(2),
            text: "x".into(),
        },
        TextDocumentContentChangeEvent {
            range: Some(Range::new(Position::new(1, 0), Position::new(1, 4))),
            range_length: Some(4),
            text: "done".into(),
        },
    ];

    assert!(apply_content_changes(&mut text, changes).is_ok());
    assert_eq!(text, "axc\ndone");
}

/// A change that splits a surrogate pair or points past the end of a line must
/// be clamped, not rejected: rejecting it used to drop the document, which the
/// next publish then reported to the client as "no diagnostics".
#[test]
fn unmappable_incremental_changes_are_clamped_rather_than_refused() {
    // A column inside the emoji's surrogate pair rounds down to its start.
    let mut text = "a😀c".to_string();
    let inside_pair = TextDocumentContentChangeEvent {
        range: Some(Range::new(Position::new(0, 2), Position::new(0, 3))),
        range_length: Some(1),
        text: "b".into(),
    };
    assert!(apply_content_changes(&mut text, vec![inside_pair]).is_ok());
    // The clamped range covered exactly the emoji, so replacing it is total.
    assert_eq!(text, "abc");

    // A column past the end of the line clamps to the line end.
    let mut text = "short\nsecond".to_string();
    let past_end = TextDocumentContentChangeEvent {
        range: Some(Range::new(Position::new(0, 0), Position::new(0, 999))),
        range_length: Some(999),
        text: "long".into(),
    };
    assert!(apply_content_changes(&mut text, vec![past_end]).is_ok());
    assert_eq!(text, "long\nsecond");
}

/// Only a line the document does not have is a hard failure, and it must not
/// leave the buffer half-updated.
#[test]
fn a_change_beyond_the_document_is_rejected_without_touching_the_text() {
    let mut text = "fun main() {}".to_string();
    let beyond = TextDocumentContentChangeEvent {
        range: Some(Range::new(Position::new(9, 0), Position::new(9, 1))),
        range_length: Some(1),
        text: "x".into(),
    };
    assert_eq!(
        apply_content_changes(&mut text, vec![beyond]),
        Err(ApplyError::LineOutOfRange { line: 9 })
    );
    assert_eq!(text, "fun main() {}");
}

/// Clamping is what makes the "can never desynchronise" claim true; an
/// unmappable *line* is still reported so the server can say so out loud.
#[test]
fn clamped_offsets_never_fail_inside_the_document() {
    let source = "中文😀\nnext";
    assert_eq!(
        offset_for_position_clamped(source, Position::new(0, 999)),
        Some("中文😀".len())
    );
    assert_eq!(
        offset_for_position_clamped(source, Position::new(0, 3)),
        Some("中文".len())
    );
    assert_eq!(
        offset_for_position_clamped(source, Position::new(7, 0)),
        None
    );
}

#[test]
fn completion_revisions_are_isolated_per_document_and_removable() {
    let revisions = RequestRevisions::default();
    let first = lsp_types::Url::parse("file:///first.rid").unwrap();
    let second = lsp_types::Url::parse("file:///second.rid").unwrap();

    let first_old = revisions.begin(&first);
    let second_current = revisions.begin(&second);
    let first_current = revisions.begin(&first);

    assert!(!revisions.is_current(&first, first_old));
    assert!(revisions.is_current(&first, first_current));
    assert!(revisions.is_current(&second, second_current));
    revisions.remove(&first);
    assert!(!revisions.is_current(&first, first_current));
}

#[test]
fn analysis_snapshots_are_isolated_per_project() {
    let first_root = temp_root("analysis-project-first");
    let second_root = temp_root("analysis-project-second");
    for root in [&first_root, &second_root] {
        fs::create_dir_all(root.join("src")).unwrap();
        fs::write(root.join("Clue.toml"), "[package]\nname = \"app\"\n").unwrap();
    }
    let first_main = lsp_types::Url::from_file_path(first_root.join("src/main.rid")).unwrap();
    let first_util = lsp_types::Url::from_file_path(first_root.join("src/util.rid")).unwrap();
    let second_main = lsp_types::Url::from_file_path(second_root.join("src/main.rid")).unwrap();
    let document = || Document::new("fun main() {}\n", Some(1));
    let docs = HashMap::from([
        (first_main.clone(), document()),
        (first_util.clone(), document()),
        (second_main.clone(), document()),
    ]);

    let snapshot = documents_for_uri(&docs, &first_main);
    assert_eq!(snapshot.len(), 2);
    assert!(snapshot.contains_key(&first_main));
    assert!(snapshot.contains_key(&first_util));
    assert!(!snapshot.contains_key(&second_main));

    fs::remove_dir_all(first_root).unwrap();
    fs::remove_dir_all(second_root).unwrap();
}

#[test]
fn semantic_token_delta_replaces_only_the_changed_middle() {
    let token = |delta_start, token_type| SemanticToken {
        delta_line: 0,
        delta_start,
        length: 1,
        token_type,
        token_modifiers_bitset: 0,
    };
    let previous = vec![
        token(0, TOKEN_KEYWORD),
        token(2, TOKEN_VARIABLE),
        token(2, TOKEN_TYPE),
    ];
    let current = vec![
        token(0, TOKEN_KEYWORD),
        token(2, TOKEN_FUNCTION),
        token(2, TOKEN_TYPE),
    ];

    let delta = semantic_token_delta(&previous, &current, "2".into());

    assert_eq!(delta.result_id.as_deref(), Some("2"));
    assert_eq!(delta.edits.len(), 1);
    assert_eq!(delta.edits[0].start, 5);
    assert_eq!(delta.edits[0].delete_count, 5);
    assert_eq!(delta.edits[0].data.as_deref(), Some(&current[1..2]));
}
