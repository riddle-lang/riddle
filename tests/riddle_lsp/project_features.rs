use super::*;

#[test]
fn project_semantic_tokens_resolve_cross_module_functions() {
    let root = temp_root("project-semantic-tokens");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    let main = root.join("src/main.rid");
    let main_text = "mod util;\nfun main() { let callable = util::make; callable; }\n";
    fs::write(&main, main_text).unwrap();
    fs::write(
        root.join("src/util.rid"),
        "pub struct Thing {}\npub fun make() -> Thing { Thing {} }\n",
    )
    .unwrap();
    let uri = lsp_types::Url::from_file_path(&main).unwrap();
    let docs = HashMap::from([(uri.clone(), Document::new(main_text, Some(1)))]);
    let sessions = AnalysisSessions::default();

    let tokens =
        semantic_tokens_for_document(&uri, &docs, CompileOptions::default(), &sessions).unwrap();
    let make = position(main_text, main_text.find("make").unwrap());
    let make_line = make.line;
    let make_start = make.character;
    assert!(semantic_token_positions(&tokens).iter().any(|token| {
        token.line == make_line && token.start == make_start && token.token_type == TOKEN_FUNCTION
    }));
    let _ = fs::remove_dir_all(root);
}

#[test]
fn project_lsp_tracks_imported_proc_macros() {
    if !c_compiler_available() {
        eprintln!("skipping proc-macro LSP test: no C compiler found");
        return;
    }
    let root = temp_root("project-proc-macro-lsp");
    fs::create_dir_all(root.join("macros/src")).unwrap();
    fs::create_dir_all(root.join("app/src")).unwrap();
    fs::write(
        root.join("macros/Clue.toml"),
        r#"[package]
name = "macros"

[lib]
path = "src/lib.rid"
proc-macro = true

[dependencies]
"#,
    )
    .unwrap();
    let macro_source = r#"#[proc_macro_derive(Debug)]
pub fun derive_debug(input: TokenStream) -> TokenStream { TokenStream::new() }

#[proc_macro]
pub fun answer(input: TokenStream) -> TokenStream {
    TokenStream::from_str("1").unwrap_or(TokenStream::new())
}

#[proc_macro_attribute]
pub fun passthrough(args: TokenStream, item: TokenStream) -> TokenStream { item }
"#;
    let macro_path = root.join("macros/src/lib.rid");
    fs::write(&macro_path, macro_source).unwrap();
    fs::write(
        root.join("app/Clue.toml"),
        r#"[package]
name = "app"

[[bin]]
path = "src/main.rid"

[dependencies]
macros = { path = "../macros" }
"#,
    )
    .unwrap();
    let source = r"mod ordinary { pub fun plain() -> i32 { 2 } }
use {macros::Debug as Inspect, macros::answer, macros::passthrough, ordinary::plain};

#[derive(Inspect)]
struct Value {}

#[passthrough]
fun value() -> i32 { 2 }

fun main() -> i32 { answer!() + plain() }
";
    let main_path = root.join("app/src/main.rid");
    fs::write(&main_path, source).unwrap();
    let uri = lsp_types::Url::from_file_path(&main_path).unwrap();
    let docs = HashMap::from([(uri.clone(), Document::new(source, Some(3)))]);
    let sessions = AnalysisSessions::default();
    let options = CompileOptions { use_std: false };

    assert_imported_macro_tokens(&uri, &docs, source, options, &sessions);

    assert_imported_macro_navigation(
        &uri,
        &docs,
        source,
        &macro_path,
        macro_source,
        options,
        &sessions,
    );

    assert_imported_macro_rename(&uri, &docs, source, options, &sessions);

    assert_imported_macro_completion(&uri, source, options);

    drop(sessions);
    let _ = fs::remove_dir_all(root);
}

fn assert_imported_macro_tokens(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document>,
    source: &str,
    options: CompileOptions,
    sessions: &AnalysisSessions,
) {
    let tokens = semantic_tokens_for_document(uri, docs, options, sessions).unwrap();
    let positions = semantic_token_positions(&tokens);
    for offset in source
        .match_indices("Inspect")
        .chain(source.match_indices("answer"))
        .chain(source.match_indices("passthrough"))
        .map(|(offset, _)| offset)
    {
        let expected = position(source, offset);
        let expected_line = expected.line;
        let expected_start = expected.character;
        assert!(
            positions.iter().any(|token| {
                token.line == expected_line
                    && token.start == expected_start
                    && token.token_type == TOKEN_MACRO
            }),
            "missing macro token at {expected:?}: {positions:#?}"
        );
    }
}

fn assert_imported_macro_navigation(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document>,
    source: &str,
    macro_path: &std::path::Path,
    macro_source: &str,
    options: CompileOptions,
    sessions: &AnalysisSessions,
) {
    let inspect_use = source.rfind("Inspect").unwrap();
    let hover = hover_for_document(
        uri,
        docs,
        position(source, inspect_use + 2),
        options,
        sessions,
    )
    .unwrap()
    .unwrap();
    let HoverContents::Markup(contents) = hover.contents else {
        panic!("expected markup hover")
    };
    assert!(contents.value.contains("derive proc macro Inspect"));

    let definition = definition_for_document(
        uri,
        docs,
        position(source, inspect_use + 2),
        options,
        sessions,
    )
    .unwrap()
    .unwrap();
    let GotoDefinitionResponse::Scalar(definition) = definition else {
        panic!("expected one macro definition")
    };
    assert_eq!(
        definition.uri,
        lsp_types::Url::from_file_path(fs::canonicalize(macro_path).unwrap()).unwrap()
    );
    let function_name = macro_source.find("derive_debug").unwrap();
    assert_eq!(
        definition.range,
        Range::new(
            position(macro_source, function_name),
            position(macro_source, function_name + "derive_debug".len()),
        )
    );

    let plain_use = source.rfind("plain").unwrap();
    let plain_definition = definition_for_document(
        uri,
        docs,
        position(source, plain_use + 2),
        options,
        sessions,
    )
    .unwrap()
    .unwrap();
    let GotoDefinitionResponse::Scalar(plain_definition) = plain_definition else {
        panic!("expected one ordinary definition")
    };
    let plain_declaration = source.find("plain").unwrap();
    assert_eq!(
        plain_definition.range,
        Range::new(
            position(source, plain_declaration),
            position(source, plain_declaration + "plain".len()),
        )
    );
}

fn assert_imported_macro_rename(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document>,
    source: &str,
    options: CompileOptions,
    sessions: &AnalysisSessions,
) {
    let answer_use = source.rfind("answer").unwrap();
    let cursor = position(source, answer_use + 2);
    let references = references_for_document(uri, docs, cursor, true, options, sessions)
        .unwrap()
        .unwrap();
    assert_eq!(references.len(), 2, "{references:#?}");
    let prepared = prepare_rename_for_document(uri, docs, cursor, options, sessions)
        .unwrap()
        .unwrap();
    assert!(matches!(
        prepared,
        PrepareRenameResponse::RangeWithPlaceholder { placeholder, .. } if placeholder == "answer"
    ));
    let edit = rename_for_document(uri, docs, cursor, "respond", options, sessions)
        .unwrap()
        .unwrap();
    let Some(DocumentChanges::Edits(documents)) = edit.document_changes else {
        panic!("expected macro rename edits")
    };
    assert_eq!(documents.len(), 1);
    assert_eq!(documents[0].edits.len(), 2);
}

fn assert_imported_macro_completion(uri: &lsp_types::Url, source: &str, options: CompileOptions) {
    let incomplete = source
        .replace("#[derive(Inspect)]", "#[derive(Ins)]")
        .replace("answer!()", "ans");
    let docs = HashMap::from([(uri.clone(), Document::new(incomplete.clone(), Some(4)))]);
    let cursor = incomplete.find("Ins)]").unwrap() + "Ins".len();
    let items = completion_items_for_document(
        uri,
        &docs,
        position(&incomplete, cursor),
        options,
        &AnalysisSessions::default(),
        || false,
    )
    .unwrap()
    .unwrap();
    assert!(
        items.iter().any(|item| item.label == "Inspect"),
        "{items:#?}"
    );

    let cursor = incomplete.find("ans +").unwrap() + "ans".len();
    let items = completion_items_for_document(
        uri,
        &docs,
        position(&incomplete, cursor),
        options,
        &AnalysisSessions::default(),
        || false,
    )
    .unwrap()
    .unwrap();
    let answer = items
        .iter()
        .find(|item| {
            item.label == "answer!" && item.detail.as_deref() == Some("function-like proc macro")
        })
        .unwrap_or_else(|| panic!("{items:#?}"));
    assert_eq!(answer.insert_text.as_deref(), Some("answer!"));
    assert_eq!(answer.filter_text.as_deref(), Some("answer"));
}

#[test]
fn project_lsp_tracks_standard_print_macros() {
    let root = temp_root("project-standard-macro-lsp");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        r#"[package]
name = "app"

[[bin]]
path = "src/main.rid"

[dependencies]
"#,
    )
    .unwrap();
    let source = r#"#[derive(Debug)]
struct Value { number: i32 }

fun main() -> i32 {
    print!("{}", 1);
    println!("value={:?}", Value { number: 1 });
    0
}
"#;
    let main_path = root.join("src/main.rid");
    fs::write(&main_path, source).unwrap();
    let uri = lsp_types::Url::from_file_path(&main_path).unwrap();
    let docs = HashMap::from([(uri.clone(), Document::new(source, Some(1)))]);
    let sessions = AnalysisSessions::default();

    let tokens =
        semantic_tokens_for_document(&uri, &docs, CompileOptions::default(), &sessions).unwrap();
    let expected = position(source, source.find("println").unwrap());
    let expected_line = expected.line;
    let expected_start = expected.character;
    let positions = semantic_token_positions(&tokens);
    assert!(
        positions.iter().any(|token| {
            token.line == expected_line
                && token.start == expected_start
                && token.token_type == TOKEN_MACRO
        }),
        "missing standard macro token at {expected:?}: {positions:#?}"
    );

    let derive = position(source, source.find("Debug").unwrap());
    let derive_line = derive.line;
    let derive_start = derive.character;
    assert!(
        positions.iter().any(|token| {
            token.line == derive_line
                && token.start == derive_start
                && token.token_type == TOKEN_MACRO
        }),
        "missing standard derive token at {derive:?}: {positions:#?}"
    );

    let hover = hover_for_document(
        &uri,
        &docs,
        position(source, source.find("println").unwrap() + 2),
        CompileOptions::default(),
        &sessions,
    )
    .unwrap()
    .unwrap();
    let HoverContents::Markup(contents) = hover.contents else {
        panic!("expected markup hover")
    };
    assert!(contents.value.contains("standard macro println!(...)"));

    let hover = hover_for_document(
        &uri,
        &docs,
        position(source, source.find("Debug").unwrap() + 2),
        CompileOptions::default(),
        &sessions,
    )
    .unwrap()
    .unwrap();
    let HoverContents::Markup(contents) = hover.contents else {
        panic!("expected markup hover")
    };
    assert!(contents.value.contains("standard derive macro Debug"));

    let hints = inlay_hints_for_document(
        &uri,
        &docs,
        Range::new(Position::new(0, 0), position(source, source.len())),
        CompileOptions::default(),
        &sessions,
    )
    .unwrap();
    assert!(hints.is_empty(), "{hints:#?}");
    let _ = fs::remove_dir_all(root);
}

#[test]
fn project_inlay_hints_infer_cross_module_return_types() {
    let root = temp_root("project-inlay-hints");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    let main = root.join("src/main.rid");
    let main_text = "mod util;\nfun main() { let value = util::make(); }\n";
    fs::write(&main, main_text).unwrap();
    fs::write(
        root.join("src/util.rid"),
        "pub struct Thing {}\npub fun make() -> Thing { Thing {} }\n",
    )
    .unwrap();
    let uri = lsp_types::Url::from_file_path(&main).unwrap();
    let docs = HashMap::from([(uri.clone(), Document::new(main_text, Some(1)))]);
    let sessions = AnalysisSessions::default();

    let hints = inlay_hints_for_document(
        &uri,
        &docs,
        Range::new(Position::new(0, 0), Position::new(2, 0)),
        CompileOptions::default(),
        &sessions,
    )
    .unwrap();

    assert!(
        hints
            .iter()
            .any(|hint| matches!(&hint.label, InlayHintLabel::String(label) if label == ": Thing"))
    );
    let _ = fs::remove_dir_all(root);
}
