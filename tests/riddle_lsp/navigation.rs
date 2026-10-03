use super::*;

#[test]
fn type_definition_of_inferred_local_targets_nominal_declaration() {
    let source = "struct Point {}\nfun main() { let value = Point {}; value; }\n";
    let definition = type_definition_for_source(
        source,
        position(source, source.rfind("value").unwrap()),
        CompileOptions { use_std: false },
    )
    .unwrap();
    let GotoDefinitionResponse::Scalar(location) = definition else {
        panic!("expected one type definition")
    };

    assert_eq!(location.range.start, Position::new(0, 7));
}

#[test]
fn primitive_has_no_type_definition() {
    let source = "fun main() { let value = 1; value; }\n";
    assert!(
        type_definition_for_source(
            source,
            position(source, source.rfind("value").unwrap()),
            CompileOptions { use_std: false },
        )
        .is_none()
    );
}

#[test]
fn hover_shows_resolved_function_and_local_types() {
    let source = "fun add(value: i32) -> i32 { value } fun main() { let answer = add(1); answer; }";
    let function_hover = hover_for_source(
        source,
        position(source, source.rfind("add(1)").unwrap() + 1),
        CompileOptions { use_std: false },
    )
    .unwrap();
    let HoverContents::Markup(function_contents) = function_hover.contents else {
        panic!("expected markup hover")
    };
    assert!(
        function_contents
            .value
            .contains("fun add(value: i32) -> i32")
    );

    let local_hover = hover_for_source(
        source,
        position(source, source.rfind("answer;").unwrap() + 1),
        CompileOptions { use_std: false },
    )
    .unwrap();
    let HoverContents::Markup(local_contents) = local_hover.contents else {
        panic!("expected markup hover")
    };
    assert!(local_contents.value.contains("let answer: i32"));
}

#[test]
fn hover_includes_function_documentation() {
    let source = "/// Adds two numbers.\nfun add(left: i32, right: i32) -> i32 { left + right }\nfun main() { add(1, 2); }";
    let hover = hover_for_source(
        source,
        position(source, source.rfind("add(1, 2)").unwrap() + 1),
        CompileOptions { use_std: false },
    )
    .unwrap();
    let HoverContents::Markup(contents) = hover.contents else {
        panic!("expected markup hover")
    };
    assert!(contents.value.contains("Adds two numbers."));
}

#[test]
fn hover_includes_trailing_function_documentation() {
    let source = "fun add(left: i32, right: i32) -> i32 { left + right } //< Adds two numbers.\nfun main() { add(1, 2); }";
    let hover = hover_for_source(
        source,
        position(source, source.rfind("add(1, 2)").unwrap() + 1),
        CompileOptions { use_std: false },
    )
    .unwrap();
    let HoverContents::Markup(contents) = hover.contents else {
        panic!("expected markup hover")
    };
    assert!(contents.value.contains("Adds two numbers."));
}

#[test]
fn hover_shows_complete_type_declarations() {
    let source = r"enum Foo {
    A,
    B(i32),
    C((i32, &Foo)),
}

struct Record {
    first: i32,
    second: bool,
    third: Foo,
    fourth: &Foo,
    fifth: (i32, Foo),
    sixth: i32,
}

type a = Foo;";

    for offset in [source.find("Foo {").unwrap(), source.rfind("Foo;").unwrap()] {
        let hover = hover_for_source(
            source,
            position(source, offset + 1),
            CompileOptions { use_std: false },
        )
        .unwrap();
        let HoverContents::Markup(contents) = hover.contents else {
            panic!("expected markup hover")
        };
        assert_eq!(
            contents.value,
            "```riddle\nenum Foo {\n    A,\n    B(i32),\n    C((i32, &Foo)),\n}\n```"
        );
    }

    let struct_hover = hover_for_source(
        source,
        position(source, source.find("Record {").unwrap() + 1),
        CompileOptions { use_std: false },
    )
    .unwrap();
    let HoverContents::Markup(struct_contents) = struct_hover.contents else {
        panic!("expected markup hover")
    };
    assert_eq!(
        struct_contents.value,
        "```riddle\nstruct Record {\n    first: i32,\n    second: bool,\n    third: Foo,\n    fourth: &Foo,\n    fifth: (i32, Foo),\n    /* ... */\n}\n```"
    );

    let alias_hover = hover_for_source(
        source,
        position(source, source.find("a =").unwrap()),
        CompileOptions { use_std: false },
    )
    .unwrap();
    let HoverContents::Markup(alias_contents) = alias_hover.contents else {
        panic!("expected markup hover")
    };
    assert_eq!(alias_contents.value, "```riddle\ntype a = Foo\n```");
}

#[test]
fn definition_and_implementation_follow_trait_dispatch() {
    let source = "trait Render { fun render(&self) -> i32; } struct View {} impl Render for View { fun render(&self) -> i32 { 1 } } fun run(value: View) -> i32 { value.render() }";
    let call = source.rfind("render()").unwrap();
    let definition = definition_for_source(
        source,
        position(source, call + 2),
        CompileOptions { use_std: false },
    )
    .unwrap();
    let GotoDefinitionResponse::Scalar(definition) = definition else {
        panic!("expected one definition")
    };
    let trait_method = source.find("render(&self)").unwrap();
    assert_eq!(
        definition.range,
        Range::new(
            position(source, trait_method),
            position(source, trait_method + "render".len()),
        )
    );

    let implementation = implementation_for_source(
        source,
        position(source, call + 2),
        CompileOptions { use_std: false },
    )
    .unwrap();
    let GotoDefinitionResponse::Array(implementations) = implementation else {
        panic!("expected implementation locations")
    };
    let impl_method = source.match_indices("render(&self)").nth(1).unwrap().0;
    assert_eq!(implementations.len(), 1);
    assert_eq!(
        implementations[0].range,
        Range::new(
            position(source, impl_method),
            position(source, impl_method + "render".len()),
        )
    );
}

#[test]
fn navigation_preserves_non_file_document_uris() {
    let source = "fun value() -> i32 { 1 } fun main() { value(); }";
    let uri = lsp_types::Url::parse("untitled:riddle-navigation.rid").unwrap();
    let docs = HashMap::from([(uri.clone(), Document::new(source, Some(1)))]);
    let cursor = position(source, source.rfind("value()").unwrap() + 2);
    let sessions = AnalysisSessions::default();

    let definition = definition_for_document(
        &uri,
        &docs,
        cursor,
        CompileOptions { use_std: false },
        &sessions,
    )
    .unwrap()
    .unwrap();
    let GotoDefinitionResponse::Scalar(definition) = definition else {
        panic!("expected one definition")
    };
    assert_eq!(definition.uri, uri);

    let references = references_for_document(
        &uri,
        &docs,
        cursor,
        true,
        CompileOptions { use_std: false },
        &sessions,
    )
    .unwrap()
    .unwrap();
    assert_eq!(references.len(), 2);
    assert!(references.iter().all(|location| location.uri == uri));

    let edit = rename_for_document(
        &uri,
        &docs,
        cursor,
        "renamed",
        CompileOptions { use_std: false },
        &sessions,
    )
    .unwrap()
    .unwrap();
    let Some(DocumentChanges::Edits(edits)) = edit.document_changes else {
        panic!("expected document edits")
    };
    assert_eq!(edits.len(), 1);
    assert_eq!(edits[0].text_document.uri, uri);
}

#[test]
fn definition_maps_project_symbols_to_unopened_modules() {
    let root = temp_root("project-definition");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    let main = root.join("src/main.rid");
    let main_source = "mod util;\nfun main() -> i32 { util::value() }\n";
    fs::write(&main, main_source).unwrap();
    let util = root.join("src/util.rid");
    let util_source = "pub fun value() -> i32 { 1 }\n";
    fs::write(&util, util_source).unwrap();
    let main_uri = lsp_types::Url::from_file_path(&main).unwrap();
    let util_uri = lsp_types::Url::from_file_path(fs::canonicalize(&util).unwrap()).unwrap();
    let docs = HashMap::from([(main_uri.clone(), Document::new(main_source, Some(1)))]);

    let definition = definition_for_document(
        &main_uri,
        &docs,
        position(main_source, main_source.find("value()").unwrap() + 2),
        CompileOptions { use_std: false },
        &AnalysisSessions::default(),
    )
    .unwrap()
    .unwrap();
    let GotoDefinitionResponse::Scalar(location) = definition else {
        panic!("expected one definition")
    };
    assert_eq!(location.uri, util_uri);
    assert_eq!(
        location.range,
        Range::new(
            position(util_source, util_source.find("value").unwrap()),
            position(
                util_source,
                util_source.find("value").unwrap() + "value".len(),
            ),
        )
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn references_respect_shadowing_declarations_and_utf16_positions() {
    let source = "fun main() { \"😀\"; let value = 1; value; { let value = 2; value; } value; }";
    let occurrences = source
        .match_indices("value")
        .map(|(offset, _)| offset)
        .collect::<Vec<_>>();
    let cursor = position(source, occurrences[4] + 2);

    let with_declaration =
        references_for_source(source, cursor, true, CompileOptions { use_std: false }).unwrap();
    assert_eq!(
        with_declaration
            .iter()
            .map(|location| location.range.start)
            .collect::<Vec<_>>(),
        [occurrences[0], occurrences[1], occurrences[4]].map(|offset| position(source, offset))
    );

    let without_declaration =
        references_for_source(source, cursor, false, CompileOptions { use_std: false }).unwrap();
    assert_eq!(
        without_declaration
            .iter()
            .map(|location| location.range.start)
            .collect::<Vec<_>>(),
        [occurrences[1], occurrences[4]].map(|offset| position(source, offset))
    );

    let prepared =
        prepare_rename_for_source(source, cursor, CompileOptions { use_std: false }).unwrap();
    assert_eq!(
        prepared,
        PrepareRenameResponse::RangeWithPlaceholder {
            range: Range::new(
                position(source, occurrences[4]),
                position(source, occurrences[4] + "value".len()),
            ),
            placeholder: "value".into(),
        }
    );
    assert!(
        rename_for_source(source, cursor, "struct", CompileOptions { use_std: false },).is_err()
    );
}

#[test]
fn references_and_rename_cover_fields_shorthand_and_trait_methods() {
    let source = r"struct Point { x: i32 }
trait Read { fun read(&self) -> i32; }
impl Read for Point { fun read(&self) -> i32 { self.x } }
fun run(value: Point, x: i32) -> i32 {
    let point = Point { x };
    point.x + value.read()
}";
    let field_definition = source.find("x: i32").unwrap();
    let field_references = references_for_source(
        source,
        position(source, field_definition + 1),
        true,
        CompileOptions { use_std: false },
    )
    .unwrap();
    assert_eq!(field_references.len(), 4, "{field_references:#?}");

    let field_edit = rename_for_source(
        source,
        position(source, field_definition + 1),
        "y",
        CompileOptions { use_std: false },
    )
    .unwrap()
    .unwrap();
    let Some(DocumentChanges::Edits(field_documents)) = field_edit.document_changes else {
        panic!("expected document edits")
    };
    assert_eq!(field_documents.len(), 1);
    let replacements = field_documents[0]
        .edits
        .iter()
        .map(|edit| match edit {
            lsp_types::OneOf::Left(edit) => edit.new_text.as_str(),
            lsp_types::OneOf::Right(_) => panic!("unexpected annotated edit"),
        })
        .collect::<Vec<_>>();
    assert_eq!(replacements.iter().filter(|text| **text == "y").count(), 3);
    assert_eq!(
        replacements.iter().filter(|text| **text == "y: x").count(),
        1
    );

    let trait_method = source.find("read(&self)").unwrap();
    let method_references = references_for_source(
        source,
        position(source, trait_method + 2),
        true,
        CompileOptions { use_std: false },
    )
    .unwrap();
    assert_eq!(method_references.len(), 3, "{method_references:#?}");
    let method_edit = rename_for_source(
        source,
        position(source, trait_method + 2),
        "inspect",
        CompileOptions { use_std: false },
    )
    .unwrap()
    .unwrap();
    let Some(DocumentChanges::Edits(method_documents)) = method_edit.document_changes else {
        panic!("expected document edits")
    };
    assert_eq!(method_documents[0].edits.len(), 3);
    assert!(method_documents[0].edits.iter().all(|edit| {
        matches!(edit, lsp_types::OneOf::Left(edit) if edit.new_text == "inspect")
    }));
}

#[test]
fn explicit_import_aliases_are_renamed_independently() {
    let source = "mod api { pub fun value() -> i32 { 1 } } use crate::api::{value as fetch}; fun main() -> i32 { fetch() }";
    let alias_use = source.rfind("fetch()").unwrap();
    let alias_references = references_for_source(
        source,
        position(source, alias_use + 2),
        true,
        CompileOptions { use_std: false },
    )
    .unwrap();
    assert_eq!(alias_references.len(), 2, "{alias_references:#?}");

    let value_definition = source.find("value()").unwrap();
    let value_references = references_for_source(
        source,
        position(source, value_definition + 2),
        true,
        CompileOptions { use_std: false },
    )
    .unwrap();
    assert_eq!(value_references.len(), 2, "{value_references:#?}");
    let import_value = source.find("{value as").unwrap() + 1;
    assert_eq!(
        references_for_source(
            source,
            position(source, import_value + 2),
            true,
            CompileOptions { use_std: false },
        )
        .unwrap(),
        value_references
    );

    let edit = rename_for_source(
        source,
        position(source, alias_use + 2),
        "load",
        CompileOptions { use_std: false },
    )
    .unwrap()
    .unwrap();
    let Some(DocumentChanges::Edits(documents)) = edit.document_changes else {
        panic!("expected document edits")
    };
    assert_eq!(documents[0].edits.len(), 2);
    assert!(
        documents[0].edits.iter().all(|edit| {
            matches!(edit, lsp_types::OneOf::Left(edit) if edit.new_text == "load")
        })
    );
}

#[test]
fn project_rename_uses_overlays_and_versions_only_open_documents() {
    let root = temp_root("project-rename");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    let main = root.join("src/main.rid");
    let main_source =
        "mod util;\nmod consumer;\nfun main() -> i32 { util::value() + consumer::read() }\n";
    fs::write(&main, main_source).unwrap();
    let util = root.join("src/util.rid");
    fs::write(&util, "pub fun stale() -> i32 { 0 }\n").unwrap();
    let util_overlay = "pub fun value() -> i32 { 1 }\n";
    let consumer = root.join("src/consumer.rid");
    let consumer_source = "pub fun read() -> i32 { crate::util::value() }\n";
    fs::write(&consumer, consumer_source).unwrap();

    let main_uri = lsp_types::Url::from_file_path(fs::canonicalize(&main).unwrap()).unwrap();
    let util_uri = lsp_types::Url::from_file_path(fs::canonicalize(&util).unwrap()).unwrap();
    let consumer_uri =
        lsp_types::Url::from_file_path(fs::canonicalize(&consumer).unwrap()).unwrap();
    let docs = HashMap::from([
        (main_uri.clone(), Document::new(main_source, Some(7))),
        (util_uri.clone(), Document::new(util_overlay, Some(9))),
    ]);
    let cursor = position(main_source, main_source.find("value()").unwrap() + 2);
    let sessions = AnalysisSessions::default();

    let references = references_for_document(
        &main_uri,
        &docs,
        cursor,
        true,
        CompileOptions { use_std: false },
        &sessions,
    )
    .unwrap()
    .unwrap();
    assert_eq!(references.len(), 3, "{references:#?}");
    assert!(
        references
            .iter()
            .any(|location| location.uri == consumer_uri)
    );

    let edit = rename_for_document(
        &main_uri,
        &docs,
        cursor,
        "answer",
        CompileOptions { use_std: false },
        &sessions,
    )
    .unwrap()
    .unwrap();
    let Some(DocumentChanges::Edits(mut documents)) = edit.document_changes else {
        panic!("expected document edits")
    };
    documents.sort_by(|left, right| {
        left.text_document
            .uri
            .as_str()
            .cmp(right.text_document.uri.as_str())
    });
    assert_eq!(documents.len(), 3);
    assert_eq!(document_edit_version(&documents, &main_uri), Some(7));
    assert_eq!(document_edit_version(&documents, &util_uri), Some(9));
    assert_eq!(document_edit_version(&documents, &consumer_uri), None);
    assert!(documents.iter().all(|document| {
        document
            .edits
            .iter()
            .all(|edit| matches!(edit, lsp_types::OneOf::Left(edit) if edit.new_text == "answer"))
    }));
    let _ = fs::remove_dir_all(root);
}

fn document_edit_version(
    documents: &[lsp_types::TextDocumentEdit],
    uri: &lsp_types::Url,
) -> Option<i32> {
    documents
        .iter()
        .find(|document| document.text_document.uri == *uri)
        .and_then(|document| document.text_document.version)
}

#[test]
fn hover_shows_inferred_lambda_parameter_type() {
    let source = "fun apply(f: impl Fn(i32) -> i32) -> i32 { f(1) } fun main() { let doubled = apply([v -> v * 2]); }";
    let declaration_hover = hover_for_source(
        source,
        position(source, source.rfind("[v ->").unwrap() + 1),
        CompileOptions { use_std: false },
    )
    .unwrap();
    let HoverContents::Markup(declaration_contents) = declaration_hover.contents else {
        panic!("expected markup hover")
    };
    assert!(declaration_contents.value.contains("parameter v: i32"));

    let use_hover = hover_for_source(
        source,
        position(source, source.rfind("v * 2").unwrap()),
        CompileOptions { use_std: false },
    )
    .unwrap();
    let HoverContents::Markup(use_contents) = use_hover.contents else {
        panic!("expected markup hover")
    };
    assert!(use_contents.value.contains("parameter v: i32"));
}

#[test]
fn hover_shows_instantiated_method_signature() {
    let source = "trait Show { fun show(self) -> i32; } struct Boxed { value: i32 } impl Show for Boxed { fun show(self) -> i32 { self.value } } fun main() { let b = Boxed { value: 1 }; let n = b.show(); }";

    let hover = hover_for_source(
        source,
        position(source, source.rfind(".show()").unwrap() + 1),
        CompileOptions { use_std: false },
    )
    .unwrap();
    let HoverContents::Markup(contents) = hover.contents else {
        panic!("expected markup hover")
    };
    assert!(contents.value.contains("fun show(self: Boxed) -> i32"));
}
