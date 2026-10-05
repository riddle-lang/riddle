use super::*;

fn diagnostics(source: &str) -> Vec<lsp_types::Diagnostic> {
    manifest_diagnostics(source)
}

fn codes(source: &str) -> Vec<String> {
    diagnostics(source)
        .iter()
        .filter_map(|diagnostic| match &diagnostic.code {
            Some(lsp_types::NumberOrString::String(code)) => Some(code.clone()),
            _ => None,
        })
        .collect()
}

#[test]
fn manifest_reports_unknown_keys_as_warnings() {
    let source =
        "[package]\nname = \"demo\"\nversion = \"0.1.0\"\nmistake = true\n\n[unknow]\nx = 1\n";

    let codes = codes(source);

    assert!(codes.contains(&"CLUE0003".to_string()));
    let messages = diagnostics(source)
        .iter()
        .map(|diagnostic| diagnostic.message.clone())
        .collect::<Vec<_>>();
    assert!(
        messages
            .iter()
            .any(|message| message.contains("unknown key `package.mistake`"))
    );
    assert!(
        messages
            .iter()
            .any(|message| message.contains("unknown manifest key `unknow`"))
    );
}

#[test]
fn manifest_reports_type_and_semver_errors() {
    let source = "[package]\nname = \"demo\"\nversion = \"not-semver\"\npublish = \"yes\"\n";

    let diagnostics = diagnostics(source);

    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("not a valid semver version"))
    );
    assert!(diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message
            .contains("`package.publish` must be a boolean")
    }));
}

#[test]
fn manifest_reports_dependency_rule_violations() {
    let source = "[dependencies]\nfoo = { path = \"../foo\", git = \"https://x\", branch = \"b\", tag = \"t\" }\nbar = 42\n";

    let diagnostics = diagnostics(source);

    assert!(diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message
            .contains("cannot specify both `path` and `git`")
    }));
    assert!(diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message
            .contains("only one of `branch`, `tag`, or `rev`")
    }));
    assert!(diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message
            .contains("dependency `bar` must be a version string or table")
    }));
}

#[test]
fn manifest_reports_syntax_errors() {
    let source = "[package\nname = ";

    assert!(codes(source).contains(&"CLUE0002".to_string()));
}

#[test]
fn manifest_requires_package_name() {
    let source = "[package]\nversion = \"0.1.0\"\n";

    assert!(diagnostics(source).iter().any(|diagnostic| {
        diagnostic
            .message
            .contains("missing required key `package.name`")
    }));
}

#[test]
fn manifest_accepts_a_valid_manifest() {
    let source = "[package]\nname = \"demo\"\nversion = \"0.2.0\"\nlicense = \"MIT\"\n\n[dependencies]\nfoo = { path = \"../foo\" }\nbar = \"1\"\n\n[lib]\npath = \"src/lib.rid\"\ncrate-type = [\"riddlelib\"]\n";

    let errors = diagnostics(source)
        .into_iter()
        .filter(|diagnostic| diagnostic.severity == Some(DiagnosticSeverity::ERROR))
        .collect::<Vec<_>>();

    assert!(errors.is_empty(), "{errors:#?}");
}

#[test]
fn manifest_completions_offer_sections_at_top_level() {
    let source = "pack";
    let items = manifest_completions(source, position_after(source));

    let package = items
        .iter()
        .find(|item| item.label == "package")
        .expect("package section expected");
    assert_eq!(package.insert_text.as_deref(), Some("[package]"));
}

#[test]
fn manifest_completions_offer_sections_on_bracket_lines() {
    let source = "[package]\nname = \"demo\"\n\n[dep";
    let items = manifest_completions(source, position_after(source));

    assert!(items.iter().any(|item| item.label == "dependencies"));
}

#[test]
fn manifest_completions_offer_keys_inside_package() {
    let source = "[package]\nname = \"demo\"\nver";
    let items = manifest_completions(source, position_after(source));

    assert!(items.iter().any(|item| item.label == "version"));
    assert!(!items.iter().any(|item| item.label == "name"));
}

#[test]
fn manifest_completions_offer_boolean_values() {
    let source = "[runtime]\ngc = ";
    let items = manifest_completions(source, position_after(source));

    let labels = items
        .iter()
        .map(|item| item.label.as_str())
        .collect::<Vec<_>>();
    assert!(labels.contains(&"true") && labels.contains(&"false"));
}

fn position_after(source: &str) -> Position {
    position(source, source.len())
}

#[test]
fn manifest_hover_documents_keys() {
    let source = "[package]\nname = \"demo\"\n";
    let cursor = position(source, source.find("name").unwrap() + 2);

    let hover = manifest_hover(source, cursor).unwrap();
    let lsp_types::HoverContents::Markup(contents) = hover.contents else {
        panic!("expected markup hover");
    };

    assert!(contents.value.contains("**package.name**"));
}

#[test]
fn manifest_document_symbols_list_sections() {
    let symbols = match manifest_document_symbols(
        "[package]\nname = \"demo\"\n\n[dependencies]\nfoo = \"1\"\n",
    ) {
        lsp_types::DocumentSymbolResponse::Nested(symbols) => symbols,
        lsp_types::DocumentSymbolResponse::Flat(_) => panic!("expected nested symbols"),
    };

    let names = symbols
        .iter()
        .map(|symbol| symbol.name.as_str())
        .collect::<Vec<_>>();
    assert!(names.contains(&"package") && names.contains(&"dependencies"));
    let package = symbols
        .iter()
        .find(|symbol| symbol.name == "package")
        .unwrap();
    assert!(
        package
            .children
            .as_deref()
            .unwrap_or_default()
            .iter()
            .any(|child| child.name == "name")
    );
}

#[test]
fn manifest_diagnostics_flow_through_the_workspace_pipeline() {
    let root = temp_root("manifest-lsp");
    fs::create_dir_all(&root).unwrap();
    let manifest_path = root.join("Clue.toml");
    fs::write(&manifest_path, "[package]\nname = \"demo\"\n").unwrap();
    let uri = lsp_types::Url::from_file_path(&manifest_path).unwrap();

    let mut docs = HashMap::new();
    docs.insert(
        uri.clone(),
        Document::new("[package]\nname = \"demo\"\nmistake = 1\n", Some(1)),
    );
    let mut sessions = DiagnosticSessions::default();
    let published = collect_workspace_diagnostics_cancellable(
        &docs,
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
        &mut sessions,
        || false,
    )
    .expect("analysis cannot be cancelled");

    let entry = published.iter().find(|entry| entry.uri == uri).unwrap();
    assert!(
        entry
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("unknown key `package.mistake`"))
    );

    let _ = fs::remove_dir_all(root);
}
