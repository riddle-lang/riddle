use super::*;

#[test]
fn every_documented_error_code_keeps_rust_style_lsp_fields() {
    let error_docs = include_str!("../../docs/zh-CN/src/errorcode.md");
    let source = "  target  ";
    let uri = lsp_types::Url::parse("file:///diagnostic-codes.rid").unwrap();
    let primary = source_label(
        text_range(0, source.len()),
        "",
        type_checker::LabelStyle::Primary,
    );

    for &code in DOCUMENTED_ERROR_CODES {
        let diagnostic = to_lsp(
            &uri,
            source,
            diagnostic_ext(code, type_checker::Severity::Error, vec![primary.clone()]),
        )
        .unwrap();

        assert_eq!(
            diagnostic.code,
            Some(lsp_types::NumberOrString::String(code.into()))
        );
        assert_eq!(diagnostic.source.as_deref(), Some("riddle"));
        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(
            diagnostic.range,
            Range::new(Position::new(0, 2), Position::new(0, 8))
        );
        let code_description = diagnostic.code_description.unwrap();
        let anchor = code.to_ascii_lowercase();
        assert_eq!(code_description.href.fragment(), Some(anchor.as_str()));
        assert!(
            error_docs.contains(&format!("<a id=\"{anchor}\"></a>")),
            "missing documentation anchor for {code}"
        );
    }
}

#[test]
fn diagnostic_conversion_uses_primary_style_utf16_and_related_labels() {
    let source = "😀  primary  secondary";
    let uri = lsp_types::Url::parse("file:///labels.rid").unwrap();
    let primary_start = source.find("primary").unwrap();
    let secondary_start = source.find("secondary").unwrap();
    let mut input = diagnostic_ext(
        "E0001",
        type_checker::Severity::Warning,
        vec![
            source_label(
                text_range(secondary_start, secondary_start + "secondary".len()),
                "secondary label",
                type_checker::LabelStyle::Secondary,
            ),
            source_label(
                text_range(primary_start - 2, primary_start + "primary".len() + 2),
                "primary label",
                type_checker::LabelStyle::Primary,
            ),
        ],
    );
    input.help = Some("fix it".into());
    input.notes.push("context".into());

    let diagnostic = to_lsp(&uri, source, input).unwrap();

    assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::WARNING));
    assert_eq!(
        diagnostic.range,
        Range::new(Position::new(0, 4), Position::new(0, 11))
    );
    assert_eq!(
        diagnostic.message,
        "message\nprimary label\nhelp: fix it\nnote: context"
    );
    let related = diagnostic.related_information.unwrap();
    assert_eq!(related.len(), 1);
    assert_eq!(related[0].location.uri, uri);
    assert_eq!(related[0].message, "secondary label");
    assert_eq!(
        related[0].location.range,
        Range::new(Position::new(0, 13), Position::new(0, 22))
    );
}

#[test]
fn diagnostic_conversion_maps_every_severity() {
    let source = "x";
    let uri = lsp_types::Url::parse("file:///severity.rid").unwrap();
    let label = source_label(
        TextRange::new(0.into(), 1.into()),
        "",
        type_checker::LabelStyle::Primary,
    );
    let cases = [
        (type_checker::Severity::Error, DiagnosticSeverity::ERROR),
        (type_checker::Severity::Warning, DiagnosticSeverity::WARNING),
        (
            type_checker::Severity::Note,
            DiagnosticSeverity::INFORMATION,
        ),
        (type_checker::Severity::Help, DiagnosticSeverity::HINT),
    ];

    for (input, expected) in cases {
        let diagnostic = to_lsp(
            &uri,
            source,
            diagnostic_ext("E0001", input, vec![label.clone()]),
        )
        .unwrap();
        assert_eq!(diagnostic.severity, Some(expected));
    }
}

#[test]
fn diagnostic_conversion_rejects_non_utf8_boundaries() {
    let source = "😀";
    let uri = lsp_types::Url::parse("file:///invalid-range.rid").unwrap();
    let diagnostic = diagnostic_ext(
        "E0001",
        type_checker::Severity::Error,
        vec![source_label(
            TextRange::new(1.into(), 2.into()),
            "",
            type_checker::LabelStyle::Primary,
        )],
    );

    assert!(to_lsp(&uri, source, diagnostic).is_none());
}

#[test]
fn parser_eof_diagnostic_stays_at_user_eof_with_std() {
    let source = "fun main() {";
    let uri = lsp_types::Url::parse("file:///eof.rid").unwrap();
    let result = riddlec::pipeline::compile(source);
    let diagnostics = collect_diagnostics(&uri, source, &result);

    assert!(!diagnostics.is_empty());
    assert!(diagnostics.iter().all(|diagnostic| {
        diagnostic.range.start
            == Position::new(
                0,
                u32::try_from(source.len()).expect("test offset should fit in u32"),
            )
            && diagnostic.range.end
                == Position::new(
                    0,
                    u32::try_from(source.len()).expect("test offset should fit in u32"),
                )
    }));
}

#[test]
fn recursive_type_alias_reports_diagnostic_without_crashing() {
    let source = "type Result = Result;";
    let uri = lsp_types::Url::parse("file:///recursive-type-alias.rid").unwrap();
    let docs = HashMap::from([(uri.clone(), Document::new(source, Some(1)))]);

    let diagnostics = collect_document_diagnostics(&uri, source, &docs, CompileOptions::default());

    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code == Some(lsp_types::NumberOrString::String("E0391".into()))
                && diagnostic
                    .message
                    .contains("cycle detected when expanding type alias `Result`")
        }),
        "{diagnostics:#?}"
    );
}
