use super::*;

#[test]
fn collect_diagnostics_ignores_appended_std_diagnostics() {
    let source = include_str!("../../std/std/array.rid");
    let result = riddlec::pipeline::compile(source);
    let uri = lsp_types::Url::parse("file:///std/std/array.rid").unwrap();
    let diagnostics = collect_diagnostics(&uri, source, &result);

    assert!(
        diagnostics.iter().all(|diagnostic| !diagnostic
            .message
            .contains("expected IntoIter<T, N>, got IntoIter<T, N>")),
        "{diagnostics:#?}"
    );
}

#[test]
fn project_diagnostics_use_unsaved_module_source() {
    let root = temp_root("project-diagnostics");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    fs::write(
        root.join("src/main.rid"),
        "mod util;\nfun main() -> i32 { util::value() }\n",
    )
    .unwrap();
    let util = root.join("src/util.rid");
    fs::write(&util, "pub fun value() -> i32 { 1 }\n").unwrap();
    let uri = lsp_types::Url::from_file_path(&util).unwrap();
    let text = "pub fun value() -> i32 { missing }\n".to_string();
    let docs = HashMap::from([(uri.clone(), Document::new(text.clone(), Some(1)))]);

    let diagnostics = collect_document_diagnostics(&uri, &text, &docs, CompileOptions::default());

    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("unresolved name: `missing`")),
        "{diagnostics:#?}"
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn project_diagnostics_include_unopened_modules() {
    let root = temp_root("unopened-module");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    let main = root.join("src/main.rid");
    let main_text = "mod util;\nfun main() { util::value(); }\n".to_string();
    fs::write(&main, &main_text).unwrap();
    let util = root.join("src/util.rid");
    let util_text = "pub fun value() { missing; }\n";
    fs::write(&util, util_text).unwrap();
    let main_uri = lsp_types::Url::from_file_path(&main).unwrap();
    let docs = HashMap::from([(main_uri, Document::new(main_text, Some(1)))]);

    let published = collect_workspace_diagnostics(&docs, CompileOptions::default());
    let util_uri = lsp_types::Url::from_file_path(fs::canonicalize(&util).unwrap()).unwrap();
    let util_diagnostics = published.iter().find(|item| item.uri == util_uri).unwrap();
    let unresolved = util_diagnostics
        .diagnostics
        .iter()
        .find(|diagnostic| {
            diagnostic.code == Some(lsp_types::NumberOrString::String("E0050".into()))
        })
        .unwrap();
    let start = util_text.find("missing").unwrap();

    assert_eq!(util_diagnostics.version, None);
    assert_eq!(
        unresolved.range,
        range(util_text, text_range(start, start + "missing".len()),)
    );
    let _ = fs::remove_dir_all(root);
}

#[cfg(windows)]
#[test]
fn project_diagnostics_preserve_the_open_document_uri() {
    let root = temp_root("open-uri-identity");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    let main = root.join("src/main.rid");
    fs::write(&main, "fun main() {}\n").unwrap();

    let aliased_path = PathBuf::from(main.as_os_str().to_string_lossy().to_ascii_uppercase());
    let opened_uri = lsp_types::Url::from_file_path(&aliased_path).unwrap();
    let docs = HashMap::from([(
        opened_uri.clone(),
        Document::new("fun main() { missing; }\n", Some(7)),
    )]);

    let published = collect_workspace_diagnostics(&docs, CompileOptions::default());
    let item = published
        .iter()
        .find(|item| {
            item.diagnostics.iter().any(|diagnostic| {
                diagnostic.code == Some(lsp_types::NumberOrString::String("E0050".into()))
            })
        })
        .unwrap();

    assert_eq!(item.uri, opened_uri);
    assert_eq!(item.version, Some(7));
    let _ = fs::remove_dir_all(root);
}

#[test]
fn mapped_diagnostic_keeps_cross_file_related_information() {
    let root = temp_root("cross-file-labels");
    fs::create_dir_all(&root).unwrap();
    let main = root.join("main.rid");
    let main_text = "mod util;\nfun main() { root_error; }\n";
    fs::write(&main, main_text).unwrap();
    let util = root.join("util.rid");
    let util_text = "pub fun value() { related; }\n";
    fs::write(&util, util_text).unwrap();
    let loaded = riddlec::pipeline::load_source_file(&main).unwrap();
    let primary_start = loaded.source.find("root_error").unwrap();
    let secondary_start = loaded.source.find("related").unwrap();
    let input = diagnostic_ext(
        "E0001",
        type_checker::Severity::Error,
        vec![
            source_label(
                text_range(secondary_start, secondary_start + "related".len()),
                "declared here",
                type_checker::LabelStyle::Secondary,
            ),
            source_label(
                text_range(primary_start, primary_start + "root_error".len()),
                "",
                type_checker::LabelStyle::Primary,
            ),
        ],
    );

    let (uri, diagnostic) = to_lsp_mapped(&loaded.source_map, input).unwrap();
    let related = diagnostic.related_information.unwrap();

    assert_eq!(
        uri,
        lsp_types::Url::from_file_path(fs::canonicalize(&main).unwrap()).unwrap()
    );
    assert_eq!(related.len(), 1);
    assert_eq!(
        related[0].location.uri,
        lsp_types::Url::from_file_path(fs::canonicalize(&util).unwrap()).unwrap()
    );
    let related_start = util_text.find("related").unwrap();
    assert_eq!(
        related[0].location.range,
        range(
            util_text,
            text_range(related_start, related_start + "related".len()),
        )
    );
    let _ = fs::remove_dir_all(root);
}

type DiagnosticSpanCase = (
    &'static str,
    &'static str,
    &'static str,
    &'static str,
    &'static str,
);

const PRODUCER_SPAN_CASES: &[DiagnosticSpanCase] = &[
    (
        "E0001",
        "let initializer",
        "fun main() { let emoji = \"😀\"; let value: bool = 1; }",
        "let value: bool = 1;",
        "let value: bool = 1;",
    ),
    (
        "E0002",
        "if branches",
        "fun main() { let value = if true { 1 } else { false }; }",
        "if true { 1 } else { false }",
        "if true { 1 } else { false }",
    ),
    (
        "E0003",
        "remainder requires integer operands",
        "fun main() { let value = true % false; }",
        "true % false",
        "true % false",
    ),
    (
        "E0004",
        "cannot call value",
        "fun main() { let value = 1; value(); }",
        "value",
        "; value",
    ),
    (
        "E0005",
        "expects 1 argument",
        "fun takes(value: i32) {} fun main() { takes(); }",
        "takes()",
        "takes()",
    ),
    (
        "E0006",
        "unknown field",
        "struct Point { x: i32 } fun main() { let value = Point { y: 1 }; }",
        "Point { y: 1 }",
        "Point { y: 1 }",
    ),
    (
        "E0007",
        "missing field",
        "struct Point { x: i32, y: i32 } fun main() { let value = Point { x: 1 }; }",
        "Point { x: 1 }",
        "Point { x: 1 }",
    ),
    (
        "E0008",
        "cannot dereference",
        "fun main() { let value = *1; }",
        "1",
        "*1",
    ),
    (
        "E0009",
        "struct literal does not resolve",
        "fun main() { let value = Missing { x: 1 }; }",
        "Missing { x: 1 }",
        "Missing { x: 1 }",
    ),
    (
        "E0010",
        "tuple pattern expects",
        "fun check(value: (i32,)) { match value { (left, right) => {} } }",
        "(left, right)",
        "(left, right)",
    ),
    (
        "E0011",
        "out of range for `u8`",
        "fun check(value: u8) { match value { 256 => {}, _ => {} } }",
        "256",
        "256",
    ),
    (
        "E0012",
        "cannot cast `bool` to `f64`",
        "fun main() { let value = true as f64; }",
        "true as f64",
        "true as f64",
    ),
    (
        "E0013",
        "unknown method",
        "fun main() { let value = 1; value.missing(); }",
        "value.missing()",
        "value.missing()",
    ),
    (
        "E0015",
        "is declared without a body",
        "fun missing(); fun main() { let value = missing(); }",
        "fun missing();",
        "fun missing();",
    ),
    (
        "E0020",
        "duplicate method",
        "trait Foo { fun bar(); fun bar(); }",
        "bar",
        "fun bar(); fun bar",
    ),
    (
        "E0022",
        "duplicate associated type",
        "trait Foo { type Item; type Item; }",
        "Item",
        "type Item; type Item",
    ),
    (
        "E0023",
        "unknown trait",
        "struct Point {}\nimpl Missing for Point {}",
        "Missing",
        "impl Missing",
    ),
    (
        "E0024",
        "duplicate method",
        "struct Point {}\nimpl Point { fun bar() {} fun bar() {} }",
        "bar",
        "fun bar() {} fun bar",
    ),
    (
        "E0025",
        "duplicate associated type",
        "struct Point {}\nimpl Point { type Item = i32; type Item = bool; }",
        "Item",
        "type Item = i32; type Item",
    ),
    (
        "E0026",
        "missing method",
        "trait Foo { fun bar(); }\nstruct Point {}\nimpl Foo for Point {}",
        "Point",
        "impl Foo for Point",
    ),
    (
        "E0027",
        "missing associated type",
        "trait Foo { type Item; }\nstruct Point {}\nimpl Foo for Point {}",
        "Point",
        "impl Foo for Point",
    ),
    (
        "E0028",
        "parameter count mismatch",
        "trait Foo { fun bar(value: i32); }\nstruct Point {}\nimpl Foo for Point { fun bar() {} }",
        "bar",
        "impl Foo for Point { fun bar",
    ),
    (
        "E0029",
        "parameter 1 type mismatch",
        "trait Foo { fun bar(value: i32); }\nstruct Point {}\nimpl Foo for Point { fun bar(value: bool) {} }",
        "bool",
        "value: bool",
    ),
    (
        "E0030",
        "return type mismatch",
        "trait Foo { fun bar() -> i32; }\nstruct Point {}\nimpl Foo for Point { fun bar() -> bool { true } }",
        "bool",
        "-> bool",
    ),
    (
        "E0031",
        "not declared as mutable",
        "fun main() { let value = 1; value = 2; }",
        "value = 2",
        "; value = 2",
    ),
    (
        "E0032",
        "expects 1 type argument",
        "struct Box<T> { value: T }\nfun main() { let value: Box<i32, bool>; }",
        "Box<i32, bool>",
        "Box<i32, bool>",
    ),
    (
        "E0033",
        "calling `g`",
        "struct Wrap<T> { inner: T } fun f<T>(x: T) -> T { return g(Wrap { inner: x }); } fun g<T>(x: T) -> T { return f(Wrap { inner: x }); }",
        "g(Wrap { inner: x })",
        "g(Wrap { inner: x })",
    ),
    (
        "E0034",
        "unknown type `Missing`",
        "fun main() { let value: Missing; }",
        "Missing",
        "Missing",
    ),
    (
        "E0035",
        "missing `IntoIterator` trait",
        "fun main() { for item in 1 {} }",
        "for item in 1 {}",
        "for item in 1 {}",
    ),
    (
        "E0036",
        "requires `PartialEq`",
        "#[lang = \"partial_eq\"] trait PartialEq {}\n#[lang = \"eq\"] trait Eq: PartialEq {}\nstruct Point {}\nimpl Eq for Point {}",
        "Point",
        "impl Eq for Point",
    ),
    (
        "E0037",
        "not strictly smaller",
        "trait Foo {}\nstruct Vec<T> { value: T }\nimpl<T> Foo for T where Vec<T>: Foo {}",
        "Foo",
        "Vec<T>: Foo",
    ),
    (
        "E0038",
        "requires a payload pattern",
        "enum State { Ready, Done(i32) }\nfun check(value: State) { match value { State::Done => {} } }",
        "State::Done",
        "State::Done",
    ),
    (
        "E0039",
        "non-exhaustive match",
        "fun check(value: bool) { match value { true => {} } }",
        "match value { true => {} }",
        "match value { true => {} }",
    ),
    (
        "E0040",
        "invalid integer literal",
        "fun main() { let value = 18446744073709551616; }",
        "18446744073709551616",
        "18446744073709551616",
    ),
    (
        "E0041",
        "non-Copy field",
        "#[lang = \"copy\"] trait Copy {}\nstruct Token { value: i32 }\nstruct Wrapper { value: Token }\nimpl Copy for Wrapper {}",
        "Wrapper",
        "impl Copy for Wrapper",
    ),
    (
        "E0042",
        "`break` outside",
        "fun main() { break; }",
        "break;",
        "break;",
    ),
    (
        "E0043",
        "contains unsized `str`",
        "fun main() { let value: str; }",
        "let value: str;",
        "let value: str;",
    ),
    (
        "E0044",
        "unknown supertrait `Missing`",
        "trait Child: Missing {}",
        "Missing",
        "Missing",
    ),
    (
        "E0045",
        "parameter `x`",
        "fun main() { let identity = [x -> x]; }",
        "x",
        "[x",
    ),
    (
        "E0021",
        "callable impl is missing required method",
        "struct Adder {} impl Fn(i32) -> i32 for Adder {}",
        "Fn(i32) -> i32",
        "impl Fn(i32) -> i32",
    ),
    (
        "E0046",
        "dereferencing a raw pointer requires an unsafe block",
        "fun main() { let p: *const i32 = 0usize as *const i32; let v = *p; }",
        "*p",
        "let v = *p",
    ),
    (
        "E0047",
        "conflicting implementations",
        "trait Foo {}\nstruct Point {}\nimpl Foo for Point {}\nimpl Foo for Point {}",
        "Point",
        "impl Foo for Point {}\nimpl Foo for Point",
    ),
    (
        "E0050",
        "unresolved name",
        "fun main() { missing; }",
        "missing",
        "missing",
    ),
    (
        "E0053",
        "unknown lang item",
        "#[lang = \"unknown_item\"] trait Foo {}",
        "Foo",
        "#[lang = \"unknown_item\"] trait Foo",
    ),
    (
        "E0054",
        "field `value` of struct `Secret` is private",
        "mod model { pub struct Secret { value: i32 } pub fun make() -> Secret { Secret { value: 1 } } } fun main() { model::make().value; }",
        "model::make().value",
        "model::make().value",
    ),
    (
        "E0055",
        "`Copy` cannot be implemented",
        "#[lang = \"copy\"] trait Copy {} #[lang = \"drop\"] trait Drop { fun drop(&mut self); } struct Guard {} impl Drop for Guard { fun drop(&mut self) {} } impl Copy for Guard {}",
        "Guard",
        "impl Copy for Guard",
    ),
    (
        "E0056",
        "explicit destructor calls are not allowed",
        "#[lang = \"drop\"] trait Drop { fun drop(&mut self); } struct Guard {} impl Drop for Guard { fun drop(&mut self) {} } fun main() { let mut guard = Guard {}; guard.drop(); }",
        "guard.drop()",
        "guard.drop()",
    ),
    (
        "E0057",
        "refutable pattern",
        "enum Opt { None, Some(i32) } fun main(value: Opt) { let Opt::Some(x) = value; }",
        "Opt::Some(x)",
        "Opt::Some(x)",
    ),
    (
        "E0058",
        "bound more than once",
        "fun main() { let (value, value) = (1, 2); }",
        "value",
        "let (value, value",
    ),
    (
        "E0059",
        "use of uninitialized binding",
        "fun main() -> i32 { let value: i32; value }",
        "value",
        "let value: i32; value",
    ),
    (
        "E0060",
        "not a constant expression",
        "fun make() -> i32 { 1 } const VALUE: i32 = make();",
        "make()",
        "= make()",
    ),
    (
        "E0051",
        "empty use declaration",
        "use crate;\nfun main() {}",
        "crate",
        "crate",
    ),
    (
        "E0052",
        "glob import target not found",
        "use missing::*;\nfun main() {}",
        "missing::*",
        "missing::*",
    ),
    (
        "E0061",
        "`?` requires a Result or Option value as its operand",
        "fun main() { 1?; }",
        "1?",
        "1?",
    ),
    (
        "E0062",
        "can only be used in a function returning Result",
        "#[lang = \"result\"] enum Result<T, E> { Ok(T), Err(E) } fun main() -> i32 { let value: Result<i32, i32> = Result::Ok(1); value?; 0 }",
        "value?",
        "value?",
    ),
    (
        "E0063",
        "cannot convert",
        "#[lang = \"result\"] enum Result<T, E> { Ok(T), Err(E) } struct Inner {} struct Outer {} fun read() -> Result<i32, Inner> { Result::Ok(1) } fun main() -> Result<i32, Outer> { let value = read()?; Result::Ok(value) }",
        "read()?",
        "read()?",
    ),
    (
        "E0064",
        "defined multiple times",
        "fun repeated() {} fun repeated() {}",
        "repeated",
        "fun repeated() {} fun repeated",
    ),
    (
        "E0065",
        "only allowed inside `loop`",
        "fun main() { while true { break 1; } }",
        "break 1;",
        "break 1;",
    ),
    (
        "E0066",
        "must diverge",
        "enum Opt { None, Some(i32) } fun main(value: Opt) { let Opt::Some(x) = value else { 0 }; }",
        "{ 0 }",
        "{ 0 }",
    ),
    (
        "E0067",
        "cannot construct an infinite type",
        "fun main() { let id = [value -> value]; id(id); }",
        "id(id)",
        "id(id)",
    ),
    (
        "E0072",
        "recursive type",
        "enum Loop { Next(Loop) }",
        "Loop",
        "enum Loop",
    ),
    (
        "E0100",
        "use of moved value",
        "struct Point { x: i32 } fun main() { let original = Point { x: 1 }; let moved = original; let second = original; }",
        "original",
        "let second = original",
    ),
    (
        "E0300",
        "borrow `point` as mutable",
        "struct Point { x: i32 } fun main() { let mut point = Point { x: 1 }; let shared = &point; let mutable = &mut point; let read = shared.x; }",
        "&mut point",
        "&mut point",
    ),
    (
        "E0301",
        "borrow `point` as immutable",
        "struct Point { x: i32 } fun main() { let mut point = Point { x: 1 }; let mutable = &mut point; let shared = &point; let written = mutable.x; }",
        "&point",
        "&point",
    ),
    (
        "E0302",
        "borrow `point` as mutable more than once",
        "struct Point { x: i32 } fun main() { let mut point = Point { x: 1 }; let first = &mut point; let second = &mut point; let read = first.x; }",
        "&mut point",
        "let second = &mut point",
    ),
    (
        "E0303",
        "assign to `point` while borrowed",
        "struct Point { x: i32 } fun main() { let mut point = Point { x: 1 }; let shared = &point; point = Point { x: 2 }; let read = shared.x; }",
        "point = Point { x: 2 }",
        "point = Point { x: 2 }",
    ),
    (
        "E0304",
        "cannot move `point` while borrowed",
        "struct Point { x: i32 } fun main() { let point = Point { x: 1 }; let shared = &point; let moved = point; let read = shared.x; }",
        "point",
        "let moved = point",
    ),
    (
        "E0305",
        "cannot move out of a field",
        "#[lang = \"drop\"] trait Drop { fun drop(&mut self); } struct Token {} struct Guard { token: Token } impl Drop for Guard { fun drop(&mut self) {} } fun consume(value: Token) {} fun main() { let guard = Guard { token: Token {} }; consume(guard.token); }",
        "guard.token",
        "guard.token",
    ),
    (
        "E0306",
        "cannot outlive its owner",
        "#[lang = \"drop\"] trait Drop { fun drop(&mut self); } struct Guard {} impl Drop for Guard { fun drop(&mut self) {} } fun leak() -> &Guard { let guard = Guard {}; return &guard; }",
        "&guard",
        "return &guard",
    ),
    (
        "E0307",
        "cannot move `token` in a match guard",
        "struct Token {} enum MaybeToken { Some(Token), None } fun consume(value: Token) -> bool { true } fun main(value: MaybeToken) { match value { MaybeToken::Some(token) if consume(token) => {}, MaybeToken::Some(token) => {}, MaybeToken::None => {} } }",
        "token",
        "consume(token",
    ),
    (
        "E0308",
        "cannot move out of dereference",
        "struct Token {} fun main() { let mut token = Token {}; let reference = &mut token; let moved = *reference; }",
        "*reference",
        "let moved = *reference",
    ),
    (
        "E0310",
        "GC is disabled",
        "struct Data { value: i32 } fun escaped() -> &Data { let value = Data { value: 1 }; &value }",
        "value",
        "let value",
    ),
    (
        "E0391",
        "cycle detected when expanding type alias",
        "type Result = Result;",
        "Result",
        "type Result",
    ),
];

#[test]
fn reachable_diagnostic_producers_have_exact_primary_and_lsp_spans() {
    let cases = PRODUCER_SPAN_CASES;
    let uri = lsp_types::Url::parse("file:///producer-spans.rid").unwrap();

    for &code in DOCUMENTED_ERROR_CODES {
        let expected_count = usize::from(!SOURCE_UNREACHABLE_CODES.contains(&code));
        assert_eq!(
            cases.iter().filter(|case| case.0 == code).count(),
            expected_count,
            "unexpected producer fixture count for {code}"
        );
    }

    for &(code, message, source, expected, marker) in cases {
        let result = if code == "E0310" {
            riddlec::pipeline::compile_with_options_and_gc(
                source,
                CompileOptions {
                    use_std: false,
                    ..Default::default()
                },
                false,
            )
        } else {
            riddlec::pipeline::compile_with_options(
                source,
                CompileOptions {
                    use_std: false,
                    ..Default::default()
                },
            )
        };
        assert!(
            result.parse_errors.is_empty(),
            "{}: {:#?}",
            code,
            result.parse_errors
        );
        let diagnostic = result
            .hir_diagnostics
            .iter()
            .chain(result.type_result.diagnostics.iter())
            .chain(result.analysis_diagnostics.iter())
            .find(|diagnostic| {
                diagnostic.code == code && diagnostic.message.contains(message)
            })
            .unwrap_or_else(|| {
                panic!(
                    "missing {code} containing {message:?}; HIR: {:#?}; type: {:#?}; analysis: {:#?}",
                    result.hir_diagnostics,
                    result.type_result.diagnostics,
                    result.analysis_diagnostics,
                )
            });
        let primary = diagnostic
            .labels
            .iter()
            .find(|label| label.style == type_checker::LabelStyle::Primary)
            .unwrap();
        let actual = &source[usize::from(primary.range.start())..usize::from(primary.range.end())];
        let marker_start = source
            .find(marker)
            .unwrap_or_else(|| panic!("{code}: missing marker {marker:?}"));
        assert!(
            marker.ends_with(expected),
            "{code}: invalid marker {marker:?}"
        );
        let expected_start = marker_start + marker.len() - expected.len();
        let expected_end = expected_start + expected.len();

        assert_eq!(actual, expected, "{code}: {diagnostic:#?}");
        assert_eq!(
            usize::from(primary.range.start()),
            expected_start,
            "{code}: {diagnostic:#?}"
        );
        let lsp = to_lsp(&uri, source, diagnostic.to_ext()).unwrap();
        assert_eq!(
            lsp.range,
            Range::new(
                position(source, expected_start),
                position(source, expected_end),
            ),
            "{code}: {diagnostic:#?}"
        );
    }
}

#[test]
fn closure_diagnostic_spans_point_at_the_relevant_source() {
    let cases = [
        (
            "E0031",
            "mutable closure",
            "fun main() { let mut total = 0; let add = [ -> { total += 1; }]; add(); }",
            "add",
            false,
        ),
        (
            "E0100",
            "use of moved value: `once`",
            "struct Token { value: i32 } fun take(value: Token) {} fun main() { let token = Token { value: 1 }; let once = [ -> { take(token); }]; once(); once(); }",
            "once",
            true,
        ),
        (
            "E0303",
            "assign to `base` while borrowed",
            "fun main() { let mut base = 1; let read = [ -> base]; base = 2; read(); }",
            "base = 2",
            true,
        ),
        (
            "E0067",
            "infinite type",
            "fun main() { let id = [value -> value]; id(id); }",
            "id(id)",
            true,
        ),
        (
            "E0031",
            "immutable parameter",
            "fun run(callback: impl FnMut() -> i32) -> i32 { callback() }",
            "callback",
            false,
        ),
        (
            "E0047",
            "requires a callable signature",
            "fun show(value: impl Fn) {}",
            "impl Fn",
            false,
        ),
        (
            "E0001",
            "unsafe function",
            "fun apply(f: impl Fn(i32) -> i32, value: i32) -> i32 { f(value) } unsafe fun dangerous(value: i32) -> i32 { value } fun main() -> i32 { apply(dangerous, 1) }",
            "dangerous",
            true,
        ),
    ];
    let uri = lsp_types::Url::parse("file:///closure-spans.rid").unwrap();

    for (code, message, source, expected, use_last) in cases {
        let result = riddlec::pipeline::compile_with_options(
            source,
            CompileOptions {
                use_std: false,
                ..Default::default()
            },
        );
        let diagnostic = result
            .type_result
            .diagnostics
            .iter()
            .chain(result.analysis_diagnostics.iter())
            .find(|diagnostic| diagnostic.code == code && diagnostic.message.contains(message))
            .unwrap_or_else(|| panic!("missing {code} containing {message:?}"));
        let primary = diagnostic
            .labels
            .iter()
            .find(|label| label.style == type_checker::LabelStyle::Primary)
            .unwrap();
        let start = if use_last {
            source.rfind(expected)
        } else {
            source.find(expected)
        }
        .unwrap();
        let end = start + expected.len();

        assert_eq!(
            &source[usize::from(primary.range.start())..usize::from(primary.range.end())],
            expected,
            "{code}: {diagnostic:#?}"
        );
        let lsp = to_lsp(&uri, source, diagnostic.to_ext()).unwrap();
        assert_eq!(
            lsp.range,
            Range::new(position(source, start), position(source, end),),
            "{code}: {diagnostic:#?}"
        );
    }

    assert_distinct_anonymous_function_diagnostic();
}

fn assert_distinct_anonymous_function_diagnostic() {
    let source = "fun main() { let value = if true { [x: i32 -> x] } else { [x: i32 -> x] }; }";
    let result = riddlec::pipeline::compile_with_options(
        source,
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    let diagnostic = result
        .type_result
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "E0002")
        .expect("missing distinct anonymous function type diagnostic");
    assert_eq!(
        diagnostic
            .labels
            .iter()
            .filter(|label| label.style == type_checker::LabelStyle::Primary)
            .count(),
        1
    );
    for label in &diagnostic.labels {
        assert!(!source[label.range].trim().is_empty());
    }
}
