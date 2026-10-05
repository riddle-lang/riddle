use riddlec::pipeline::*;
use riddlec::proc_macro::{
    ProcMacroExpansion, ProcMacroProvider, ProcMacroTokenStream, ProcMacroTokenTree, expand_source,
};
use std::{
    cell::Cell,
    collections::HashMap,
    fmt::Write as _,
    fs, io,
    path::PathBuf,
    time::{SystemTime, UNIX_EPOCH},
};

fn text_size(value: usize) -> rowan::TextSize {
    rowan::TextSize::from(u32::try_from(value).expect("test offset should fit in u32"))
}

fn text_range(start: usize, end: usize) -> rowan::TextRange {
    rowan::TextRange::new(text_size(start), text_size(end))
}

fn c_symbol(kind: char, name: &str) -> String {
    let mut suffix = String::with_capacity(name.len() * 2);
    for byte in name.bytes() {
        write!(suffix, "{byte:02x}").expect("writing to a String should not fail");
    }
    format!("riddle_{kind}_{suffix}")
}

fn c_function(name: &str) -> String {
    c_symbol('f', name)
}

fn c_member(name: &str) -> String {
    c_symbol('m', name)
}

fn temp_source_root(name: &str) -> PathBuf {
    std::env::temp_dir().join(format!(
        "riddle-load-source-{name}-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ))
}

#[test]
fn no_gc_rejects_a_reference_to_local_stack_storage() {
    let result = compile_with_options_and_gc(
        r"
        struct Data { value: i32 }

        fun escaped() -> &Data {
            let local = Data { value: 1 };
            &local
        }
        ",
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
        false,
    );

    let diagnostic = result
        .analysis_diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "E0310")
        .expect("missing no-GC reference escape diagnostic");
    assert!(diagnostic.message.contains("GC is disabled"));
    assert!(result.mir_module.is_none());
}

#[test]
fn no_gc_allows_forwarding_an_input_reference() {
    let result = compile_with_options_and_gc(
        "fun identity(value: &i32) -> &i32 { value }",
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
        false,
    );

    assert!(result.success(), "{:?}", result.analysis_diagnostics);
    assert!(result.mir_module.is_some());
}

#[test]
fn pointer_width_sizes_every_integer_check_that_depends_on_the_target() {
    // `usize` and `isize` are the only integer types whose range is not fixed,
    // and three separate checks ask about it: an expression literal, a `const`
    // initializer, and a match-pattern literal. All three now read the width
    // from the target, so a 64-bit host compiling for a 32-bit one cannot let
    // through a constant that the generated C would truncate in silence.
    let source = r"
        const BIG: usize = 4294967296;

        fun pick(value: usize) -> i32 {
            match value {
                4294967296 => 1,
                _ => 0,
            }
        }

        fun main() -> i32 {
            let wide = 4294967296usize;
            if wide == BIG { pick(wide) } else { 1 }
        }
    ";

    let narrow = check_with_options(
        source,
        CompileOptions {
            use_std: false,
            pointer_width_bits: 32,
        },
    );
    let out_of_range = narrow
        .type_result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "E0011")
        .map(|diagnostic| diagnostic.message.clone())
        .collect::<Vec<_>>();
    // Three literal sites (the `const` initializer, the match pattern, and the
    // expression) plus the folded `const` value itself.
    assert_eq!(
        out_of_range
            .iter()
            .filter(|message| message.as_str()
                == "integer literal `4294967296` is out of range for `usize`")
            .count(),
        3,
        "{out_of_range:?}"
    );
    assert_eq!(
        out_of_range
            .iter()
            .filter(|message| message.as_str()
                == "constant value `4294967296` is out of range for `usize`")
            .count(),
        1,
        "{out_of_range:?}"
    );

    let wide = check_with_options(
        source,
        CompileOptions {
            use_std: false,
            pointer_width_bits: 64,
        },
    );
    assert!(wide.success(), "{:?}", wide.type_result.diagnostics);
}

#[test]
fn no_gc_rejects_reference_escape_through_dyn_callable() {
    let result = compile_with_options_and_gc(
        r#"
        fun identity(value: &mut i32) -> &mut i32 { value }
        fun call(callback: &dyn Fn(&mut i32) -> &mut i32, value: &mut i32) -> &mut i32 {
            callback(value)
        }
        fun escaped() -> &mut i32 {
            let mut value = 1;
            let callback: dyn Fn(&mut i32) -> &mut i32 = identity;
            call(&callback, &mut value)
        }
        "#,
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
        false,
    );

    assert!(
        result
            .analysis_diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0310"),
        "expected dynamic callable reference escape diagnostic: {:#?}",
        result.analysis_diagnostics
    );
    assert!(result.mir_module.is_none());
}

#[test]
fn no_gc_allows_forwarding_reference_through_dyn_callable() {
    let result = compile_with_options_and_gc(
        r#"
        fun identity(value: &mut i32) -> &mut i32 { value }
        fun call(callback: &dyn Fn(&mut i32) -> &mut i32, value: &mut i32) -> &mut i32 {
            callback(value)
        }
        "#,
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
        false,
    );

    assert!(result.success(), "{:#?}", result.analysis_diagnostics);
    assert!(result.mir_module.is_some());
}

#[test]
fn no_gc_allows_an_owned_escaping_closure() {
    let result = compile_with_options_and_gc(
        r"
        fun make() -> impl Fn() -> i32 {
            let value = 42;
            move [ -> value]
        }
        ",
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
        false,
    );

    assert!(result.success(), "{:?}", result.analysis_diagnostics);
    assert!(result.mir_module.is_some());
}

#[test]
fn no_gc_rejects_a_borrowed_escaping_closure() {
    let result = compile_with_options_and_gc(
        r"
        fun make() -> impl Fn() -> i32 {
            let value = 42;
            [ -> value]
        }
        ",
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
        false,
    );

    assert!(
        result
            .analysis_diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0310"),
        "{:#?}",
        result.analysis_diagnostics
    );
    assert!(result.mir_module.is_none());
}

fn assert_same_check_result(left: &CompileResult, right: &CompileResult) {
    let parse_errors = |result: &CompileResult| {
        result
            .parse_errors
            .iter()
            .map(|error| (error.message.clone(), error.span))
            .collect::<Vec<_>>()
    };
    assert_eq!(parse_errors(left), parse_errors(right));
    assert_eq!(left.hir_diagnostics, right.hir_diagnostics);
    assert_eq!(left.type_result.diagnostics, right.type_result.diagnostics);
    assert_eq!(left.analysis_diagnostics, right.analysis_diagnostics);
}

#[test]
fn check_session_matches_stateless_checks_across_edits() {
    let mut session = CheckSession::new();
    let options = CompileOptions {
        use_std: true,
        ..Default::default()
    };
    let sources = [
        "fun stable() -> i32 { 1 }\nfun main() { let value = 1; value; }",
        "// 😀\nfun stable() -> i32 { 1 }\nfun main() { missing; }",
        "// 😀\nfun stable() -> i32 { 1 }\nfun main() { let value = 2; value; }",
    ];

    for source in sources {
        let expected = check_with_options(source, options);
        let actual = session.check_with_options(source, options);
        assert_same_check_result(&actual, &expected);
    }
}

#[test]
fn check_session_shifts_cached_diagnostics_with_their_bodies() {
    let source = r"
struct Wrap<T> { inner: T }
struct Bad { value: str }
trait Flag { fun value() -> bool; }
struct Marker {}
impl Flag for Marker {}
fun f<T>(x: T) -> T { g(Wrap { inner: x }) }
fun g<T>(x: T) -> T { f(Wrap { inner: x }) }
fun bad() { let value: bool = 1; }
";
    let options = CompileOptions {
        use_std: false,
        ..Default::default()
    };
    let mut session = CheckSession::new();
    let first = session.check_with_options(source, options);
    assert_same_check_result(&first, &check_with_options(source, options));

    let shifted = format!("// 😀\n{source}");
    let actual = session.check_with_options(&shifted, options);
    let expected = check_with_options(&shifted, options);
    assert_same_check_result(&actual, &expected);
    assert!(
        actual
            .type_result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0033")
    );
    assert!(["E0026", "E0043"].iter().all(|code| {
        actual
            .type_result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == *code)
    }));
}

#[test]
fn check_session_does_not_shift_signature_diagnostics_with_body() {
    let options = CompileOptions {
        use_std: false,
        ..Default::default()
    };
    let mut session = CheckSession::new();
    let source = "fun bad(value: str) {}";
    let first = session.check_with_options(source, options);
    assert_same_check_result(&first, &check_with_options(source, options));

    let shifted = "fun bad(value: str)  {}";
    let actual = session.check_with_options(shifted, options);
    let expected = check_with_options(shifted, options);
    assert_same_check_result(&actual, &expected);

    let signature_label = actual
        .type_result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "E0043")
        .flat_map(|diagnostic| &diagnostic.labels)
        .find(|label| {
            let range = std::ops::Range::<usize>::from(label.range);
            shifted.get(range) == Some("str")
        });
    assert!(signature_label.is_some());
}

#[test]
fn check_session_invalidates_globals_when_declarations_change() {
    let options = CompileOptions {
        use_std: false,
        ..Default::default()
    };
    let mut session = CheckSession::new();
    let valid = "struct Value { field: &str }\nfun main() {}";
    let first = session.check_with_options(valid, options);
    assert_same_check_result(&first, &check_with_options(valid, options));

    let invalid = "struct Value { field: str }\nfun main() {}";
    let actual = session.check_with_options(invalid, options);
    let expected = check_with_options(invalid, options);
    assert_same_check_result(&actual, &expected);
    assert!(
        actual
            .type_result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0043")
    );
}

#[test]
fn load_source_file_expands_external_mods() {
    let root = temp_source_root("external-mods");
    fs::create_dir_all(&root).unwrap();
    fs::write(
        root.join("main.rid"),
        "mod util;\nfun main() -> i32 { util::one() }\n",
    )
    .unwrap();
    fs::write(root.join("util.rid"), "fun one() -> i32 { 1 }\n").unwrap();

    let loaded = load_source_file(root.join("main.rid")).unwrap();
    assert!(loaded.source.contains("mod util {"));
    assert!(loaded.source.contains("fun one() -> i32 { 1 }"));
    assert_eq!(loaded.files.len(), 2);

    let _ = fs::remove_dir_all(root);
}

#[test]
fn source_map_points_into_external_module() {
    let root = temp_source_root("source-map");
    fs::create_dir_all(&root).unwrap();
    fs::write(
        root.join("main.rid"),
        "mod util;\nfun main() -> i32 { util::value() }\n",
    )
    .unwrap();
    fs::write(
        root.join("util.rid"),
        "pub fun value() -> i32 { missing }\n",
    )
    .unwrap();

    let loaded = load_source_file(root.join("main.rid")).unwrap();
    let start = loaded.source.find("missing").unwrap();
    let mapped = loaded
        .source_map
        .map_range(text_range(start, start + "missing".len()))
        .unwrap();

    assert_eq!(
        mapped.path,
        fs::canonicalize(root.join("util.rid")).unwrap()
    );
    assert_eq!(
        &mapped.source[usize::from(mapped.range.start())..usize::from(mapped.range.end())],
        "missing"
    );
    let generated_eof =
        loaded.source.find("pub fun").unwrap() + "pub fun value() -> i32 { missing }\n".len();
    let mapped_eof = loaded
        .source_map
        .map_range(rowan::TextRange::empty(text_size(generated_eof)))
        .unwrap();
    assert_eq!(mapped_eof.path, mapped.path);
    assert_eq!(usize::from(mapped_eof.range.start()), mapped.source.len());

    let _ = fs::remove_dir_all(root);
}

#[test]
fn source_map_points_generated_macro_code_at_the_derive() {
    struct Provider;
    impl ProcMacroProvider for Provider {
        fn expand(
            &mut self,
            _package: &str,
            _macro_name: &str,
            _kind: riddlec::proc_macro::ProcMacroKind,
            _input: &ProcMacroTokenStream,
            _second_input: Option<&ProcMacroTokenStream>,
            call_site: std::ops::Range<usize>,
        ) -> Result<ProcMacroExpansion, String> {
            let mut output =
                ProcMacroTokenStream::from_source("const GENERATED: i32 = 1;", 0).unwrap();
            output.set_span(call_site);
            Ok(ProcMacroExpansion {
                output,
                diagnostics: Vec::new(),
            })
        }
    }

    let root = temp_source_root("proc-macro-source-map");
    fs::create_dir_all(&root).unwrap();
    let path = root.join("main.rid");
    fs::write(&path, "#[derive(macros::Generated)]\nstruct Value {}\n").unwrap();
    let mut loaded = load_source_file(&path).unwrap();
    let expansion = expand_source(&loaded.source, &mut Provider);
    loaded.apply_expansion(expansion.source, &expansion.mappings);
    let start = loaded.source.find("GENERATED").unwrap();
    let mapped = loaded
        .source_map
        .map_range(text_range(start, start + "GENERATED".len()))
        .unwrap();

    assert_eq!(mapped.path, fs::canonicalize(&path).unwrap());
    assert_eq!(
        &mapped.source[usize::from(mapped.range.start())..usize::from(mapped.range.end())],
        "#[derive(macros::Generated)]"
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn source_map_preserves_spans_copied_into_macro_output() {
    fn find_ident_span(
        stream: &ProcMacroTokenStream,
        expected: &str,
    ) -> Option<std::ops::Range<usize>> {
        for tree in &stream.trees {
            match tree {
                ProcMacroTokenTree::Ident { text, span } if text == expected => {
                    return Some(span.clone());
                }
                ProcMacroTokenTree::Group { stream, .. } => {
                    if let Some(span) = find_ident_span(stream, expected) {
                        return Some(span);
                    }
                }
                _ => {}
            }
        }
        None
    }

    fn set_ident_span(
        stream: &mut ProcMacroTokenStream,
        expected: &str,
        replacement: &std::ops::Range<usize>,
    ) {
        for tree in &mut stream.trees {
            match tree {
                ProcMacroTokenTree::Ident { text, span } if text == expected => {
                    *span = replacement.clone();
                }
                ProcMacroTokenTree::Group { stream, .. } => {
                    set_ident_span(stream, expected, replacement);
                }
                _ => {}
            }
        }
    }

    struct Provider;
    impl ProcMacroProvider for Provider {
        fn expand(
            &mut self,
            _package: &str,
            _macro_name: &str,
            _kind: riddlec::proc_macro::ProcMacroKind,
            input: &ProcMacroTokenStream,
            _second_input: Option<&ProcMacroTokenStream>,
            call_site: std::ops::Range<usize>,
        ) -> Result<ProcMacroExpansion, String> {
            let copied = find_ident_span(input, "copied").unwrap();
            let mut output =
                ProcMacroTokenStream::from_source("const COPIED: i32 = 1;", 0).unwrap();
            output.set_span(call_site);
            set_ident_span(&mut output, "COPIED", &copied);
            Ok(ProcMacroExpansion {
                output,
                diagnostics: Vec::new(),
            })
        }
    }

    let root = temp_source_root("proc-macro-token-source-map");
    fs::create_dir_all(&root).unwrap();
    let path = root.join("main.rid");
    fs::write(
        &path,
        "#[derive(macros::Generated)]\nstruct Value {\n    copied: i32,\n}\n",
    )
    .unwrap();
    let mut loaded = load_source_file(&path).unwrap();
    let expansion = expand_source(&loaded.source, &mut Provider);
    loaded.apply_expansion(expansion.source, &expansion.mappings);
    let start = loaded.source.find("COPIED").unwrap();
    let mapped = loaded
        .source_map
        .map_range(text_range(start, start + "COPIED".len()))
        .unwrap();

    assert_eq!(mapped.path, fs::canonicalize(&path).unwrap());
    assert_eq!(
        &mapped.source[usize::from(mapped.range.start())..usize::from(mapped.range.end())],
        "copied"
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn source_map_keeps_empty_files() {
    let root = temp_source_root("empty-source-map");
    fs::create_dir_all(&root).unwrap();
    let path = root.join("main.rid");
    fs::write(&path, "").unwrap();

    let loaded = load_source_file(&path).unwrap();
    let mapped = loaded
        .source_map
        .map_range(rowan::TextRange::empty(0.into()))
        .unwrap();

    assert_eq!(mapped.path, fs::canonicalize(path).unwrap());
    assert_eq!(mapped.source, "");
    assert!(mapped.range.is_empty());
    let _ = fs::remove_dir_all(root);
}

#[test]
fn syntax_error_at_eof_stays_in_user_source_with_std_enabled() {
    let source = "fun main() {";
    let result = compile(source);

    assert!(!result.parse_errors.is_empty());
    assert!(
        result
            .parse_errors
            .iter()
            .all(|error| usize::from(error.span.end()) <= source.len()),
        "{:#?}",
        result.parse_errors
    );
    assert!(
        result
            .parse_errors
            .iter()
            .any(|error| usize::from(error.span.start()) == source.len()),
        "{:#?}",
        result.parse_errors
    );
}

#[test]
fn mut_belongs_on_the_binding_not_the_let_pattern() {
    // As in Rust: `let (mut a, b)` is legal, `let mut (a, b)` is not.
    let result = compile("fun main() { let mut (a, b) = (1i32, 2i32); }");

    assert!(
        !result.parse_errors.is_empty(),
        "{:#?}",
        result.parse_errors
    );
}

#[test]
fn destructuring_let_patterns_parse() {
    let result = compile(
        r"
            struct Point { x: i32, y: i32 }
            fun main() {
                let (mut a, b) = (1i32, 2i32);
                let Point { x, y } = Point { x: 3i32, y: 4i32 };
                a = a + b + x + y;
            }
        ",
    );

    assert!(result.parse_errors.is_empty(), "{:#?}", result.parse_errors);
    assert!(result.success(), "{:#?}", result.type_result.diagnostics);
}

#[test]
fn reference_patterns_parse() {
    let result = compile(
        r"
            fun shared(value: &i32) { let &copy = value; }
            fun mutable(value: &mut i32) { let &mut copy = value; }
            fun nested(value: & &mut i32) { let &&mut copy = value; }
        ",
    );

    assert!(result.parse_errors.is_empty(), "{:#?}", result.parse_errors);
}

#[test]
fn extern_blocks_require_unsafe_modifier() {
    let result = compile_with_options(
        r#"
            extern "C" { fun external(); }
            fun main() {}
        "#,
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(
        result
            .parse_errors
            .iter()
            .any(|error| error.message.contains("unsafe extern")),
        "{:#?}",
        result.parse_errors
    );
}

#[test]
fn single_function_extern_imports_are_rejected() {
    let result = compile_with_options(
        r#"
            extern "C" fun external();
            fun main() {}
        "#,
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(
        result.parse_errors.iter().any(|error| error
            .message
            .contains("single-function extern declarations")),
        "{:#?}",
        result.parse_errors
    );
}

#[test]
fn generic_extern_declarations_are_rejected() {
    let result = compile_with_options(
        r#"
            unsafe extern "C" { fun external<T>(value: T); }
            fun main() {}
        "#,
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(
        result.parse_errors.iter().any(|error| error
            .message
            .contains("extern function declarations cannot have generic parameters")),
        "{:#?}",
        result.parse_errors
    );
}

#[test]
fn pipeline_stops_at_the_requested_stage() {
    let source = "fun main() { let value = 1; value; }";
    let options = CompileOptions {
        use_std: false,
        ..Default::default()
    };

    let resolved = resolve_with_options(source, options);
    assert!(resolved.hir.is_some());
    assert!(resolved.type_result.expr_types.is_empty());
    assert!(resolved.mir_module.is_none());

    let checked = check_with_options(source, options);
    assert!(!checked.type_result.expr_types.is_empty());
    assert!(checked.mir_module.is_none());

    let built = compile_with_options(source, options);
    assert!(built.success());
    assert!(built.mir_module.is_some());
}

#[test]
fn inference_stage_skips_ownership_analysis() {
    let source = r#"
        struct Token { value: i32 }
        fun main() {
            let token = Token { value: 1 };
            let first = token;
            let second = token;
        }
    "#;
    let mut session = CheckSession::new();
    let inferred = session
        .infer_with_options_cancellable(
            source,
            CompileOptions {
                use_std: false,
                ..Default::default()
            },
            || false,
        )
        .expect("inference should not be cancelled");

    assert!(inferred.hir.is_some());
    assert!(!inferred.type_result.expr_types.is_empty());
    assert!(inferred.analysis_diagnostics.is_empty());
    assert!(inferred.mir_module.is_none());
}

#[test]
fn source_loader_uses_in_memory_overlays() {
    let root = temp_source_root("source-overlay");
    fs::create_dir_all(&root).unwrap();
    fs::write(root.join("main.rid"), "mod util;\n").unwrap();
    fs::write(root.join("util.rid"), "pub fun value() -> i32 { 1 }\n").unwrap();
    let mut overlays = HashMap::new();
    overlays.insert(
        root.join("util.rid"),
        "pub fun value() -> i32 { 2 }\n".into(),
    );

    let loaded = load_source_file_with_overlays(root.join("main.rid"), &overlays).unwrap();

    assert!(loaded.source.contains("value() -> i32 { 2 }"));
    assert!(!loaded.source.contains("value() -> i32 { 1 }"));

    let _ = fs::remove_dir_all(root);
}

#[test]
fn load_source_file_uses_rust_style_mod_rid_tree() {
    let root = temp_source_root("mod-rid-tree");
    fs::create_dir_all(root.join("foo")).unwrap();
    fs::write(
        root.join("main.rid"),
        "mod foo;\nfun main() -> i32 { foo::value() }\n",
    )
    .unwrap();
    fs::write(
        root.join("foo").join("mod.rid"),
        "mod bar;\npub fun value() -> i32 { bar::value() }\n",
    )
    .unwrap();
    fs::write(
        root.join("foo").join("bar.rid"),
        "pub fun value() -> i32 { 1 }\n",
    )
    .unwrap();

    let loaded = load_source_file(root.join("main.rid")).unwrap();
    assert!(
        loaded
            .files
            .contains(&fs::canonicalize(root.join("foo").join("mod.rid")).unwrap())
    );
    assert!(
        loaded
            .files
            .contains(&fs::canonicalize(root.join("foo").join("bar.rid")).unwrap())
    );
    assert!(compile(&loaded.source).success());

    let _ = fs::remove_dir_all(root);
}

#[test]
fn flat_modules_resolve_children_from_module_directory() {
    let root = temp_source_root("flat-module-children");
    fs::create_dir_all(root.join("foo")).unwrap();
    fs::write(
        root.join("main.rid"),
        "mod foo;\nfun main() -> i32 { foo::value() }\n",
    )
    .unwrap();
    fs::write(
        root.join("foo.rid"),
        "mod bar;\npub fun value() -> i32 { bar::value() }\n",
    )
    .unwrap();
    fs::write(
        root.join("foo").join("bar.rid"),
        "pub fun value() -> i32 { 1 }\n",
    )
    .unwrap();
    fs::write(root.join("bar.rid"), "pub fun value() -> i32 { 99 }\n").unwrap();

    let loaded = load_source_file(root.join("main.rid")).unwrap();
    assert!(loaded.source.contains("pub fun value() -> i32 { 1 }"));
    assert!(!loaded.source.contains("pub fun value() -> i32 { 99 }"));
    assert!(compile(&loaded.source).success());

    let _ = fs::remove_dir_all(root);
}

#[test]
fn inline_modules_resolve_children_from_module_directory() {
    let root = temp_source_root("inline-module-children");
    fs::create_dir_all(root.join("foo")).unwrap();
    fs::write(
            root.join("main.rid"),
            "mod foo { mod bar; pub fun value() -> i32 { bar::value() } }\nfun main() -> i32 { foo::value() }\n",
        )
        .unwrap();
    fs::write(
        root.join("foo").join("bar.rid"),
        "pub fun value() -> i32 { 1 }\n",
    )
    .unwrap();

    let loaded = load_source_file(root.join("main.rid")).unwrap();
    assert!(
        loaded
            .files
            .contains(&fs::canonicalize(root.join("foo").join("bar.rid")).unwrap())
    );
    assert!(compile(&loaded.source).success());

    let _ = fs::remove_dir_all(root);
}

#[test]
fn duplicate_flat_and_mod_rid_modules_are_rejected() {
    let root = temp_source_root("duplicate-module-files");
    fs::create_dir_all(root.join("foo")).unwrap();
    fs::write(root.join("main.rid"), "mod foo;\n").unwrap();
    fs::write(root.join("foo.rid"), "pub fun value() -> i32 { 1 }\n").unwrap();
    fs::write(
        root.join("foo").join("mod.rid"),
        "pub fun value() -> i32 { 2 }\n",
    )
    .unwrap();

    let error = load_source_file(root.join("main.rid")).unwrap_err();
    assert_eq!(error.kind(), io::ErrorKind::InvalidData);
    assert!(error.to_string().contains("ambiguous"));

    let _ = fs::remove_dir_all(root);
}

#[test]
fn undeclared_directory_modules_are_not_loaded() {
    let root = temp_source_root("undeclared-module");
    fs::create_dir_all(root.join("foo")).unwrap();
    fs::write(root.join("main.rid"), "fun main() -> i32 { 0 }\n").unwrap();
    fs::write(root.join("foo").join("mod.rid"), "this is not parsed\n").unwrap();

    let loaded = load_source_file(root.join("main.rid")).unwrap();
    assert_eq!(
        loaded.files,
        vec![fs::canonicalize(root.join("main.rid")).unwrap()]
    );
    assert!(compile(&loaded.source).success());

    let _ = fs::remove_dir_all(root);
}

#[test]
fn std_range_iterator_type_checks() {
    let result = compile(
        r"
            use std::ops::range;

            fun main() {
                let mut iter = range(0, 3);
                let first = iter.next();
            }
            ",
    );

    assert!(result.success(), "{:#?}", result.type_result.diagnostics);
}

#[test]
fn incremental_pipeline_stops_when_cancelled_between_stages() {
    let mut session = CheckSession::default();
    let polls = Cell::new(0);

    let result = session.check_with_options_cancellable(
        "fun main() { let value = 1; value; }",
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
        || {
            polls.set(polls.get() + 1);
            polls.get() >= 3
        },
    );

    assert!(result.is_none());
    assert!(polls.get() >= 3);
}

#[test]
fn std_basic_value_methods_compile() {
    let result = compile(
        r"
            struct Token {
                value: i32,
            }

            fun main() -> i32 {
                let some: Option<i32> = Some(2);
                let option_value = some.unwrap_or(0);
                let none: Option<i32> = None;
                let fallback = none.or(Some(4)).unwrap_or(0);

                let ok: Result<i32, bool> = Ok(3);
                let result_value = ok.unwrap_or(0);
                let has_value = ok.ok().is_some();
                let err: Result<i32, bool> = Err(true);
                let has_error = err.err().is_some();

                let token: Option<Token> = Some(Token { value: 5 });
                let token_is_present = token.is_some();
                let token_value = token.unwrap_or(Token { value: 0 }).value;

                if some.is_some() && none.is_none() && ok.is_ok() && err.is_err()
                    && has_value && has_error && token_is_present && token_value == 5 {
                    option_value + fallback + result_value
                } else {
                    0
                }
            }
            ",
    );

    assert!(
        result.success(),
        "hir: {:#?}\ntype: {:#?}\nanalysis: {:#?}",
        result.hir_diagnostics,
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
    let c = generate_c(result.mir_module.as_ref().unwrap()).unwrap();
    assert!(c.contains(&c_function("is_some__10:Option_i32")), "{c}");
}

#[test]
fn str_and_slice_have_no_layout_fields() {
    // Fat-pointer layout is reached through casts in `std`, never through
    // fields, so `.len` / `.ptr` do not exist on `str` or `[T]` for anyone.
    for source in [
        r#"
            fun main() {
                let text: &str = "hello";
                let length = text.len;
            }
            "#,
        r"
            fun main() {
                let values: [i32; 1] = [1];
                let slice: &[i32] = &values;
                let pointer = slice.ptr;
            }
            ",
    ] {
        let result = compile(source);
        assert!(!result.success(), "{source}");
        assert!(
            result
                .type_result
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.code == "E0006"),
            "{:#?}",
            result.type_result.diagnostics
        );
    }
}

#[test]
fn std_string_and_vector_compile_without_string_runtime_helpers() {
    let result = compile(
        r#"
            fun main() -> i32 {
                let mut values: Vector<i32> = Vector::new();
                values.push(1);
                values.push(2);
                let fallback = 0;
                let slice = values.as_slice();
                let slice_value = *slice.get(1usize).unwrap_or(&fallback);
                let first = *values.get(0usize).unwrap_or(&fallback);
                let missing = values.get(2usize).is_none();
                let last = values.pop().unwrap_or(0);

                let mut text = String::from_str("hello");
                text.push_str(" world");
                let literal_bytes = "hello world".as_bytes();
                let first_byte = match literal_bytes.get(0usize) {
                    Option::Some(value) => *value,
                    Option::None => 0u8,
                };
                let text_matches = text.len() == 11usize && text.as_str() == "hello world"
                    && literal_bytes.len() == 11usize && first_byte == 104u8;
                text.clear();

                if slice_value == 2 && first == 1 && last == 2 && missing
                    && text_matches && text.is_empty() && text.as_str() == "" {
                    0
                } else {
                    1
                }
            }
            "#,
    );

    assert!(
        result.success(),
        "parse: {:#?}\nhir: {:#?}\ntype: {:#?}\nanalysis: {:#?}",
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
    let c = generate_c(result.mir_module.as_ref().unwrap()).unwrap();
    assert!(c.contains(&c_function("new__10:Vector_i32")), "{c}");
    assert!(!c.contains("extern void* malloc(size_t)"), "{c}");
    assert!(c.contains("extern void* rgc_realloc(void*, size_t)"), "{c}");
    assert!(c.contains("extern void rgc_free(void*)"), "{c}");
    assert_eq!(c.matches("extern void abort(void);").count(), 1, "{c}");
    assert!(c.matches("abort();").count() >= 1, "{c}");
    assert!(c.contains("sizeof(int32_t)"), "{c}");
    assert!(c.contains(&c_function("as_slice__10:Vector_i32")), "{c}");
    assert!(!c.contains(&c_function("from_raw_parts")), "{c}");
    assert!(!c.contains("vector_grow"), "{c}");
    assert!(!c.contains("str_len"), "{c}");
    assert!(!c.contains("str_byte"), "{c}");
    assert!(!c.contains("str_from_raw"), "{c}");
    assert!(!c.contains("byte_at"), "{c}");
}

#[test]
fn vector_mutation_is_rejected_while_element_reference_is_live() {
    let result = compile(
        r"
            fun main() {
                let mut values: Vector<i32> = Vector::new();
                values.push(1);
                let mut fallback = 0;
                let reference = values.get_mut(0usize).unwrap_or(&mut fallback);
                values.push(2);
                *reference = 3;
            }
            ",
    );

    assert!(!result.success());
    assert!(result.mir_module.is_none());
    assert!(
        result
            .analysis_diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0302"),
        "{:#?}",
        result.analysis_diagnostics
    );
}

#[test]
fn while_statement_does_not_consume_following_dereference_assignment() {
    let result = compile(
        r"
            fun first(values: &mut Vector<i32>) -> &mut i32 {
                &mut values[0]
            }

            fun main() {
                let mut values: Vector<i32> = Vector::new();
                values.push(1);
                let reference = first(&mut values);
                let mut index = 0;
                while index < 100 {
                    values.push(100 + index);
                    index += 1;
                }
                *reference = 999;
            }
            ",
    );

    assert!(result.parse_errors.is_empty(), "{:#?}", result.parse_errors);
    assert!(
        result.type_result.diagnostics.is_empty(),
        "{:#?}",
        result.type_result.diagnostics
    );
    assert_eq!(
        result
            .analysis_diagnostics
            .iter()
            .map(|diagnostic| diagnostic.code)
            .collect::<Vec<_>>(),
        ["E0302"],
        "{:#?}",
        result.analysis_diagnostics
    );
}

#[test]
fn vector_mutation_is_rejected_while_shared_element_reference_is_live() {
    let result = compile(
        r"
            fun main() {
                let mut values: Vector<i32> = Vector::new();
                values.push(1);
                let fallback = 0;
                let reference = values.get(0usize).unwrap_or(&fallback);
                values.push(2);
                *reference;
            }
            ",
    );

    assert!(
        result
            .analysis_diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0300"),
        "{:#?}",
        result.analysis_diagnostics
    );
}

#[test]
fn returned_reference_cannot_borrow_drop_parameter() {
    let result = compile(
        r"
            fun first(values: Vector<i32>) -> &i32 {
                &values[0]
            }

            fun main() {
                let mut values: Vector<i32> = Vector::new();
                values.push(42);
                let reference = first(values);
                *reference;
            }
            ",
    );

    assert!(!result.success());
    assert!(result.mir_module.is_none());
    assert!(
        result
            .analysis_diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0306"),
        "{:#?}",
        result.analysis_diagnostics
    );
}

#[test]
fn closure_returned_reference_cannot_borrow_drop_local() {
    let result = compile(
        r"
            fun main() {
                let make = [ -> {
                    let mut values: Vector<i32> = Vector::new();
                    values.push(42);
                    &values[0]
                }];
                let reference = make();
                *reference;
            }
            ",
    );

    assert!(!result.success());
    assert!(result.mir_module.is_none());
    assert!(
        result
            .analysis_diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0306"),
        "{:#?}",
        result.analysis_diagnostics
    );
}

#[test]
fn dereferencing_non_copy_reference_cannot_move_value_out() {
    let result = compile(
        r"
            struct Point { x: i32, y: i32 }

            fun forward(value: &mut Point) -> &mut Point { value }

            fun main() {
                let mut point = Point { x: 3, y: 4 };
                let moved = *forward(&mut point);
            }
            ",
    );

    assert!(!result.success());
    assert!(result.mir_module.is_none());
    assert!(
        result.analysis_diagnostics.iter().any(|diagnostic| {
            diagnostic.code == "E0308"
                && diagnostic
                    .message
                    .contains("cannot move out of dereference")
        }),
        "{:#?}",
        result.analysis_diagnostics
    );
}

#[test]
fn implicit_reference_field_deref_cannot_move_non_copy_value_out() {
    let result = compile(
        r"
            struct Token { value: i32 }
            struct Wrap { token: Token }

            fun main() {
                let wrap = Wrap { token: Token { value: 1 } };
                let reference: &Wrap = &wrap;
                let moved = reference.token;
            }
            ",
    );

    assert!(!result.success());
    assert!(result.mir_module.is_none());
    assert!(
        result.analysis_diagnostics.iter().any(|diagnostic| {
            diagnostic.code == "E0308"
                && diagnostic
                    .message
                    .contains("cannot move out of dereference")
        }),
        "{:#?}",
        result.analysis_diagnostics
    );
}

#[test]
fn implicit_reference_field_deref_keeps_copy_value_readable() {
    let result = compile(
        r"
            struct Wrap { value: i32 }

            fun main() -> i32 {
                let wrap = Wrap { value: 7 };
                let reference: &Wrap = &wrap;
                reference.value
            }
            ",
    );

    assert!(result.success(), "{:#?}", result.analysis_diagnostics);
}

#[test]
fn implicit_reference_index_deref_cannot_move_non_copy_value_out() {
    let result = compile(
        r"
            struct Token { value: i32 }

            fun main() {
                let values = [Token { value: 1 }];
                let reference: &[Token; 1] = &values;
                let moved = reference[0usize];
            }
            ",
    );

    assert!(!result.success());
    assert!(
        result.analysis_diagnostics.iter().any(|diagnostic| {
            diagnostic.code == "E0308"
                && diagnostic
                    .message
                    .contains("cannot move out of dereference")
        }),
        "{:#?}",
        result.analysis_diagnostics
    );
}

#[test]
fn try_propagates_result_and_converts_error() {
    let result = compile_with_options(
        r"
            enum Result<T, E> { Ok(T), Err(E) }
            trait Into<T> { fun into(self) -> T; }

            struct InnerError {}
            struct OtherError {}
            struct OuterError {}

            impl Into<OtherError> for InnerError {
                fun into(self) -> OtherError { OtherError {} }
            }

            impl Into<OuterError> for InnerError {
                fun into(self) -> OuterError { OuterError {} }
            }

            fun read() -> Result<i32, InnerError> { Result::Ok(2) }

            fun add_one() -> Result<i32, OuterError> {
                let value = read()?;
                Result::Ok(value + 1)
            }

            fun main() -> i32 {
                match add_one() {
                    Result::Ok(value) => value,
                    Result::Err(_) => 0,
                }
            }
            ",
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(
        result.success(),
        "parse: {:#?}\ntype: {:#?}\nanalysis: {:#?}",
        result.parse_errors,
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
    let c = generate_c(result.mir_module.as_ref().unwrap()).unwrap();
    assert!(c.contains("try_err"), "{c}");
}

#[test]
fn try_requires_result_operand() {
    let result = compile_with_options(
        "fun main() -> i32 { let value = 1?; value }",
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(!result.success());
    assert!(
        result
            .type_result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic
                .message
                .contains("`?` requires a Result or Option value as its operand")),
        "{:#?}",
        result.type_result.diagnostics
    );
}

#[test]
fn std_supports_float_remainder_and_scalar_protocols() {
    let result = compile(
        r#"
            fun main() -> f64 {
                let value: i64 = 42;
                let flag = true;
                print!("{}", value);
                print!("{}", flag);
                value as f64 % 5.0
            }
            "#,
    );

    assert!(
        result.success(),
        "parse: {:#?}\ntype: {:#?}\nanalysis: {:#?}",
        result.parse_errors,
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
    let c = generate_c(result.mir_module.as_ref().unwrap()).unwrap();
    assert!(c.contains("fmod"), "{c}");
}

#[test]
fn std_p0_cli_foundations_typecheck() {
    let result = compile(
        r#"
            use std::collections::HashMap;
            use std::option::Option;
            use std::result::Result;
            use std::string::String;

            fun read(value: i32) -> Result<i32, i32> { Result::Ok(value) }

            fun chained() -> Result<i32, i32> {
                let value = read(1)?;
                let base: Result<i32, i32> = Result::Ok(value);
                base
                    .map([value: i32 -> value + 1])
                    .and_then([value: i32 -> Result::Ok(value)])
            }

            fun main() -> i32 {
                let mut values: std::vector::Vector<i32> = std::vector::Vector::new();
                values.push(1);
                values.push(2);
                let mut sum = 0;
                for value in &values { sum += *value; }
                for value in &mut values { *value += 1; }

                let mapped = Option::Some(1)
                    .map([value: i32 -> value + 1])
                    .and_then([value: i32 -> Option::Some(value)])
                    .unwrap();

                let key = String::from_str("name");
                let mut map: HashMap<String, i32> = HashMap::new();
                map.insert(key.clone(), 7);
                match map.get_mut(&key) {
                    Option::Some(value) => { *value += 1; },
                    Option::None => {},
                }
                let mut stored = 0;
                for entry in &map { stored += *entry.1; }

                let text = String::from_str("  --name=value  ");
                let valid_utf8 = [228u8, 189u8, 160u8, 229u8, 165u8, 189u8];
                let invalid_utf8 = [255u8];
                let _args_os = std::env::args_os();
                let _args = std::env::args();
                let result = chained().unwrap();
                if sum == 3 && values[0] == 2 && values[1] == 3
                    && mapped == 2 && stored == 8 && result == 2
                    && text.trim().starts_with("--name")
                    && text.contains("value") && text.find("name").unwrap() == 4usize
                    && text.ends_with("  ") && text.slice(2usize, 8usize).unwrap() == "--name"
                    && String::from_utf8(&valid_utf8).unwrap().as_str() == "你好"
                    && String::from_utf8(&invalid_utf8).is_none() {
                    0
                } else {
                    1
                }
            }
            "#,
    );

    assert!(
        result.success(),
        "parse: {:#?}\ntype: {:#?}\nanalysis: {:#?}",
        result.parse_errors,
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
}

#[test]
fn std_rejects_legacy_display_write_method() {
    let result = compile(
        r"
            use std::fmt::Display;

            struct Legacy {}

            impl Display for Legacy {
                fun write(self) {}
            }

            fun main() {}
            ",
    );

    assert!(!result.success());
    assert!(
        result
            .type_result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("missing method `fmt`")),
        "{:#?}",
        result.type_result.diagnostics
    );
}

#[test]
fn std_exposes_default_hash_parse_collections_and_time() {
    let result = compile(
        r#"
            use std::collections::{HashMap, HashSet, TreeMap, TreeSet};
            use std::hash::Hash;
            use std::parse::parse_i32;
            use std::time::time_now;

            fun main() -> i32 {
                let value: i32 = Default::default();
                let hash = value.hash();
                let parsed = parse_i32("42").unwrap_or(0);
                let mut tree_map: TreeMap<i32, i32> = TreeMap::new();
                tree_map.insert(1, parsed);
                let mut tree_set: TreeSet<usize> = TreeSet::new();
                tree_set.insert(hash);
                let mut hash_map: HashMap<i32, i32> = HashMap::new();
                hash_map.insert(1, parsed);
                let mut hash_set: HashSet<usize> = HashSet::new();
                hash_set.insert(hash);
                let _now = time_now();
                let mapped = match tree_map.get(&1) {
                    Some(reference) => *reference,
                    None => value,
                };
                mapped + parsed
                    + (tree_set.contains(&hash) as i32)
                    + (hash_map.contains_key(&1) as i32)
                    + (hash_set.contains(&hash) as i32)
            }
            "#,
    );

    assert!(
        result.success(),
        "parse: {:#?}\ntype: {:#?}\nanalysis: {:#?}",
        result.parse_errors,
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
}

#[test]
fn std_does_not_expose_legacy_map_and_set_names() {
    let result = compile(
        r"
            fun legacy(map: Map<i32, i32>, set: Set<i32>) {}
            fun main() {}
            ",
    );

    assert!(!result.success());
    assert!(
        result
            .type_result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("unknown type `Map<i32, i32>`")),
        "{:#?}",
        result.type_result.diagnostics
    );
    assert!(
        result
            .type_result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("unknown type `Set<i32>`")),
        "{:#?}",
        result.type_result.diagnostics
    );
}

#[test]
fn std_collections_require_the_collections_module() {
    let implicit = compile(
        r"
            fun main() {
                let map: HashMap<i32, i32> = HashMap::new();
            }
            ",
    );
    assert!(
        implicit
            .type_result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic
                .message
                .contains("unknown type `HashMap<i32, i32>`")),
        "{:#?}",
        implicit.type_result.diagnostics
    );

    for (module, item, ty) in [
        ("hash_map", "HashMap", "HashMap<i32, i32>"),
        ("hash_set", "HashSet", "HashSet<i32>"),
        ("tree_map", "TreeMap", "TreeMap<i32, i32>"),
        ("tree_set", "TreeSet", "TreeSet<i32>"),
    ] {
        let source =
            format!("use std::{module}::{item}; fun main() {{ let value: {ty} = {item}::new(); }}");
        let result = compile(&source);
        assert!(!result.success(), "legacy module `{module}` still resolves");
    }
}

#[test]
fn dereferencing_copy_reference_reads_a_copy() {
    let result = compile(
        r"
            struct Point { x: i32, y: i32 }
            impl Copy for Point {}

            fun forward(value: &mut Point) -> &mut Point { value }

            fun main() {
                let mut point = Point { x: 3, y: 4 };
                let first = *forward(&mut point);
                let second = *forward(&mut point);
                let still_available = point;
                let mut reference = forward(&mut point);
                reference.x = 1;
            }
            ",
    );

    assert!(result.success(), "{:#?}", result.analysis_diagnostics);
}

#[test]
fn string_mutation_is_rejected_while_str_view_is_live() {
    let result = compile(
        r#"
            fun main() {
                let mut text = String::from_str("hello");
                let view = text.as_str().as_bytes();
                text.push_str(" world");
                view.len();
            }
            "#,
    );

    assert!(!result.success());
    assert!(result.mir_module.is_none());
    assert!(
        result
            .analysis_diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0300"),
        "{:#?}",
        result.analysis_diagnostics
    );
}

#[test]
fn nested_non_generic_struct_return_keeps_reference_borrowed() {
    let result = compile(
        r#"
            struct Inner { text: &String }
            struct Wrapper { inner: Inner }

            fun wrap(text: &String) -> Wrapper {
                Wrapper { inner: Inner { text } }
            }

            fun main() {
                let mut text = String::from_str("hello");
                let wrapper = wrap(&text);
                text.push_str(" world");
                wrapper.inner.text.len();
            }
            "#,
    );

    assert!(!result.success());
    assert!(result.mir_module.is_none());
    assert!(
        result
            .analysis_diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0300"),
        "{:#?}",
        result.analysis_diagnostics
    );
}

#[test]
fn vector_mutation_is_allowed_after_element_reference_last_use() {
    let result = compile(
        r"
            fun main() {
                let mut values: Vector<i32> = Vector::new();
                values.push(1);
                let mut fallback = 0;
                let reference = values.get_mut(0usize).unwrap_or(&mut fallback);
                *reference = 3;
                values.push(2);
            }
            ",
    );

    assert!(result.success(), "{:#?}", result.analysis_diagnostics);
}

#[test]
fn mutable_reference_parameters_accept_mutable_method_calls() {
    // A `&mut T` binding is reborrowed for a `&mut self` receiver; only
    // auto-referencing a place requires that place to be declared mutable.
    let result = compile(
        r"
            fun fill(target: &mut Vector<i32>) {
                target.push(7);
            }

            fun first_mut(values: &mut [i32], fallback: &mut i32) -> &mut i32 {
                values.get_mut(0usize).unwrap_or(fallback)
            }

            fun main() {
                let mut values: Vector<i32> = Vector::new();
                fill(&mut values);
            }
            ",
    );

    assert!(
        result.success(),
        "type: {:#?}\nanalysis: {:#?}",
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
}

#[test]
fn immutable_bindings_still_reject_mutable_method_calls() {
    let result = compile(
        r"
            fun main() {
                let values: Vector<i32> = Vector::new();
                values.push(1);
            }
            ",
    );

    assert!(!result.success());
    assert!(
        result
            .type_result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0031"),
        "{:#?}",
        result.type_result.diagnostics
    );
}

#[test]
fn std_clone_and_comparison_methods_are_callable() {
    let result = compile(
        r"
            fun main() -> i32 {
                let value: i32 = 7;
                let cloned = value.clone();
                let equal = value.eq(&7);
                let ordering = value.cmp(&cloned);
                let partial = value.partial_cmp(&cloned);
                if equal { cloned } else { 0 }
            }
            ",
    );

    assert!(
        result.success(),
        "hir: {:#?}\ntype: {:#?}\nanalysis: {:#?}",
        result.hir_diagnostics,
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
    let c = generate_c(result.mir_module.as_ref().unwrap()).unwrap();
    assert!(c.contains(&c_function("clone__i32")), "{c}");
    assert!(c.contains(&c_function("cmp__i32")), "{c}");
    assert!(!c.contains("ref_tmp"), "{c}");
    assert!(c.contains(&format!("{}((&a", c_function("eq__i32"))), "{c}");
    assert!(!c.contains("&((int32_t)7)"), "{c}");
}

#[test]
fn std_operator_trait_methods_emit_native_c_without_wrappers() {
    let result = compile(
        r"
            fun main() -> i64 {
                let mut value: i64 = 64i64;
                let added = value.add(2i64);
                let subtracted = added.sub(1i64);
                let multiplied = subtracted.mul(3i64);
                let divided = multiplied.div(2i64);
                let remainder = divided.rem(5i64);
                let masked = remainder.bitand(7i64);
                let combined = masked.bitor(8i64);
                let toggled = combined.bitxor(3i64);
                let shifted = toggled.shl(1i64).shr(1i64);
                let negated = shifted.not().neg();

                value.add_assign(3i64);
                value.sub_assign(1i64);
                value.mul_assign(2i64);
                value.div_assign(2i64);
                value.rem_assign(63i64);
                value.bitand_assign(31i64);
                value.bitor_assign(8i64);
                value.bitxor_assign(1i64);
                value.shl_assign(1i64);
                value.shr_assign(1i64);

                if true.not() { value } else { value + negated }
            }
            ",
    );

    assert!(
        result.success(),
        "hir: {:#?}\ntype: {:#?}\nanalysis: {:#?}",
        result.hir_diagnostics,
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
    let c = generate_c(result.mir_module.as_ref().unwrap()).unwrap();
    for method in [
        "add__",
        "sub__",
        "mul__",
        "div__",
        "rem__",
        "neg__",
        "not__",
        "bitand__",
        "bitor__",
        "bitxor__",
        "shl__",
        "shr__",
        "add_assign__",
        "sub_assign__",
        "mul_assign__",
        "div_assign__",
        "rem_assign__",
        "bitand_assign__",
        "bitor_assign__",
        "bitxor_assign__",
        "shl_assign__",
        "shr_assign__",
    ] {
        assert!(!c.contains(method), "unexpected `{method}` wrapper:\n{c}");
    }
    assert!(c.contains(" + "), "expected native C addition:\n{c}");
    assert!(
        c.contains("RIDDLE_I64_FROM_BITS(((uint64_t)0 - (uint64_t)"),
        "expected defined native C negation:\n{c}"
    );
}

#[test]
fn compile_can_skip_std() {
    let result = compile_with_options(
        r"
            fun main() {
                let value = range(0, 3);
            }
            ",
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(!result.success());
    assert!(
        result
            .hir_diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("unresolved name: `range`")),
        "{:#?}",
        result.hir_diagnostics
    );
}

#[test]
fn compile_without_std_accepts_basic_program() {
    let result = compile_with_options(
        r"
            fun main() {
                let value = 1;
            }
            ",
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(result.success(), "{:#?}", result.type_result.diagnostics);
}

#[test]
fn const_declarations_require_an_initializer() {
    let result = compile_with_options(
        "const ANSWER: i32; fun main() {}",
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(
        !result.parse_errors.is_empty(),
        "const without a value must be rejected"
    );
}

#[test]
fn type_aliases_require_a_value_outside_trait_declarations() {
    for source in [
        "type Missing; fun main() {}",
        "struct Item {} impl Item { type Missing; } fun main() {}",
    ] {
        let result = compile_with_options(
            source,
            CompileOptions {
                use_std: false,
                ..Default::default()
            },
        );
        assert!(!result.parse_errors.is_empty(), "{source}");
    }

    let result = compile_with_options(
        "trait HasItem { type Item; } fun main() {}",
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(result.success(), "{:#?}", result.type_result.diagnostics);
}

#[test]
fn rejects_non_finite_float_literals() {
    for literal in ["1e999", "1e999f32", "3.5e38f32"] {
        let result = compile_with_options(
            &format!("fun main() {{ let value = {literal}; }}"),
            CompileOptions {
                use_std: false,
                ..Default::default()
            },
        );

        assert!(!result.success(), "{literal} should be rejected");
        assert!(
            result
                .hir_diagnostics
                .iter()
                .any(|diagnostic| diagnostic.message == "non-finite float literal"),
            "{literal}: {:#?}",
            result.hir_diagnostics
        );
    }
}

#[test]
fn malformed_radix_integer_literals_report_one_lowering_error() {
    for literal in ["0x", "0x_", "0b102", "0o8"] {
        let result = compile_with_options(
            &format!("fun main() {{ let value = {literal}; }}"),
            CompileOptions {
                use_std: false,
                ..Default::default()
            },
        );

        assert!(
            result.parse_errors.is_empty(),
            "{literal}: {:#?}",
            result.parse_errors
        );
        assert_eq!(
            result.hir_diagnostics.len(),
            1,
            "{literal}: {:#?}",
            result.hir_diagnostics
        );
        assert_eq!(result.hir_diagnostics[0].message, "invalid integer literal");
    }
}

#[test]
fn unit_uses_empty_tuple_syntax() {
    let result = compile_with_options(
        r"
            fun identity(value: ()) -> () {
                value
            }

            fun main() {
                identity(());
            }
            ",
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(result.success(), "{:#?}", result.type_result.diagnostics);
}

#[test]
fn std_prelude_reexports_core_items() {
    let result = compile(
        r"
            use std::ops::range;

            fun main() {
                let value: Option<i32> = Option::Some(1);
                let mut iter = range(0, 3);
                let first = iter.next();
            }
            ",
    );

    assert!(result.success(), "{:#?}", result.type_result.diagnostics);
}

#[test]
fn std_option_and_result_copy_depends_on_payloads() {
    let copy = compile(
        r"
            fun main() {
                let option: Option<i32> = Some(1);
                let first_option = option;
                let second_option = option;
                let result: Result<i32, bool> = Ok(1);
                let first_result = result;
                let second_result = result;
            }
            ",
    );
    assert!(copy.success(), "{:#?}", copy.analysis_diagnostics);
    assert!(
        !generate_c(copy.mir_module.as_ref().unwrap())
            .unwrap()
            .is_empty()
    );

    let moved = compile(
        r"
            struct Token { value: i32 }

            fun main() {
                let option: Option<Token> = Option::Some(Token { value: 1 });
                let first = option;
                let second = option;
            }
            ",
    );
    assert!(
        moved
            .analysis_diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("use of moved value: `option`")),
        "{:#?}",
        moved.analysis_diagnostics
    );
}

#[test]
fn loop_backedges_reject_repeated_outer_moves_but_refresh_for_bindings() {
    for source in [
        r"
            struct Token { value: i32 }
            fun take(value: Token) {}

            fun main() {
                let token = Token { value: 1 };
                let mut index = 0;
                while index < 2 {
                    take(token);
                    index += 1;
                }
            }
            ",
        r"
            struct Token { value: i32 }
            fun take(value: Token) {}

            fun main() {
                let token = Token { value: 1 };
                for index in [0, 1] {
                    take(token);
                }
            }
            ",
    ] {
        let result = compile(source);
        assert!(
            result.analysis_diagnostics.iter().any(|diagnostic| {
                diagnostic.code == "E0100"
                    && diagnostic.message.contains("use of moved value: `token`")
            }),
            "{:#?}",
            result.analysis_diagnostics
        );
    }

    let fresh_binding = compile(
        r"
            struct Token { value: i32 }
            fun take(value: Token) {}

            fun main() {
                for token in [Token { value: 1 }, Token { value: 2 }] {
                    take(token);
                }
            }
            ",
    );
    assert!(
        fresh_binding.success(),
        "{:#?}",
        fresh_binding.analysis_diagnostics
    );
}

#[test]
fn copy_impl_requires_every_payload_to_be_copy() {
    let invalid = compile(
        r"
            struct Token { value: i32 }
            struct Wrapper<T> { value: T }
            enum TokenState { Empty, Full(Token) }

            impl<T> Copy for Wrapper<T> {}
            impl Copy for TokenState {}

            fun main() {}
            ",
    );

    let copy_errors = invalid
        .type_result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "E0041")
        .collect::<Vec<_>>();
    assert_eq!(
        copy_errors.len(),
        2,
        "{:#?}",
        invalid.type_result.diagnostics
    );
    assert!(
        copy_errors
            .iter()
            .any(|diagnostic| diagnostic.message.contains("Wrapper<T>"))
    );
    assert!(
        copy_errors
            .iter()
            .any(|diagnostic| diagnostic.message.contains("TokenState"))
    );
}

#[test]
fn copy_impl_accepts_nested_conditional_copy_fields() {
    let result = compile(
        r"
            struct Nested<T> { value: Option<T> }

            impl<T: Copy> Copy for Nested<T> {}

            fun main() {
                let value: Nested<i32> = Nested { value: Some(1) };
                let first = value;
                let second = value;
            }
            ",
    );

    assert!(
        result.success(),
        "type: {:#?}\nanalysis: {:#?}",
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
}

#[test]
fn enum_match_lowers_variants_guards_bindings_and_values() {
    let result = compile(
        r"
            enum Message {
                Quit,
                Number(i32),
                Pair { left: i32, right: i32 },
            }

            fun select(value: Message) -> i32 {
                match value {
                    Message::Quit => 0,
                    Message::Number(number) if number > 10 => number,
                    Message::Number(number) => number + 1,
                    Message::Pair { left, right: other } => left + other,
                }
            }

            fun main() -> i32 {
                let pair = Message::Pair { right: 22, left: 20 };
                select(pair)
            }
            ",
    );

    assert!(
        result.success(),
        "hir: {:#?}\ntype: {:#?}\nanalysis: {:#?}",
        result.hir_diagnostics,
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
    let c = generate_c(result.mir_module.as_ref().unwrap()).unwrap();
    assert!(c.contains(&format!(" {};", c_member("Number_0"))), "{c}");
    assert!(c.contains(&format!(".{}", c_member("Pair_left"))), "{c}");
    assert!(c.contains("if ("), "{c}");
}

#[test]
fn enum_constructor_uses_the_flattened_payload_offset() {
    let result = compile(
        r"
            enum Value {
                First(i32),
                Second(i32),
            }

            fun main() -> i32 {
                match Value::Second(7) {
                    Value::First(value) => value,
                    Value::Second(value) => value,
                }
            }
            ",
    );

    assert!(result.success(), "{:#?}", result.type_result.diagnostics);
    let c = generate_c(result.mir_module.as_ref().unwrap()).unwrap();
    assert!(c.contains(&format!(".{} =", c_member("Second_0"))), "{c}");
    assert!(
        !c.contains(&format!(".{} = ((int32_t)7)", c_member("First_0"))),
        "{c}"
    );
}

#[test]
fn literal_match_preserves_values_and_string_comparison() {
    let result = compile(
        r#"
            fun classify(value: i32) -> i32 {
                match value {
                    0 => 10,
                    1 => 20,
                    other => other,
                }
            }

            fun is_yes(value: &str) -> bool {
                match value {
                    "yes" => true,
                    _ => false,
                }
            }

            fun main() -> i32 {
                classify(1)
            }
            "#,
    );

    assert!(result.success(), "{:#?}", result.type_result.diagnostics);
    let c = generate_c(result.mir_module.as_ref().unwrap()).unwrap();
    assert!(c.contains("memcmp"), "{c}");
    assert!(c.contains("== ((int32_t)0)"), "{c}");
}

#[test]
fn non_exhaustive_enum_match_is_rejected() {
    let result = compile(
        r"
            enum State { Ready, Done }

            fun main() -> i32 {
                match State::Ready {
                    State::Ready => 1,
                }
            }
            ",
    );

    assert!(!result.success());
    assert!(
        result
            .type_result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0039"),
        "{:#?}",
        result.type_result.diagnostics
    );
}

#[test]
fn unit_return_does_not_hide_non_exhaustive_payload_match() {
    let result = compile(
        r"
            enum State { Ready, Done(i32) }

            fun consume(state: State) {
                match state {
                    State::Ready => { return; },
                    State::Done(1) => {},
                }
            }

            fun main() {
                consume(State::Ready);
            }
        ",
    );

    assert!(!result.success());
    let diagnostic = result
        .type_result
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "E0039")
        .unwrap();
    assert!(
        diagnostic.message.contains("State::Done(_)"),
        "{diagnostic:#?}"
    );
    assert!(
        diagnostic.notes.iter().any(|note| {
            note == "uncovered i32 ranges for `State::Done(_)`: `-2147483648..=0`, `2..=2147483647`"
        }),
        "{diagnostic:#?}"
    );
}

#[test]
fn std_modules_expose_core_items() {
    let result = compile(
        r"
            use std::ops::Range;
            use std::vector::Vector;

            fun main() {
                let value = std::option::Option::Some(1);
                let mut iter: Range<i32> = std::ops::range(0, 3);
                let first = iter.next();
                let values: Vector<i32> = Vector::new();
            }
            ",
    );

    assert!(result.success(), "{:#?}", result.type_result.diagnostics);
}

#[test]
fn std_array_into_iterator_accepts_non_copy_items() {
    let result = compile(
        r"
            struct Token {
                value: i32,
            }

            fun main() {
                let values = [Token { value: 1 }, Token { value: 2 }];
                let mut iter = values.into_iter();
                let first = iter.next();

                for item in [Token { value: 3 }, Token { value: 4 }] {
                    let next = item.value + 1;
                }
            }
            ",
    );

    assert!(
        result.success(),
        "type: {:#?}\nanalysis: {:#?}",
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
}

#[test]
fn std_range_for_loop_lowers_to_mir_loop() {
    let result = compile(
        r"
            use std::ops::range;

            fun main() {
                let mut sum = 0;
                for item in range(0, 3) {
                    sum += item;
                }
            }
            ",
    );

    assert!(result.success(), "{:#?}", result.type_result.diagnostics);
    let module = result
        .mir_module
        .expect("successful compile should lower MIR");
    let main_id = module
        .function_order
        .iter()
        .copied()
        .find(|id| module.functions[*id].name == "main")
        .expect("main function should be lowered");
    let main = &module.functions[main_id];
    let has_loop_branch = main
        .blocks
        .iter()
        .any(|(_, block)| matches!(block.terminator, mir::instr::Terminator::CondBranch(..)));
    assert!(has_loop_branch, "{main:#?}");
    assert!(
        !generate_c(&module)
            .expect("C backend should lower for loop")
            .is_empty()
    );
}

#[test]
fn loop_break_value_compiles_and_runs_to_exit_code() {
    let compiler = std::env::var_os("CC").unwrap_or_else(|| "cc".into());
    if !std::process::Command::new(&compiler)
        .arg("--version")
        .output()
        .is_ok_and(|output| output.status.success())
    {
        eprintln!("skipping loop end-to-end test: no usable C compiler");
        return;
    }

    let result = compile_with_options(
        r"
        fun main() -> i32 {
            let mut i = 0;
            let result = loop {
                i += 1;
                let inner = loop {
                    break i > 4;
                };
                if inner {
                    break i * 10;
                }
            };
            result
        }
        ",
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(result.success(), "{:#?}", result.type_result.diagnostics);
    let generated = generate_c(result.mir_module.as_ref().unwrap()).unwrap();

    let root = std::env::temp_dir().join(format!(
        "riddle-loop-e2e-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&root).unwrap();
    let source = root.join("main.c");
    let executable = root.join(if cfg!(windows) { "main.exe" } else { "main" });
    fs::write(&source, generated).unwrap();

    let compile_output = std::process::Command::new(&compiler)
        .arg(&source)
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        compile_output.status.success(),
        "C compile failed:\n{}",
        String::from_utf8_lossy(&compile_output.stderr)
    );
    let run = std::process::Command::new(&executable).output().unwrap();
    let _ = fs::remove_dir_all(root);
    assert_eq!(
        run.status.code(),
        Some(50),
        "loop should break with i * 10 == 50"
    );
}

#[test]
fn if_let_and_while_let_compile_and_run_to_exit_code() {
    let compiler = std::env::var_os("CC").unwrap_or_else(|| "cc".into());
    if !std::process::Command::new(&compiler)
        .arg("--version")
        .output()
        .is_ok_and(|output| output.status.success())
    {
        eprintln!("skipping if-let/while-let end-to-end test: no usable C compiler");
        return;
    }

    let result = compile_with_options(
        r"
        enum Option { Some(i32), None }

        fun next(state: i32) -> Option {
            if state > 0 { Option::Some(state - 1) } else { Option::None }
        }

        fun main() -> i32 {
            let opt = Option::Some(41);
            let mut total = 0;
            if let Option::Some(x) = opt {
                total += x;
            } else {
                total += 100;
            }

            let mut state = 5;
            while let Option::Some(v) = next(state) {
                total += v;
                state = v;
            }
            total
        }
        ",
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(result.success(), "{:#?}", result.type_result.diagnostics);
    let generated = generate_c(result.mir_module.as_ref().unwrap()).unwrap();

    let root = std::env::temp_dir().join(format!(
        "riddle-let-condition-e2e-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&root).unwrap();
    let source = root.join("main.c");
    let executable = root.join(if cfg!(windows) { "main.exe" } else { "main" });
    fs::write(&source, generated).unwrap();

    let compile_output = std::process::Command::new(&compiler)
        .arg(&source)
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        compile_output.status.success(),
        "C compile failed:\n{}",
        String::from_utf8_lossy(&compile_output.stderr)
    );
    let run = std::process::Command::new(&executable).output().unwrap();
    let _ = fs::remove_dir_all(root);
    assert_eq!(
        run.status.code(),
        Some(51),
        "if-let contributes 41 and the while-let chain sums 4 + 3 + 2 + 1 + 0"
    );
}

#[test]
fn std_for_loop_destructures_tuple_items() {
    let result = compile(
        r"
            fun main() -> i32 {
                let mut pairs: Vector<(i32, i32)> = Vector::new();
                pairs.push((1, 10));
                pairs.push((2, 20));
                let mut sum = 0;
                for (k, v) in &pairs {
                    sum += *k + *v;
                }
                for (k, v) in pairs {
                    sum += k + v;
                }
                sum
            }
            ",
    );

    assert!(
        result.success(),
        "parse: {:#?}\ntype: {:#?}\nanalysis: {:#?}",
        result.parse_errors,
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
}

#[test]
fn for_pattern_destructuring_compiles_and_runs_to_exit_code() {
    let compiler = std::env::var_os("CC").unwrap_or_else(|| "cc".into());
    if !std::process::Command::new(&compiler)
        .arg("--version")
        .output()
        .is_ok_and(|output| output.status.success())
    {
        eprintln!("skipping for-pattern end-to-end test: no usable C compiler");
        return;
    }

    let result = compile_with_options(
        r"
        struct Point { x: i32, y: i32 }

        fun main() -> i32 {
            let pairs = [(1, 10), (2, 20), (3, 30)];
            let mut sum = 0;
            for (a, b) in pairs {
                sum += a + b;
            }
            let points = [Point { x: 1, y: 2 }, Point { x: 3, y: 4 }];
            for Point { x, y } in points {
                sum += x + y;
            }
            sum
        }
        ",
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(result.success(), "{:#?}", result.type_result.diagnostics);
    let generated = generate_c(result.mir_module.as_ref().unwrap()).unwrap();

    let root = std::env::temp_dir().join(format!(
        "riddle-for-pattern-e2e-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&root).unwrap();
    let source = root.join("main.c");
    let executable = root.join(if cfg!(windows) { "main.exe" } else { "main" });
    fs::write(&source, generated).unwrap();

    let compile_output = std::process::Command::new(&compiler)
        .arg(&source)
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        compile_output.status.success(),
        "C compile failed:\n{}",
        String::from_utf8_lossy(&compile_output.stderr)
    );
    let run = std::process::Command::new(&executable).output().unwrap();
    let _ = fs::remove_dir_all(root);
    assert_eq!(
        run.status.code(),
        Some(76),
        "tuple pairs sum 66 and struct points sum 10"
    );
}

#[test]
fn panic_locations_resolve_to_the_original_module_file() {
    let compiler = std::env::var_os("CC").unwrap_or_else(|| "cc".into());
    if !std::process::Command::new(&compiler)
        .arg("--version")
        .output()
        .is_ok_and(|output| output.status.success())
    {
        eprintln!("skipping panic-location end-to-end test: no usable C compiler");
        return;
    }

    let root = temp_source_root("panic-location");
    fs::create_dir_all(&root).unwrap();
    let entry = root.join("main.rid");
    fs::write(
        &entry,
        "mod helper;\n\nfun main() {\n    helper::boom();\n}\n",
    )
    .unwrap();
    let helper = root.join("helper.rid");
    // `panic!` sits on line 2, column 5 of helper.rid.
    fs::write(&helper, "pub fun boom() {\n    panic!(\"exploded\");\n}\n").unwrap();

    let mut loaded = load_source_file(&entry).unwrap();
    let expansion = riddlec::proc_macro::expand_standard_macros(&loaded.source);
    assert!(
        expansion.diagnostics.is_empty(),
        "{:#?}",
        expansion.diagnostics
    );
    loaded.apply_expansion(expansion.source, &expansion.mappings);

    let package_range = 0..loaded.source.len();
    let result = compile_package_with_options_and_gc(
        &loaded.source,
        std::slice::from_ref(&package_range),
        CompileOptions {
            use_std: true,
            ..Default::default()
        },
        true,
    );
    assert!(result.success(), "{:#?}", result.type_result.diagnostics);
    let module = result.mir_module.expect("successful build produces MIR");
    let c_code = generate_c_for_package_with_source_map(
        &module,
        0,
        true,
        &loaded.source_map,
        &entry.display().to_string(),
    )
    .unwrap();
    let panic_call = c_code
        .lines()
        .find(|line| line.trim_start().starts_with("riddle_panic("))
        .expect("generated C calls riddle_panic");
    assert!(panic_call.contains("helper.rid"), "{panic_call}");
    assert!(panic_call.contains(", 2, 5);"), "{panic_call}");

    let runtime = root.join("main.runtime.c");
    fs::write(&runtime, gc::RUNTIME_C.as_bytes()).unwrap();
    let args_runtime = root.join("main.args.c");
    fs::write(&args_runtime, gc::ARGS_RUNTIME_C.as_bytes()).unwrap();
    let source = root.join("main.c");
    fs::write(&source, c_code.as_bytes()).unwrap();
    let executable = root.join(if cfg!(windows) { "main.exe" } else { "main" });
    let compile_output = std::process::Command::new(&compiler)
        .arg(&source)
        .arg(&runtime)
        .arg(&args_runtime)
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        compile_output.status.success(),
        "C compile failed:\n{}",
        String::from_utf8_lossy(&compile_output.stderr)
    );
    let run = std::process::Command::new(&executable).output().unwrap();
    let stderr = String::from_utf8_lossy(&run.stderr);
    let _ = fs::remove_dir_all(&root);
    assert!(
        stderr.contains("panicked at") && stderr.contains("helper.rid:2:5:"),
        "panic location should point at helper.rid:2:5, got: {stderr}"
    );
}

#[test]
fn user_result_enum_does_not_hijack_the_try_operator() {
    // With std loaded, `?` must bind to the lang `Result`/`Option` enums by
    // definition, not by name: a user enum merely named `Result` is rejected
    // as a `?` operand while the std enum keeps working.
    let result = compile_with_options_and_gc(
        r#"
        enum Result<T, E> {
            Ok(T),
            Err(E),
        }

        fun user_try(value: Result<i32, bool>) -> Result<i32, bool> {
            let inner = value?;
            Result::Ok(inner)
        }

        fun std_try(value: std::result::Result<i32, bool>) -> std::result::Result<i32, bool> {
            let inner = value?;
            std::result::Result::Ok(inner)
        }

        fun main() -> i32 { 0 }
        "#,
        CompileOptions::default(),
        false,
    );

    let diagnostics = &result.type_result.diagnostics;
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0061"),
        "expected E0061 for the user-defined Result used with `?`, got {:#?}",
        diagnostics
    );
    assert!(
        !diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0062"),
        "std Result must keep working with `?`, got {:#?}",
        diagnostics
    );
}

#[test]
fn hash_map_entry_or_insert_matches_rust_idiom() {
    let result = compile(
        r"
        use crate::std::collections::HashMap;

        fun main() -> i32 {
            let mut counts: HashMap<i32, i32> = HashMap::new();
            let slot = counts.entry(7i32).or_insert(0i32);
            *slot = *slot + 1i32;
            let slot2 = counts.entry(7i32).or_insert(100i32);
            if *slot2 != 1i32 { return 1; }
            let slot3 = counts.entry(8i32).or_insert_with([ -> 40i32]);
            *slot3 = *slot3 + 2i32;
            if counts.len() != 2usize { return 2; }
            let slot4 = counts.entry(7i32).or_insert(0i32);
            if *slot4 != 1i32 { return 3; }
            0
        }
        ",
    );
    assert!(
        result.success(),
        "parse: {:#?}
type: {:#?}
analysis: {:#?}
hir: {:#?}
macro: {:#?}",
        result.parse_errors,
        result.type_result.diagnostics,
        result.analysis_diagnostics,
        result.hir_diagnostics,
        result.macro_diagnostics
    );
}
