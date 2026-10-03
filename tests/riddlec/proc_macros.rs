use riddlec::pipeline::{CompileOptions, check_with_options};
use riddlec::proc_macro::{
    ProcMacroDiagnostic, ProcMacroExpansion, ProcMacroExport, ProcMacroKind, ProcMacroProvider,
    ProcMacroTokenStream, expand_source, expand_standard_macros,
};
use std::ops::Range;
use type_checker::Severity;

#[derive(Default)]
struct FakeProvider {
    calls: Vec<(String, String, String)>,
}

#[derive(Default)]
struct GeneralProvider {
    calls: Vec<(ProcMacroKind, String, String, Option<String>)>,
}

impl ProcMacroProvider for GeneralProvider {
    fn exports(&self, package: &str) -> Option<Vec<ProcMacroExport>> {
        (package == "macros").then(|| {
            [
                ("answer", ProcMacroKind::FunctionLike),
                ("make_item", ProcMacroKind::FunctionLike),
                ("make_type", ProcMacroKind::FunctionLike),
                ("make_pattern", ProcMacroKind::FunctionLike),
                ("replace", ProcMacroKind::Attribute),
            ]
            .into_iter()
            .map(|(name, kind)| ProcMacroExport {
                name: name.into(),
                kind,
                helper_attributes: Vec::new(),
            })
            .collect()
        })
    }

    fn expand(
        &mut self,
        _package: &str,
        macro_name: &str,
        kind: ProcMacroKind,
        input: &ProcMacroTokenStream,
        second_input: Option<&ProcMacroTokenStream>,
        _call_site: Range<usize>,
    ) -> Result<ProcMacroExpansion, String> {
        self.calls.push((
            kind,
            macro_name.into(),
            input.to_source(),
            second_input.map(ProcMacroTokenStream::to_source),
        ));
        let output = match (kind, macro_name) {
            (ProcMacroKind::FunctionLike, "answer") => token_stream("42"),
            (ProcMacroKind::FunctionLike, "make_item") => {
                token_stream("fun generated() -> i32 { 7 }")
            }
            (ProcMacroKind::FunctionLike, "make_type") => token_stream("i32"),
            (ProcMacroKind::FunctionLike, "make_pattern") => token_stream("0"),
            (ProcMacroKind::Attribute, "replace") => token_stream(&format!(
                "fun replaced() -> i32 {{ {} }}",
                input.to_source()
            )),
            _ => return Err(format!("unknown macro `{macro_name}`")),
        };
        Ok(ProcMacroExpansion {
            output,
            diagnostics: Vec::new(),
        })
    }
}

impl ProcMacroProvider for FakeProvider {
    fn exports(&self, package: &str) -> Option<Vec<ProcMacroExport>> {
        (package == "macros").then(|| {
            vec![
                "Named",
                "First",
                "Second",
                "Recursive",
                "Ignored",
                "Nested",
                "Invalid",
                "Helpers",
            ]
            .into_iter()
            .map(|name| ProcMacroExport {
                name: name.into(),
                kind: ProcMacroKind::Derive,
                helper_attributes: if name == "Helpers" {
                    vec!["answer".into()]
                } else {
                    Vec::new()
                },
            })
            .collect()
        })
    }

    fn expand(
        &mut self,
        package: &str,
        macro_name: &str,
        kind: ProcMacroKind,
        input: &ProcMacroTokenStream,
        _second_input: Option<&ProcMacroTokenStream>,
        _call_site: Range<usize>,
    ) -> Result<ProcMacroExpansion, String> {
        assert_eq!(kind, ProcMacroKind::Derive);
        self.calls
            .push((package.into(), macro_name.into(), input.to_source()));
        let output = match macro_name {
            "Named" => "impl Named for User { fun name(&self) -> i32 { 7 } }",
            "First" => "const FIRST: i32 = 1;",
            "Second" => "const SECOND: i32 = 2;",
            "Recursive" => "#[derive(Ignored)] struct Generated {}",
            "Ignored" => "const RECURSIVE_OUTPUT: i32 = 1;",
            "Nested" => "pub fun generated() -> i32 { 9 }",
            "Invalid" => "let value = 1;",
            "Helpers" => "",
            _ => return Err(format!("unknown macro `{macro_name}`")),
        };
        Ok(ProcMacroExpansion {
            output: token_stream(output),
            diagnostics: Vec::new(),
        })
    }
}

fn token_stream(source: &str) -> ProcMacroTokenStream {
    ProcMacroTokenStream::from_source(source, 0).unwrap()
}

#[test]
fn function_like_macros_expand_in_expression_item_type_and_pattern_positions() {
    let source = r"
        use macros::{answer, make_item, make_type, make_pattern};
        make_item!();
        fun main() -> i32 {
            let value: make_type!() = answer!();
            match value {
                make_pattern!() => generated(),
                _ => value,
            }
        }
    ";
    let mut provider = GeneralProvider::default();
    let expanded = expand_source(source, &mut provider);

    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}\n{}",
        expanded.diagnostics,
        expanded.source
    );
    assert!(expanded.source.contains("fun generated"));
    assert!(!expanded.source.contains("answer!"));
    let result = check_with_options(&expanded.source, CompileOptions { use_std: false });
    assert!(
        result.success(),
        "parse={:?} hir={:?} type={:?}",
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics
    );
    assert_eq!(provider.calls.len(), 4);
}

#[test]
fn standard_print_macros_expand_and_type_check() {
    let source = r#"
        fun main() -> i32 {
            print!();
            print!("value={} {{ok}}", 7);
            println!(" {:?} {}", true, 9,);
            println!();
            0
        }
    "#;
    let expanded = expand_standard_macros(source);

    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}\n{}",
        expanded.diagnostics,
        expanded.source
    );
    assert!(!expanded.source.contains("print!("));
    assert!(!expanded.source.contains("println!("));
    assert!(expanded.source.contains("crate :: std :: io :: _print"));
    assert!(!expanded.source.contains("crate :: std :: io :: println"));
    assert!(
        expanded
            .source
            .contains("crate :: std :: fmt :: append_display")
    );
    assert!(
        expanded
            .source
            .contains("crate :: std :: fmt :: append_debug")
    );
    assert_eq!(
        expanded
            .macro_occurrences
            .iter()
            .map(|occurrence| occurrence.name.as_str())
            .collect::<Vec<_>>(),
        ["print", "print", "println", "println"]
    );

    let result = check_with_options(source, CompileOptions { use_std: true });
    assert!(
        result.success(),
        "macro={:?} parse={:?} hir={:?} type={:?}\n{}",
        result.macro_diagnostics,
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics,
        expanded.source
    );
}

#[test]
fn standard_format_macro_expands_to_string_and_type_checks() {
    let source = r#"
        fun main() -> i32 {
            let value = String::from_str("value");
            let message = format!("{}={} {:?} {{done}}", value, 7, true);
            if message.as_str() == "value=7 true {done}" { 0 } else { 1 }
        }
    "#;
    let expanded = expand_standard_macros(source);

    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}\n{}",
        expanded.diagnostics,
        expanded.source
    );
    assert!(!expanded.source.contains("format!("));
    assert!(expanded.macro_occurrences.iter().any(|occurrence| {
        occurrence.name == "format" && occurrence.kind == ProcMacroKind::FunctionLike
    }));

    let result = check_with_options(source, CompileOptions { use_std: true });
    assert!(
        result.success(),
        "macro={:?} parse={:?} hir={:?} type={:?}\n{}",
        result.macro_diagnostics,
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics,
        expanded.source
    );
}

#[test]
fn standard_vec_macro_expands_and_type_checks() {
    let source = r#"
        fun main() -> i32 {
            let list = vec![1, 2, 3,];
            let empty: Vector<i32> = vec![];
            let repeated = vec![7; 4usize];
            let nested = vec![vec![1, 2], vec![3]];
            if list.len() == 3usize
                && empty.is_empty()
                && repeated.len() == 4usize
                && nested.len() == 2usize
            {
                0
            } else {
                1
            }
        }
    "#;
    let expanded = expand_standard_macros(source);

    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}\n{}",
        expanded.diagnostics,
        expanded.source
    );
    assert!(!expanded.source.contains("vec!"));
    assert!(
        expanded
            .source
            .contains("crate :: std :: vector :: Vector :: new")
    );
    assert!(expanded.source.contains(". push ("));
    assert!(
        expanded
            .source
            .contains("crate :: std :: vector :: Vector :: from_elem")
    );
    let vec_occurrences = expanded
        .macro_occurrences
        .iter()
        .filter(|occurrence| {
            occurrence.name == "vec" && occurrence.kind == ProcMacroKind::FunctionLike
        })
        .count();
    // The two vec! calls nested inside `nested`'s input are not parsed as
    // separate MacroCall nodes, so only the four top-level calls report.
    assert_eq!(vec_occurrences, 4);

    let result = check_with_options(source, CompileOptions { use_std: true });
    assert!(
        result.success(),
        "macro={:?} parse={:?} hir={:?} type={:?}\n{}",
        result.macro_diagnostics,
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics,
        expanded.source
    );
}

#[test]
fn standard_vec_macro_empty_form_hints_at_user_binding() {
    // Empty `vec![]` expands to a block binding a hidden `__riddle_vec_*`
    // temporary around `Vector::new`, so the inference failure surfaces in
    // expanded code. The diagnostic must skip the synthetic binding and hint
    // at the user's `t` instead.
    let source = r#"
        fun main() -> i32 {
            let t = vec![];
            0
        }
    "#;
    let expanded = expand_standard_macros(source);
    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}\n{}",
        expanded.diagnostics,
        expanded.source
    );

    let result = check_with_options(source, CompileOptions { use_std: true });
    assert!(!result.success());
    let diag = result
        .type_result
        .diagnostics
        .iter()
        .find(|diag| diag.code == "E0005")
        .expect("uninferred type argument diagnostic");
    assert_eq!(
        diag.message,
        "cannot infer type argument `T` for function `new`"
    );
    assert_eq!(diag.labels.len(), 2);
    assert_eq!(&source[diag.labels[1].range], "t");
    assert_eq!(
        diag.labels[1].message,
        "consider giving `t` an explicit type"
    );
    assert!(
        diag.notes
            .iter()
            .any(|note| note.contains("explicit type annotation"))
    );
}

#[test]
fn standard_debug_derive_expands_and_type_checks() {
    let source = r#"
        #[derive(Debug)]
        struct Pair<T> where T: Copy {
            left: T,
            right: bool,
        }

        #[derive(Debug)]
        enum Message {
            Empty,
            Tuple(i32),
            Named { value: Pair<i32> },
        }

        fun main() {
            println!("{:?}", Pair { left: 7, right: true });
            println!("{:?} {:?}", Message::Tuple(1), "line\n");
        }
    "#;
    let expanded = expand_standard_macros(source);

    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}\n{}",
        expanded.diagnostics,
        expanded.source
    );
    assert!(!expanded.source.contains("#[derive"));
    assert!(expanded.source.contains("crate :: std :: fmt :: Debug"));
    assert!(
        expanded
            .source
            .contains("crate :: std :: fmt :: append_debug")
    );
    assert!(expanded.macro_occurrences.iter().any(|occurrence| {
        occurrence.name == "Debug" && occurrence.kind == ProcMacroKind::Derive
    }));

    let result = check_with_options(source, CompileOptions { use_std: true });
    assert!(
        result.success(),
        "macro={:?} parse={:?} hir={:?} type={:?}\n{}",
        result.macro_diagnostics,
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics,
        expanded.source
    );
}

#[test]
fn standard_derives_expand_and_type_check() {
    let source = r#"
        #[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
        struct Pair<T> {
            left: T,
            right: bool,
        }

        #[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
        enum Message<T> {
            #[default]
            Empty,
            Tuple(T, i32),
            Named { value: T },
        }

        fun require_eq<T: Eq>(_value: &T) {}

        fun main() -> i32 {
            let pair = Pair { left: 7, right: true };
            let copied = pair;
            let cloned = pair.clone();
            let default_pair: Pair<i32> = Default::default();
            let _pair_hash = pair.hash();
            let _pair_ordering = pair.cmp(&copied);
            let _pair_partial_ordering = pair.partial_cmp(&copied);
            require_eq(&cloned);
            if pair != copied || copied != cloned {
                return 1;
            }

            let message = Message::Named { value: 7 };
            let copied_message = message;
            let cloned_message = message.clone();
            let default_message: Message<i32> = Default::default();
            let _message_hash = message.hash();
            let _message_ordering = message.cmp(&copied_message);
            let _message_partial_ordering = message.partial_cmp(&copied_message);
            if message != copied_message || copied_message != cloned_message {
                return 2;
            }
            0
        }
    "#;
    let expanded = expand_standard_macros(source);

    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}\n{}",
        expanded.diagnostics,
        expanded.source
    );
    for trait_name in [
        "Debug",
        "Clone",
        "Copy",
        "Default",
        "Hash",
        "PartialEq",
        "Eq",
        "PartialOrd",
        "Ord",
    ] {
        assert!(
            expanded
                .macro_occurrences
                .iter()
                .any(|occurrence| occurrence.name == trait_name),
            "missing {trait_name} occurrence: {:#?}",
            expanded.macro_occurrences
        );
    }

    let result = check_with_options(source, CompileOptions { use_std: true });
    assert!(
        result.success(),
        "macro={:?} parse={:?} hir={:?} type={:?}\n{}",
        result.macro_diagnostics,
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics,
        expanded.source
    );
}

#[test]
fn standard_default_derive_requires_one_unit_default_variant() {
    let missing = expand_standard_macros("#[derive(Default)] enum Missing { A, B }");
    assert!(missing.diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message
            .contains("requires exactly one `#[default]` variant")
    }));

    let non_unit =
        expand_standard_macros("#[derive(Default)] enum NonUnit { #[default] Value(i32) }");
    assert!(
        non_unit
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("must be a unit variant"))
    );

    let multiple = expand_standard_macros(
        "#[derive(Default)] enum Multiple { #[default] First, #[default] Second }",
    );
    assert!(multiple.diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message
            .contains("requires exactly one `#[default]` variant")
    }));

    let unrelated = expand_standard_macros("#[derive(Clone)] enum Unrelated { #[default] Value }");
    assert!(unrelated.diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message
            .contains("cannot find attribute `default`")
    }));
}

#[test]
fn standard_clone_derive_supports_generic_structs() {
    let source = r#"
        #[derive(Clone)]
        struct Wrapper<T> { value: T }

        fun clone_wrapper<T: Clone>(value: &Wrapper<T>) -> Wrapper<T> {
            value.clone()
        }
    "#;
    let result = check_with_options(source, CompileOptions { use_std: true });
    assert!(
        result.success(),
        "macro={:?} parse={:?} hir={:?} type={:?}",
        result.macro_diagnostics,
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics
    );
}

#[test]
fn standard_partial_eq_derive_supports_generic_structs() {
    let source = r#"
        #[derive(PartialEq)]
        struct Wrapper<T> { value: T }

        fun equal_wrappers<T: PartialEq>(left: &Wrapper<T>, right: &Wrapper<T>) -> bool {
            left.eq(right)
        }
    "#;
    let result = check_with_options(source, CompileOptions { use_std: true });
    assert!(
        result.success(),
        "macro={:?} parse={:?} hir={:?} type={:?}",
        result.macro_diagnostics,
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics
    );
}

#[test]
fn standard_eq_derive_supports_generic_structs() {
    let source = r#"
        #[derive(PartialEq, Eq)]
        struct Wrapper<T> { value: T }

        fun require_eq<T: Eq>(_value: &T) {}

        fun require_wrapper_eq<T: Eq>(value: &Wrapper<T>) {
            require_eq(value);
        }
    "#;
    let result = check_with_options(source, CompileOptions { use_std: true });
    assert!(
        result.success(),
        "macro={:?} parse={:?} hir={:?} type={:?}",
        result.macro_diagnostics,
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics
    );
}

#[test]
fn standard_clone_derive_supports_generic_enums() {
    let source = r#"
        #[derive(Clone)]
        enum Maybe<T> { Some(T), None }

        fun clone_maybe() -> Maybe<i32> {
            Maybe::Some(7).clone()
        }
    "#;
    let result = check_with_options(source, CompileOptions { use_std: true });
    assert!(
        result.success(),
        "macro={:?} parse={:?} hir={:?} type={:?}",
        result.macro_diagnostics,
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics
    );
}

#[test]
fn standard_partial_eq_derive_supports_generic_enums() {
    let source = r#"
        #[derive(PartialEq)]
        enum Maybe<T> { Some(T), None }

        fun equal_maybe() -> bool {
            Maybe::Some(7) == Maybe::Some(7)
        }
    "#;
    let expanded = expand_standard_macros(source);
    let result = check_with_options(source, CompileOptions { use_std: true });
    assert!(
        result.success(),
        "macro={:?} parse={:?} hir={:?} type={:?}\n{}",
        result.macro_diagnostics,
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics,
        expanded.source
    );
}

#[test]
fn standard_formatting_macros_reject_invalid_format_strings() {
    let mismatch = expand_standard_macros(r#"fun main() { print!("{} {}", 1); }"#);
    assert_eq!(mismatch.diagnostics.len(), 1);
    assert!(
        mismatch.diagnostics[0]
            .message
            .contains("references more than the 1 supplied argument(s)")
    );

    let open = '{';
    let close = '}';
    let unsupported_source = format!(r#"fun main() {{ println!("{open}:x{close}", 1); }}"#);
    let unsupported = expand_standard_macros(&unsupported_source);
    assert_eq!(unsupported.diagnostics.len(), 1);
    assert_eq!(
        unsupported.diagnostics[0].message,
        format!(
            "only `{open}{close}`, `{open}0{close}`, and `{open}name{close}` format placeholders (each optionally with `:?`) are supported"
        )
    );

    let format_mismatch = expand_standard_macros(r#"fun main() { let _ = format!("{} {}", 1); }"#);
    assert_eq!(format_mismatch.diagnostics.len(), 1);
    assert!(
        format_mismatch.diagnostics[0]
            .message
            .contains("references more than the 1 supplied argument(s)")
    );

    let format_unsupported =
        expand_standard_macros(r#"fun main() { let _ = format!("{:x}", 1); }"#);
    assert_eq!(format_unsupported.diagnostics.len(), 1);
    assert_eq!(
        format_unsupported.diagnostics[0].message,
        format!(
            "only `{open}{close}`, `{open}0{close}`, and `{open}name{close}` format placeholders (each optionally with `:?`) are supported"
        )
    );

    let panic_mismatch = expand_standard_macros(r#"fun main() { panic!("{} {}", 1); }"#);
    assert_eq!(panic_mismatch.diagnostics.len(), 1);
    assert!(
        panic_mismatch.diagnostics[0]
            .message
            .contains("references more than the 1 supplied argument(s)")
    );
}

#[test]
fn standard_panic_macro_preserves_never_type_and_expands_default_message() {
    let source = r#"fun value() -> i32 {
    print!();
    panic!("value={}", 7)
}
fun empty() -> i32 { panic!() }
"#;
    let expanded = expand_standard_macros(source);
    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}",
        expanded.diagnostics
    );
    assert!(
        expanded.source.contains("explicit panic"),
        "{}",
        expanded.source
    );
    let compact = expanded
        .source
        .chars()
        .filter(|ch| !ch.is_whitespace())
        .collect::<String>();
    assert!(compact.contains(",3u32,5u32)"), "{}", expanded.source);

    let result = check_with_options(source, CompileOptions::default());
    assert!(
        result.success(),
        "parse={:?} hir={:?} type={:?} analysis={:?}",
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
}

#[test]
fn unreachable_macro_in_statement_position_keeps_the_following_statement() {
    // Reported from an editor: typing a bare identifier after
    // `unreachable!("...");` broke the language server. The coverage above only
    // exercises these macros inside match arms, without the trailing semicolon.
    let source = r#"
        fun main() {
            unreachable!("Fuckypi");
            let after = 1;
        }
    "#;
    let expanded = expand_standard_macros(source);

    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}\n{}",
        expanded.diagnostics,
        expanded.source
    );
    assert!(
        !expanded.source.contains("unreachable!"),
        "the macro should be gone:\n{}",
        expanded.source
    );
    assert!(
        expanded.source.contains("let after = 1;"),
        "the statement after the macro must survive:\n{}",
        expanded.source
    );

    let result = check_with_options(source, CompileOptions::default());
    assert!(
        result.success(),
        "macro={:?}\nparse={:?}\nhir={:?}\ntype={:?}",
        result.macro_diagnostics,
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics,
    );
}

#[test]
fn an_incomplete_identifier_after_a_discarded_macro_stays_diagnosable() {
    // The editor state from the report: the user has typed the macro's
    // statement and is one character into the next one. Expansion, parsing and
    // diagnostics all have to terminate and stay well-formed, because the
    // language server runs them on every keystroke.
    let source = "fun main() {\n    unreachable!(\"Fuckypi\");\n    p\n}\n";

    let expanded = expand_standard_macros(source);
    assert!(
        !expanded.source.contains("unreachable!"),
        "the macro should be gone:\n{}",
        expanded.source
    );
    assert!(
        expanded.source.contains('p'),
        "the identifier being typed must survive:\n{}",
        expanded.source
    );

    let result = check_with_options(source, CompileOptions::default());
    assert!(
        !result.success(),
        "an incomplete statement is an error, but it must be reported, not lost"
    );
}

#[test]
fn standard_panic_family_macros_expand_and_type_check() {
    let source = r#"
        #[derive(Debug, PartialEq)]
        struct Pair { value: i32 }

        fun terminal(kind: i32) -> i32 {
            match kind {
                0 => todo!(),
                1 => unimplemented!("branch {}", kind),
                2 => unreachable!(),
                _ => kind,
            }
        }

        fun main() -> i32 {
            assert!(true);
            assert!(1 < 2, "ordered {}", 2);
            assert_eq!(Pair { value: 1 }, Pair { value: 1 });
            assert_eq!(1, 1, "equal {}", 1);
            assert_ne!(1, 2);
            assert_ne!(1, 2, "different");
            debug_assert!(true);
            debug_assert_eq!(3, 3);
            debug_assert_ne!(3, 4);
            0
        }
    "#;
    let expanded = expand_standard_macros(source);

    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}\n{}",
        expanded.diagnostics,
        expanded.source
    );
    for name in [
        "assert!",
        "assert_eq!",
        "assert_ne!",
        "debug_assert!",
        "debug_assert_eq!",
        "debug_assert_ne!",
        "todo!",
        "unimplemented!",
        "unreachable!",
    ] {
        assert!(!expanded.source.contains(name), "{}", expanded.source);
    }
    assert!(expanded.source.contains("assertion `left == right` failed"));
    assert!(expanded.source.contains("not yet implemented"));
    assert!(expanded.source.contains("not implemented"));
    assert!(expanded.source.contains("entered unreachable code"));

    let result = check_with_options(source, CompileOptions::default());
    assert!(
        result.success(),
        "macro={:?} parse={:?} hir={:?} type={:?} analysis={:?}\n{}",
        result.macro_diagnostics,
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics,
        result.analysis_diagnostics,
        expanded.source
    );
}

#[test]
fn standard_assert_macros_validate_required_arguments() {
    for (source, message) in [
        (
            "fun main() { assert!(); }",
            "assert! requires at least 1 argument",
        ),
        (
            "fun main() { assert_eq!(1); }",
            "assert_eq! requires at least 2 arguments",
        ),
        (
            "fun main() { debug_assert_ne!(1); }",
            "debug_assert_ne! requires at least 2 arguments",
        ),
    ] {
        let expanded = expand_standard_macros(source);
        assert_eq!(expanded.diagnostics.len(), 1, "{}", expanded.source);
        assert_eq!(expanded.diagnostics[0].message, message);
    }
}

#[test]
fn attribute_macros_receive_arguments_and_the_annotated_item_separately() {
    let source = r"
        use macros::replace;
        #[replace(9)]
        fun original() -> i32 { 1 }
        fun main() -> i32 { replaced() }
    ";
    let mut provider = GeneralProvider::default();
    let expanded = expand_source(source, &mut provider);

    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}",
        expanded.diagnostics
    );
    assert!(expanded.source.contains("fun replaced"));
    assert!(!expanded.source.contains("fun original"));
    assert_eq!(provider.calls.len(), 1);
    assert_eq!(provider.calls[0].0, ProcMacroKind::Attribute);
    assert_eq!(provider.calls[0].2, "9");
    assert!(
        provider.calls[0]
            .3
            .as_deref()
            .unwrap()
            .contains("fun original")
    );
    let result = check_with_options(&expanded.source, CompileOptions { use_std: false });
    assert!(result.success(), "{:?}", result.type_result.diagnostics);
}

#[test]
fn mixed_imports_preserve_ordinary_bindings() {
    let source = r"
        mod values { pub fun plain() -> i32 { 1 } }
        use {macros::answer, values::plain};
        fun main() -> i32 { answer!() + plain() }
    ";
    let mut provider = GeneralProvider::default();
    let expanded = expand_source(source, &mut provider);

    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}",
        expanded.diagnostics
    );
    assert!(expanded.source.contains("use values::plain;"));
    assert!(!expanded.source.contains("macros::answer"));
    let result = check_with_options(&expanded.source, CompileOptions { use_std: false });
    assert!(result.success(), "{:?}", result.hir_diagnostics);
}

#[test]
fn public_macro_reexports_can_be_imported_through_modules() {
    let source = r"
        mod first { pub use macros::answer; }
        mod prelude { pub use crate::first::answer; }
        use prelude::answer;
        fun main() -> i32 { answer!() }
    ";
    let mut provider = GeneralProvider::default();
    let expanded = expand_source(source, &mut provider);

    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}\n{}",
        expanded.diagnostics,
        expanded.source
    );
    assert!(!expanded.source.contains("use macros::answer"));
    assert!(!expanded.source.contains("use prelude::answer"));
    let result = check_with_options(&expanded.source, CompileOptions { use_std: false });
    assert!(result.success(), "{:?}", result.hir_diagnostics);
}

#[test]
fn proc_macro_token_wire_round_trips_nested_utf8_and_spans() {
    let source =
        "#[helper(r#\"quoted \\\" text 中\"#, r#\"nul:\0\"#)] struct Demo<T> { value: [T; 3] }";
    let tokens = ProcMacroTokenStream::from_source(source, 37).unwrap();
    let encoded = tokens.encode();
    let decoded = ProcMacroTokenStream::decode(&encoded).unwrap();

    assert!(encoded.is_ascii());
    assert_eq!(decoded, tokens);
    assert_eq!(
        decoded.to_source(),
        "# [helper (r#\"quoted \\\" text 中\"# , r#\"nul:\0\"#)] struct Demo < T > {value : [T ; 3]}"
    );
    assert_eq!(decoded.trees[0].span(), &(37..38));
}

#[test]
fn proc_macro_api_type_checks() {
    let source = r#"
        #[proc_macro_derive(Display)]
        pub fun derive_display(input: TokenStream) -> TokenStream {
            let span = Span::call_site();
            let _mixed = Span::mixed_site();
            let _joined = span.join(span.located_at(span));
            let _resolved = span.resolved_at(span);
            let diagnostic = Diagnostic::warning(span, "unused demo input");
            diagnostic.emit();
            Diagnostic::note(span, "note").emit();
            Diagnostic::help(span, "help").emit();
            let trait_copy = input.clone();
            let rendered = trait_copy.to_string();
            let parsed = match TokenStream::from_str(rendered.as_str()) {
                Result::Ok(value) => value,
                Result::Err(error) => {
                    Diagnostic::error(error.span(), error.message()).emit();
                    return TokenStream::new();
                },
            };
            let mut rebuilt = TokenStream::new();
            for tree in parsed {
                rebuilt.push(tree);
            }
            let mut ident = Ident::new("Generated", Span::call_site());
            ident.set_span(Span::call_site());
            let mut punct = Punct::new(';', Spacing::Alone);
            punct.set_span(Span::call_site());
            let mut literal = match Literal::from_str("1") {
                Result::Ok(value) => value,
                Result::Err(error) => {
                    Diagnostic::error(error.span(), error.message()).emit();
                    return TokenStream::new();
                },
            };
            literal.set_span(Span::call_site());
            let string_literal = Literal::string("quoted \"text\" # UTF-8: 中");
            let group = Group::new(
                Delimiter::None,
                TokenStream::from_tree(TokenTree::Ident(ident)),
            );
            rebuilt.push(TokenTree::Group(group));
            rebuilt.push(TokenTree::Punct(punct));
            rebuilt.push(TokenTree::Literal(literal));
            rebuilt.push(TokenTree::Literal(string_literal));
            let copied = rebuilt.cloned();
            for tree in copied {
                let _span = tree.span();
            }
            rebuilt
        }
    "#;

    let source = format!("{}\n{source}", include_str!("../../std/std/proc_macro.rid"));
    let result = check_with_options(&source, CompileOptions::default());
    assert!(
        result.success(),
        "parse={:?} hir={:?} type={:?} analysis={:?}",
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
}

#[test]
fn syn_derive_input_api_type_checks() {
    let syn = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../std/std/syn.rid"),
    )
    .expect("the built-in syn module source should exist");
    let source = r#"
        use syn::{Data, DeriveInput, GenericParam, Parse, ToTokens, parse, parse_str};

        fun inspect(input: TokenStream) -> TokenStream {
            let parsed: DeriveInput = match parse::<DeriveInput>(input) {
                Result::Ok(value) => value,
                Result::Err(error) => {
                    error.emit();
                    return TokenStream::new();
                },
            };
            let mut metadata = TokenStream::new();
            parsed.generics.tokens.to_tokens(&mut metadata);
            parsed.generics.where_clause.to_tokens(&mut metadata);
            for param in parsed.generics.params.as_slice() {
                match param {
                    GenericParam::Type(param) => param.to_tokens(&mut metadata),
                    GenericParam::Const(param) => param.to_tokens(&mut metadata),
                }
            }
            for predicate in parsed.generics.predicates.as_slice() {
                predicate.ty.to_tokens(&mut metadata);
            }
            let reparsed = parse_str::<DeriveInput>(parsed.to_token_stream().to_string().as_str());
            match &parsed.data {
                Data::Struct(data) => {
                    data.fields.to_tokens(&mut metadata);
                    for field in data.named.as_slice() {
                        field.ty.to_tokens(&mut metadata);
                    }
                },
                Data::Enum(data) => {
                    data.variants.to_tokens(&mut metadata);
                    for variant in data.items.as_slice() {
                        variant.to_tokens(&mut metadata);
                    }
                },
            }
            match reparsed {
                Result::Ok(value) => value.to_token_stream(),
                Result::Err(error) => {
                    error.emit();
                    TokenStream::new()
                },
            }
        }
    "#;

    let source = format!(
        "{}\n{syn}\n{source}",
        include_str!("../../std/std/proc_macro.rid")
    );
    let result = check_with_options(&source, CompileOptions::default());
    assert!(
        result.success(),
        "parse={:?} hir={:?} type={:?} analysis={:?}",
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
}

#[test]
fn syn_core_syntax_categories_type_check() {
    let syn = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../std/std/syn.rid"),
    )
    .expect("the built-in syn module source should exist");
    let source = r#"
        use syn::{Expr, File, Item, Pat, Stmt, ToTokens, Type, parse_str};

        fun inspect_syntax() -> TokenStream {
            let file = match parse_str::<File>("const ANSWER: i32 = 42;") {
                Result::Ok(value) => value,
                Result::Err(error) => { error.emit(); return TokenStream::new(); },
            };
            let item = match parse_str::<Item>("fun answer() -> i32 { 42 }") {
                Result::Ok(value) => value,
                Result::Err(error) => { error.emit(); return TokenStream::new(); },
            };
            let stmt = match parse_str::<Stmt>("let value = 42;") {
                Result::Ok(value) => value,
                Result::Err(error) => { error.emit(); return TokenStream::new(); },
            };
            let expr = match parse_str::<Expr>("value + 1") {
                Result::Ok(value) => value,
                Result::Err(error) => { error.emit(); return TokenStream::new(); },
            };
            let ty = match parse_str::<Type>("&mut [i32; 3]") {
                Result::Ok(value) => value,
                Result::Err(error) => { error.emit(); return TokenStream::new(); },
            };
            let const_ty = match parse_str::<Type>("3") {
                Result::Ok(value) => value,
                Result::Err(error) => { error.emit(); return TokenStream::new(); },
            };
            let pat = match parse_str::<Pat>("Option::Some(value)") {
                Result::Ok(value) => value,
                Result::Err(error) => { error.emit(); return TokenStream::new(); },
            };

            let mut output = TokenStream::new();
            file.to_tokens(&mut output);
            match &item { Item::Function(tokens) => tokens.to_tokens(&mut output), _ => {} }
            match &stmt { Stmt::Local(tokens) => tokens.to_tokens(&mut output), _ => {} }
            match &expr { Expr::Binary(tokens) => tokens.to_tokens(&mut output), _ => {} }
            match &ty { Type::Reference(tokens) => tokens.to_tokens(&mut output), _ => {} }
            match &const_ty { Type::Const(tokens) => tokens.to_tokens(&mut output), _ => {} }
            match &pat { Pat::Enum(tokens) => tokens.to_tokens(&mut output), _ => {} }
            output
        }
    "#;

    let source = format!(
        "{}\n{syn}\n{source}",
        include_str!("../../std/std/proc_macro.rid")
    );
    let result = check_with_options(&source, CompileOptions::default());
    assert!(
        result.success(),
        "parse={:?} hir={:?} type={:?} analysis={:?}",
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
}

#[test]
fn syn_visit_and_fold_api_type_checks() {
    let syn = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../std/std/syn.rid"),
    )
    .expect("the built-in syn module source should exist");
    let source = r#"
        use crate::std::vector::Vector;
        use syn::{File, Fold, Item, Visit, fold_file, parse_str, walk_item};

        struct ItemVisitor { pub count: usize }

        impl Visit for ItemVisitor {
            fun visit_item(&mut self, node: &Item) {
                self.count += 1usize;
                walk_item(self, node);
            }
        }

        struct IdentityFolder {}
        impl Fold for IdentityFolder {}

        fun transform() -> File {
            let file = match parse_str::<File>("const A: i32 = 1; fun answer() -> i32 { A }") {
                Result::Ok(value) => value,
                Result::Err(error) => { error.emit(); return File { stmts: Vector::new() }; },
            };
            let mut visitor = ItemVisitor { count: 0usize };
            visitor.visit_file(&file);
            let folded = match parse_str::<File>("const A: i32 = 1; fun answer() -> i32 { A }") {
                Result::Ok(value) => value,
                Result::Err(error) => { error.emit(); return File { stmts: Vector::new() }; },
            };
            let mut folder = IdentityFolder {};
            fold_file(&mut folder, folded)
        }
    "#;

    let source = format!(
        "{}\n{syn}\n{source}",
        include_str!("../../std/std/proc_macro.rid")
    );
    let result = check_with_options(&source, CompileOptions::default());
    assert!(
        result.success(),
        "parse={:?} hir={:?} type={:?} analysis={:?}",
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics,
        result.analysis_diagnostics
    );
}

#[test]
fn derive_expansion_preserves_the_item_and_declared_order() {
    let source = "#[derive(macros::First, macros::Second)]\nstruct User {}";
    let mut provider = FakeProvider::default();
    let expanded = expand_source(source, &mut provider);

    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}",
        expanded.diagnostics
    );
    assert!(expanded.source.contains("struct User {}"));
    let first = expanded.source.find("const FIRST").unwrap();
    let second = expanded.source.find("const SECOND").unwrap();
    assert!(first < second, "{}", expanded.source);
    assert_eq!(provider.calls.len(), 2);
    assert_eq!(provider.calls[0].0, "macros");
    assert_eq!(provider.calls[0].1, "First");
    assert_eq!(provider.calls[0].2, "struct User {}");
    assert_eq!(provider.calls[1].2, provider.calls[0].2);
}

#[test]
fn derive_helper_attributes_are_scoped_to_the_registered_derive() {
    let source = r"
        use macros::Helpers;
        #[answer]
        #[derive(Helpers)]
        struct User {
            #[answer(value)]
            value: i32,
        }
    ";
    let mut provider = FakeProvider::default();
    let expanded = expand_source(source, &mut provider);

    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}",
        expanded.diagnostics
    );
    assert_eq!(provider.calls.len(), 1);

    let source = "#[unknown]\n#[derive(macros::Helpers)]\nstruct User {}";
    let mut provider = FakeProvider::default();
    let expanded = expand_source(source, &mut provider);
    assert!(expanded.diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message
            .contains("derive helper attributes must be declared")
    }));
    assert!(provider.calls.is_empty());
}

#[test]
fn generated_impl_reenters_the_existing_pipeline() {
    let source = r"
        trait Named { fun name(&self) -> i32; }
        #[derive(macros::Named)]
        struct User {}
        fun main() -> i32 { User {}.name() }
    ";
    let mut provider = FakeProvider::default();
    let expanded = expand_source(source, &mut provider);
    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}",
        expanded.diagnostics
    );

    let result = check_with_options(&expanded.source, CompileOptions::default());
    assert!(
        result.success(),
        "hir={:?} type={:?}",
        result.hir_diagnostics,
        result.type_result.diagnostics
    );
}

#[test]
fn invalid_macro_output_is_rejected_before_hir() {
    let source = "#[derive(macros::Invalid)]\nstruct User {}";
    let mut provider = FakeProvider::default();
    let expanded = expand_source(source, &mut provider);

    assert!(expanded.insertions.is_empty());
    assert!(expanded.diagnostics.iter().any(|diagnostic| {
        diagnostic.severity == Severity::Error
            && diagnostic
                .message
                .contains("must contain only top-level items")
    }));
}

#[test]
fn unqualified_derive_names_require_an_import() {
    let source = "#[derive(Display)]\nstruct User {}";
    let mut provider = FakeProvider::default();
    let expanded = expand_source(source, &mut provider);

    assert!(provider.calls.is_empty());
    assert!(expanded.diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message
            .contains("cannot find derive macro `Display`")
    }));
}

#[test]
fn imported_derive_names_use_a_separate_macro_namespace() {
    let source = r"
        use macros::{First, Second as Other};
        struct First {}
        #[derive(First, Other)]
        struct User {}
    ";
    let mut provider = FakeProvider::default();
    let expanded = expand_source(source, &mut provider);

    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}",
        expanded.diagnostics
    );
    assert_eq!(provider.calls.len(), 2);
    assert_eq!(provider.calls[0].0, "macros");
    assert_eq!(provider.calls[0].1, "First");
    assert_eq!(provider.calls[1].0, "macros");
    assert_eq!(provider.calls[1].1, "Second");
    assert!(
        !expanded.source.contains("use macros"),
        "{}",
        expanded.source
    );
    assert!(expanded.source.contains("struct First {}"));

    let result = check_with_options(&expanded.source, CompileOptions::default());
    assert!(result.success(), "{:?}", result.hir_diagnostics);
}

#[test]
fn expansion_indexes_macro_bindings_and_uses_before_erasing_them() {
    let source = r"
        use macros::{First as Inspect};
        #[derive(Inspect)]
        struct User {}
    ";
    let mut provider = FakeProvider::default();
    let expanded = expand_source(source, &mut provider);

    let occurrences = expanded
        .macro_occurrences
        .iter()
        .filter(|occurrence| occurrence.name == "Inspect")
        .collect::<Vec<_>>();
    assert_eq!(occurrences.len(), 2, "{:#?}", expanded.macro_occurrences);
    let declaration = occurrences
        .iter()
        .find(|occurrence| occurrence.is_declaration)
        .unwrap();
    let usage = occurrences
        .iter()
        .find(|occurrence| !occurrence.is_declaration)
        .unwrap();
    assert_eq!(&source[declaration.range.clone()], "Inspect");
    assert_eq!(&source[usage.range.clone()], "Inspect");
    assert_eq!(declaration.binding, Some(declaration.range.clone()));
    assert_eq!(usage.binding, declaration.binding);
    assert_eq!(usage.kind, ProcMacroKind::Derive);
    assert_eq!(usage.macro_name, "First");
}

#[test]
fn glob_imports_add_all_exported_derive_names() {
    let source = "use macros::*;\n#[derive(First)]\nstruct User {}";
    let mut provider = FakeProvider::default();
    let expanded = expand_source(source, &mut provider);

    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}",
        expanded.diagnostics
    );
    assert_eq!(provider.calls[0].0, "macros");
    assert_eq!(provider.calls[0].1, "First");
}

#[test]
fn derive_macros_reject_non_data_items_before_invocation() {
    let source = "use macros::First;\n#[derive(First)]\nfun user() {}";
    let mut provider = FakeProvider::default();
    let expanded = expand_source(source, &mut provider);

    assert!(provider.calls.is_empty());
    assert!(expanded.diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message
            .contains("may only be applied to structs or enums")
    }));
}

#[test]
fn generated_derive_is_expanded_recursively() {
    let source = "use macros::{Recursive, Ignored};\n#[derive(Recursive)]\nstruct User {}";
    let mut provider = FakeProvider::default();
    let expanded = expand_source(source, &mut provider);

    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}",
        expanded.diagnostics
    );
    assert_eq!(provider.calls.len(), 2);
    assert_eq!(provider.calls[1].1, "Ignored");
    assert!(expanded.source.contains("RECURSIVE_OUTPUT"));
}

#[test]
fn recursive_derive_expansion_has_a_depth_limit() {
    #[derive(Default)]
    struct Provider {
        calls: usize,
    }

    impl ProcMacroProvider for Provider {
        fn expand(
            &mut self,
            _package: &str,
            _macro_name: &str,
            _kind: ProcMacroKind,
            _input: &ProcMacroTokenStream,
            _second_input: Option<&ProcMacroTokenStream>,
            _call_site: Range<usize>,
        ) -> Result<ProcMacroExpansion, String> {
            self.calls += 1;
            Ok(ProcMacroExpansion {
                output: token_stream("#[derive(macros::Loop)] struct Generated {}"),
                diagnostics: Vec::new(),
            })
        }
    }

    let source = "#[derive(macros::Loop)]\nstruct User {}";
    let mut provider = Provider::default();
    let expanded = expand_source(source, &mut provider);

    assert_eq!(provider.calls, 32);
    assert!(expanded.diagnostics.iter().any(|diagnostic| {
        diagnostic.severity == Severity::Error && diagnostic.message.contains("maximum depth")
    }));
}

#[test]
fn derive_inside_inline_module_expands_inside_that_module() {
    let source = r"
        mod inner {
            use macros::Nested;
            #[derive(Nested)]
            struct Value {}
        }
        fun main() -> i32 { inner::generated() }
    ";
    let mut provider = FakeProvider::default();
    let expanded = expand_source(source, &mut provider);
    let result = check_with_options(&expanded.source, CompileOptions::default());

    assert!(
        expanded.diagnostics.is_empty(),
        "{:?}",
        expanded.diagnostics
    );
    assert!(result.success(), "{:?}", result.type_result.diagnostics);
    assert_eq!(provider.calls.len(), 1);
}

#[test]
fn macro_diagnostics_keep_the_call_site() {
    struct WarningProvider;
    impl ProcMacroProvider for WarningProvider {
        fn expand(
            &mut self,
            _package: &str,
            _macro_name: &str,
            _kind: ProcMacroKind,
            _input: &ProcMacroTokenStream,
            _second_input: Option<&ProcMacroTokenStream>,
            call_site: Range<usize>,
        ) -> Result<ProcMacroExpansion, String> {
            Ok(ProcMacroExpansion {
                output: ProcMacroTokenStream::default(),
                diagnostics: vec![ProcMacroDiagnostic {
                    severity: Severity::Warning,
                    message: "macro warning".into(),
                    span: call_site,
                }],
            })
        }
    }

    let source = "#[derive(macros::Warn)]\nstruct User {}";
    let mut provider = WarningProvider;
    let expanded = expand_source(source, &mut provider);
    let diagnostic = &expanded.diagnostics[0];
    assert_eq!(diagnostic.severity, Severity::Warning);
    assert_eq!(usize::from(diagnostic.labels[0].range.start()), 0);
    assert!(usize::from(diagnostic.labels[0].range.end()) > 0);
}

#[test]
fn one_failed_derive_does_not_block_later_derives() {
    struct Provider;
    impl ProcMacroProvider for Provider {
        fn expand(
            &mut self,
            _package: &str,
            macro_name: &str,
            _kind: ProcMacroKind,
            _input: &ProcMacroTokenStream,
            _second_input: Option<&ProcMacroTokenStream>,
            _call_site: Range<usize>,
        ) -> Result<ProcMacroExpansion, String> {
            if macro_name == "Rejected" {
                return Err("rejected".into());
            }
            Ok(ProcMacroExpansion {
                output: token_stream("const LATER: i32 = 1;"),
                diagnostics: Vec::new(),
            })
        }
    }

    let source = "#[derive(macros::Rejected, macros::Later)]\nstruct Value {}";
    let expanded = expand_source(source, &mut Provider);

    assert!(
        expanded
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("failed to expand"))
    );
    assert!(
        expanded.source.contains("const LATER"),
        "{}",
        expanded.source
    );
}

#[test]
fn deep_nesting_parse_stays_bounded() {
    // Debug-build frames need a large stack for error recovery; the CLIs run
    // their pipelines on a 256 MiB stack.
    let handle = std::thread::Builder::new()
        .stack_size(256 * 1024 * 1024)
        .spawn(deep_nesting_parse_stays_bounded_inner)
        .unwrap();
    handle.join().unwrap();
}

fn deep_nesting_parse_stays_bounded_inner() {
    let n = std::env::var("RIDDLE_DEEP_N")
        .ok()
        .and_then(|v| v.parse::<usize>().ok())
        .unwrap_or(8000);
    let mut source = String::new();
    source.push_str(
        "fun main() -> i32 {
    let x = ",
    );
    source.push_str(&"(".repeat(n));
    source.push('1');
    source.push_str(&")".repeat(n));
    source.push_str(
        ";
    0
}
",
    );
    // The lexer-parser-tree_builder pipeline must reject the input through
    // the nesting diagnostic, and the macro-expansion loop (which re-parses
    // per iteration) must terminate with the same diagnostic instead of
    // overflowing the stack.
    let tokens = frontend::lexer::lex(&source);
    let parse = frontend::parser::Parser::new(&source, tokens).parse();
    let tree = frontend::tree_builder::build_tree(&parse.0, &parse.1, parse.3, parse.2);
    assert!(!tree.errors.is_empty(), "deep nesting must be rejected");

    let expansion = riddlec::proc_macro::expand_standard_macros(&source);
    assert!(
        !expansion.diagnostics.is_empty() || expansion.parse.is_some(),
        "expansion must terminate on deep input"
    );
}
