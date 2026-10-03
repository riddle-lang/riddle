use crate::{lower_and_resolve, messages};
use frontend::incremental::{IncrementalParser, ReparseMode};
use type_checker::{Diagnostic, IncrementalTypeChecker, TypeCheckResult, check_hir};

fn diagnostics_with_code(result: &TypeCheckResult, code: &str) -> Vec<Diagnostic> {
    result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == code)
        .cloned()
        .collect()
}

#[test]
fn incremental_reports_unsized_declarations() {
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source(
        r"
        struct Bad { value: str }
        fun main() {}
        ",
    );
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);

    let mut checker = IncrementalTypeChecker::new();
    let hir = lower_and_resolve(parse);
    let result = checker.check(&hir);
    assert!(
        result
            .result
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E0043")
    );
}

#[test]
fn incremental_rechecks_const_initializers_when_functions_are_reused() {
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source(
        r"
        const BAD: i32 = true;

        fun stable() -> i32 { 1 }
        ",
    );
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);

    let mut checker = IncrementalTypeChecker::new();
    let hir = lower_and_resolve(parse);
    let first = checker.check_with_syntax(&hir, &parser.current_parse().unwrap().syntax());
    assert_eq!(diagnostics_with_code(&first.result, "E0001").len(), 1);

    let offset = parser.source().find("1 }").unwrap();
    parser.apply_edit(offset, 1, "2");
    let parse = parser.current_parse().unwrap();
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);
    let hir = lower_and_resolve(parse);
    let second = checker.check_with_syntax(&hir, &parser.current_parse().unwrap().syntax());
    assert_eq!(diagnostics_with_code(&second.result, "E0001").len(), 1);
}

#[test]
fn incremental_public_check_invalidates_moved_spans() {
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source("fun bad(value: str) {}");
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);

    let mut checker = IncrementalTypeChecker::new();
    let hir = lower_and_resolve(parse);
    let first = checker.check(&hir);
    assert_eq!(
        diagnostics_with_code(&first.result, "E0043"),
        diagnostics_with_code(&check_hir(&hir), "E0043")
    );

    parser.apply_edit(0, 0, "// moved\n");
    let parse = parser.current_parse().unwrap();
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);
    let hir = lower_and_resolve(parse);
    let second = checker.check(&hir);

    assert_eq!(second.stats.checked_bodies, 1);
    assert_eq!(second.stats.reused_bodies, 0);
    assert_eq!(
        diagnostics_with_code(&second.result, "E0043"),
        diagnostics_with_code(&check_hir(&hir), "E0043")
    );
}

#[test]
fn the_type_context_fingerprint_is_a_function_of_the_tree() {
    // A fingerprint that depends on anything but the tree cannot be compared
    // across requests, and the incremental checker compares it on every one.
    use type_checker::incremental::context_fingerprint_parts;

    let source = "pub fun helper_a() -> i32 { 1 }\nfun main() { let value = helper_a(); }\n";
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source(source);
    let first: (u64, u64, u64) = {
        let hir = lower_and_resolve(parse);
        context_fingerprint_parts(&hir.item_tree)
    };

    // Same text, parsed afresh: a second, independent `HirFile`.
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source(source);
    let second: (u64, u64, u64) = {
        let hir = lower_and_resolve(parse);
        context_fingerprint_parts(&hir.item_tree)
    };

    assert_eq!(
        first, second,
        "fingerprinting identical source twice must agree: {first:#x?} vs {second:#x?}"
    );
}

#[test]
fn a_standard_library_suffix_survives_user_code_edits() {
    // The pipeline bundles user code *first* and the standard library after it
    // (`format!("{source}\n\n{std_prelude()}")`). Anything keyed on the suffix's
    // byte offsets therefore moves whenever the user's code changes length, and
    // the whole standard library stops matching its cache — even though not one
    // character of it changed.
    let std_like = "pub fun helper_a() -> i32 { 1 }\npub fun helper_b() -> i32 { 2 }\n";
    let bundle = |user: &str| format!("{user}\n\n{std_like}");

    let mut parser = IncrementalParser::new();
    let first_parse = parser.set_source(&bundle("fun main() { 1 }\n"));
    let mut checker = IncrementalTypeChecker::new();
    let first_hir = lower_and_resolve(first_parse);
    let first = checker.check_with_syntax(&first_hir, &first_parse.syntax());
    assert_eq!(first.stats.reused_bodies, 0);
    let std_bodies = first.stats.checked_bodies;

    // Edit only the user's function; the suffix is byte-identical but moved.
    let mut parser = IncrementalParser::new();
    let second_parse = parser.set_source(&bundle("fun main() { 100 }\n"));
    let second_hir = lower_and_resolve(second_parse);
    let second = checker.check_with_syntax(&second_hir, &second_parse.syntax());

    assert!(
        second.stats.reused_bodies >= std_bodies.saturating_sub(1),
        "the standard library suffix must stay cached across a user edit: \
         checked {}, reused {}, absent {}, missed_body {}",
        second.stats.checked_bodies,
        second.stats.reused_bodies,
        second.stats.missed_absent,
        second.stats.missed_body,
    );
}

#[test]
fn the_type_context_ignores_edits_outside_declarations() {
    // The pipeline bundles the user's source *first* and the standard library
    // after it, so editing the user's code moves every standard-library byte.
    // If that movement alone can change the type-context fingerprint, every
    // cached body is discarded — including the ~827 standard-library ones that
    // nothing actually changed.
    use type_checker::incremental::context_fingerprint_parts;

    let std_like = r"
        pub struct Pair { pub left: i32, pub right: i32 }
        pub trait Show { fun show(self) -> str }
        pub fun helper(value: i32) -> i32 { value }
        pub fun other(value: bool) -> bool { value }
    ";

    let fingerprint = |user: &str| {
        let mut parser = IncrementalParser::new();
        let bundle = format!("{user}\n\n{std_like}");
        let parse = parser.set_source(&bundle);
        let hir = lower_and_resolve(parse);
        context_fingerprint_parts(&hir.item_tree)
    };

    let short = fingerprint("fun main() { 1 }\n");
    // Same declarations, one character longer: only a *body* got bigger.
    let long = fingerprint("fun main() { 100 }\n");

    assert_eq!(
        short, long,
        "an edit inside a function body must not move the type context: {short:#x?} vs {long:#x?}"
    );
}

#[test]
fn the_type_context_ignores_edits_outside_declarations_against_the_real_std() {
    // Same idea as the synthetic case, but with the standard library the server
    // actually bundles: the synthetic one did not reproduce the failure, so the
    // offending hash input is somewhere in the real prelude.
    use type_checker::incremental::context_fingerprint_parts;

    let fingerprint = |user: &str| {
        let mut parser = IncrementalParser::new();
        let bundle = format!("{user}\n\n{}", riddlec::pipeline::expanded_std_prelude());
        let parse = parser.set_source(&bundle);
        let hir = lower_and_resolve(parse);
        context_fingerprint_parts(&hir.item_tree)
    };

    let short = fingerprint("fun main() { let value = 1; }\n");
    let long = fingerprint("fun main() { let value = 100; }\n");

    assert_eq!(
        short, long,
        "a longer function body must not move the type context, or every \
         standard-library body is re-checked on each keystroke \
         (declarations, structure, signatures were {short:#x?} then {long:#x?})"
    );
}

#[test]
fn repeated_checks_of_identical_inputs_reuse_every_body() {
    // The language server checks the same bundled source (user code plus the
    // standard library) repeatedly between edits. Nothing changed, so nothing
    // may be re-checked.
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source("fun main() { let value = 1; }\n");
    let mut checker = IncrementalTypeChecker::new();

    let hir = lower_and_resolve(parse);
    let first = checker.check_with_syntax(&hir, &parse.syntax());
    assert!(
        first.stats.checked_bodies > 0,
        "the first pass checks bodies"
    );
    assert_eq!(first.stats.reused_bodies, 0);
    let known = checker.known_bodies();
    assert_eq!(known, first.stats.checked_bodies);

    // Same text, same syntax: the second pass must come entirely from cache.
    let second = checker.check_with_syntax(&hir, &parse.syntax());
    assert_eq!(
        second.stats.checked_bodies,
        0,
        "re-checking identical input must not recheck any body \
         (absent {}, missed_context {}, missed_body {}, missed_replay {})",
        second.stats.missed_absent,
        second.stats.missed_context,
        second.stats.missed_body,
        second.stats.missed_replay,
    );
    assert_eq!(second.stats.reused_bodies, known);
}

#[test]
fn incremental_type_checker_reuses_unchanged_bodies() {
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source(
        r"
        fun stable() -> i32 {
            1
        }

        fun edited() -> bool {
            let value: bool = true;
            value
        }
        ",
    );
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);

    let mut checker = IncrementalTypeChecker::new();
    let hir = lower_and_resolve(parse);
    let first = checker.check(&hir);
    assert_eq!(first.result.diagnostics, vec![]);
    assert_eq!(first.stats.checked_bodies, 2);
    assert_eq!(first.stats.reused_bodies, 0);

    let offset = parser.source().find("true").unwrap();
    parser.apply_edit(offset, "true".len(), "1");
    assert!(matches!(
        parser.last_reparse_mode(),
        ReparseMode::Incremental(_)
    ));
    let parse = parser.current_parse().unwrap();
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);

    let hir = lower_and_resolve(parse);
    let second = checker.check(&hir);
    assert_eq!(second.stats.checked_bodies, 1);
    assert_eq!(second.stats.reused_bodies, 1);
    assert!(
        messages(&second.result)
            .iter()
            .any(|msg| msg.contains("let initializer type mismatch"))
    );
}

#[test]
fn incremental_replay_preserves_pattern_types() {
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source(
        r"
        struct Token {}
        enum MaybeToken { Some(Token), None }

        fun stable(value: &MaybeToken) {
            match value {
                MaybeToken::Some(token) => {},
                MaybeToken::None => {},
            }
        }

        fun edited() { 0; }
        ",
    );
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);

    let mut checker = IncrementalTypeChecker::new();
    let hir = lower_and_resolve(parse);
    let first = checker.check(&hir);
    assert_eq!(first.result.pattern_types.len(), 3);
    assert_eq!(first.result.pattern_binding_types.len(), 1);
    assert_eq!(first.result.pattern_binding_modes.len(), 1);

    let offset = parser.source().find("0;").unwrap();
    parser.apply_edit(offset, 1, "1");
    let parse = parser.current_parse().unwrap();
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);
    let hir = lower_and_resolve(parse);
    let second = checker.check(&hir);

    assert_eq!(second.stats.reused_bodies, 1);
    assert_eq!(second.result.pattern_types.len(), 3);
    assert_eq!(second.result.pattern_binding_types.len(), 1);
    assert_eq!(second.result.pattern_binding_modes.len(), 1);
    assert_eq!(second.result.pattern_types, check_hir(&hir).pattern_types);
    assert_eq!(
        second.result.pattern_binding_types,
        check_hir(&hir).pattern_binding_types
    );
    assert_eq!(
        second.result.pattern_binding_modes,
        check_hir(&hir).pattern_binding_modes
    );
}

#[test]
fn incremental_trait_impl_edit_updates_contract_diagnostics() {
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source(
        r"
        trait Flag {
            fun value() -> bool;
        }

        struct Marker {}

        impl Flag for Marker {
            fun value() -> bool { 1 == 1 }
        }
        ",
    );
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);

    let mut checker = IncrementalTypeChecker::new();
    let hir = lower_and_resolve(parse);
    let first = checker.check(&hir);
    assert_eq!(first.result.diagnostics, vec![]);

    let offset = parser.source().find("bool").unwrap();
    parser.apply_edit(offset, "bool".len(), "i32");
    assert!(matches!(
        parser.last_reparse_mode(),
        ReparseMode::Incremental(_)
    ));
    let parse = parser.current_parse().unwrap();
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);

    let hir = lower_and_resolve(parse);
    let second = checker.check(&hir);
    assert!(
        messages(&second.result)
            .iter()
            .any(|msg| msg.contains("impl method `value` for trait `Flag` return type mismatch"))
    );
}

#[test]
fn incremental_function_safety_edit_invalidates_callers() {
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source(
        r"
        fun operation() {}

        fun main() {
            operation();
        }
        ",
    );
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);

    let mut checker = IncrementalTypeChecker::new();
    let hir = lower_and_resolve(parse);
    let first = checker.check(&hir);
    assert!(diagnostics_with_code(&first.result, "E0046").is_empty());

    let offset = parser.source().find("fun operation").unwrap();
    parser.apply_edit(offset, 0, "unsafe ");
    let parse = parser.current_parse().unwrap();
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);

    let hir = lower_and_resolve(parse);
    let second = checker.check(&hir);
    assert_eq!(second.stats.checked_bodies, 2);
    assert_eq!(second.stats.reused_bodies, 0);
    assert_eq!(diagnostics_with_code(&second.result, "E0046").len(), 1);
}

#[test]
fn incremental_field_visibility_edit_invalidates_callers() {
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source(
        r"
        mod model {
            pub struct Point { pub value: i32 }
            pub fun make() -> Point { Point { value: 1 } }
        }

        fun main() -> i32 { model::make().value }
        ",
    );
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);

    let mut checker = IncrementalTypeChecker::new();
    let hir = lower_and_resolve(parse);
    let first = checker.check_with_syntax(&hir, &parse.syntax());
    assert!(diagnostics_with_code(&first.result, "E0054").is_empty());

    let offset = parser.source().find("pub value: i32").unwrap();
    parser.apply_edit(offset, "pub ".len(), "    ");
    let parse = parser.current_parse().unwrap();
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);

    let hir = lower_and_resolve(parse);
    let second = checker.check_with_syntax(&hir, &parse.syntax());
    assert_eq!(diagnostics_with_code(&second.result, "E0054").len(), 1);
    assert_eq!(
        diagnostics_with_code(&second.result, "E0054"),
        diagnostics_with_code(&check_hir(&hir), "E0054")
    );
}

#[test]
fn incremental_generic_recursion_matches_full_check_before_and_after_reuse() {
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source(
        r"
        struct Wrap<T> { inner: T }

        fun f<T>(x: T) -> T {
            g(Wrap { inner: x })
        }

        fun g<T>(x: T) -> T {
            f(Wrap { inner: x })
        }

        fun edited() {
            0;
        }
        ",
    );
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);

    let mut checker = IncrementalTypeChecker::new();
    let hir = lower_and_resolve(parse);
    let full = check_hir(&hir);
    let first = checker.check(&hir);
    let expected = diagnostics_with_code(&full, "E0033");

    assert!(!expected.is_empty());
    assert_eq!(diagnostics_with_code(&first.result, "E0033"), expected);
    assert_eq!(first.stats.checked_bodies, 3);
    assert_eq!(first.stats.reused_bodies, 0);

    let offset = parser.source().find("0;").unwrap();
    parser.apply_edit(offset, 1, "1");
    assert!(matches!(
        parser.last_reparse_mode(),
        ReparseMode::Incremental(_)
    ));
    let parse = parser.current_parse().unwrap();
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);

    let hir = lower_and_resolve(parse);
    let full = check_hir(&hir);
    let second = checker.check(&hir);

    assert_eq!(second.stats.checked_bodies, 1);
    assert_eq!(second.stats.reused_bodies, 2);
    assert_eq!(
        diagnostics_with_code(&second.result, "E0033"),
        diagnostics_with_code(&full, "E0033")
    );
}
