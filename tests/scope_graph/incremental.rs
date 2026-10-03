use frontend::incremental::IncrementalParser;

use crate::{
    DefKind, build_incremental_graph, module_id_by_name, replace_once, resolve_paths,
    resolved_struct_id, struct_id_in_module,
};

#[test]
fn set_source_reuses_the_parse_when_the_text_is_unchanged() {
    let mut parser = IncrementalParser::new();
    let source = "fun main() { let value = 1; }\n";
    let first = parser.set_source(source).green.clone();
    let second = parser.set_source(source).green.clone();

    // The language server hands the same bundled source (user code plus the
    // standard library) to this every time it checks a document, so a reparse
    // here is the dominant cost of a keystroke.
    assert!(
        second == first,
        "parsing identical text must reuse the previous green tree"
    );

    // An actual edit must still produce a new tree.
    let edited = parser
        .set_source("fun main() { let value = 2; }\n")
        .green
        .clone();
    assert!(edited != first, "a changed source must be parsed again");
}

#[test]
fn incremental_impl_method_body_rebuilds_resolution() {
    let mut parser = IncrementalParser::new();
    parser.set_source(
        r"
        struct S {}

        impl S {
            fun value(arg: i32) {
                arg;
            }
        }
        ",
    );

    let (_, sg) = build_incremental_graph(&parser);
    assert_eq!(resolve_paths(&sg, "arg"), vec![vec![DefKind::Param]]);

    let offset = parser.source().rfind("arg").unwrap();
    parser.apply_edit(offset, "arg".len(), "bad");
    assert!(
        matches!(
            parser.last_reparse_mode(),
            frontend::incremental::ReparseMode::Incremental(_)
        ),
        "expected incremental reparse after replacing body `arg`, got {:?}",
        parser.last_reparse_mode()
    );
    let (_, sg) = build_incremental_graph(&parser);
    assert_eq!(resolve_paths(&sg, "arg"), Vec::<Vec<DefKind>>::new());
    assert_eq!(resolve_paths(&sg, "bad"), vec![vec![]]);
}

#[test]
fn incremental_multi_edit_local_shadowing_rebuilds_statement_chain() {
    let mut parser = IncrementalParser::new();
    parser.set_source(
        r"
        fun f(value: int) {
            let left = value;
            left;
        }
        ",
    );

    let (_, sg) = build_incremental_graph(&parser);
    assert_eq!(resolve_paths(&sg, "value"), vec![vec![DefKind::Param]]);
    assert_eq!(resolve_paths(&sg, "left"), vec![vec![DefKind::Local]]);

    replace_once(&mut parser, "left", "right");
    let (_, sg) = build_incremental_graph(&parser);
    assert_eq!(resolve_paths(&sg, "value"), vec![vec![DefKind::Param]]);
    assert_eq!(resolve_paths(&sg, "left"), vec![vec![]]);
    assert_eq!(resolve_paths(&sg, "right"), Vec::<Vec<DefKind>>::new());

    replace_once(&mut parser, "left", "right");
    let (_, sg) = build_incremental_graph(&parser);
    assert_eq!(resolve_paths(&sg, "value"), vec![vec![DefKind::Param]]);
    assert_eq!(resolve_paths(&sg, "right"), vec![vec![DefKind::Local]]);

    replace_once(&mut parser, "right", "left");
    let (_, sg) = build_incremental_graph(&parser);
    assert_eq!(resolve_paths(&sg, "right"), vec![vec![]]);
    assert_eq!(resolve_paths(&sg, "left"), Vec::<Vec<DefKind>>::new());

    replace_once(&mut parser, "right", "left");
    let (_, sg) = build_incremental_graph(&parser);
    assert_eq!(resolve_paths(&sg, "right"), Vec::<Vec<DefKind>>::new());
    assert_eq!(resolve_paths(&sg, "left"), vec![vec![DefKind::Local]]);
}

#[test]
fn incremental_multi_edit_inline_module_definition_updates_path_resolution() {
    let mut parser = IncrementalParser::new();
    parser.set_source(
        r"
        fun f() {
            mod local {
                pub struct Hidden {}
            }

            local::Thing;
        }
        ",
    );

    let (_, sg) = build_incremental_graph(&parser);
    assert_eq!(resolve_paths(&sg, "local::Thing"), vec![vec![]]);

    replace_once(&mut parser, "Hidden", "Thing");
    let (hir, sg) = build_incremental_graph(&parser);
    let local_mid = module_id_by_name(&hir, "local");
    let local_thing = struct_id_in_module(&hir, local_mid, "Thing");
    assert_eq!(resolved_struct_id(&sg, "local::Thing"), local_thing);

    replace_once(&mut parser, "Thing", "Hidden");
    let (_, sg) = build_incremental_graph(&parser);
    assert_eq!(resolve_paths(&sg, "local::Thing"), vec![vec![]]);

    replace_once(&mut parser, "Hidden", "Thing");
    let (hir, sg) = build_incremental_graph(&parser);
    let local_mid = module_id_by_name(&hir, "local");
    let local_thing = struct_id_in_module(&hir, local_mid, "Thing");
    assert_eq!(resolved_struct_id(&sg, "local::Thing"), local_thing);
}

#[test]
fn incremental_multi_edit_use_alias_retargets_resolution() {
    let mut parser = IncrementalParser::new();
    parser.set_source(
        r"
        mod a {
            pub struct Left {}
            pub struct Right {}
        }

        use crate::a::Left as Pick;

        fun f() {
            Pick;
        }
        ",
    );

    let (hir, sg) = build_incremental_graph(&parser);
    let a_mid = module_id_by_name(&hir, "a");
    let left = struct_id_in_module(&hir, a_mid, "Left");
    assert_eq!(resolved_struct_id(&sg, "Pick"), left);

    replace_once(&mut parser, "Left", "Gone");
    let (_, sg) = build_incremental_graph(&parser);
    assert_eq!(resolve_paths(&sg, "Pick"), vec![vec![]]);

    replace_once(&mut parser, "Right", "Left");
    let (hir, sg) = build_incremental_graph(&parser);
    let a_mid = module_id_by_name(&hir, "a");
    let new_left = struct_id_in_module(&hir, a_mid, "Left");
    assert_eq!(resolved_struct_id(&sg, "Pick"), new_left);

    replace_once(&mut parser, "Pick", "Alias");
    let (_, sg) = build_incremental_graph(&parser);
    assert_eq!(resolve_paths(&sg, "Pick"), vec![vec![]]);
    assert_eq!(resolve_paths(&sg, "Alias"), Vec::<Vec<DefKind>>::new());

    replace_once(&mut parser, "Pick", "Alias");
    let (hir, sg) = build_incremental_graph(&parser);
    let a_mid = module_id_by_name(&hir, "a");
    let left = struct_id_in_module(&hir, a_mid, "Left");
    assert_eq!(resolved_struct_id(&sg, "Alias"), left);
}
