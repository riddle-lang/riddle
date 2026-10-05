use super::*;

#[test]
fn completion_filters_keywords_globals_and_locals_by_prefix() {
    let source = "struct Widget {}\nfun helper() {}\nfun main(value: i32) { let local = 1; loc }";
    let local_position = position(source, source.rfind("loc").unwrap() + 3);
    let local = completion_items_for_source(
        source,
        local_position,
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert_eq!(
        local
            .iter()
            .map(|item| item.label.as_str())
            .collect::<Vec<_>>(),
        vec!["local"]
    );

    let helper_start = source.find("helper() {}").unwrap();
    let helper_position = position(source, helper_start + 3);
    let globals = completion_items_for_source(
        source,
        helper_position,
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    let helper = globals.iter().find(|item| item.label == "helper").unwrap();
    let helper_label = helper.label_details.as_ref().unwrap();
    assert_eq!(helper_label.detail.as_deref(), Some("()"));
    assert_eq!(helper_label.description.as_deref(), Some("()"));

    let keyword_source = "fun main() { ret }";
    let keywords = completion_items_for_source(
        keyword_source,
        position(keyword_source, keyword_source.find("ret").unwrap() + 3),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(keywords.iter().any(|item| item.label == "return"));

    let loop_source = "fun main() { loo }";
    let loop_items = completion_items_for_source(
        loop_source,
        position(loop_source, loop_source.find("loo").unwrap() + 3),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(loop_items.iter().any(|item| item.label == "loop"));
}

#[test]
fn completion_matches_free_functions_case_insensitively() {
    let source = "fun Foo() {} fun main() { f }";
    let items = completion_items_for_source(
        source,
        position(source, source.rfind("f }").unwrap() + 1),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(items.iter().any(|item| item.label == "Foo"), "{items:#?}");
}

#[test]
fn completion_uses_a_precise_plain_text_edit_for_the_prefix() {
    let source = "fun helper(value: i32) -> i32 {}\nfun main() { hel }";
    let prefix_start = source.rfind("hel").unwrap();
    let cursor = position(source, prefix_start + 3);
    let items = completion_items_for_source(
        source,
        cursor,
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    let helper = items.iter().find(|item| item.label == "helper").unwrap();

    assert_eq!(
        helper.insert_text_format,
        Some(lsp_types::InsertTextFormat::PLAIN_TEXT)
    );
    let Some(lsp_types::CompletionTextEdit::Edit(edit)) = helper.text_edit.as_ref() else {
        panic!("expected a text edit: {helper:#?}");
    };
    assert_eq!(
        edit.range,
        Range::new(position(source, prefix_start), cursor)
    );
    assert_eq!(edit.new_text, "helper");
}

#[test]
fn completion_includes_struct_and_enum_names_in_type_positions() {
    let source = "struct Point {}\nenum State { Ready }\nfun inspect(value: ) {}";
    let items = completion_items_for_source(
        source,
        position(source, source.find(") {}").unwrap()),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(
        items.iter().any(|item| {
            item.label == "Point" && item.kind == Some(lsp_types::CompletionItemKind::STRUCT)
        }),
        "{items:#?}"
    );
    assert!(
        items.iter().any(|item| {
            item.label == "State" && item.kind == Some(lsp_types::CompletionItemKind::ENUM)
        }),
        "{items:#?}"
    );
    assert!(
        !items
            .iter()
            .any(|item| { item.kind == Some(lsp_types::CompletionItemKind::KEYWORD) }),
        "{items:#?}"
    );
    assert!(
        !items.iter().any(|item| item.label == "inspect"),
        "{items:#?}"
    );
    for unsupported in ["i128", "u128", "f16", "f128"] {
        assert!(
            !items.iter().any(|item| item.label == unsupported),
            "unexpected unsupported type {unsupported}: {items:#?}"
        );
    }
}

#[test]
fn completion_keeps_private_imports_and_generics_in_type_positions() {
    let imported =
        "mod models { pub struct Widget {} } use crate::models::Widget; fun inspect(value: Wid) {}";
    let imported_items = completion_items_for_source(
        imported,
        position(imported, imported.rfind("Wid").unwrap() + 3),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(
        imported_items.iter().any(|item| item.label == "Widget"),
        "{imported_items:#?}"
    );

    let generic = "fun inspect<T>(value: T) {}";
    let generic_items = completion_items_for_source(
        generic,
        position(generic, generic.rfind('T').unwrap() + 1),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(
        generic_items.iter().any(|item| item.label == "T"),
        "{generic_items:#?}"
    );
}

#[test]
fn completion_keeps_expression_candidates_after_struct_field_colons() {
    let source =
        "struct Foo { field: i32 }\nfun main() { let value = 1; let item = Foo { field: val }; }";
    let items = completion_items_for_source(
        source,
        position(source, source.find("val }").unwrap() + 3),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(items.iter().any(|item| item.label == "value"), "{items:#?}");
}

#[test]
fn completion_includes_missing_fields_in_literals_and_patterns() {
    let literal = "struct Point { x: i32, y: bool }\nenum Event { Move { dx: i32, dy: i32 } }\nfun main() { let point = Point { x: 1,  }; let event = Event::Move { dx: 1, d }; }";
    let point_items = completion_items_for_source(
        literal,
        position(literal, literal.find("x: 1,  }").unwrap() + "x: 1, ".len()),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(point_items.iter().any(|item| {
        item.label == "y" && item.kind == Some(lsp_types::CompletionItemKind::FIELD)
    }));
    assert!(!point_items.iter().any(|item| item.label == "x"));

    let variant_items = completion_items_for_source(
        literal,
        position(
            literal,
            literal.find("dx: 1, d").unwrap() + "dx: 1, d".len(),
        ),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(variant_items.iter().any(|item| item.label == "dy"));
    assert!(!variant_items.iter().any(|item| item.label == "dx"));

    let pattern = "struct Point { x: i32, y: bool }\nenum Event { Move { dx: i32, dy: i32 } }\nfun inspect(point: Point, event: Event) { match point { Point { x,  } => {} } match event { Event::Move { dx, d } => {} } }";
    let point_pattern_items = completion_items_for_source(
        pattern,
        position(pattern, pattern.find("x,  }").unwrap() + "x, ".len()),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(point_pattern_items.iter().any(|item| item.label == "y"));
    assert!(!point_pattern_items.iter().any(|item| item.label == "x"));

    let variant_pattern_items = completion_items_for_source(
        pattern,
        position(pattern, pattern.find("dx, d").unwrap() + "dx, d".len()),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(variant_pattern_items.iter().any(|item| item.label == "dy"));
    assert!(!variant_pattern_items.iter().any(|item| item.label == "dx"));

    let visibility = "mod model { pub struct Point { hidden: i32, pub shown: i32 } }\nfun main() { let point = model::Point {  }; }";
    let visibility_items = completion_items_for_source(
        visibility,
        position(
            visibility,
            visibility.find("Point {  }").unwrap() + "Point { ".len(),
        ),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(visibility_items.iter().any(|item| item.label == "shown"));
    assert!(!visibility_items.iter().any(|item| item.label == "hidden"));
}

#[test]
fn completion_resolves_import_paths() {
    let module = "mod model { pub struct Widget {} pub fun make() {} struct Hidden {} }\nuse crate::model::;";
    let module_items = completion_items_for_source(
        module,
        position(module, module.find("model::;").unwrap() + "model::".len()),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    let widget = module_items
        .iter()
        .find(|item| item.label == "Widget")
        .unwrap_or_else(|| panic!("{module_items:#?}"));
    assert_eq!(widget.kind, Some(lsp_types::CompletionItemKind::STRUCT));
    assert!(module_items.iter().any(|item| item.label == "make"));
    assert!(!module_items.iter().any(|item| item.label == "Hidden"));

    let root = "mod model {}\nuse mo;";
    let root_items = completion_items_for_source(
        root,
        position(root, root.find("mo;").unwrap() + 2),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(root_items.iter().any(|item| item.label == "model"));

    let list = "mod model { pub fun make() {} }\nuse crate::model::{ma};";
    let list_items = completion_items_for_source(
        list,
        position(list, list.find("ma}").unwrap() + 2),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(list_items.iter().any(|item| item.label == "make"));

    let enumeration = "enum State { Ready, Running }\nuse crate::State::Ru;";
    let variant_items = completion_items_for_source(
        enumeration,
        position(enumeration, enumeration.find("Ru;").unwrap() + 2),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(variant_items.iter().any(|item| {
        item.label == "Running" && item.kind == Some(lsp_types::CompletionItemKind::ENUM_MEMBER)
    }));
}

#[test]
fn completion_filters_qualified_type_paths() {
    let source =
        "mod model { pub struct Widget {} pub fun make() {} }\nfun inspect(value: model::Wid) {}";
    let items = completion_items_for_source(
        source,
        position(source, source.find("Wid)").unwrap() + 3),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(
        items.iter().any(|item| item.label == "Widget"),
        "{items:#?}"
    );
    assert!(!items.iter().any(|item| item.label == "make"));
    assert!(
        !items
            .iter()
            .any(|item| item.kind == Some(lsp_types::CompletionItemKind::KEYWORD))
    );
}

#[test]
fn completion_resolves_fields_and_instance_methods() {
    let source = "struct Point { x: i32, y: i32 }\nimpl Point { fun origin() -> Point { Point { x: 0, y: 0 } } fun magnitude(&self) -> i32 { self.x } fun offset(&self, value: i32) -> i32 { value } }\nfun main() { let point = Point { x: 1, y: 2 }; point. }";
    let items = completion_items_for_source(
        source,
        position(source, source.rfind("point.").unwrap() + "point.".len()),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    let labels = items
        .iter()
        .map(|item| item.label.as_str())
        .collect::<Vec<_>>();

    assert!(labels.contains(&"x"), "{items:#?}");
    assert!(labels.contains(&"y"), "{items:#?}");
    assert!(labels.contains(&"magnitude"), "{items:#?}");
    assert!(labels.contains(&"offset"), "{items:#?}");
    assert!(!labels.contains(&"origin"), "{items:#?}");

    let magnitude = items.iter().find(|item| item.label == "magnitude").unwrap();
    let magnitude_label = magnitude.label_details.as_ref().unwrap();
    assert_eq!(magnitude_label.detail.as_deref(), Some("(&self)"));
    assert_eq!(magnitude_label.description.as_deref(), Some("i32"));
    assert_eq!(magnitude.insert_text.as_deref(), Some("magnitude"));

    let offset = items.iter().find(|item| item.label == "offset").unwrap();
    let offset_label = offset.label_details.as_ref().unwrap();
    assert_eq!(offset_label.detail.as_deref(), Some("(&self, value: i32)"));
    assert_eq!(offset_label.description.as_deref(), Some("i32"));
}

#[test]
fn completion_hides_private_fields_outside_the_defining_module() {
    let source = "mod model { pub struct Point { x: i32, pub y: i32 } pub fun make() -> Point { Point { x: 1, y: 2 } } } fun main() { model::make(). }";
    let items = completion_items_for_source(
        source,
        position(
            source,
            source.rfind("model::make().").unwrap() + "model::make().".len(),
        ),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(items.iter().any(|item| item.label == "y"), "{items:#?}");
    assert!(!items.iter().any(|item| item.label == "x"), "{items:#?}");
}

#[test]
fn completion_hides_private_methods_outside_the_defining_module() {
    let member_source = "mod model { pub struct Thing {} impl Thing { fun secret(&self) {} pub fun shown(&self) {} } pub fun make() -> Thing { Thing {} } } fun main() { model::make(). }";
    let member_items = completion_items_for_source(
        member_source,
        position(
            member_source,
            member_source.rfind("model::make().").unwrap() + "model::make().".len(),
        ),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(member_items.iter().any(|item| item.label == "shown"));
    assert!(!member_items.iter().any(|item| item.label == "secret"));

    let associated_source = "mod model { pub struct Thing {} impl Thing { fun secret() {} pub fun shown() {} } } fun main() { model::Thing:: }";
    let associated_items = completion_items_for_source(
        associated_source,
        position(
            associated_source,
            associated_source.rfind("::").unwrap() + 2,
        ),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(associated_items.iter().any(|item| item.label == "shown"));
    assert!(!associated_items.iter().any(|item| item.label == "secret"));
}

#[test]
fn completion_filters_member_candidates_after_the_dot() {
    let source = "struct Foo { bar: i32 }\nfun main() { let c = Foo { bar: 1 }; c.b }";
    let items = completion_items_for_source(
        source,
        position(source, source.rfind("c.b").unwrap() + "c.b".len()),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(items.iter().any(|item| item.label == "bar"), "{items:#?}");
}

#[test]
fn completion_recovers_incomplete_let_member_access() {
    let source = "use std::vector::Vector;\n\nfun main() {\n    let c: Vector<i32> = Vector::new();\n    let d = c.\n}";
    let items = completion_items_for_source(
        source,
        position(source, source.find("c.\n").unwrap() + "c.".len()),
        CompileOptions::default(),
    );

    assert!(items.iter().any(|item| item.label == "len"), "{items:#?}");
}

#[test]
fn completion_resolves_slice_methods() {
    let source = "fun inspect(values: &[i32]) { values. }";
    let items = completion_items_for_source(
        source,
        position(source, source.find("values.").unwrap() + "values.".len()),
        CompileOptions::default(),
    );

    assert!(items.iter().any(|item| item.label == "len"), "{items:#?}");
    assert!(items.iter().any(|item| item.label == "get"), "{items:#?}");
    assert!(items.iter().any(|item| item.label == "iter"), "{items:#?}");
}

#[test]
fn completion_resolves_std_associated_functions() {
    let source = "fun main() { let value = Vector::ne; }";
    let items = completion_items_for_source(
        source,
        position(source, source.find("ne;").unwrap() + 2),
        CompileOptions::default(),
    );

    assert!(
        items.iter().any(|item| {
            item.label == "new"
                && item.label_details.as_ref().is_some_and(|label| {
                    label.detail.as_deref() == Some("()")
                        && label
                            .description
                            .as_deref()
                            .is_some_and(|description| description.contains("Vector"))
                })
                && item.kind == Some(lsp_types::CompletionItemKind::FUNCTION)
        }),
        "{items:#?}"
    );
}

#[test]
fn completion_includes_std_prelude_imports() {
    let source = "fun main() { Vec }";
    let items = completion_items_for_source(
        source,
        position(source, source.find("Vec").unwrap() + 3),
        CompileOptions::default(),
    );

    assert!(
        items.iter().any(|item| item.label == "Vector"),
        "{items:#?}"
    );
}

#[test]
fn completion_includes_standard_function_macros() {
    let source = "use std::io::print; fun main() { prin }";
    let items = completion_items_for_source(
        source,
        position(source, source.rfind("prin").unwrap() + "prin".len()),
        CompileOptions::default(),
    );

    let print = items
        .iter()
        .find(|item| item.label == "print!" && item.detail.as_deref() == Some("standard macro"))
        .unwrap_or_else(|| panic!("{items:#?}"));
    assert_eq!(print.insert_text.as_deref(), Some("print!"));
    assert_eq!(print.filter_text.as_deref(), Some("print"));

    assert!(
        !items.iter().any(|item| item.label == "print"),
        "{items:#?}"
    );

    let println = items
        .iter()
        .find(|item| item.label == "println!" && item.detail.as_deref() == Some("standard macro"))
        .unwrap_or_else(|| panic!("{items:#?}"));
    assert_eq!(println.insert_text.as_deref(), Some("println!"));
    assert_eq!(println.filter_text.as_deref(), Some("println"));

    let panic_source = "fun main() { pan }";
    let panic_items = completion_items_for_source(
        panic_source,
        position(panic_source, panic_source.find("pan").unwrap() + 3),
        CompileOptions::default(),
    );
    let panic = panic_items
        .iter()
        .find(|item| item.label == "panic!" && item.detail.as_deref() == Some("standard macro"))
        .unwrap_or_else(|| panic!("{panic_items:#?}"));
    assert_eq!(panic.insert_text.as_deref(), Some("panic!"));
    assert_eq!(panic.filter_text.as_deref(), Some("panic"));
    assert!(
        !panic_items.iter().any(|item| item.label == "panic"),
        "{panic_items:#?}"
    );

    let hidden_panic = "fun main() { std::panic::pan }";
    let panic_items = completion_items_for_source(
        hidden_panic,
        position(
            hidden_panic,
            hidden_panic.rfind("pan").unwrap() + "pan".len(),
        ),
        CompileOptions::default(),
    );
    assert!(
        !panic_items
            .iter()
            .any(|item| item.label == "panic" || item.label == "panic_at"),
        "{panic_items:#?}"
    );

    let existing_bang = "fun main() { p!(); }";
    let items = completion_items_for_source(
        existing_bang,
        position(existing_bang, existing_bang.find("p!").unwrap() + 1),
        CompileOptions::default(),
    );
    let print = items
        .iter()
        .find(|item| item.label == "print" && item.detail.as_deref() == Some("standard macro"))
        .unwrap_or_else(|| panic!("{items:#?}"));
    assert_eq!(print.insert_text.as_deref(), Some("print"));
    assert_eq!(print.filter_text.as_deref(), Some("print"));

    let hidden = "fun main() { std::io::_pr }";
    let items = completion_items_for_source(
        hidden,
        position(hidden, hidden.rfind("_pr").unwrap() + "_pr".len()),
        CompileOptions::default(),
    );
    assert!(
        !items.iter().any(|item| item.label == "_print"),
        "{items:#?}"
    );

    let import = "use std::io::_pr;";
    let items = completion_items_for_source(
        import,
        position(import, import.find("_pr").unwrap() + "_pr".len()),
        CompileOptions::default(),
    );
    assert!(
        !items.iter().any(|item| item.label == "_print"),
        "{items:#?}"
    );

    for (prefix, expected) in [
        ("ass", &["assert!", "assert_eq!", "assert_ne!"][..]),
        (
            "debug_ass",
            &["debug_assert!", "debug_assert_eq!", "debug_assert_ne!"][..],
        ),
        ("unimpl", &["unimplemented!"][..]),
        ("unreach", &["unreachable!"][..]),
        ("todo", &["todo!"][..]),
    ] {
        let source = format!("fun main() {{ {prefix} }}");
        let items = completion_items_for_source(
            &source,
            position(&source, source.find(prefix).unwrap() + prefix.len()),
            CompileOptions::default(),
        );
        for label in expected {
            assert!(
                items.iter().any(|item| item.label == *label),
                "missing {label}: {items:#?}"
            );
        }
    }
}

#[test]
fn completion_hides_standard_panic_runtime_auto_imports() {
    let root = temp_root("completion-hidden-panic-runtime");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    let main = root.join("src/main.rid");
    let main_text = "fun main() { pan }\n";
    fs::write(&main, main_text).unwrap();
    let uri = lsp_types::Url::from_file_path(&main).unwrap();
    let docs = HashMap::from([(uri.clone(), Document::new(main_text, Some(1)))]);

    let items = completion_items_for_document(
        &uri,
        &docs,
        position(main_text, main_text.find("pan }").unwrap() + 3),
        CompileOptions::default(),
        &AnalysisSessions::default(),
        || false,
    )
    .unwrap()
    .unwrap();

    assert!(
        items
            .iter()
            .any(|item| item.label == "panic!" && item.detail.as_deref() == Some("standard macro")),
        "{items:#?}"
    );
    assert!(
        !items.iter().any(|item| item.label == "panic"),
        "{items:#?}"
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn completion_includes_standard_derives() {
    let source = "#[derive()] struct Value {}";
    let items = completion_items_for_source(
        source,
        position(source, source.find(')').unwrap()),
        CompileOptions::default(),
    );

    for name in [
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
            items.iter().any(|item| {
                item.label == name && item.detail.as_deref() == Some("standard derive macro")
            }),
            "missing {name}: {items:#?}"
        );
    }
}

#[test]
fn completion_respects_nested_block_scope() {
    let source = "fun main() { if true { let hidden = 1; } hid }";
    let items = completion_items_for_source(
        source,
        position(source, source.rfind("hid").unwrap() + 3),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(
        !items.iter().any(|item| item.label == "hidden"),
        "{items:#?}"
    );
}

#[test]
fn completion_includes_for_and_match_pattern_bindings() {
    let for_source = "fun main() { for item in [1] { ite } }";
    let for_items = completion_items_for_source(
        for_source,
        position(for_source, for_source.find("ite }").unwrap() + 3),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(
        for_items.iter().any(|item| item.label == "item"),
        "{for_items:#?}"
    );

    let match_source = "enum Value { Item(i32) } fun main(value: Value) { match value { Value::Item(inner) => { inn } } }";
    let match_items = completion_items_for_source(
        match_source,
        position(match_source, match_source.find("inn }").unwrap() + 3),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(
        match_items.iter().any(|item| item.label == "inner"),
        "{match_items:#?}"
    );
}

#[test]
fn completion_includes_private_imports() {
    let source =
        "mod models { pub struct Widget {} } use crate::models::Widget; fun main() { Wid }";
    let items = completion_items_for_source(
        source,
        position(source, source.rfind("Wid").unwrap() + 3),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(
        items.iter().any(|item| item.label == "Widget"),
        "{items:#?}"
    );
}

#[test]
fn completion_resolves_associated_items_through_import_aliases() {
    let source = "mod models { pub struct Widget {} impl Widget { pub fun build() {} } } use crate::models::Widget as Alias; fun main() { Alias::bu }";
    let items = completion_items_for_source(
        source,
        position(source, source.rfind("bu }").unwrap() + 2),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(items.iter().any(|item| item.label == "build"), "{items:#?}");
}

#[test]
fn completion_loads_unopened_project_modules() {
    let root = temp_root("project-completion");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    let main = root.join("src/main.rid");
    let main_text = "mod util;\nfun main() { util::va }\n";
    fs::write(&main, main_text).unwrap();
    fs::write(root.join("src/util.rid"), "pub fun value() {}\n").unwrap();
    let uri = lsp_types::Url::from_file_path(&main).unwrap();
    let docs = HashMap::from([(uri.clone(), Document::new(main_text, Some(1)))]);
    let sessions = AnalysisSessions::default();

    let items = completion_items_for_document(
        &uri,
        &docs,
        position(main_text, main_text.find("va }").unwrap() + 2),
        CompileOptions::default(),
        &sessions,
        || false,
    )
    .unwrap()
    .unwrap();

    assert!(items.iter().any(|item| item.label == "value"), "{items:#?}");
    let _ = fs::remove_dir_all(root);
}

#[test]
fn completion_auto_imports_public_symbol_with_bare_insertion() {
    let root = temp_root("completion-auto-import");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    let main = root.join("src/main.rid");
    let main_text = "mod geometry { pub struct Point {} }\nfun main() { let point: Poi }\n";
    fs::write(&main, main_text).unwrap();
    let uri = lsp_types::Url::from_file_path(&main).unwrap();
    let docs = HashMap::from([(uri.clone(), Document::new(main_text, Some(1)))]);

    let items = completion_items_for_document(
        &uri,
        &docs,
        position(main_text, main_text.find("Poi }").unwrap() + 3),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
        &AnalysisSessions::default(),
        || false,
    )
    .unwrap()
    .unwrap();
    let point = items
        .iter()
        .find(|item| {
            item.label == "Point"
                && item
                    .label_details
                    .as_ref()
                    .and_then(|details| details.description.as_deref())
                    == Some("geometry::Point")
        })
        .unwrap_or_else(|| panic!("{items:#?}"));

    assert_eq!(point.insert_text.as_deref(), Some("Point"));
    assert_eq!(point.filter_text.as_deref(), Some("Point"));
    assert_eq!(point.sort_text.as_deref(), Some("3:Point:geometry::Point"));
    assert_eq!(
        point.additional_text_edits.as_deref(),
        Some(
            &[lsp_types::TextEdit {
                range: Range::new(Position::new(0, 0), Position::new(0, 0)),
                new_text: "use geometry::Point;\n".into(),
            }][..]
        )
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn completion_keeps_same_named_auto_imports_separate_and_hides_private_items() {
    let root = temp_root("completion-auto-import-collision");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    let main = root.join("src/main.rid");
    let main_text = "mod a { pub struct Point {} struct Private {} }\nmod b { pub struct Point {} }\nfun main() { let point: P }\n";
    fs::write(&main, main_text).unwrap();
    let uri = lsp_types::Url::from_file_path(&main).unwrap();
    let docs = HashMap::from([(uri.clone(), Document::new(main_text, Some(1)))]);

    let items = completion_items_for_document(
        &uri,
        &docs,
        position(main_text, main_text.find("P }").unwrap() + 1),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
        &AnalysisSessions::default(),
        || false,
    )
    .unwrap()
    .unwrap();
    let point_paths = items
        .iter()
        .filter(|item| item.label == "Point")
        .filter_map(|item| item.label_details.as_ref()?.description.clone())
        .collect::<Vec<_>>();

    assert_eq!(point_paths, ["a::Point", "b::Point"]);
    assert!(!items.iter().any(|item| item.label == "Private"));
    let _ = fs::remove_dir_all(root);
}

#[test]
fn completion_uses_unsaved_project_module_overlays() {
    let root = temp_root("project-completion-overlay");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    let main = root.join("src/main.rid");
    let main_text = "mod util;\nfun main() { util::fr }\n";
    fs::write(&main, main_text).unwrap();
    let util = root.join("src/util.rid");
    fs::write(&util, "pub fun stale() {}\n").unwrap();
    let main_uri = lsp_types::Url::from_file_path(&main).unwrap();
    let util_uri = lsp_types::Url::from_file_path(&util).unwrap();
    let docs = HashMap::from([
        (main_uri.clone(), Document::new(main_text, Some(1))),
        (util_uri, Document::new("pub fun fresh() {}\n", Some(2))),
    ]);
    let sessions = AnalysisSessions::default();

    let items = completion_items_for_document(
        &main_uri,
        &docs,
        position(main_text, main_text.find("fr }").unwrap() + 2),
        CompileOptions::default(),
        &sessions,
        || false,
    )
    .unwrap()
    .unwrap();

    assert!(items.iter().any(|item| item.label == "fresh"), "{items:#?}");
    assert!(
        !items.iter().any(|item| item.label == "stale"),
        "{items:#?}"
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn project_member_completion_uses_active_module_coordinates() {
    let root = temp_root("project-member-completion-coordinates");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    fs::write(
        root.join("src/main.rid"),
        "mod padding;\nmod model;\nfun main() {}\n",
    )
    .unwrap();
    fs::write(
        root.join("src/padding.rid"),
        format!("// {}\n", "padding".repeat(80)),
    )
    .unwrap();
    let model = root.join("src/model.rid");
    let model_text = "pub struct Point { field: i32 }\npub fun complete() { let point = Point { field: 1 }; point.fi }\n";
    fs::write(&model, model_text).unwrap();
    let uri = lsp_types::Url::from_file_path(&model).unwrap();
    let docs = HashMap::from([(uri.clone(), Document::new(model_text, Some(1)))]);

    let items = completion_items_for_document(
        &uri,
        &docs,
        position(
            model_text,
            model_text.find("point.fi").unwrap() + "point.fi".len(),
        ),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
        &AnalysisSessions::default(),
        || false,
    )
    .unwrap()
    .unwrap();

    assert!(items.iter().any(|item| item.label == "field"), "{items:#?}");
    let _ = fs::remove_dir_all(root);
}

#[test]
fn completion_preserves_std_member_items_through_document_sessions() {
    let uri = lsp_types::Url::parse("file:///riddle-lsp-completion.rid").unwrap();
    let text =
        "use std::vector::Vector; fun main() { let c: Vector<i32> = Vector::new(); let d = c.i }";
    let docs = HashMap::from([(uri.clone(), Document::new(text, Some(1)))]);
    let sessions = AnalysisSessions::default();

    let items = completion_items_for_document(
        &uri,
        &docs,
        position(text, text.find("c.i").unwrap() + 3),
        CompileOptions::default(),
        &sessions,
        || false,
    )
    .unwrap()
    .unwrap();

    assert!(
        items.iter().any(|item| item.label == "is_empty"),
        "{items:#?}"
    );
}

#[test]
fn completion_offers_missing_trait_methods_in_impl_body() {
    let source = "trait Shape { fun area(self) -> f64; }\nstruct Square {}\nimpl Shape for Square {\n    ar\n}\n";
    let cursor = position(source, source.find("ar\n").unwrap() + 2);

    let items = completion_items_for_source(
        source,
        cursor,
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    let area = items
        .iter()
        .find(|item| item.label == "area")
        .unwrap_or_else(|| {
            panic!(
                "missing `area`; got {:?}",
                items.iter().map(|item| &item.label).collect::<Vec<_>>()
            )
        });
    assert_eq!(area.kind, Some(CompletionItemKind::METHOD));
    assert_eq!(area.insert_text_format, Some(InsertTextFormat::SNIPPET));
    assert!(
        area.insert_text
            .as_deref()
            .is_some_and(|text| text.contains("fun area(self) -> f64 {"))
    );
}

#[test]
fn completion_offers_missing_associated_types_in_impl_body() {
    let source =
        "trait Shape { type Unit; }\nstruct Square {}\nimpl Shape for Square {\n    Un\n}\n";
    let cursor = position(source, source.find("Un\n").unwrap() + 2);

    let items = completion_items_for_source(
        source,
        cursor,
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(
        items
            .iter()
            .any(|item| item.label == "Unit"
                && item.kind == Some(CompletionItemKind::TYPE_PARAMETER))
    );
}

#[test]
fn completion_hides_implemented_trait_members() {
    let source = "trait Shape { fun area(self) -> f64; }\nstruct Square {}\nimpl Shape for Square {\n    fun area(self) -> f64 { 1 }\n    ar\n}\n";
    let cursor = position(source, source.rfind("ar\n").unwrap() + 2);

    let items = completion_items_for_source(
        source,
        cursor,
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(!items.iter().any(|item| item.label == "area"));
}

#[test]
fn completion_filters_declaration_keywords_in_expression_positions() {
    let source = "fun main() { let value = 1 + str }";
    let cursor = position(source, source.find("str }").unwrap() + 3);

    let items = completion_items_for_source(
        source,
        cursor,
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(!items.iter().any(|item| item.label == "struct"));
    assert!(!items.iter().any(|item| item.label == "impl"));
}

#[test]
fn completion_keeps_declaration_keywords_at_statement_starts() {
    let source = "fun main() { le }";
    let cursor = position(source, source.find("le }").unwrap() + 2);

    let items = completion_items_for_source(
        source,
        cursor,
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    assert!(items.iter().any(|item| item.label == "let"));

    let expression_source = "fun main() { let value = 1 + le }";
    let expression_items = completion_items_for_source(
        expression_source,
        position(
            expression_source,
            expression_source.find("le }").unwrap() + 2,
        ),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(!expression_items.iter().any(|item| item.label == "let"));
}

#[test]
fn completion_offers_statement_snippets() {
    let source = "fun main() { mat }";
    let cursor = position(source, source.find("mat }").unwrap() + 3);

    let items = completion_items_for_source(
        source,
        cursor,
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );

    let snippet = items
        .iter()
        .find(|item| item.label == "match" && item.kind == Some(CompletionItemKind::SNIPPET))
        .expect("match snippet expected");
    assert_eq!(snippet.insert_text_format, Some(InsertTextFormat::SNIPPET));
    assert!(
        snippet
            .insert_text
            .as_deref()
            .is_some_and(|text| text.starts_with("match "))
    );
}
