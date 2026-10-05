use ast::{self, support::AstNode};
use frontend::incremental::IncrementalParser;

use crate::{check, check_with_package_ranges, messages};

#[test]
fn accepts_drop_lang_item_contract() {
    let result = check(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard {}

        impl Drop for Guard {
            fun drop(&mut self) {}
        }
        "#,
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn accepts_dyn_trait_reference_and_method_call() {
    let result = check(
        r#"
        trait Speak { fun speak(&self) -> i32; }
        struct Speaker { value: i32 }
        impl Speak for Speaker {
            fun speak(&self) -> i32 { self.value }
        }
        fun call(value: &dyn Speak) -> i32 { value.speak() }
        fun main() -> i32 {
            let speaker = Speaker { value: 7 };
            call(&speaker)
        }
        "#,
    );
    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn accepts_dyn_trait_alias_in_owned_and_borrowed_positions() {
    let result = check(
        r#"
        trait Speak { fun speak(&self) -> i32; }
        type SpeakObject = dyn Speak;
        struct Speaker { value: i32 }
        impl Speak for Speaker {
            fun speak(&self) -> i32 { self.value }
        }
        fun borrowed(value: &SpeakObject) -> i32 { value.speak() }
        fun owned(value: SpeakObject) -> i32 { value.speak() }
        fun make() -> SpeakObject { Speaker { value: 7 } }
        fun main() -> i32 {
            let speaker = Speaker { value: 7 };
            borrowed(&speaker) + owned(make())
        }
        "#,
    );
    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn accepts_owned_dyn_trait_value_and_method_call() {
    let result = check(
        r#"
        trait Speak { fun speak(&self) -> i32; }
        struct Speaker { value: i32 }
        impl Speak for Speaker {
            fun speak(&self) -> i32 { self.value }
        }
        fun call(value: dyn Speak) -> i32 { value.speak() }
        fun make() -> dyn Speak { Speaker { value: 7 } }
        fun main() -> i32 { call(make()) }
        "#,
    );
    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn accepts_dyn_callable_value_and_reference() {
    let result = check(
        r#"
        fun increment(value: i32) -> i32 { value + 1 }
        fun apply(value: dyn Fn(i32) -> i32, input: i32) -> i32 { value(input) }
        fun apply_ref(value: &dyn Fn(i32) -> i32, input: i32) -> i32 { value(input) }
        fun main() -> i32 {
            let callback: dyn Fn(i32) -> i32 = increment;
            let borrowed = &callback;
            apply(callback, 41) + apply_ref(borrowed, 41)
        }
        "#,
    );
    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn accepts_dyn_fn_mut_and_fn_once_boundaries() {
    let result = check(
        r#"
        fun apply_mut(value: &mut dyn FnMut(i32) -> i32, input: i32) -> i32 { value(input) }
        fun apply_once(value: dyn FnOnce() -> i32) -> i32 { value() }
        fun main() -> i32 {
            let mut total = 0;
            let mut callback: dyn FnMut(i32) -> i32 = [value: i32 -> {
                total += value;
                total
            }];
            let first = apply_mut(&mut callback, 2);
            let once: dyn FnOnce() -> i32 = [ -> first];
            apply_once(once)
        }
        "#,
    );
    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn rejects_immutable_dyn_fn_mut_and_dyn_fn_once_references() {
    let result = check(
        r#"
        fun call_mut(value: &dyn FnMut() -> i32) -> i32 { value() }
        fun call_once(value: &mut dyn FnOnce() -> i32) -> i32 { value() }
        "#,
    );
    assert!(
        result.diagnostics.iter().any(|diagnostic| {
            diagnostic.code == "E0031" && diagnostic.message.contains("immutable reference")
        }),
        "expected immutable FnMut reference diagnostic: {:?}",
        result.diagnostics
    );
    assert!(
        result.diagnostics.iter().any(|diagnostic| {
            diagnostic.code == "E0035" && diagnostic.message.contains("FnOnce")
        }),
        "expected FnOnce reference diagnostic: {:?}",
        result.diagnostics
    );
}

#[test]
fn accepts_borrowing_owned_dyn_trait_value() {
    let result = check(
        r#"
        trait Speak { fun speak(&self) -> i32; }
        struct Speaker { value: i32 }
        impl Speak for Speaker {
            fun speak(&self) -> i32 { self.value }
        }
        fun take(value: &dyn Speak) -> i32 { value.speak() }
        fun main() -> i32 {
            let speaker: dyn Speak = Speaker { value: 2 };
            let borrowed = &speaker;
            take(&speaker) + borrowed.speak()
        }
        "#,
    );
    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn accepts_dyn_trait_upcast() {
    let result = check(
        r#"
        trait Speak { fun speak(&self) -> i32; }
        trait Loud: Speak { fun volume(&self) -> i32; }
        struct Speaker { value: i32 }
        impl Speak for Speaker {
            fun speak(&self) -> i32 { self.value }
        }
        impl Loud for Speaker {
            fun volume(&self) -> i32 { self.value + 1 }
        }
        fun take(value: &dyn Speak) -> i32 { value.speak() }
        fun main() -> i32 {
            let loud: dyn Loud = Speaker { value: 2 };
            let parent: dyn Speak = loud;
            let borrowed: &dyn Speak = &parent;
            take(borrowed)
        }
        "#,
    );
    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn accepts_generic_owned_dyn_trait_factory() {
    let result = check(
        r#"
        trait Speak { fun speak(&self) -> i32; }
        struct Speaker { value: i32 }
        impl Speak for Speaker {
            fun speak(&self) -> i32 { self.value }
        }
        fun wrap<T>(value: T) -> dyn Speak
        where T: Speak
        { value }
        fun main() -> i32 { wrap(Speaker { value: 2 }).speak() }
        "#,
    );
    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn accepts_generic_dyn_trait_factory_with_associated_binding() {
    let result = check(
        r#"
        trait Producer {
            type Item;
            fun get(&self) -> Self::Item;
        }
        struct Number { value: i32 }
        impl Producer for Number {
            type Item = i32;
            fun get(&self) -> i32 { self.value }
        }
        fun wrap<T>(value: T) -> dyn Producer<Item = i32>
        where T: Producer<Item = i32>
        { value }
        fun main() -> i32 { wrap(Number { value: 2 }).get() }
        "#,
    );
    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn accepts_owned_dyn_trait_array_literal() {
    let result = check(
        r#"
        trait Speak { fun speak(&self) -> i32; }
        struct Speaker { value: i32 }
        impl Speak for Speaker {
            fun speak(&self) -> i32 { self.value }
        }
        fun main() -> i32 {
            let values: [dyn Speak; 2] = [
                Speaker { value: 1 },
                Speaker { value: 2 },
            ];
            values[0].speak() + values[1].speak()
        }
        "#,
    );
    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn rejects_ambiguous_dyn_trait_method() {
    let result = check(
        r#"
        trait Left { fun name(&self) -> i32; }
        trait Right { fun name(&self) -> i32; }
        trait Both: Left + Right {}
        struct Speaker { value: i32 }
        impl Left for Speaker {
            fun name(&self) -> i32 { self.value }
        }
        impl Right for Speaker {
            fun name(&self) -> i32 { self.value + 1 }
        }
        impl Both for Speaker {}
        fun call(value: &dyn Both) -> i32 { value.name() }
        "#,
    );
    let msgs = messages(&result);
    assert!(
        msgs.iter()
            .any(|message| message.contains("ambiguous method")),
        "{msgs:?}"
    );
    assert!(
        !msgs
            .iter()
            .any(|message| message.contains("unknown method `name`")),
        "{msgs:?}"
    );
}

#[test]
fn reports_non_object_safe_dyn_trait_method() {
    let result = check(
        r#"
        trait Duplicate { fun duplicate(self) -> Self; }
        struct Speaker { value: i32 }
        impl Duplicate for Speaker {
            fun duplicate(self) -> Self { self }
        }
        fun call(value: &dyn Duplicate) -> &dyn Duplicate { value.duplicate() }
        "#,
    );
    let msgs = messages(&result);
    assert!(
        msgs.iter()
            .any(|message| message.contains("not object-safe")),
        "{msgs:?}"
    );
}

#[test]
fn accepts_dyn_trait_associated_type_binding_and_projection() {
    let result = check(
        r#"
        trait Producer {
            type Item;
            fun get(&self) -> Self::Item;
        }
        struct Number { value: i32 }
        impl Producer for Number {
            type Item = i32;
            fun get(&self) -> i32 { self.value }
        }
        fun read(value: &dyn Producer<Item = i32>) -> i32 { value.get() }
        fun main() -> i32 {
            let number = Number { value: 7 };
            read(&number)
        }
        "#,
    );
    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn reports_missing_dyn_trait_associated_type_binding() {
    let result = check(
        r#"
        trait Producer {
            type Item;
            fun get(&self) -> Self::Item;
        }
        fun read(value: &dyn Producer) -> i32 { value.get() }
        "#,
    );
    assert!(
        result.diagnostics.iter().any(|diagnostic| {
            diagnostic.code == "E0034"
                && diagnostic
                    .message
                    .contains("requires associated type `Item`")
        }),
        "expected a missing dyn associated type diagnostic, got {:?}",
        result.diagnostics
    );
}

#[test]
fn rejects_dyn_trait_associated_type_mismatch() {
    let result = check(
        r#"
        trait Producer {
            type Item;
            fun get(&self) -> Self::Item;
        }
        struct Number { value: i32 }
        impl Producer for Number {
            type Item = i32;
            fun get(&self) -> i32 { self.value }
        }
        fun read(value: &dyn Producer<Item = bool>) -> bool { value.get() }
        fun main() -> bool {
            let number = Number { value: 7 };
            read(&number)
        }
        "#,
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0001"),
        "expected an associated type coercion diagnostic, got {:?}",
        result.diagnostics
    );
}

#[test]
fn accepts_owned_generic_dyn_trait_value_and_method_call() {
    let result = check(
        r#"
        trait Convert<T> { fun convert(&self) -> T; }
        struct Value { value: i32 }
        impl Convert<i32> for Value {
            fun convert(&self) -> i32 { self.value }
        }
        fun call(value: dyn Convert<i32>) -> i32 { value.convert() }
        fun make() -> dyn Convert<i32> { Value { value: 7 } }
        fun main() -> i32 { call(make()) }
        "#,
    );
    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn rejects_owned_dyn_trait_coercion_without_an_impl() {
    let result = check(
        r#"
        trait Speak { fun speak(&self) -> i32; }
        struct Other { value: i32 }
        fun make() -> dyn Speak { Other { value: 7 } }
        "#,
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0001"),
        "expected an invalid owned dyn trait coercion diagnostic, got {:?}",
        result.diagnostics
    );
}

#[test]
fn accepts_generic_dyn_trait_reference_and_method_call() {
    let result = check(
        r#"
        trait Convert<T> { fun convert(&self) -> T; }
        struct Value { value: i32 }
        impl Convert<i32> for Value {
            fun convert(&self) -> i32 { self.value }
        }
        fun call(value: &dyn Convert<i32>) -> i32 { value.convert() }
        fun main() -> i32 {
            let value = Value { value: 7 };
            call(&value)
        }
        "#,
    );
    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn accepts_supertrait_methods_on_dyn_trait_reference() {
    let result = check(
        r#"
        trait Speak { fun speak(&self) -> i32; }
        trait Loud: Speak { fun volume(&self) -> i32; }
        struct Speaker { value: i32 }
        impl Speak for Speaker {
            fun speak(&self) -> i32 { self.value }
        }
        impl Loud for Speaker {
            fun volume(&self) -> i32 { self.value + 1 }
        }
        fun call(value: &dyn Loud) -> i32 { value.speak() + value.volume() }
        "#,
    );
    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn accepts_generic_supertrait_methods_on_dyn_trait_reference() {
    let result = check(
        r#"
        trait Read<T> { fun read(&self) -> T; }
        trait Loud<T>: Read<T> { fun volume(&self) -> T; }
        struct Speaker { value: i32 }
        impl Read<i32> for Speaker {
            fun read(&self) -> i32 { self.value }
        }
        impl Loud<i32> for Speaker {
            fun volume(&self) -> i32 { self.value + 1 }
        }
        fun call(value: &dyn Loud<i32>) -> i32 { value.read() + value.volume() }
        "#,
    );
    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn rejects_dyn_trait_coercion_without_an_impl() {
    let result = check(
        r#"
        trait Speak { fun speak(&self) -> i32; }
        struct Other { value: i32 }
        fun call(value: &dyn Speak) -> i32 { value.speak() }
        fun main() -> i32 {
            let other = Other { value: 7 };
            call(&other)
        }
        "#,
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0001"),
        "expected an invalid dyn trait coercion diagnostic, got {:?}",
        result.diagnostics
    );
}

#[test]
fn parses_nested_block_and_documentation_comments() {
    let source = r#"
    /** outer /* nested */ documentation */
    /// function documentation
    fun main() {}
    "#;
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source(source);
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);

    let root = ast::Root::cast(parse.syntax()).expect("root syntax");
    let function = root.stmts().find_map(|stmt| match stmt {
        ast::Stmt::FuncDecl(function) => Some(function),
        _ => None,
    });
    let function = function.expect("function declaration");
    let docs = ast::doc_comments_for_node(function.syntax());
    assert_eq!(
        docs.iter().map(|token| token.text()).collect::<Vec<_>>(),
        vec![
            "/** outer /* nested */ documentation */",
            "/// function documentation"
        ]
    );
}

#[test]
fn attaches_only_double_slash_angle_comments_to_the_previous_node() {
    let source = r#"
        struct Point {
            x: i32, //< horizontal coordinate
            y: i32, ///< remains a leading-style doc comment
            z: i32,
            w: i32, //< vertical coordinate
        }
    "#;
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source(source);
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);
    let root = ast::Root::cast(parse.syntax()).expect("root syntax");
    let structure = root.stmts().find_map(|stmt| match stmt {
        ast::Stmt::StructDecl(structure) => Some(structure),
        _ => None,
    });
    let structure = structure.expect("struct declaration");
    let fields = structure
        .field_list()
        .expect("field list")
        .fields()
        .collect::<Vec<_>>();

    assert_eq!(
        ast::doc_comments_for_node(fields[0].syntax())
            .iter()
            .map(|token| token.text())
            .collect::<Vec<_>>(),
        vec!["//< horizontal coordinate"]
    );
    assert!(ast::doc_comments_for_node(fields[1].syntax()).is_empty());
    assert_eq!(
        ast::doc_comments_for_node(fields[2].syntax())
            .iter()
            .map(|token| token.text())
            .collect::<Vec<_>>(),
        vec!["///< remains a leading-style doc comment"]
    );
    assert_eq!(
        ast::doc_comments_for_node(fields[3].syntax())
            .iter()
            .map(|token| token.text())
            .collect::<Vec<_>>(),
        vec!["//< vertical coordinate"]
    );
}

#[test]
fn rejects_invalid_drop_lang_item_contracts() {
    for source in [
        r#"#[lang = "drop"] trait Drop<T> { fun drop(&mut self); }"#,
        r#"#[lang = "drop"] trait Drop { fun drop(&self); }"#,
        r#"#[lang = "drop"] trait Drop { fun drop(&mut self) -> i32; }"#,
        r#"#[lang = "drop"] trait Drop { fun drop(&mut self); fun extra(); }"#,
    ] {
        let result = check(source);
        assert!(
            result.diagnostics.iter().any(|d| d.code == "E0053"),
            "expected invalid Drop contract diagnostic for {source:?}, got {:?}",
            result.diagnostics
        );
    }
}

#[test]
fn rejects_direct_drop_call() {
    let result = check(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard {}

        impl Drop for Guard {
            fun drop(&mut self) {}
        }

        fun consume() {
            let mut guard = Guard {};
            guard.drop();
        }
        "#,
    );

    assert!(
        result.diagnostics.iter().any(|d| d.code == "E0056"),
        "expected direct Drop call diagnostic, got {:?}",
        result.diagnostics
    );
}

#[test]
fn rejects_copy_for_drop_type() {
    let result = check(
        r#"
        #[lang = "copy"]
        trait Copy {}

        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard {}

        impl Copy for Guard {}

        impl Drop for Guard {
            fun drop(&mut self) {}
        }
        "#,
    );

    assert!(
        result.diagnostics.iter().any(|d| d.code == "E0055"),
        "expected Copy + Drop conflict, got {:?}",
        result.diagnostics
    );
}

#[test]
fn accepts_matching_trait_impl_required_items() {
    let result = check(
        r#"
        trait Show {
            fun show(value: i32) -> &str;
            type Output;
            type Default = bool;
        }

        struct Widget {}

        impl Show for Widget {
            fun show(value: i32) -> &str {
                "ok"
            }

            type Output = i32;

            fun helper() -> i32 {
                1
            }
        }
        "#,
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn reports_trait_impl_contract_mismatches() {
    let result = check(
        r"
        trait Convert {
            fun value(input: i32) -> bool;
            type Item;
        }

        struct Source {}

        impl Convert for Source {
            fun value(input: bool) -> i32 {
                1
            }
        }
        ",
    );

    let msgs = messages(&result);
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("parameter 1 type mismatch"))
    );
    assert!(msgs.iter().any(|msg| msg.contains("return type mismatch")));
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("missing associated type `Item`"))
    );
}

#[test]
fn accepts_inherent_method_call_with_self_receiver() {
    let result = check(
        r#"
        enum Foo {
            A,
            B,
        }

        #[lang = "partial_eq"]
        trait PartialEq {
            fun eq(&self, other: &Self) -> bool;
        }

        impl PartialEq for Foo {
            fun eq(&self, other: &Self) -> bool { false }
        }

        impl Foo {
            fun get(&self) -> &str {
                if *self == Foo::A {
                    "A"
                } else {
                    "B"
                }
            }
        }

        fun main() {
            let x = Foo::A;
            let t = x.get();
        }
        "#,
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn mutable_self_method_requires_mutable_receiver_binding() {
    let result = check(
        r"
        struct Cell {
            value: i32,
        }

        impl Cell {
            fun set(&mut self, value: i32) {
                self.value = value;
            }
        }

        fun main() {
            let cell = Cell { value: 1 };
            cell.set(42);
        }
        ",
    );

    assert!(result.diagnostics.iter().any(|diag| diag.code == "E0031"));
}

#[test]
fn mutable_reference_requires_mutable_binding() {
    let result = check(
        r"
        struct Cell {
            value: i32,
        }

        fun main() {
            let cell = Cell { value: 1 };
            let ref_cell = &mut cell;
        }
        ",
    );

    assert!(result.diagnostics.iter().any(|diag| diag.code == "E0031"));
}

#[test]
fn resolves_impl_associated_type_path() {
    let result = check(
        r"
        struct Foo {}

        trait Bar {
            type X;
        }

        impl Bar for Foo {
            type X = i32;
        }

        fun main() {
            let r = 10 as Foo::X;
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
    assert!(
        result
            .expr_types
            .values()
            .any(|ty| matches!(ty, type_checker::Type::Int(type_checker::IntTy::I32)))
    );
}

#[test]
fn accepts_add_operator_impl_call() {
    let result = check(
        r#"
        #[lang = "add"]
        trait Add {
            type Output;
            fun add(self, rhs: Self) -> Self::Output;
        }

        struct Box<T> {
            value: T,
        }

        impl Add for Box<i32> {
            type Output = i32;

            fun add(self, rhs: Self) -> Self::Output {
                self.value + rhs.value
            }
        }

        fun main() {
            let a: Box<i32> = Box { value: 1 };
            let b: Box<i32> = Box { value: 2 };
            let sum: i32 = a + b;
            let c: Box<i32> = Box { value: 3 };
            let d: Box<i32> = Box { value: 4 };
            let direct: i32 = c.add(d);
        }
        "#,
    );

    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(result.operator_calls.len(), 1);
}

#[test]
fn operator_traits_accept_defaulted_and_heterogeneous_rhs() {
    let result = check(
        r#"
        #[lang = "add"]
        trait Add<Rhs = Self> {
            type Output;
            fun add(self, rhs: Rhs) -> Self::Output;
        }

        struct Number { value: i32 }
        struct Delta { value: i32 }

        impl Add for Number {
            type Output = Number;
            fun add(self, rhs: Self) -> Self::Output {
                Number { value: self.value + rhs.value }
            }
        }

        impl Add<Delta> for Number {
            type Output = i32;
            fun add(self, rhs: Delta) -> Self::Output {
                self.value + rhs.value
            }
        }

        fun main() {
            let same: Number = Number { value: 1 } + Number { value: 2 };
            let mixed: i32 = Number { value: 3 } + Delta { value: 4 };
        }
        "#,
    );

    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(result.operator_calls.len(), 2);
}

#[test]
fn binary_expected_output_selects_heterogeneous_operator_impl() {
    let result = check(
        r#"
        unsafe extern "C" {
            safe fun fail() -> !;
        }

        #[lang = "add"]
        trait Add<Rhs = Self> {
            type Output;
            fun add(self, rhs: Rhs) -> Self::Output;
        }

        struct Left {}
        struct First {}
        struct Second {}

        impl Add<First> for Left {
            type Output = i32;
            fun add(self, rhs: First) -> i32 { 0 }
        }

        impl Add<Second> for Left {
            type Output = bool;
            fun add(self, rhs: Second) -> bool { true }
        }

        fun make<T>() -> T { fail() }

        fun main() -> bool {
            Left {} + make()
        }
        "#,
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn trait_type_arguments_respect_required_and_defaulted_parameters() {
    let result = check(
        r"
        trait Required<T> {}
        trait Defaulted<T = i32> {}
        struct Item {}

        impl Required for Item {}
        impl Required<i32, bool> for bool {}
        impl Defaulted for Item {}
        ",
    );

    let arity_errors = result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "E0032")
        .collect::<Vec<_>>();
    assert_eq!(arity_errors.len(), 2, "{:?}", result.diagnostics);
    assert!(
        arity_errors
            .iter()
            .any(|diagnostic| diagnostic.message.contains("got 0"))
    );
    assert!(
        arity_errors
            .iter()
            .any(|diagnostic| diagnostic.message.contains("got 2"))
    );
}

#[test]
fn supertrait_impl_requires_matching_trait_arguments() {
    let result = check(
        r"
        trait Parent<Rhs = Self> {}
        trait Child<Rhs = Self>: Parent<Rhs> {}

        struct Left {}
        struct Right {}
        struct Other {}

        impl Parent<Other> for Left {}
        impl Child<Right> for Left {}
        ",
    );

    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0036"),
        "{:?}",
        result.diagnostics
    );
}

#[test]
fn generic_supertrait_bound_substitutes_parent_arguments() {
    let result = check(
        r"
        trait Parent<X> {
            fun parent(&self, value: X) -> X;
        }

        trait Child<Y>: Parent<Y> {}

        struct Item {}

        impl Parent<i32> for Item {
            fun parent(&self, value: i32) -> i32 { value }
        }

        impl Child<i32> for Item {}

        fun use_parent<T: Child<i32>>(value: T) -> i32 {
            value.parent(1)
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn overloaded_operator_checks_rhs_once() {
    let result = check(
        r#"
        #[lang = "add"]
        trait Add<Rhs = Self> {
            type Output;
            fun add(self, rhs: Rhs) -> Self::Output;
        }

        struct Number { value: i32 }

        impl Add for Number {
            type Output = Number;
            fun add(self, rhs: Self) -> Self::Output { self }
        }

        fun main() {
            let value = Number { value: 1 } + 1();
        }
        "#,
    );

    assert_eq!(
        result
            .diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.code == "E0004")
            .count(),
        1
    );
}

#[test]
fn rejects_duplicate_trait_impls() {
    let result = check(
        r#"
        #[lang = "add"]
        trait Add {
            type Output;
            fun add(self, rhs: Self) -> Self::Output;
        }

        struct Number { value: i32 }

        impl Add for Number {
            type Output = Number;
            fun add(self, rhs: Self) -> Self::Output { self }
        }

        impl Add for Number {
            type Output = Number;
            fun add(self, rhs: Self) -> Self::Output { rhs }
        }
        "#,
    );

    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0047")
    );
}

#[test]
fn rejects_overlapping_trait_impls() {
    let result = check(
        r#"
        #[lang = "add"]
        trait Add<Rhs = Self> {
            type Output;
            fun add(self, rhs: Rhs) -> Self::Output;
        }

        struct Number { value: i32 }
        struct Delta { value: i32 }

        impl<R> Add<R> for Number {
            type Output = Number;
            fun add(self, rhs: R) -> Self::Output { self }
        }

        impl Add<Delta> for Number {
            type Output = Number;
            fun add(self, rhs: Delta) -> Self::Output { self }
        }
        "#,
    );

    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0047")
    );
}

#[test]
fn accepts_disjoint_trait_impls() {
    let result = check(
        r#"
        #[lang = "add"]
        trait Add<Rhs = Self> {
            type Output;
            fun add(self, rhs: Rhs) -> Self::Output;
        }

        struct Number { value: i32 }
        struct Delta { value: i32 }
        struct Offset { value: i32 }

        impl Add<Delta> for Number {
            type Output = Number;
            fun add(self, rhs: Delta) -> Self::Output { self }
        }

        impl Add<Offset> for Number {
            type Output = Number;
            fun add(self, rhs: Offset) -> Self::Output { self }
        }
        "#,
    );

    assert!(
        !result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0047")
    );
}

#[test]
fn accepts_disjoint_impls_with_shared_generic_relationship() {
    let result = check(
        r"
        trait Marker<A> {}

        impl<T> Marker<T> for T {}
        impl Marker<i32> for bool {}
        ",
    );

    assert!(
        !result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0047")
    );
}

#[test]
fn enforces_orphan_rules_per_package() {
    let foreign = r"
        trait Foreign<T = i32> {}
        struct ForeignType {}
        struct ForeignBox<T> { value: T }
        #[fundamental]
        struct FundBox<T> { value: T }
    ";
    let local = r"
        struct Local<T> { value: T }
        trait LocalTrait {}

        impl Foreign for ForeignType {}
        impl<T> Foreign<T> for Local<T> {}
        impl LocalTrait for ForeignType {}
        impl<T> Foreign<Local<T>> for T {}
        impl Foreign for &Local<i32> {}
        impl Foreign for ForeignBox<Local<i32>> {}
        impl<T> Foreign<Local<i32>> for ForeignBox<T> {}
        impl Foreign for FundBox<Local<i32>> {}
    ";
    let source = format!("{foreign}{local}");
    let result =
        check_with_package_ranges(&source, &[0..foreign.len(), foreign.len()..source.len()]);
    let orphan_errors = result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "E0048")
        .collect::<Vec<_>>();

    assert_eq!(orphan_errors.len(), 4, "{:?}", result.diagnostics);
    assert!(
        orphan_errors
            .iter()
            .any(|diagnostic| diagnostic.message.contains("foreign type `ForeignType`"))
    );
    assert!(
        orphan_errors
            .iter()
            .any(|diagnostic| diagnostic.message.contains("type parameter `T`"))
    );
    assert!(
        orphan_errors
            .iter()
            .any(|diagnostic| diagnostic.message.contains("foreign type `ForeignBox"))
    );
}

#[test]
fn rejects_arbitrary_composite_trait_bound() {
    let result = check(
        r"
        trait Marker {}

        impl Marker for i32 {}

        fun accepts_marker<T: Marker>(value: T) {}

        fun main() {
            accepts_marker((1i32, 2i32));
            accepts_marker([1i32, 2i32]);
        }
        ",
    );

    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0035"),
        "expected tuple to be rejected for Marker: {:?}",
        result.diagnostics
    );
}

#[test]
fn comparison_traits_accept_heterogeneous_rhs() {
    let result = check(
        r#"
        #[lang = "partial_eq"]
        trait PartialEq<Rhs = Self> {
            fun eq(&self, other: &Rhs) -> bool;
            fun ne(&self, other: &Rhs) -> bool { !self.eq(other) }
        }

        struct Left { value: i32 }
        struct Right { value: i32 }

        impl PartialEq<Right> for Left {
            fun eq(&self, other: &Right) -> bool {
                self.value == other.value
            }
        }

        fun main() {
            let left = Left { value: 1 };
            let right = Right { value: 1 };
            let equal: bool = left == right;
        }
        "#,
    );

    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(result.operator_calls.len(), 1);
}

#[test]
fn composite_comparison_preserves_heterogeneous_rhs() {
    let result = check(
        r#"
        #[lang = "partial_eq"]
        trait PartialEq<Rhs = Self> {
            fun eq(&self, other: &Rhs) -> bool;
        }

        struct Left { value: i32 }
        struct Right { value: i32 }

        impl PartialEq for i32 {
            fun eq(&self, other: &i32) -> bool {
                *self == *other
            }
        }

        impl PartialEq<Right> for Left {
            fun eq(&self, other: &Right) -> bool {
                self.value == other.value
            }
        }

        fun main() {
            let left: (Left, i32) = (Left { value: 1 }, 2);
            let right: (Right, i32) = (Right { value: 1 }, 2);
            let equal: bool = left == right;
        }
        "#,
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn accepts_binary_unary_and_assign_operator_impls() {
    let result = check(
        r#"
        #[lang = "sub"]
        trait Sub { type Output; fun sub(self, rhs: Self) -> Self::Output; }
        #[lang = "neg"]
        trait Neg { type Output; fun neg(self) -> Self::Output; }
        #[lang = "add_assign"]
        trait AddAssign { fun add_assign(&mut self, rhs: Self); }

        struct Number { value: i32 }

        impl Sub for Number {
            type Output = Number;
            fun sub(self, rhs: Self) -> Self::Output {
                Number { value: self.value - rhs.value }
            }
        }
        impl Neg for Number {
            type Output = Number;
            fun neg(self) -> Self::Output {
                Number { value: -self.value }
            }
        }
        impl AddAssign for Number {
            fun add_assign(&mut self, rhs: Self) {
                self.value += rhs.value;
            }
        }

        fun main() {
            let left = Number { value: 7 };
            let right = Number { value: 2 };
            let difference = left - right;
            let negated = -difference;
            let mut total = Number { value: 10 };
            total += negated;
        }
        "#,
    );

    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(result.operator_calls.len(), 3);
}

#[test]
fn rejects_add_impl_missing_add_method() {
    let result = check(
        r#"
        #[lang = "add"]
        trait Add {
            type Output;
            fun add(self, rhs: Self) -> Self::Output;
        }

        struct Box<T> {
            value: T,
        }

        impl Add for Box<i32> {
            type Output = i32;
        }
        "#,
    );

    let msgs = messages(&result);
    assert!(
        msgs.iter().any(|msg| msg.contains("missing method `add`")),
        "{msgs:?}"
    );
}

#[test]
fn accepts_generic_add_impl_with_output_bound() {
    let result = check(
        r#"
        #[lang = "add"]
        trait Add {
            type Output;
            fun add(self, rhs: Self) -> Self::Output;
        }

        impl Add for i32 {
            type Output = i32;
            fun add(self, rhs: Self) -> Self::Output {
                self + rhs
            }
        }

        struct Box<T> {
            value: T,
        }

        impl<T: Add<Output = T>> Add for Box<T> {
            type Output = T;

            fun add(self, rhs: Self) -> Self::Output {
                self.value + rhs.value
            }
        }

        fun main() {
            let a: Box<i32> = Box { value: 1 };
            let b: Box<i32> = Box { value: 2 };
            let sum: i32 = a + b;
        }
        "#,
    );

    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(result.operator_calls.len(), 2);
}

#[test]
fn generic_add_impl_respects_type_argument_bound() {
    let result = check(
        r#"
        #[lang = "add"]
        trait Add {
            type Output;
            fun add(self, rhs: Self) -> Self::Output;
        }

        impl Add for i32 {
            type Output = i32;
            fun add(self, rhs: Self) -> Self::Output {
                self + rhs
            }
        }

        struct Box<T> {
            value: T,
        }

        impl<T: Add<Output = T>> Add for Box<T> {
            type Output = T;

            fun add(self, rhs: Self) -> Self::Output {
                self.value + rhs.value
            }
        }

        fun main() {
            let a: Box<bool> = Box { value: true };
            let b: Box<bool> = Box { value: false };
            let sum = a + b;
        }
        "#,
    );

    assert!(
        !result.diagnostics.is_empty(),
        "Box<bool> must not satisfy Box<T>'s Add impl when bool lacks Add"
    );
}

#[test]
fn rejects_generic_add_impl_without_add_bound() {
    let result = check(
        r#"
        #[lang = "add"]
        trait Add {
            type Output;
            fun add(self, rhs: Self) -> Self::Output;
        }

        struct Box<T> {
            value: T,
        }

        impl<T> Add for Box<T> {
            type Output = T;

            fun add(self, rhs: Self) -> Self::Output {
                self.value + rhs.value
            }
        }
        "#,
    );

    assert!(
        !result.diagnostics.is_empty(),
        "generic add without a T: Add bound should be rejected"
    );
}

#[test]
fn checks_partial_eq_for_user_equality() {
    let result = check(
        r#"
        #[lang = "partial_eq"]
        trait PartialEq {
            fun eq(&self, other: &Self) -> bool;
        }

        enum Foo {
            A,
            B,
        }

        fun main() {
            let a = Foo::A;
            let b = Foo::B;
            let same = a == b;
        }
        "#,
    );

    let msgs = messages(&result);
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("must implement `PartialEq`")),
        "{msgs:?}"
    );

    let result = check(
        r#"
        #[lang = "partial_eq"]
        trait PartialEq {
            fun eq(&self, other: &Self) -> bool;
        }

        enum Foo {
            A,
            B,
        }

        impl PartialEq for Foo {
            fun eq(&self, other: &Self) -> bool { false }
        }

        fun main() {
            let a = Foo::A;
            let b = Foo::B;
            let same = a == b;
        }
        "#,
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn checks_eq_marker_dependencies() {
    let result = check(
        r#"
        #[lang = "partial_eq"]
        trait PartialEq {
            fun eq(&self, other: &Self) -> bool;
        }

        #[lang = "eq"]
        trait Eq: PartialEq {}

        #[lang = "partial_ord"]
        trait PartialOrd: PartialEq {
            fun lt(&self, other: &Self) -> bool;
        }

        enum Ordering { Equal }

        #[lang = "ord"]
        trait Ord: Eq + PartialOrd {
            fun cmp(&self, other: &Self) -> Ordering;
        }

        struct MissingEq {}
        struct MissingPartialOrd {}
        struct MissingOrdDeps {}

        impl Eq for MissingEq {}
        impl PartialOrd for MissingPartialOrd {}
        impl Ord for MissingOrdDeps {}
        "#,
    );

    let msgs = messages(&result);
    assert!(
        msgs.iter().any(|msg| msg.contains("requires `PartialEq`")),
        "{msgs:?}"
    );
    assert!(
        msgs.iter().any(|msg| msg.contains("requires `Eq`")),
        "{msgs:?}"
    );
    assert!(
        msgs.iter().any(|msg| msg.contains("requires `PartialOrd`")),
        "{msgs:?}"
    );
}

#[test]
fn checks_generic_trait_bounds() {
    let result = check(
        r"
        trait Marker {}

        struct Good {}
        struct Bad {}

        impl Marker for Good {}

        fun accept<T: Marker>(value: T) -> T {
            value
        }

        fun main() {
            let good = Good {};
            let bad = Bad {};
            let ok = accept(good);
            let nope = accept(bad);
        }
        ",
    );

    let msgs = messages(&result);
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("does not satisfy bound `Marker`")),
        "{msgs:?}"
    );
}

#[test]
fn checks_struct_and_enum_where_clause_bounds() {
    let result = check(
        r"
        trait Marker {}

        struct Good {}
        struct Bad {}

        impl Marker for Good {}

        struct Box<T> where T: Marker {
            value: T,
        }

        enum Slot<T> where T: Marker {
            Some(T),
            None,
        }

        fun takes_marker<T: Marker>(value: T) {
            let ok: Box<T> = Box { value: value };
        }

        fun main() {
            let good_box = Box { value: Good {} };
            let good_slot = Slot::Some(Good {});
            let bad_box = Box { value: Bad {} };
            let bad_slot = Slot::Some(Bad {});
        }
        ",
    );

    let msgs = messages(&result);
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("type `Bad` does not satisfy bound `Marker` for `Box`")),
        "{msgs:?}"
    );
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("type `Bad` does not satisfy bound `Marker` for `Slot`")),
        "{msgs:?}"
    );
}

#[test]
fn allows_trait_bound_method_call_in_generic_body() {
    let result = check(
        r"
        trait Named {
            fun name(&self) -> i32;
        }

        struct User { id: i32 }

        impl Named for User {
            fun name(&self) -> i32 {
                self.id
            }
        }

        fun read<T: Named>(value: T) -> i32 {
            value.name()
        }

        fun main() {
            let user = User { id: 1 };
            let id = read(user);
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn accepts_iterator_next_protocol() {
    let result = check(
        r"
        enum Option<T> {
            Some(T),
            None,
        }

        trait Iterator {
            type Item;
            fun next(&mut self) -> Option<Self::Item>;
        }

        struct Counter {
            current: i32,
        }

        impl Iterator for Counter {
            type Item = i32;

            fun next(&mut self) -> Option<Self::Item> {
                if self.current < 10 {
                    Option::Some(self.current)
                } else {
                    Option::None
                }
            }
        }

        fun main() {
            let mut counter = Counter { current: 0 };
            let value = counter.next();
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn accepts_for_loop_over_into_iterator() {
    let result = check(
        r"
        enum Option<T> {
            Some(T),
            None,
        }

        trait Iterator {
            type Item;
            fun next(&mut self) -> Option<Self::Item>;
        }

        trait IntoIterator {
            type Item;
            type IntoIter;
            fun into_iter(self) -> Self::IntoIter;
        }

        struct Counter {
            current: i32,
        }

        impl Iterator for Counter {
            type Item = i32;

            fun next(&mut self) -> Option<Self::Item> {
                if self.current < 10 {
                    Option::Some(self.current)
                } else {
                    Option::None
                }
            }
        }

        impl IntoIterator for Counter {
            type Item = i32;
            type IntoIter = Counter;

            fun into_iter(self) -> Self::IntoIter {
                self
            }
        }

        fun main() {
            let counter = Counter { current: 0 };
            for item in counter {
                let next = item + 1;
            }
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn accepts_for_loop_over_array() {
    let result = check(
        r"
        fun main() {
            let values = [1, 2, 3];
            for item in values {
                let next = item + 1;
            }
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn matches_const_generic_trait_impl_for_arrays() {
    let result = check(
        r"
        trait Marker {}

        impl<T, const N: usize> Marker for [T; N] {}

        fun takes_marker<T: Marker>(value: T) {}

        fun main() {
            takes_marker([1, 2, 3]);
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn array_into_iterator_impl_type_checks_with_const_generics() {
    let result = check(
        r"
        enum Option<T> {
            Some(T),
            None,
        }

        trait Iterator {
            type Item;
            fun next(&mut self) -> Option<Self::Item>;
        }

        trait IntoIterator {
            type Item;
            type IntoIter;
            fun into_iter(self) -> Self::IntoIter;
        }

        struct ArrayIter<T, const N: usize> {
            values: [T; N],
            index: usize,
        }

        impl<T, const N: usize> Iterator for ArrayIter<T, N> {
            type Item = T;

            fun next(&mut self) -> Option<Self::Item> {
                if self.index < N {
                    let value = self.values[self.index];
                    self.index += 1usize;
                    Option::Some(value)
                } else {
                    Option::None
                }
            }
        }

        impl<T, const N: usize> IntoIterator for [T; N] {
            type Item = T;
            type IntoIter = ArrayIter<T, N>;

            fun into_iter(self) -> Self::IntoIter {
                ArrayIter {
                    values: self,
                    index: 0usize,
                }
            }
        }

        struct Token {
            value: i32,
        }

        fun main() {
            let values = [Token { value: 1 }, Token { value: 2 }, Token { value: 3 }];
            let mut iter = values.into_iter();
            let first = iter.next();

            for item in [Token { value: 4 }, Token { value: 5 }] {
                let next = item.value + 1;
            }
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn checks_multiple_generic_trait_bounds() {
    let result = check(
        r"
        trait Named {
            fun name(&self) -> i32;
        }

        trait Tagged {
            fun tag(&self) -> i32;
        }

        struct Good { id: i32, tag_value: i32 }
        struct MissingTag { id: i32 }

        impl Named for Good {
            fun name(&self) -> i32 { self.id }
        }

        impl Tagged for Good {
            fun tag(&self) -> i32 { self.tag_value }
        }

        impl Named for MissingTag {
            fun name(&self) -> i32 { self.id }
        }

        fun read<T: Named + Tagged>(value: T) -> i32 {
            value.name() + value.tag()
        }

        fun main() {
            let good = Good { id: 1, tag_value: 2 };
            let missing = MissingTag { id: 3 };
            let ok = read(good);
            let err = read(missing);
        }
        ",
    );

    let msgs = messages(&result);
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("does not satisfy bound `Tagged`")),
        "{msgs:?}"
    );
}

#[test]
fn accepts_where_clause_on_function_bound() {
    let result = check(
        r"
        trait Named {
            fun name(&self) -> i32;
        }

        struct User { id: i32 }

        impl Named for User {
            fun name(&self) -> i32 {
                self.id
            }
        }

        fun read<T>(value: T) -> i32
        where T: Named
        {
            value.name()
        }

        fun main() {
            let user = User { id: 1 };
            let id = read(user);
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn accepts_where_clause_on_impl_bound() {
    let result = check(
        r"
        trait Marker {}
        trait Wrap {}

        struct Box<T> { value: T }
        struct Bad {}

        impl Marker for i32 {}

        impl<T> Wrap for Box<T>
        where T: Marker
        {}

        fun takes_wrap<T: Wrap>(value: T) {}

        fun main() {
            takes_wrap(Box { value: 1 });
            takes_wrap(Box { value: Bad {} });
        }
        ",
    );

    let msgs = messages(&result);
    assert_eq!(
        msgs.iter()
            .filter(|msg| msg.contains("does not satisfy bound `Wrap`"))
            .count(),
        1,
        "{msgs:?}"
    );
}

#[test]
fn generic_bound_proves_nested_generic_impl_bound() {
    let result = check(
        r"
        trait Debug {}

        struct Vector<T> { value: T }

        impl Debug for i32 {}

        impl<T> Debug for Vector<T>
        where T: Debug
        {}

        fun write_debug<T: Debug>(value: &T) {}

        fun forward<T: Debug>(value: &Vector<T>) {
            write_debug(value);
        }

        fun main() {
            let value = Vector { value: 1 };
            forward(&value);
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn rejects_impl_where_clause_that_violates_paterson_condition() {
    let result = check(
        r"
        trait Foo {}

        struct Vec<T> { value: T }

        impl<T> Foo for T
        where Vec<T>: Foo
        {}
        ",
    );

    let msgs = messages(&result);
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("not strictly smaller than implemented type")),
        "{msgs:?}"
    );
}

#[test]
fn reports_unknown_generic_trait_bound() {
    let result = check(
        r"
        fun accept<T: Missing>(value: T) -> T {
            value
        }

        fun main() {
            let value = accept(1);
        }
        ",
    );

    let msgs = messages(&result);
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("generic bound references unknown trait `Missing`")),
        "{msgs:?}"
    );
}

#[test]
fn checks_struct_and_enum_inline_bounds() {
    let result = check(
        r"
        trait First {}
        trait Second {}

        struct Good {}
        struct FirstOnly {}
        struct Bad {}

        impl First for Good {}
        impl Second for Good {}
        impl First for FirstOnly {}

        struct Box<T: First + Second> { value: T }
        enum Slot<T: First> { Some(T), None }

        fun main() {
            let good_box = Box { value: Good {} };
            let good_slot = Slot::Some(Good {});
            let missing_second = Box { value: FirstOnly {} };
            let bad_box = Box { value: Bad {} };
            let bad_slot = Slot::Some(Bad {});
        }
        ",
    );

    let msgs = messages(&result);
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("type `Bad` does not satisfy bound `First` for `Box`")),
        "{msgs:?}"
    );
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("type `Bad` does not satisfy bound `First` for `Slot`")),
        "{msgs:?}"
    );
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("type `FirstOnly` does not satisfy bound `Second` for `Box`")),
        "{msgs:?}"
    );
}

#[test]
fn accepts_for_loop_over_generic_into_iterator_bound() {
    let result = check(
        r"
        enum Option<T> { Some(T), None }

        trait Iterator {
            type Item;
            fun next(&mut self) -> Option<Self::Item>;
        }

        trait IntoIterator {
            type Item;
            type IntoIter;
            fun into_iter(self) -> Self::IntoIter;
        }

        struct Counter { current: i32 }

        impl Iterator for Counter {
            type Item = i32;
            fun next(&mut self) -> Option<Self::Item> { Option::None }
        }

        impl IntoIterator for Counter {
            type Item = i32;
            type IntoIter = Counter;
            fun into_iter(self) -> Self::IntoIter { self }
        }

        fun consume<T: IntoIterator<Item = i32, IntoIter = Counter>>(values: T) {
            for value in values {
                let next = value + 1;
            }
        }

        fun main() {
            consume(Counter { current: 0 });
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn rejects_iterator_next_with_non_option_result() {
    let result = check(
        r"
        enum Option<T> { Some(T), None }

        trait Iterator {
            type Item;
            fun next(&mut self) -> bool;
        }

        trait IntoIterator {
            type Item;
            type IntoIter;
            fun into_iter(self) -> Self::IntoIter;
        }

        struct Counter { current: i32 }

        impl Iterator for Counter {
            type Item = i32;
            fun next(&mut self) -> bool { false }
        }

        impl IntoIterator for Counter {
            type Item = i32;
            type IntoIter = Counter;
            fun into_iter(self) -> Self::IntoIter { self }
        }

        fun main() {
            let counter = Counter { current: 0 };
            for value in counter {
                let next = value + 1;
            }
        }
        ",
    );

    assert!(
        result
            .diagnostics
            .iter()
            .any(|diag| diag.message.contains("Iterator::next"))
    );
}

#[test]
fn accepts_outer_attributes_on_common_ast_nodes() {
    let result = check(
        r"
        #[item]
        struct Boxed {
            #[field]
            value: i32,
        }

        #[item]
        enum Option {
            #[variant]
            Some(#[variant_ty] i32),
            None,
        }

        #[item]
        fun id(#[param] value: #[ty] i32) -> i32 {
            let x = #[expr] value;
            match x {
                #[arm] #[pat] other => other,
            }
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn supertrait_bound_exposes_parent_methods() {
    let result = check(
        r"
        trait Named {
            fun name(&self) -> i32;
        }

        trait Tagged: Named {
            fun tag(&self) -> i32;
        }

        struct Item { value: i32 }

        impl Named for Item {
            fun name(&self) -> i32 { self.value }
        }

        impl Tagged for Item {
            fun tag(&self) -> i32 { self.value }
        }

        fun describe<T: Tagged>(value: T) -> i32 {
            value.name() + value.tag()
        }

        fun main() {
            let value = describe(Item { value: 1 });
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn supertrait_impl_requires_parent_impl() {
    let result = check(
        r"
        trait Parent {}
        trait Child: Parent {}

        struct Item {}
        impl Child for Item {}
        ",
    );

    let msgs = messages(&result);
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("impl `Child` for `Item` requires `Parent`")),
        "{msgs:?}"
    );
}

#[test]
fn reports_invalid_supertraits() {
    let result = check(
        r"
        trait MissingParent: Unknown {}
        trait First: Second {}
        trait Second: First {}
        ",
    );

    let msgs = messages(&result);
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("unknown supertrait `Unknown`")),
        "{msgs:?}"
    );
    assert!(
        msgs.iter().any(|msg| msg.contains("supertrait cycle")),
        "{msgs:?}"
    );
}

#[test]
fn trait_default_method_can_call_required_method() {
    let result = check(
        r"
        trait Value {
            fun base(&self) -> i32;
            fun value(&self) -> i32 {
                self.base() + 1
            }
        }

        struct Item {}
        impl Value for Item {
            fun base(&self) -> i32 { 6 }
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn std_mode_rejects_user_defined_lang_copy_trait() {
    let result = riddlec::pipeline::check_with_options(
        r#"#[lang = "copy"] trait UserCopy {}"#,
        riddlec::pipeline::CompileOptions::default(),
    );
    let diagnostics = &result.type_result.diagnostics;
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0049"),
        "expected E0049 diagnostic, got: {diagnostics:?}"
    );
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0049" && diagnostic.message.contains("copy")),
        "expected message to contain \"copy\": {diagnostics:?}"
    );
}

#[test]
fn std_mode_rejects_user_defined_fundamental_struct() {
    let result = riddlec::pipeline::check_with_options(
        r"#[fundamental] struct MyBox<T> { value: T }",
        riddlec::pipeline::CompileOptions::default(),
    );
    let has_e0049 = result
        .type_result
        .diagnostics
        .iter()
        .any(|diagnostic| diagnostic.code == "E0049");
    assert!(
        has_e0049,
        "expected E0049 diagnostic, got: {:?}",
        result.type_result.diagnostics
    );
}

#[test]
fn no_std_mode_reports_unknown_lang_item() {
    let result = riddlec::pipeline::check_with_options(
        r#"#[lang = "unknown_item"] trait Foo {}"#,
        riddlec::pipeline::CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    let e0053: Vec<_> = result
        .type_result
        .diagnostics
        .iter()
        .filter(|d| d.code == "E0053")
        .collect();
    assert!(
        !e0053.is_empty(),
        "expected E0053 diagnostic, got: {:?}",
        result.type_result.diagnostics
    );
    assert!(
        e0053
            .iter()
            .any(|d| d.message.contains("unknown lang item")),
        "expected message to contain \"unknown lang item\": {e0053:?}"
    );
}

#[test]
fn no_std_mode_reports_duplicate_lang_item() {
    let result = riddlec::pipeline::check_with_options(
        r#"
        #[lang = "copy"] trait Copy1 {}
        #[lang = "copy"] trait Copy2 {}
        "#,
        riddlec::pipeline::CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    let e0053: Vec<_> = result
        .type_result
        .diagnostics
        .iter()
        .filter(|d| d.code == "E0053")
        .collect();
    assert_eq!(
        e0053.len(),
        1,
        "expected exactly 1 E0053 diagnostic, got: {:?}",
        result.type_result.diagnostics
    );
    assert!(
        e0053[0].message.contains("defined more than once"),
        "expected message to contain \"defined more than once\": {:?}",
        e0053[0].message
    );
}

#[test]
fn no_std_mode_reports_missing_lang_value() {
    let result = riddlec::pipeline::check_with_options(
        "#[lang] trait Copy {}",
        riddlec::pipeline::CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(result.type_result.diagnostics.iter().any(|diagnostic| {
        diagnostic.code == "E0053" && diagnostic.message.contains("requires a string value")
    }));
}

#[test]
fn no_std_mode_rejects_multiple_lang_items_on_one_trait() {
    let result = riddlec::pipeline::check_with_options(
        r#"
        #[lang = "copy"]
        #[lang = "eq"]
        trait Marker {}
        "#,
        riddlec::pipeline::CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(result.type_result.diagnostics.iter().any(|diagnostic| {
        diagnostic.code == "E0053"
            && diagnostic
                .message
                .contains("at most one `#[lang]` attribute")
    }));
}

#[test]
fn std_mode_rejects_internal_attributes_before_validating_their_shape() {
    let cases = [
        r"#[lang] trait UserCopy {}",
        r#"#[lang = "copy"] struct UserCopy {}"#,
        r"#[fundamental] fun user_function() {}",
    ];

    for source in cases {
        let result = riddlec::pipeline::check_with_options(
            source,
            riddlec::pipeline::CompileOptions::default(),
        );
        assert!(
            result
                .type_result
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.code == "E0049"),
            "expected E0049 for {source:?}, got: {:?}",
            result.type_result.diagnostics
        );
        assert!(
            result
                .type_result
                .diagnostics
                .iter()
                .all(|diagnostic| diagnostic.code != "E0053"),
            "source gating must precede shape validation for {source:?}: {:?}",
            result.type_result.diagnostics
        );
    }
}

#[test]
fn no_std_mode_rejects_lang_on_every_non_trait_target() {
    let cases = [
        r#"#[lang = "copy"] mod nested {}"#,
        r#"mod nested {} #[lang = "copy"] use nested;"#,
        r#"struct Item {} #[lang = "copy"] impl Item {}"#,
        r#"#[lang = "copy"] const VALUE: i32 = 1;"#,
        r#"#[lang = "copy"] type Value = i32;"#,
        r#"struct Item { #[lang = "copy"] value: i32 }"#,
        r#"fun take(#[lang = "copy"] value: i32) {}"#,
        r#"enum State { #[lang = "copy"] Ready }"#,
        r#"trait Value { #[lang = "copy"] fun get(&self); }"#,
        r#"trait Value { #[lang = "copy"] type Item; }"#,
        r#"fun take(value: #[lang = "copy"] i32) {}"#,
        r#"fun main() { let value = #[lang = "copy"] 1; }"#,
    ];

    for source in cases {
        let result = riddlec::pipeline::check_with_options(
            source,
            riddlec::pipeline::CompileOptions {
                use_std: false,
                ..Default::default()
            },
        );
        assert!(
            result.type_result.diagnostics.iter().any(|diagnostic| {
                diagnostic.code == "E0053"
                    && diagnostic
                        .message
                        .contains("can only be applied to a trait")
            }),
            "expected invalid lang target diagnostic for {source:?}, got: {:?}",
            result.type_result.diagnostics
        );
    }
}

#[test]
fn no_std_mode_rejects_fundamental_on_non_type_targets() {
    let cases = [
        r"#[fundamental] fun user_function() {}",
        r"#[fundamental] trait UserTrait {}",
        r"struct Item { #[fundamental] value: i32 }",
        r"#[fundamental] type Value = i32;",
    ];

    for source in cases {
        let result = riddlec::pipeline::check_with_options(
            source,
            riddlec::pipeline::CompileOptions {
                use_std: false,
                ..Default::default()
            },
        );
        assert!(
            result.type_result.diagnostics.iter().any(|diagnostic| {
                diagnostic.code == "E0053"
                    && diagnostic
                        .message
                        .contains("can only be applied to a struct or enum")
            }),
            "expected invalid fundamental target diagnostic for {source:?}, got: {:?}",
            result.type_result.diagnostics
        );
    }
}

#[test]
fn no_std_mode_rejects_fundamental_values() {
    let result = riddlec::pipeline::check_with_options(
        r#"#[fundamental = "ignored"] struct Wrapper<T> { value: T }"#,
        riddlec::pipeline::CompileOptions {
            use_std: false,
            ..Default::default()
        },
    );
    assert!(result.type_result.diagnostics.iter().any(|diagnostic| {
        diagnostic.code == "E0053" && diagnostic.message.contains("does not accept a value")
    }));
}

#[test]
fn no_std_mode_rejects_incomplete_lang_item_signatures() {
    let cases = [
        r#"#[lang = "copy"] trait Copy<T> {}"#,
        r#"#[lang = "eq"] trait Eq<T> {}"#,
        r#"#[lang = "clone"] trait Clone { fun clone<T>(&self) -> Self; }"#,
        r#"#[lang = "partial_eq"] trait PartialEq { fun eq(&self, other: i32) -> bool; }"#,
        r#"#[lang = "partial_ord"] trait PartialOrd { fun partial_cmp() -> bool; }"#,
        r#"#[lang = "ord"] trait Ord { fun cmp(&self, other: i32) -> bool; }"#,
        r#"#[lang = "add"] trait Add<Rhs = Self> { type Output; fun add(self, rhs: bool) -> Self::Output; }"#,
        r#"#[lang = "add_assign"] trait AddAssign<Rhs = Self> { fun add_assign(&mut self, rhs: bool); }"#,
        r#"#[lang = "index"] trait Index { type Output; fun index(&self, index: usize) -> &Self::Output; }"#,
        r#"#[lang = "index_mut"] trait IndexMut<Idx> { fun index_mut(&mut self, index: Idx) -> &mut Self::Output; }"#,
    ];

    for source in cases {
        let result = riddlec::pipeline::check_with_options(
            source,
            riddlec::pipeline::CompileOptions {
                use_std: false,
                ..Default::default()
            },
        );
        assert!(
            result.type_result.diagnostics.iter().any(|diagnostic| {
                diagnostic.code == "E0053" && diagnostic.message.contains("invalid trait signature")
            }),
            "expected invalid lang signature diagnostic for {source:?}, got: {:?}",
            result.type_result.diagnostics
        );
    }
}

#[test]
fn rejects_callable_impl_without_signature() {
    let result = check(
        r"
        struct Callable {}
        impl Fn for Callable {}
        ",
    );
    assert!(result.diagnostics.iter().any(|diagnostic| {
        diagnostic.code == "E0047" && diagnostic.message.contains("requires a callable signature")
    }));
}

#[test]
fn by_value_operator_capture_is_fn_once() {
    let result = check(
        r#"
        #[lang = "add"]
        trait Add {
            type Output;
            fun add(self, rhs: Self) -> Self::Output;
        }
        struct Token { value: i32 }
        impl Add for Token {
            type Output = i32;
            fun add(self, rhs: Self) -> i32 { self.value + rhs.value }
        }
        fun main() {
            let left = Token { value: 1 };
            let right = Token { value: 2 };
            let add = [ -> left + right];
        }
        "#,
    );

    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(
        result.lambda_infos.values().next().unwrap().kind,
        type_checker::ClosureKind::FnOnce
    );
}

// == coherence 重叠判定的结构化（别名、统一） ==

#[test]
fn conflicting_impls_through_type_alias_are_rejected() {
    // `type AliasPoint = Point` must not dodge the E0047 overlap check:
    // both impls denote the same nominal type after alias resolution.
    let result = check(
        r#"
        trait Greeter {
            fun hi(&self) -> i32;
        }

        struct Point { x: i32 }

        type AliasPoint = Point;

        impl Greeter for Point {
            fun hi(&self) -> i32 { 1 }
        }

        impl Greeter for AliasPoint {
            fun hi(&self) -> i32 { 2 }
        }
        "#,
    );

    assert!(
        result.diagnostics.iter().any(|d| d.code == "E0047"),
        "expected E0047 for conflicting impls written through an alias, got {:#?}",
        result.diagnostics
    );
}

#[test]
fn conflicting_generic_impl_unification_is_rejected() {
    // `impl<T> Trait for Wrap<T>` overlaps `impl Trait for Wrap<u8>` by
    // structural unification, regardless of how each header is printed.
    let result = check(
        r#"
        trait Sized2 {
            fun size(&self) -> usize;
        }

        struct Wrap<T> { inner: T }

        impl<T> Sized2 for Wrap<T> {
            fun size(&self) -> usize { 1 }
        }

        impl Sized2 for Wrap<u8> {
            fun size(&self) -> usize { 2 }
        }
        "#,
    );

    assert!(
        result.diagnostics.iter().any(|d| d.code == "E0047"),
        "expected E0047 for overlapping generic/specific impls, got {:#?}",
        result.diagnostics
    );
}

#[test]
fn distinct_generic_impls_still_coexist() {
    // `Wrap<u8>` and `Wrap<i8>` are distinct under unification: no E0047.
    let result = check(
        r#"
        trait Sized2 {
            fun size(&self) -> usize;
        }

        struct Wrap<T> { inner: T }

        impl Sized2 for Wrap<u8> {
            fun size(&self) -> usize { 1 }
        }

        impl Sized2 for Wrap<i8> {
            fun size(&self) -> usize { 2 }
        }
        "#,
    );

    assert!(
        !result.diagnostics.iter().any(|d| d.code == "E0047"),
        "distinct instantiations must not conflict, got {:#?}",
        result.diagnostics
    );
}

#[test]
fn fundamental_foreign_types_cover_orphan_params() {
    // `impl<T> Foreign<FundBox<T>, Local> for Foreign2` style headers: a
    // `#[fundamental]` foreign type covers its parameters, so the param under
    // `FundBox` no longer triggers the uncovered-parameter orphan error.
    let foreign = r"
        trait Foreign2<A, B> {}
        struct Foreign2Type {}
        #[fundamental]
        struct FundBox<T> { value: T }
    ";
    let local = r"
        struct Local { value: i32 }

        impl<T> Foreign2<FundBox<T>, Local> for Foreign2Type {}
    ";
    let source = format!("{foreign}{local}");
    let result =
        check_with_package_ranges(&source, &[0..foreign.len(), foreign.len()..source.len()]);
    assert!(
        !result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0048"),
        "fundamental foreign types must cover their params, got {:#?}",
        result.diagnostics
    );
}

#[test]
fn accepts_structured_associated_type_bindings() {
    // The bound value is a pattern over the callee's generics, so it must be
    // matched against the impl's associated type at the parameter's own
    // position. Binding the whole associated type to every parameter the
    // pattern mentions rejected every reference, tuple, and generic-struct
    // constraint with a spurious E0035.
    let reference_case = check(
        r"
        trait Source {
            type Out;
            fun get(&self) -> Self::Out;
        }

        struct Holder {
            value: i32,
        }

        impl Source for Holder {
            type Out = &i32;
            fun get(&self) -> &i32 { &self.value }
        }

        fun read<S, T>(source: &S) -> bool
        where S: Source<Out = &T> {
            let held = source.get();
            true
        }

        fun main() -> bool {
            let holder = Holder { value: 7 };
            read(&holder)
        }
        ",
    );
    assert_eq!(reference_case.diagnostics, vec![]);

    let tuple_case = check(
        r"
        trait Pairing {
            type Out;
            fun get(&self) -> Self::Out;
        }

        struct Holder {
            value: (i32, bool),
        }

        impl Pairing for Holder {
            type Out = (i32, bool);
            fun get(&self) -> (i32, bool) { self.value }
        }

        fun read<S, T>(source: &S) -> bool
        where S: Pairing<Out = (T, bool)> {
            let held = source.get();
            true
        }

        fun main() -> bool {
            let holder = Holder { value: (1, true) };
            read(&holder)
        }
        ",
    );
    assert_eq!(tuple_case.diagnostics, vec![]);

    let loop_case = check(
        r"
        enum Option<T> {
            Some(T),
            None,
        }

        trait Iterator {
            type Item;
            fun next(&mut self) -> Option<Self::Item>;
        }

        trait IntoIterator {
            type Item;
            type IntoIter;
            fun into_iter(self) -> Self::IntoIter;
        }

        struct Items {
            value: i32,
        }

        impl Iterator for Items {
            type Item = &i32;

            fun next(&mut self) -> Option<Self::Item> {
                Option::None
            }
        }

        impl IntoIterator for Items {
            type Item = &i32;
            type IntoIter = Items;

            fun into_iter(self) -> Self::IntoIter {
                self
            }
        }

        fun count<I, J, T>(source: I) -> bool
        where I: IntoIterator<Item = &T, IntoIter = J>,
              J: Iterator<Item = &T> {
            for item in source {
                let held = item;
            }
            true
        }

        fun main() -> bool {
            count(Items { value: 1 })
        }
        ",
    );
    assert_eq!(loop_case.diagnostics, vec![]);
}
