use crate::{check, messages};

#[test]
fn checks_struct_literals_and_field_access() {
    let result = check(
        r"
        struct Point {
            x: i32,
            y: bool,
        }

        fun main() -> i32 {
            let p = Point{x: 1, y: false};
            p.x
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn checks_tuple_field_access() {
    let result = check(
        r"
        fun first() -> i32 { (1, true).0 }

        fun second() -> bool {
            let values = (1, true);
            values.1
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn checks_struct_literal_shorthand_and_associated_function_call() {
    let result = check(
        r#"
        struct Point {
            x: i32,
            y: i64,
            label: &str,
        }

        impl Point {
            fun new(x: i32, y: i64, label: &str) -> Point {
                Point{x, y, label}
            }
        }

        fun main() -> i32 {
            let p = Point::new(1, 2, "origin");
            p.x
        }
        "#,
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn checks_generic_struct_literals_and_field_access() {
    let result = check(
        r"
        struct Box<T> {
            value: T,
        }

        fun main() -> i32 {
            let b: Box<i32> = Box { value: 1 };
            b.value
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn field_access_expected_type_constrains_generic_base() {
    let result = check(
        r#"
        unsafe extern "C" {
            safe fun fail() -> !;
        }

        struct Box<T> {
            value: T,
        }

        fun make<T>() -> Box<T> { Box { value: fail() } }
        fun consume(value: impl Fn(i32) -> i32) {}

        fun main() {
            consume(make().value);
        }
        "#,
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn infers_associated_function_generics_from_bound_aggregate_fields() {
    let result = check(
        r"
        struct Inner<K, V> {}

        impl<K, V> Inner<K, V> {
            fun new() -> Inner<K, V> {
                Inner {}
            }
        }

        struct Outer<T> {
            inner: Inner<T, bool>,
        }

        impl<T> Outer<T> {
            fun new() -> Outer<T> {
                Outer { inner: Inner::new() }
            }
        }

        enum NamedOuter<T> {
            Value { inner: Inner<T, bool> },
        }

        impl<T> NamedOuter<T> {
            fun new() -> NamedOuter<T> {
                NamedOuter::Value { inner: Inner::new() }
            }
        }

        enum TupleOuter<T> {
            Value(Inner<T, bool>),
        }

        impl<T> TupleOuter<T> {
            fun new() -> TupleOuter<T> {
                TupleOuter::Value(Inner::new())
            }
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn checks_generic_impl_receiver_substitution() {
    let result = check(
        r"
        struct Box<T> {
            value: T,
        }

        impl<T> Box<T> {
            fun get(&self) -> T {
                self.value
            }
        }

        fun main() -> i32 {
            let b: Box<i32> = Box { value: 1 };
            b.get()
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn checks_generic_impl_ref_return_substitution() {
    let result = check(
        r"
        struct Box<T> {
            value: T,
        }

        impl<T> Box<T> {
            fun get(&self) -> &T {
                &self.value
            }
        }

        fun main() {
            let b: Box<i32> = Box { value: 1 };
            let value = b.get();
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn checks_nested_generic_type_args_without_spaces() {
    let result = check(
        r"
        struct Box<T> {
            value: T,
        }

        fun main() -> i32 {
            let b: Box<Box<Box<i32>>> = Box { value: Box { value: Box { value: 1 } } };
            b.value.value.value
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn reports_struct_literal_field_errors() {
    let result = check(
        r"
        struct Point {
            x: i32,
            y: bool,
        }

        fun main() {
            Point{x: true, z: 1};
        }
        ",
    );

    let msgs = messages(&result);
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("struct field type mismatch"))
    );
    assert!(msgs.iter().any(|msg| msg.contains("unknown field `z`")));
    assert!(msgs.iter().any(|msg| msg.contains("missing field `y`")));
}

#[test]
fn reports_invalid_field_access() {
    let result = check(
        r"
        struct Point {
            x: i32,
        }

        fun main() {
            let p = Point{x: 1};
            p.y;
            (1).x;
        }
        ",
    );

    let msgs = messages(&result);
    assert!(msgs.iter().any(|msg| msg.contains("unknown field `y`")));
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("cannot access field `x` on type i32"))
    );
}

#[test]
fn rejects_private_field_access_outside_its_module() {
    let result = check(
        r"
        mod model {
            pub struct Point {
                x: i32,
                pub y: i32,
            }

            pub fun make() -> Point {
                Point { x: 1, y: 2 }
            }

            pub fun read(point: &Point) -> i32 {
                point.x
            }

            pub mod nested {
                pub fun read(point: &super::Point) -> i32 {
                    point.x
                }
            }
        }

        fun main() -> i32 {
            let point = model::make();
            point.x + point.y + model::read(&point) + model::nested::read(&point)
        }
        ",
    );

    let private = result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "E0054")
        .collect::<Vec<_>>();
    assert_eq!(private.len(), 1, "diagnostics: {:?}", result.diagnostics);
    assert!(
        private[0]
            .message
            .contains("field `x` of struct `Point` is private")
    );
}

#[test]
fn rejects_private_method_calls_outside_the_defining_module() {
    let result = check(
        r"
        mod model {
            pub struct Point {}

            impl Point {
                fun secret(&self) -> i32 { 1 }
                pub fun shown(&self) -> i32 { 2 }
            }

            pub fun make() -> Point { Point {} }
            pub fun inside(point: &Point) -> i32 { point.secret() }

            pub mod nested {
                pub fun inside(point: &super::Point) -> i32 { point.secret() }
            }
        }

        fun main() -> i32 {
            let point = model::make();
            point.secret() + point.shown() + model::inside(&point) + model::nested::inside(&point)
        }
        ",
    );

    let private = result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "E0054")
        .collect::<Vec<_>>();
    assert_eq!(private.len(), 1, "diagnostics: {:?}", result.diagnostics);
    assert!(
        private[0]
            .message
            .contains("method `secret` of struct `Point` is private")
    );
}

#[test]
fn rejects_private_field_in_struct_literal_outside_its_module() {
    let result = check(
        r"
        mod model {
            pub struct Point {
                x: i32,
                pub y: i32,
            }
        }

        fun main() {
            model::Point { x: 1, y: 2 };
        }
        ",
    );

    let private = result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "E0054")
        .collect::<Vec<_>>();
    assert_eq!(private.len(), 1, "diagnostics: {:?}", result.diagnostics);
    assert!(
        private[0]
            .message
            .contains("field `x` of struct `Point` is private")
    );
}

#[test]
fn rejects_private_field_in_struct_pattern_outside_its_module() {
    let result = check(
        r"
        mod model {
            pub struct Point {
                x: i32,
                pub y: i32,
            }

            pub fun make() -> Point {
                Point { x: 1, y: 2 }
            }
        }

        fun main() -> i32 {
            match model::make() {
                model::Point { x, y } => x + y,
            }
        }
        ",
    );

    let private = result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "E0054")
        .collect::<Vec<_>>();
    assert_eq!(private.len(), 1, "diagnostics: {:?}", result.diagnostics);
    assert!(
        private[0]
            .message
            .contains("field `x` of struct `Point` is private")
    );
}

#[test]
fn reports_generic_type_arg_count_mismatch() {
    let result = check(
        r"
        struct Box<T> {
            value: T,
        }

        fun main() {
            let b: Box<i32, bool> = Box { value: 1 };
        }
        ",
    );

    let msgs = messages(&result);
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("expects 1 type argument(s), got 2"))
    );
}

#[test]
fn reports_direct_recursive_struct_with_infinite_size() {
    let result = check(
        r"
        struct KSK {
            x: KSK,
        }
        ",
    );

    assert!(result.diagnostics.iter().any(|diag| {
        diag.code == "E0072"
            && diag
                .message
                .contains("recursive type `KSK` has infinite size")
    }));
}

#[test]
fn reports_indirect_recursive_struct_with_infinite_size() {
    let result = check(
        r"
        struct A {
            b: B,
        }

        struct B {
            a: A,
        }
        ",
    );

    let msgs = messages(&result);
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("recursive type `A` has infinite size"))
    );
    assert!(
        msgs.iter()
            .any(|msg| msg.contains("recursive type `B` has infinite size"))
    );
}

#[test]
fn accepts_recursive_struct_behind_indirection() {
    let result = check(
        r"
        struct Node {
            next: &Node,
        }

        struct RawNode {
            next: *const RawNode,
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn accepts_a_mut_field_on_a_struct() {
    let result = check(
        r"
        struct Counter {
            mut hits: i32,
            pub mut name: i32,
        }

        fun main() -> i32 {
            let counter = Counter { hits: 0, name: 1 };
            counter.hits
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn rejects_a_mut_field_on_an_enum_variant() {
    // Variant fields are only reachable through a pattern, and a pattern
    // binding is a local of its own, so the modifier could never take effect.
    let result = check(
        r"
        enum Shape {
            Circle { mut radius: i32 },
            Square { side: i32 },
        }
        ",
    );

    assert!(
        result.diagnostics.iter().any(|d| d.code == "E0014"),
        "{:?}",
        result.diagnostics
    );
}
