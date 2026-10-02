use crate::{analyze, messages};

const INDEX_TRAITS: &str = r#"
#[lang = "index"]
trait Index<Idx = usize> {
    type Output;
    fun index(&self, index: Idx) -> &Self::Output;
}

#[lang = "index_mut"]
trait IndexMut<Idx = usize>: Index<Idx> {
    fun index_mut(&mut self, index: Idx) -> &mut Self::Output;
}
"#;

#[test]
fn delayed_initialization_is_allowed_before_use() {
    let result = analyze(
        r"
        fun f() -> i32 {
            let value: i32;
            value = 7;
            value
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn trait_index_cannot_move_non_copy_output() {
    let result = analyze(&format!(
        r"
        {INDEX_TRAITS}

        struct Token {{ value: i32 }}
        struct Slot {{ value: Token }}

        impl Index for Slot {{
            type Output = Token;
            fun index(&self, index: usize) -> &Self::Output {{ &self.value }}
        }}

        fun consume(value: Token) {{}}

        fun main() {{
            let slot = Slot {{ value: Token {{ value: 1 }} }};
            consume(slot[0usize]);
        }}
        "
    ));
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0308"),
        "{:#?}",
        result.diagnostics
    );
}

#[test]
fn trait_index_borrow_blocks_mutable_receiver_call() {
    let result = analyze(&format!(
        r"
        {INDEX_TRAITS}

        struct Slot {{ value: i32 }}

        impl Index for Slot {{
            type Output = i32;
            fun index(&self, index: usize) -> &Self::Output {{ &self.value }}
        }}

        impl Slot {{
            fun set(&mut self, value: i32) {{ self.value = value; }}
        }}

        fun main() {{
            let mut slot = Slot {{ value: 1 }};
            let value = &slot[0usize];
            slot.set(2);
            *value;
        }}
        "
    ));
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0300"),
        "{:#?}",
        result.diagnostics
    );
}

#[test]
fn delayed_tuple_bindings_are_initialized_independently() {
    let result = analyze(
        r"
        fun f() -> i32 {
            let (first, second): (i32, i32);
            first = 1;
            second = 2;
            first + second
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn delayed_initialization_use_before_assignment_is_rejected() {
    let result = analyze(
        r"
        fun f() -> i32 {
            let value: i32;
            value
        }
        ",
    );
    assert!(
        result.diagnostics.iter().any(|diagnostic| {
            diagnostic.code == "E0059" && diagnostic.message.contains("uninitialized")
        }),
        "{:#?}",
        result.diagnostics
    );
}

#[test]
fn delayed_initialization_requires_every_branch() {
    let complete = analyze(
        r"
        fun f(flag: bool) -> i32 {
            let value: i32;
            if flag { value = 1; } else { value = 2; }
            value
        }
        ",
    );
    assert!(
        complete.diagnostics.is_empty(),
        "{:#?}",
        complete.diagnostics
    );

    let incomplete = analyze(
        r"
        fun f(flag: bool) -> i32 {
            let value: i32;
            if flag { value = 1; }
            value
        }
        ",
    );
    assert!(
        incomplete
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0059"),
        "{:#?}",
        incomplete.diagnostics
    );
}

#[test]
fn immutable_delayed_binding_rejects_second_assignment() {
    let result = analyze(
        r"
        fun f() {
            let value: i32;
            value = 1;
            value = 2;
        }
        ",
    );
    assert!(
        result.diagnostics.iter().any(|diagnostic| {
            diagnostic.code == "E0031" && diagnostic.message.contains("not declared as mutable")
        }),
        "{:#?}",
        result.diagnostics
    );
}

#[test]
fn mutable_delayed_binding_can_be_assigned_repeatedly() {
    let result = analyze(
        r"
        fun f() -> i32 {
            let mut value: i32;
            value = 1;
            value = 2;
            value
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn delayed_compound_assignment_requires_initialization() {
    let result = analyze(
        r"
        fun f() {
            let mut value: i32;
            value += 1;
        }
        ",
    );
    assert_eq!(
        result
            .diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.code == "E0059")
            .count(),
        1,
        "{:#?}",
        result.diagnostics
    );
}

// == Copy types: no errors ==

#[test]
fn ints_are_copy_no_move_errors() {
    let result = analyze(
        r"
        fun f() {
            let a: i32 = 1;
            let b = a;
            let c = a;
        }
        ",
    );
    assert!(result.diagnostics.is_empty());
}

#[test]
fn bools_are_copy_no_move_errors() {
    let result = analyze(
        r"
        fun f() {
            let a: bool = true;
            let b = a;
            let c = a;
        }
        ",
    );
    assert!(result.diagnostics.is_empty());
}

#[test]
fn floats_are_copy_no_move_errors() {
    let result = analyze(
        r"
        fun f() {
            let a: f64 = 3.14;
            let b = a;
            let c = a;
        }
        ",
    );
    assert!(result.diagnostics.is_empty());
}

#[test]
fn references_are_copy() {
    let result = analyze(
        r"
        fun f() {
            let x: i32 = 42;
            let a: &i32 = &x;
            let b = a;
            let c = a;
        }
        ",
    );
    assert!(result.diagnostics.is_empty());
}

#[test]
fn assignment_does_not_move_copy_types() {
    let result = analyze(
        r"
        fun f() {
            let a: i32 = 1;
            let mut b: i32 = 2;
            b = a;
            let c = a;
        }
        ",
    );
    assert!(result.diagnostics.is_empty());
}

// == Struct move (non-Copy) ==

#[test]
fn struct_let_binding_moves_source() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun f() {
            let p = Point{x: 1};
            let q = p;
            let r = p;
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved value") && m.contains('p'))
    );
}

#[test]
fn struct_let_binding_first_use_is_ok() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun f() {
            let a = Point{x: 1};
            let b = a;
        }
        ",
    );
    assert!(result.diagnostics.is_empty());
}

#[test]
fn struct_move_in_function_call() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun take(p: Point) -> bool { true }

        fun f() {
            let p = Point{x: 1};
            take(p);
            take(p);
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved value") && m.contains('p'))
    );
}

#[test]
fn method_call_after_move_is_error() {
    let result = analyze(
        r"
        struct Box<T> { value: T }

        impl<T> Box<T> {
            fun get(&self) -> &T {
                &self.value
            }
        }

        fun f() {
            let x = Box { value: 1 };
            let y = x;
            x.get();
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved value") && m.contains('x'))
    );
}

#[test]
fn moved_local_is_error_on_plain_use() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun f() {
            let p = Point{x: 1};
            let q = p;
            let r = p;
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved value") && m.contains('p'))
    );
}

#[test]
fn moved_local_is_error_on_method_receiver() {
    let result = analyze(
        r"
        struct Box<T> { value: T }

        impl<T> Box<T> {
            fun get(&self) -> &T {
                &self.value
            }
        }

        fun f() {
            let x = Box { value: 1 };
            let y = x;
            x.get();
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved value") && m.contains('x'))
    );
}

#[test]
fn moved_field_blocks_parent_use() {
    let result = analyze(
        r"
        struct Inner { value: i32 }
        struct Outer { inner: Inner, tag: i32 }

        fun f() {
            let outer = Outer{inner: Inner{value: 1}, tag: 2};
            let inner = outer.inner;
            let again = outer;
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved value") && m.contains("outer"))
    );
}

#[test]
fn moved_array_element_blocks_array_use() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun f() {
            let p = Point{x: 1};
            let arr = [p];
            let again = p;
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved value") && m.contains('p'))
    );
}

#[test]
fn copy_types_remain_usable_after_assignment() {
    let result = analyze(
        r"
        fun f() {
            let x: i32 = 1;
            let y = x;
            let z = x;
        }
        ",
    );
    assert!(result.diagnostics.is_empty());
}

#[test]
fn moved_parameter_is_error_on_second_plain_use() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun f(p: Point) {
            let q = p;
            let r = p;
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved value") && m.contains('p'))
    );
}

#[test]
fn moved_parameter_is_error_on_method_receiver() {
    let result = analyze(
        r"
        struct Box<T> { value: T }

        impl<T> Box<T> {
            fun get(&self) -> &T {
                &self.value
            }
        }

        fun f(x: Box<i32>) {
            let y = x;
            x.get();
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved value") && m.contains('x'))
    );
}

#[test]
fn copy_parameter_remains_usable_after_assignment() {
    let result = analyze(
        r"
        fun f(x: i32) {
            let y = x;
            let z = x;
        }
        ",
    );
    assert!(result.diagnostics.is_empty());
}

#[test]
fn moved_whole_value_blocks_field_use() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun f() {
            let p = Point{x: 1};
            let q = p;
            let x = p.x;
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved field") && m.contains('x'))
    );
}

#[test]
fn moved_field_blocks_method_on_parent() {
    let result = analyze(
        r"
        struct Inner { value: i32 }
        struct Outer { inner: Inner, tag: i32 }

        impl Outer {
            fun tag(&self) -> i32 {
                self.tag
            }
        }

        fun f() {
            let outer = Outer{inner: Inner{value: 1}, tag: 2};
            let inner = outer.inner;
            outer.tag();
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved value") && m.contains("outer"))
    );
}

#[test]
fn moving_one_field_allows_sibling_field_use() {
    let result = analyze(
        r"
        struct Inner { value: i32 }
        struct Outer { inner: Inner, tag: i32 }

        fun f() {
            let outer = Outer{inner: Inner{value: 1}, tag: 2};
            let inner = outer.inner;
            let tag = outer.tag;
        }
        ",
    );
    assert!(result.diagnostics.is_empty());
}

#[test]
fn moved_array_blocks_index_use() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun f() {
            let a = Point{x: 1};
            let b = Point{x: 2};
            let arr = [a, b];
            let moved = arr;
            let first = arr[0];
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved value from array"))
    );
}

#[test]
fn struct_move_in_return_then_use_is_error() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun consume(p: Point) {}

        fun f() {
            let p = Point{x: 1};
            consume(p);
            let q = p;
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved value") && m.contains('p'))
    );
}

#[test]
fn struct_move_in_return_no_reuse_is_ok() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun f() -> Point {
            let p = Point{x: 1};
            return p;
        }
        ",
    );
    assert!(result.diagnostics.is_empty());
}

// == Assignment ==

#[test]
fn assignment_moves_rhs_struct() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun f() {
            let a = Point{x: 1};
            let mut b = Point{x: 2};
            b = a;
            let c = a;
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved value") && m.contains('a'))
    );
}

#[test]
fn assignment_reinitializes_binding_after_rhs_move() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun consume(value: Point) -> Point { value }

        fun f(flag: bool) {
            let mut p = Point{x: 1};
            if flag { p = consume(p); } else { }
            let q = p;
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn assignment_reinitializes_previously_moved_binding() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun f() {
            let mut p = Point{x: 1};
            let moved = p;
            p = Point{x: 2};
            let reused = p;
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

// == Match scrutinee ==

#[test]
fn match_copy_fields_keep_scrutinee_available() {
    let result = analyze(
        r"
        struct Point { x: i32, y: i32 }

        fun f() {
            let p = Point{x: 1, y: 2};
            match p {
                Point { x, y } => { let tmp = x; }
            }
            let q = p;
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn match_move_keeps_unbound_sibling_field_available() {
    let result = analyze(
        r"
        struct Token {}
        struct Pair { left: Token, right: Token }
        fun consume(value: Token) {}

        fun f() {
            let pair = Pair { left: Token {}, right: Token {} };
            match pair {
                Pair { left } => { consume(left); }
            }
            consume(pair.right);
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn match_moved_field_remains_unavailable() {
    let result = analyze(
        r"
        struct Token {}
        struct Pair { left: Token, right: Token }
        fun consume(value: Token) {}

        fun f() {
            let pair = Pair { left: Token {}, right: Token {} };
            match pair {
                Pair { left } => {}
            }
            consume(pair.left);
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|message| message.contains("use of moved field") && message.contains("left")),
        "{:#?}",
        result.diagnostics
    );
}

#[test]
fn match_int_scrutinee_is_not_moved() {
    let result = analyze(
        r"
        fun f() {
            let v: i32 = 42;
            match v {
                other => {}
            }
            let w = v;
        }
        ",
    );
    assert!(result.diagnostics.is_empty());
}

#[test]
fn cannot_move_field_out_of_drop_type_with_pattern() {
    let result = analyze(
        r#"
        #[lang = "drop"]
        trait Drop { fun drop(&mut self); }

        struct Guard {}
        struct Owner { guard: Guard }
        impl Drop for Owner { fun drop(&mut self) {} }

        fun main() {
            let owner = Owner { guard: Guard {} };
            match owner {
                Owner { guard } => {}
            }
        }
        "#,
    );

    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0305"),
        "{:#?}",
        result.diagnostics
    );
}

#[test]
fn cannot_move_nested_field_out_of_drop_type_with_pattern() {
    let result = analyze(
        r#"
        #[lang = "drop"]
        trait Drop { fun drop(&mut self); }

        struct Guard {}
        struct Inner { guard: Guard }
        struct Outer { inner: Inner }
        impl Drop for Inner { fun drop(&mut self) {} }

        fun main() {
            let outer = Outer { inner: Inner { guard: Guard {} } };
            match outer {
                Outer { inner: Inner { guard } } => {}
            }
        }
        "#,
    );

    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0305"),
        "{:#?}",
        result.diagnostics
    );
}

#[test]
fn cannot_move_pattern_binding_in_match_guard() {
    let result = analyze(
        r"
        struct Token {}
        enum MaybeToken { Some(Token), None }
        fun consume(value: Token) -> bool { true }

        fun main(value: MaybeToken) {
            match value {
                MaybeToken::Some(token) if consume(token) => {},
                MaybeToken::Some(token) => {},
                MaybeToken::None => {},
            }
        }
        ",
    );

    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0307"),
        "{:#?}",
        result.diagnostics
    );
}

// == Struct literal fields ==

#[test]
fn struct_literal_moves_fields() {
    let result = analyze(
        r"
        struct Inner { value: i32 }
        struct Outer { inner: Inner }

        fun f() {
            let inner = Inner{value: 1};
            let outer = Outer{inner};
            let x = inner;
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved value") && m.contains("inner"))
    );
}

// == Array ==

#[test]
fn array_moves_elements() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun f() {
            let p = Point{x: 1};
            let arr = [p];
            let q = p;
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved value") && m.contains('p'))
    );
}

#[test]
fn dynamic_array_element_move_is_tracked() {
    let source = r"
        struct Guard {}
        fun consume(value: Guard) {}

        fun test(index: usize) {
            let values = [Guard {}, Guard {}];
            consume(values[index]);
            consume(values[index]);
        }
    ";
    let result = analyze(source);
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0100"),
        "expected repeated dynamic element move to be rejected: {:#?}",
        result.diagnostics
    );
}

#[test]
fn array_repeat_copy_value_remains_usable() {
    let result = analyze(
        r"
        fun f() {
            let x = 1;
            let arr = [x; 3];
            let y = x;
        }
        ",
    );
    assert!(result.diagnostics.is_empty());
}

// == Field access (borrow, not move) ==

#[test]
fn field_access_does_not_move() {
    let result = analyze(
        r"
        struct Point { x: i32, y: i32 }

        fun f() {
            let p = Point{x: 1, y: 2};
            let a = p.x;
            let b = p.y;
        }
        ",
    );
    assert!(result.diagnostics.is_empty());
}

// == Reference (borrow, not move) ==

#[test]
fn taking_reference_does_not_move() {
    // &p does not move p — reading a field through the reference is fine.
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun f() {
            let p = Point{x: 1};
            let r = &p;
            let a = p.x;
        }
        ",
    );
    assert!(result.diagnostics.is_empty());
}

#[test]
fn mutable_references_move_instead_of_copying() {
    let result = analyze(
        r"
        fun f() {
            let mut value: i32 = 1;
            let first: &mut i32 = &mut value;
            let second = first;
            *first = 2;
            *second = 3;
        }
        ",
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0100" && diagnostic.message.contains("first"))
    );
}

#[test]
fn mutable_reference_arguments_are_automatically_reborrowed() {
    let result = analyze(
        r"
        fun touch(value: &mut i32) {
            *value += 1;
        }

        fun f() {
            let mut value: i32 = 1;
            let reference: &mut i32 = &mut value;
            touch(reference);
            touch(reference);
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
}

#[test]
fn shared_reborrow_freezes_the_parent_mutable_reference() {
    let result = analyze(
        r"
        fun touch(value: &mut i32) {
            *value += 1;
        }

        fun f() {
            let mut value: i32 = 1;
            let reference: &mut i32 = &mut value;
            let shared: &i32 = &*reference;
            touch(reference);
            *shared;
        }
        ",
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0300")
    );
}

#[test]
fn mutable_borrow_ends_after_the_last_use() {
    let result = analyze(
        r"
        struct Boxed { value: i32 }

        impl Boxed {
            fun set(&mut self) {
                self.value = 4;
            }
        }

        fun f() {
            let mut boxed = Boxed { value: 1 };
            let reference: &mut i32 = &mut boxed.value;
            *reference = 2;
            boxed.set();
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
}

#[test]
fn method_return_reference_keeps_receiver_borrowed() {
    let result = analyze(
        r"
        struct Boxed { value: i32 }

        impl Boxed {
            fun get_mut(&mut self) -> &mut i32 {
                &mut self.value
            }

            fun set(&mut self) {
                self.value = 4;
            }
        }

        fun f() {
            let mut boxed = Boxed { value: 1 };
            let reference = boxed.get_mut();
            boxed.set();
            *reference = 2;
        }
        ",
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0302")
    );
}

#[test]
fn wrapped_method_return_preserves_receiver_provenance() {
    let result = analyze(
        r"
        enum Maybe<T> { Some(T), None }

        impl<T> Maybe<T> {
            fun unwrap_or(self, fallback: T) -> T {
                match self {
                    Maybe::Some(value) => value,
                    Maybe::None => fallback,
                }
            }
        }

        struct Boxed { value: i32 }

        impl Boxed {
            fun get_mut(&mut self) -> Maybe<&mut i32> {
                Maybe::Some(&mut self.value)
            }

            fun set(&mut self) {
                self.value = 4;
            }
        }

        fun f() {
            let mut boxed = Boxed { value: 1 };
            let mut fallback = 0;
            let reference = boxed.get_mut().unwrap_or(&mut fallback);
            boxed.set();
            *reference = 2;
        }
        ",
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0302")
    );
}

#[test]
fn generic_supertrait_shared_methods_do_not_move_receiver() {
    let result = analyze(
        r"
        trait Named { fun name(&self) -> i32; }
        trait Tagged: Named { fun tag(&self) -> i32; }

        fun describe<T: Tagged>(value: T) -> i32 {
            value.name() + value.tag()
        }
        ",
    );

    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
}

#[test]
fn non_generic_enum_preserves_reference_provenance() {
    let result = analyze(
        r"
        enum Slot { Some(&mut i32), None }

        impl Slot {
            fun unwrap_or(self, fallback: &mut i32) -> &mut i32 {
                match self {
                    Slot::Some(value) => value,
                    Slot::None => fallback,
                }
            }
        }

        struct Boxed { value: i32 }

        impl Boxed {
            fun get_mut(&mut self) -> Slot {
                Slot::Some(&mut self.value)
            }

            fun set(&mut self) {
                self.value = 4;
            }
        }

        fun f() {
            let mut boxed = Boxed { value: 1 };
            let mut fallback = 0;
            let reference = boxed.get_mut().unwrap_or(&mut fallback);
            boxed.set();
            *reference = 2;
        }
        ",
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0302")
    );
}

#[test]
fn mutable_borrows_of_disjoint_fields_can_coexist() {
    let result = analyze(
        r"
        struct Pair { left: i32, right: i32 }

        fun f() {
            let mut pair = Pair { left: 1, right: 2 };
            let left: &mut i32 = &mut pair.left;
            let right: &mut i32 = &mut pair.right;
            *left = 3;
            *right = 4;
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
}

#[test]
fn move_while_borrowed_is_error() {
    // Moving p while a shared borrow exists is E0304. `r` is used after the
    // move site; a borrow whose binding is never used dies at its binding
    // (NLL) and no longer conflicts.
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun f() -> i32 {
            let p = Point{x: 1};
            let r = &p;
            let q = p;
            r.x
        }
        ",
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.code == "E0304" && d.message.contains('p'))
    );
}

#[test]
fn rejects_moving_a_field_out_of_explicit_drop_type() {
    let result = analyze(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Token {}
        struct Guard { value: Token }

        impl Drop for Guard {
            fun drop(&mut self) {}
        }

        fun take(value: Token) {}

        fun f() {
            let guard = Guard { value: Token {} };
            take(guard.value);
        }
        "#,
    );

    assert!(
        result.diagnostics.iter().any(|d| d.code == "E0305"),
        "expected move-out-of-Drop diagnostic, got {:?}",
        result.diagnostics
    );
}

// == Explicit Copy impl ==

#[test]
fn explicit_copy_impl_makes_struct_copyable() {
    let result = analyze(
        r#"
        #[lang = "copy"]
        trait Copy {}

        struct Vec2 { x: i32, y: i32 }

        impl Copy for Vec2 {}

        fun f() {
            let a = Vec2{x: 1, y: 2};
            let b = a;
            let c = a;
        }
        "#,
    );
    assert!(result.diagnostics.is_empty());
}

#[test]
fn namespaced_copy_impl_makes_struct_copyable() {
    let result = analyze(
        r#"
        pub mod std {
            pub mod marker {
                #[lang = "copy"]
                pub trait Copy {}
            }
        }

        struct Vec2 { x: i32, y: i32 }

        impl std::marker::Copy for Vec2 {}

        fun f() {
            let a = Vec2{x: 1, y: 2};
            let b = a;
            let c = a;
        }
        "#,
    );
    assert!(result.diagnostics.is_empty());
}

#[test]
fn generic_copy_impl_makes_instantiations_copyable() {
    let result = analyze(
        r#"
        #[lang = "copy"]
        trait Copy {}

        struct Box<T> { value: T }

        impl<T: Copy> Copy for Box<T> {}

        fun f() {
            let a: Box<i32> = Box{value: 1};
            let b = a;
            let c = a;
        }
        "#,
    );
    assert!(result.diagnostics.is_empty());
}

#[test]
fn generic_copy_bound_allows_deref_move_and_reuse() {
    // A `T: Copy` bound makes the parameter copyable even though the global
    // environment holds no impl for the bare parameter: moving out of `&T`
    // and using the value twice are both allowed.
    let result = analyze(
        r#"
        #[lang = "copy"]
        trait Copy {}

        fun duplicate<T: Copy>(value: &T) -> T {
            *value
        }

        fun reuse<T: Copy>(value: T) -> T {
            let first = value;
            let second = value;
            second
        }

        fun main() {
            let number = 1i32;
            let copy = duplicate(&number);
            let again = reuse(copy);
        }
        "#,
    );
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
}

#[test]
fn unbound_parameter_still_cannot_move_out_of_reference() {
    let result = analyze(
        r#"
        fun take<T>(value: &T) -> T {
            *value
        }
        "#,
    );
    // The root block's tail is visited by both the body walk and the block
    // walk; the repeat used to be reported as a second identical E0308.
    assert_eq!(result.diagnostics.len(), 1, "{:?}", result.diagnostics);
    assert_eq!(result.diagnostics[0].code, "E0308");
    assert!(
        result.diagnostics[0]
            .message
            .contains("cannot move out of dereference"),
        "{:?}",
        result.diagnostics
    );
}

#[test]
fn without_copy_impl_struct_is_not_copyable() {
    let result = analyze(
        r"
        trait Copy {}

        struct Vec2 { x: i32 }

        fun f() {
            let a = Vec2{x: 1};
            let b = a;
            let c = a;
        }
        ",
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("use of moved value"))
    );
}

#[test]
fn unannotated_copy_trait_does_not_enable_copy_hook() {
    let result = analyze(
        r"
        trait Copy {}

        struct Vec2 { x: i32 }

        impl Copy for Vec2 {}

        fun f() {
            let a = Vec2{x: 1};
            let b = a;
            let c = a;
        }
        ",
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("use of moved value"))
    );
}

// == Pattern bindings (whole scrutinee) ==

#[test]
fn match_binding_whole_scrutinee_is_consumed() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun consume(p: Point) {}

        fun f() {
            let p = Point{x: 1};
            match p {
                val => { consume(val); consume(val); }
            }
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved value") && m.contains("val"))
    );
}

// == Block tail ==

#[test]
fn block_tail_moves_value() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun f() {
            let p = Point{x: 1};
            let q = { p };
            let r = p;
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|m| m.contains("use of moved value") && m.contains('p'))
    );
}

#[test]
fn anonymous_function_parameters_follow_move_rules() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun main() {
            let consume_twice = [value: Point -> {
                let first = value;
                let second = value;
            }];
        }
        ",
    );

    assert!(
        messages(&result)
            .iter()
            .any(|message| message.contains("use of moved value") && message.contains("value"))
    );
}

#[test]
fn inferred_anonymous_function_parameters_follow_move_rules() {
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun main() {
            let consume_twice = [value -> {
                let first = value;
                let second = value;
                second
            }];
            consume_twice(Point { x: 1 });
        }
        ",
    );

    assert!(
        result.diagnostics.iter().any(|diagnostic| {
            diagnostic.code == "E0100"
                && diagnostic.message.contains("use of moved value")
                && diagnostic.message.contains("value")
        }),
        "{:#?}",
        result.diagnostics
    );
}

#[test]
fn value_capture_moves_non_copy_binding() {
    let result = analyze(
        r"
        struct Token { value: i32 }
        fun consume(value: Token) {}

        fun main() {
            let token = Token { value: 1 };
            let once = [ -> { consume(token); }];
            let again = token;
        }
        ",
    );

    assert!(
        messages(&result)
            .iter()
            .any(|message| message.contains("use of moved value") && message.contains("token"))
    );
}

#[test]
fn once_closure_cannot_be_called_twice() {
    let result = analyze(
        r"
        struct Token { value: i32 }
        fun consume(value: Token) {}

        fun main() {
            let token = Token { value: 1 };
            let once = [ -> { consume(token); }];
            once();
            once();
        }
        ",
    );

    assert!(
        messages(&result)
            .iter()
            .any(|message| message.contains("use of moved value") && message.contains("once"))
    );
}

#[test]
fn closure_value_moves_when_passed_by_value() {
    let result = analyze(
        r"
        fun consume(f: impl Fn(i32) -> i32) {}

        fun test() {
            let add_one = [value: i32 -> value + 1];
            consume(add_one);
            consume(add_one);
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|message| message.contains("use of moved value") && message.contains("add_one")),
        "closure values own their environment and must move by value: {:#?}",
        result.diagnostics
    );
}

#[test]
fn once_closure_stays_once_after_branch_join() {
    let result = analyze(
        r"
        struct Token { value: i32 }
        fun consume(value: Token) {}

        fun main() {
            let token = Token { value: 1 };
            let once = [ -> { consume(token); }];
            let once = if true { once } else { once };
            once();
            once();
        }
        ",
    );

    assert!(
        messages(&result)
            .iter()
            .any(|message| message.contains("use of moved value") && message.contains("once"))
    );
}

#[test]
fn fn_once_parameter_is_consumed_by_its_first_call() {
    let result = analyze(
        r"
        fun call_twice(callback: impl FnOnce() -> i32) -> i32 {
            callback();
            callback()
        }
        ",
    );

    assert!(result.diagnostics.iter().any(|diagnostic| {
        diagnostic.code == "E0100" && diagnostic.message.contains("callback")
    }));
}

#[test]
fn borrowed_dyn_callable_preserves_argument_borrow_modes() {
    let result = analyze(
        r#"
        fun call(callback: &dyn Fn(&[i32], &mut [i32]) -> i32, mut token: [i32; 1]) -> i32 {
            callback(&token, &mut token)
        }
        "#,
    );

    assert!(
        result.diagnostics.iter().any(|diagnostic| {
            diagnostic.code == "E0300" && diagnostic.message.contains("token")
        }),
        "expected the callback borrow modes to conflict: {:#?}",
        result.diagnostics
    );
}

#[test]
fn shared_pattern_capture_blocks_later_move() {
    let result = analyze(
        r"
        struct Token { value: i32 }
        fun consume(value: Token) {}
        fun inspect(value: &Token) -> i32 { value.value }

        fun main() {
            let source = Token { value: 1 };
            match source {
                token => {
                    let read = [ -> inspect(&token)];
                    consume(token);
                    read();
                }
            }
        }
        ",
    );

    let diagnostics = messages(&result);
    assert!(
        diagnostics.iter().any(|message| {
            message.contains("cannot move")
                && message.contains("token")
                && message.contains("borrowed")
        }),
        "{diagnostics:?}"
    );
}

#[test]
fn shared_capture_blocks_assignment_while_closure_is_live() {
    let result = analyze(
        r"
        fun main() {
            let mut base = 1;
            let read = [ -> base];
            base = 2;
            read();
        }
        ",
    );

    assert!(
        messages(&result)
            .iter()
            .any(|message| message.contains("cannot assign") && message.contains("base"))
    );
}

#[test]
fn destructured_bindings_are_tracked_independently() {
    let result = analyze(
        r"
        struct Token { value: i32 }
        fun consume(value: Token) {}

        fun main() {
            let pair = (Token { value: 1 }, Token { value: 2 });
            let (first, second) = pair;
            consume(first);
            consume(second);
        }
        ",
    );

    assert_eq!(messages(&result), Vec::<&str>::new());
}

#[test]
fn moving_a_destructured_binding_twice_is_rejected() {
    let result = analyze(
        r"
        struct Token { value: i32 }
        fun consume(value: Token) {}

        fun main() {
            let (first, second) = (Token { value: 1 }, Token { value: 2 });
            consume(first);
            consume(first);
        }
        ",
    );

    assert!(
        messages(&result)
            .iter()
            .any(|message| message.contains("use of moved value") && message.contains("first")),
        "{:?}",
        messages(&result)
    );
}

#[test]
fn by_value_operator_makes_its_closure_single_use() {
    let result = analyze(
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
            add();
            add();
        }
        "#,
    );

    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| { diagnostic.code == "E0100" && diagnostic.message.contains("add") })
    );
}

#[test]
fn loop_break_moves_the_broken_value() {
    let result = analyze(
        r"
        struct Token { value: i32 }

        fun consume(value: Token) {}

        fun f() {
            let token = Token { value: 1 };
            loop {
                break token;
            }
            consume(token);
        }
        ",
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0100" && diagnostic.message.contains("token")),
        "{:#?}",
        result.diagnostics
    );
}

#[test]
fn loop_result_origin_merges_break_values() {
    let result = analyze(
        r"
        struct Token { value: i32 }

        fun consume(value: Token) {}

        fun f() {
            let token = Token { value: 1 };
            let result = loop {
                break &token;
            };
            consume(token);
            consume_ref(result);
        }

        fun consume_ref(value: &Token) {}
        ",
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0304" && diagnostic.message.contains("token")),
        "{:#?}",
        result.diagnostics
    );
}

#[test]
fn loop_result_borrow_ends_before_move() {
    let result = analyze(
        r"
        struct Token { value: i32 }

        fun consume(value: Token) {}

        fun f() {
            let token = Token { value: 1 };
            let result = loop {
                break &token;
            };
            let value = result.value;
            consume(token);
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn loop_break_of_fresh_value_is_accepted() {
    let result = analyze(
        r"
        struct Token { value: i32 }

        fun consume(value: Token) {}

        fun f() {
            let kept = Token { value: 1 };
            let result = loop {
                break Token { value: 2 };
            };
            consume(result);
            consume(kept);
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn loop_break_propagates_definite_initialization() {
    let result = analyze(
        r"
        fun f() -> i32 {
            let mut value: i32;
            loop {
                value = 1;
                break;
            }
            value
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn loop_break_on_partial_path_keeps_use_rejected() {
    let result = analyze(
        r"
        fun f(flag: bool) -> i32 {
            let mut value: i32;
            loop {
                if flag {
                    break;
                }
                value = 1;
                break;
            }
            value
        }
        ",
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0059"),
        "{:#?}",
        result.diagnostics
    );
}

#[test]
fn if_let_arm_can_move_the_bound_payload() {
    let result = analyze(
        r"
        struct Token {}
        enum Option { Some(Token), None }
        fun consume(value: Token) {}

        fun f() {
            let opt = Option::Some(Token {});
            if let Option::Some(token) = opt {
                consume(token);
            }
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn if_let_moved_payload_cannot_be_reused_in_the_arm() {
    let result = analyze(
        r"
        struct Token {}
        enum Option { Some(Token), None }
        fun consume(value: Token) {}

        fun f() {
            let opt = Option::Some(Token {});
            if let Option::Some(token) = opt {
                consume(token);
                consume(token);
            }
        }
        ",
    );
    assert!(
        messages(&result)
            .iter()
            .any(|message| message.contains("use of moved value") && message.contains("token")),
        "{:#?}",
        result.diagnostics
    );
}

#[test]
fn while_let_body_can_move_the_bound_payload() {
    let result = analyze(
        r"
        struct Token {}
        enum Option { Some(Token), None }
        fun consume(value: Token) {}

        fun f(source: Option) {
            let mut current = source;
            while let Option::Some(token) = current {
                consume(token);
                current = Option::None;
            }
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn if_let_branches_merge_definite_initialization() {
    let complete = analyze(
        r"
        enum Option { Some(i32), None }

        fun f(opt: Option) -> i32 {
            let value: i32;
            if let Option::Some(x) = opt { value = x; } else { value = 0; }
            value
        }
        ",
    );
    assert!(
        complete.diagnostics.is_empty(),
        "{:#?}",
        complete.diagnostics
    );

    let incomplete = analyze(
        r"
        enum Option { Some(i32), None }

        fun f(opt: Option) -> i32 {
            let value: i32;
            if let Option::Some(x) = opt { value = x; }
            value
        }
        ",
    );
    assert!(
        incomplete
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0059"),
        "{:#?}",
        incomplete.diagnostics
    );
}

#[test]
fn for_tuple_pattern_moves_each_element_binding_independently() {
    let result = analyze(
        r"
        struct Wrapper { value: i32 }

        fun consume(wrapper: Wrapper) {}

        fun f() {
            let pairs = [(Wrapper { value: 1 }, Wrapper { value: 2 })];
            for (first, second) in pairs {
                consume(first);
                consume(second);
            }
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn for_tuple_pattern_rejects_use_after_moving_a_binding() {
    let result = analyze(
        r"
        struct Wrapper { value: i32 }

        fun consume(wrapper: Wrapper) {}

        fun f() {
            let pairs = [(Wrapper { value: 1 }, 1)];
            for (wrapper, tag) in pairs {
                consume(wrapper);
                consume(wrapper);
            }
        }
        ",
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0100" && diagnostic.message.contains("wrapper")),
        "{:#?}",
        result.diagnostics
    );
}

#[test]
fn let_else_binding_is_initialized_after_the_statement() {
    let result = analyze(
        r"
        enum Option { Some(i32), None }

        fun f(opt: Option) -> i32 {
            let Option::Some(x) = opt else {
                return -1;
            };
            x
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn let_else_moves_in_the_else_block_do_not_leak() {
    let result = analyze(
        r"
        enum Option { Some(i32), None }

        struct Token { value: i32 }

        fun consume(token: Token) {}

        fun f(opt: Option) -> i32 {
            let token = Token { value: 1 };
            let Option::Some(x) = opt else {
                consume(token);
                return -1;
            };
            consume(token);
            x
        }
        ",
    );
    assert!(
        result.diagnostics.is_empty(),
        "the diverging else path must not move outer values for the fallthrough path: {:#?}",
        result.diagnostics
    );
}

#[test]
fn let_else_binding_can_move_out_of_the_scrutinee() {
    let result = analyze(
        r"
        struct Token { value: i32 }

        enum Maybe { Some(Token), None }

        fun consume(token: Token) {}

        fun f(maybe: Maybe) {
            let Maybe::Some(token) = maybe else {
                return;
            };
            consume(token);
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

// == 循环不动点诊断（收敛后最终重放恰好上报一次） ==

#[test]
fn while_backedge_move_reported_exactly_once() {
    // Moving `token` every iteration: iteration 2 only sees the move via the
    // back edge, so the error depends on fixpoint convergence. It must be
    // reported exactly once — not zero times, not once per replay iteration.
    let result = analyze(
        r"
        struct Token { value: i32 }
        fun consume(token: Token) {}

        fun f(n: i32) -> i32 {
            let token = Token { value: 1 };
            let mut i = 0;
            while i < n {
                consume(token);
                i = i + 1;
            }
            i
        }
        ",
    );
    let count = result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "E0100")
        .count();
    assert_eq!(count, 1, "{:#?}", result.diagnostics);
}

#[test]
fn while_body_borrow_conflict_reported_exactly_once() {
    // A conflict that fires within every iteration must survive the fixpoint
    // replay deduplication and be reported exactly once.
    let result = analyze(
        r"
        struct Point { x: i32 }

        fun f(n: i32) -> i32 {
            let mut p = Point { x: 1 };
            let mut i = 0;
            while i < n {
                let a = &mut p;
                let b = &mut p;
                i = i + a.x;
                i = i + 1;
            }
            i
        }
        ",
    );
    let count = result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "E0302")
        .count();
    assert_eq!(count, 1, "{:#?}", result.diagnostics);
}

#[test]
fn nested_loop_diagnostics_not_duplicated_by_outer_replay() {
    // The inner loop's error must not be re-emitted by each outer fixpoint
    // iteration or the outer final replay.
    let result = analyze(
        r"
        struct Token { value: i32 }
        fun consume(token: Token) {}

        fun f(n: i32) -> i32 {
            let token = Token { value: 1 };
            let mut i = 0;
            while i < n {
                let mut j = 0;
                while j < n {
                    consume(token);
                    j = j + 1;
                }
                i = i + 1;
            }
            i
        }
        ",
    );
    let count = result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "E0100")
        .count();
    assert_eq!(count, 1, "{:#?}", result.diagnostics);
}

#[test]
fn loop_body_move_through_break_state_reported() {
    // `loop` (not `while`) with the move reachable only via the converged
    // back edge must still report once.
    let result = analyze(
        r"
        struct Token { value: i32 }
        fun consume(token: Token) {}

        fun f(n: i32) -> i32 {
            let token = Token { value: 1 };
            let mut i = 0;
            loop {
                if i >= n {
                    break;
                }
                consume(token);
                i = i + 1;
            }
            i
        }
        ",
    );
    let count = result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "E0100")
        .count();
    assert_eq!(count, 1, "{:#?}", result.diagnostics);
}

// == 引用参数检查（不再整体豁免，种子 loan 除外） ==

#[test]
fn assign_through_reference_param_fields_is_allowed() {
    // Writing through a `&mut` parameter must not conflict with the
    // parameter's own incoming (seed) loan.
    let result = analyze(
        r"
        struct Point { x: i32, y: i32 }

        fun f(p: &mut Point) {
            p.x = 1;
            p.y = 2;
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn assign_through_reference_param_conflicts_with_local_reborrow() {
    // A reborrow taken inside the body (not the seed loan) must conflict
    // with a later write through the reference parameter.
    let result = analyze(
        r"
        struct Point { x: i32, y: i32 }

        fun f(p: &mut Point) -> i32 {
            let b = &mut *p;
            p.x = 1;
            b.y
        }
        ",
    );
    assert!(
        result.diagnostics.iter().any(|d| d.code == "E0303"),
        "expected E0303 for assign through reference param while reborrowed: {:#?}",
        result.diagnostics
    );
}

#[test]
fn reference_field_chain_does_not_borrow_whole_param() {
    // Reading a reference-typed field and calling a `&self` method on it
    // borrows only that field; assigning a sibling field while the result
    // is alive must stay legal.
    let result = analyze(
        r"
        enum Cell { Ref(&i32), Empty }

        struct Inner { v: i32 }

        impl Inner {
            fun peek(&self) -> Cell { Cell::Ref(&self.v) }
        }

        struct Outer { inner: Inner, index: usize }

        fun take(o: &mut Outer) -> i32 {
            match o.inner.peek() {
                Cell::Ref(value) => {
                    o.index += 1usize;
                    *value
                },
                Cell::Empty => 0,
            }
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn reference_field_chain_borrow_conflicts_within_field() {
    // The precise field loan must still catch writes into the borrowed
    // field itself while the returned reference is alive.
    let result = analyze(
        r"
        enum Cell { Ref(&i32), Empty }

        struct Inner { v: i32 }

        impl Inner {
            fun peek(&self) -> Cell { Cell::Ref(&self.v) }
        }

        struct Outer { inner: Inner, index: usize }

        fun take(o: &mut Outer) -> i32 {
            match o.inner.peek() {
                Cell::Ref(value) => {
                    o.inner.v += 1;
                    *value
                },
                Cell::Empty => 0,
            }
        }
        ",
    );
    assert!(
        result.diagnostics.iter().any(|d| d.code == "E0303"),
        "expected E0303 for write into the borrowed field: {:#?}",
        result.diagnostics
    );
}

#[test]
fn wildcard_index_moves_within_one_iteration_are_rejected() {
    // Dynamic-index moves reset at the loop head (the std array
    // `IntoIterator` moves a different element each iteration), but two
    // dynamic-index moves of the same array within ONE iteration body are
    // still rejected.
    let result = analyze(
        r"
        struct Token { value: i32 }
        fun consume(t: Token) {}

        fun f(n: i32) {
            let arr = [Token { value: 1 }, Token { value: 2 }];
            let mut i = 0;
            while i < n {
                consume(arr[i]);
                consume(arr[i]);
                i = i + 1;
            }
        }
        ",
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.code == "E0100" && d.message.contains("moved value")),
        "expected E0100 for a repeated dynamic-index move in one iteration, got {:#?}",
        result.diagnostics
    );
}

#[test]
fn let_destructuring_cannot_move_out_of_drop_type() {
    // `match` arms reject moving non-Copy fields out of a Drop owner; `let`
    // destructuring must obey the same rule (previously unenforced).
    let result = analyze(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard { value: Inner }
        struct Inner { count: i32 }

        impl Drop for Guard {
            fun drop(&mut self) {}
        }

        fun f(guard: Guard) {
            let Guard { value } = guard;
            let Inner { count } = value;
        }
        "#,
    );
    assert!(
        result.diagnostics.iter().any(|d| d.code == "E0305"),
        "expected E0305 for let-destructuring a non-Copy field out of a Drop owner, got {:#?}",
        result.diagnostics
    );
}

#[test]
fn let_destructuring_copy_fields_out_of_drop_type_is_allowed() {
    let result = analyze(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard { value: i32 }

        impl Drop for Guard {
            fun drop(&mut self) {}
        }

        fun f(guard: Guard) {
            let Guard { value } = guard;
            let other = value + 1;
        }
        "#,
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn loop_body_use_keeps_loan_live_to_loop_end() {
    // The borrow `r` is used inside the loop, so it may be live at any point
    // of any iteration; assigning to `v` mid-body must conflict (Rust E0502).
    let result = analyze(
        r"
        fun f(n: i32) -> i32 {
            let mut v = 5;
            let r = &v;
            let mut i = 0;
            while i < n {
                let x = *r;
                v = 6;
                i = i + 1;
            }
            0
        }
        ",
    );
    assert!(
        result.diagnostics.iter().any(|d| d.code == "E0303"),
        "expected E0303 for assign while a loop-used borrow is live, got {:#?}",
        result.diagnostics
    );
}

#[test]
fn lambda_assignment_does_not_definitely_initialize() {
    // A lambda may run zero or more times, so an assignment inside its body
    // never counts as a definite initialization of an outer binding.
    let result = analyze(
        r"
        fun f() -> i32 {
            let mut x: i32;
            let mut assign = [v -> x = v];
            assign(5);
            x
        }
        ",
    );
    assert!(
        result.diagnostics.iter().any(|d| d.code == "E0059"),
        "expected E0059 for reading a binding only assigned inside a lambda, got {:#?}",
        result.diagnostics
    );
}

#[test]
fn lambda_body_reads_respect_outer_initialization() {
    // The lambda body is checked against the state at its definition point:
    // reading an initialized outer binding inside the lambda is fine.
    let result = analyze(
        r"
        fun f() -> i32 {
            let x = 3;
            let read = [v -> v + x];
            read(1)
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn borrow_assigned_on_the_backedge_conflicts_on_the_next_iteration() {
    let result = analyze(
        r"
        fun f(n: i32) {
            let mut value = 1;
            let other = 2;
            let mut reference = &other;
            let mut i = 0;
            while i < n {
                value = 3;
                let previous = *reference;
                reference = &value;
                i += 1;
            }
        }
    ",
    );
    assert_eq!(
        result
            .diagnostics
            .iter()
            .filter(|d| d.code == "E0303")
            .count(),
        1,
        "{:#?}",
        result.diagnostics
    );
}

#[test]
fn dead_branch_uses_do_not_keep_a_loan_live_after_the_branch() {
    let result = analyze(
        r"
        fun f(flag: bool) {
            let mut value = 1;
            let reference = &value;
            if flag {
                return;
                let dead = *reference;
            }
            value = 2;
        }
    ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn lambda_assignment_may_initialize_an_outer_immutable_binding() {
    let result = analyze(
        r"
        fun f() -> i32 {
            let value: i32;
            let assign = [ -> { value = 1; }];
            value = 2;
            value
        }
    ",
    );
    assert!(
        result.diagnostics.iter().any(|d| d.code == "E0031"),
        "{:#?}",
        result.diagnostics
    );
}

#[test]
fn sequential_fnmut_lambdas_may_recapture_mutably() {
    // The first closure's capture loan expires at its last use (the call),
    // so a second closure may capture the same binding mutably afterwards.
    let result = analyze(
        r"
        fun f() -> i32 {
            let mut produced = 0i32;
            let mut first = [ -> { produced = produced + 1i32; 1i32 }];
            first();
            let mut second = [ -> { produced = produced + 1i32; 2i32 }];
            second();
            produced
        }
        ",
    );
    assert!(result.diagnostics.is_empty(), "{:#?}", result.diagnostics);
}

#[test]
fn overlapping_fnmut_captures_are_still_rejected() {
    // Two live closures capturing the same binding mutably at once must
    // still conflict.
    let result = analyze(
        r"
        fun f() -> i32 {
            let mut produced = 0i32;
            let mut first = [ -> { produced = produced + 1i32; 1i32 }];
            let mut second = [ -> { produced = produced + 1i32; 2i32 }];
            first();
            second();
            produced
        }
        ",
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0302"),
        "expected E0302 for overlapping mutable captures, got {:#?}",
        result.diagnostics
    );
}

#[test]
fn loop_carried_reference_through_recursive_type_converges() {
    // `current = next`, where `next` was read out of `current.link` and
    // `Link::Next(&Node)` makes the type recursive: the loop-head fixpoint used
    // to compose one more place projection per round, so every round produced a
    // deeper borrow (and a fresh loan) and the head never stopped changing. The
    // analysis now truncates projection chains, which keeps the state finite.
    // Pinned here because the run does not terminate at all without that cap.
    let result = analyze(
        r"
        enum Link {
            End,
            Next(&Node),
        }

        struct Node {
            data: i32,
            link: Link,
        }

        fun walk(node: &Node) -> i32 {
            let mut current = node;
            let mut guard = 0;
            while guard < 4 {
                match current.link {
                    Link::End => {
                        break;
                    }
                    Link::Next(next) => {
                        current = next;
                    }
                }
                guard = guard + 1;
            }
            current.data
        }
        ",
    );
    assert_eq!(result.diagnostics, vec![]);
}
