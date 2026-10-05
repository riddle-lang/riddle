use crate::analyze;

#[test]
fn rejects_reference_escaping_a_drop_owner() {
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

        fun leak() -> &i32 {
            let guard = Guard { value: 1 };
            &guard.value
        }
        "#,
    );

    assert!(
        result.diagnostics.iter().any(|d| d.code == "E0306"),
        "expected escaping Drop borrow diagnostic, got {:?}",
        result.diagnostics
    );
}

#[test]
fn rejects_returned_closure_borrowing_a_drop_owner() {
    let result = analyze(
        r#"
        #[lang = "drop"]
        trait Drop { fun drop(&mut self); }

        struct Guard { value: i32 }
        impl Drop for Guard { fun drop(&mut self) {} }
        fun inspect(value: &Guard) {}

        fun leak() -> impl Fn() -> () {
            let guard = Guard { value: 1 };
            [ -> { inspect(&guard); }]
        }
        "#,
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0306"),
        "expected escaping closure borrow of Drop owner to be rejected: {:#?}",
        result.diagnostics
    );
}

fn has_code(source: &str, code: &str) -> bool {
    analyze(source)
        .diagnostics
        .iter()
        .any(|diagnostic| diagnostic.code == code)
}

fn is_clean(source: &str) {
    let result = analyze(source);
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
}

#[test]
fn multiple_shared_borrows_are_allowed() {
    is_clean(
        r"
        fun f() {
            let value = 1;
            let first = &value;
            let second = &value;
            let third = first;
            *second;
            *third;
        }
        ",
    );
}

#[test]
fn shared_then_mutable_borrow_is_rejected() {
    assert!(has_code(
        r"
        fun f() {
            let mut value = 1;
            let shared = &value;
            let mutable = &mut value;
            *shared;
            *mutable = 2;
        }
        ",
        "E0300"
    ));
}

#[test]
fn mutable_then_shared_borrow_is_rejected() {
    assert!(has_code(
        r"
        fun f() {
            let mut value = 1;
            let mutable = &mut value;
            let shared = &value;
            *mutable = 2;
            *shared;
        }
        ",
        "E0301"
    ));
}

#[test]
fn two_mutable_borrows_are_rejected() {
    assert!(has_code(
        r"
        fun f() {
            let mut value = 1;
            let first = &mut value;
            let second = &mut value;
            *first = 2;
            *second = 3;
        }
        ",
        "E0302"
    ));
}

#[test]
fn assigning_while_shared_borrowed_is_rejected() {
    assert!(has_code(
        r"
        fun f() {
            let mut value = 1;
            let shared = &value;
            value = 2;
            *shared;
        }
        ",
        "E0303"
    ));
}

#[test]
fn moving_while_mutably_borrowed_is_rejected() {
    assert!(has_code(
        r"
        struct Token { value: i32 }

        fun f() {
            let mut token = Token { value: 1 };
            let reference = &mut token;
            let moved = token;
            *reference;
            moved.value;
        }
        ",
        "E0304"
    ));
}

#[test]
fn disjoint_struct_fields_can_be_mutably_borrowed() {
    is_clean(
        r"
        struct Pair { left: i32, right: i32 }

        fun f() {
            let mut pair = Pair { left: 1, right: 2 };
            let left = &mut pair.left;
            let right = &mut pair.right;
            *left = 3;
            *right = 4;
        }
        ",
    );
}

#[test]
fn explicit_reference_pattern_copies_without_moving_the_reference() {
    is_clean(
        r"
        fun f(reference: &mut i32) -> i32 {
            let &mut copied = reference;
            *reference = copied + 1;
            *reference
        }
        ",
    );
}

#[test]
fn explicit_reference_pattern_releases_a_temporary_borrow() {
    is_clean(
        r"
        fun f() -> i32 {
            let mut original = 3;
            let (&mut copied, plain) = (&mut original, 4);
            original = 5;
            copied + plain + original
        }
        ",
    );
}

#[test]
fn explicit_reference_pattern_rejects_moving_borrowed_content() {
    let result = analyze(
        r"
        struct Token { value: i32 }

        fun f(reference: &mut Token) {
            let &mut moved = reference;
        }
        ",
    );

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
fn ergonomic_mutable_pattern_borrow_ends_after_all_bindings_last_use() {
    is_clean(
        r"
        struct Pair { left: i32, right: i32 }

        fun f() {
            let mut pair = Pair { left: 1, right: 2 };
            let Pair { left, right } = &mut pair;
            *left = 3;
            *right = 4;
            pair.left = 5;
        }
        ",
    );
}

#[test]
fn ergonomic_mutable_pattern_keeps_the_source_borrowed_while_live() {
    assert!(has_code(
        r"
        struct Pair { left: i32, right: i32 }

        fun f() {
            let mut pair = Pair { left: 1, right: 2 };
            let Pair { left, right } = &mut pair;
            pair.left = 3;
            *left = 4;
            *right = 5;
        }
        ",
        "E0303"
    ));
}

#[test]
fn ergonomic_mutable_pattern_reborrow_freezes_the_parent_reference() {
    assert!(has_code(
        r"
        struct Pair { left: i32, right: i32 }
        fun update(value: &mut Pair) { value.left = 9; }

        fun f() {
            let mut pair = Pair { left: 1, right: 2 };
            let parent = &mut pair;
            let Pair { left, right } = parent;
            update(parent);
            *left = 3;
            *right = 4;
        }
        ",
        "E0302"
    ));
}

#[test]
fn ergonomic_mutable_pattern_reborrow_releases_the_parent_reference() {
    is_clean(
        r"
        struct Pair { left: i32, right: i32 }
        fun update(value: &mut Pair) { value.left = 9; }

        fun f() {
            let mut pair = Pair { left: 1, right: 2 };
            let parent = &mut pair;
            let Pair { left, right } = parent;
            *left = 3;
            *right = 4;
            update(parent);
        }
        ",
    );
}

#[test]
fn ergonomic_pattern_reborrow_rejects_an_existing_overlapping_child() {
    assert!(has_code(
        r"
        struct Pair { left: i32, right: i32 }

        fun f() {
            let mut pair = Pair { left: 1, right: 2 };
            let mut parent = &mut pair;
            let existing = &mut parent.left;
            let Pair { left, right } = parent;
            *existing = 3;
            *left = 4;
            *right = 5;
        }
        ",
        "E0302"
    ));
}

#[test]
fn tuple_destructuring_keeps_reference_provenance_per_element() {
    is_clean(
        r"
        fun f() {
            let mut left = 1;
            let mut right = 2;
            let (left_ref, right_ref) = (&mut left, &mut right);
            *right_ref = 20;
            right = 30;
            *left_ref = 10;
        }
        ",
    );
}

#[test]
fn returned_tuple_destructuring_keeps_reference_provenance_per_element() {
    is_clean(
        r"
        fun references(left: &mut i32, right: &mut i32) -> (&mut i32, &mut i32) {
            (left, right)
        }

        fun f() {
            let mut left = 1;
            let mut right = 2;
            let (left_ref, right_ref) = references(&mut left, &mut right);
            *right_ref = 20;
            right = 30;
            *left_ref = 10;
        }
        ",
    );
}

#[test]
fn match_binding_keeps_reference_provenance_from_a_local_container() {
    assert!(has_code(
        r"
        enum Maybe<T> { None, Some(T) }

        fun f() {
            let mut value = 0;
            let holder = Maybe::Some(&mut value);
            match holder {
                Maybe::Some(reference) => {
                    let second = &mut value;
                    *reference = 1;
                    *second = 2;
                },
                Maybe::None => {},
            }
        }
        ",
        "E0302"
    ));
}

#[test]
fn for_binding_keeps_reference_provenance_from_a_local_container() {
    assert!(has_code(
        r"
        fun f() {
            let mut value = 0;
            let holders = [&mut value];
            for reference in holders {
                let second = &mut value;
                *reference = 1;
                *second = 2;
            }
        }
        ",
        "E0302"
    ));
}

#[test]
fn whole_struct_borrow_overlaps_a_field_borrow() {
    assert!(has_code(
        r"
        struct Pair { left: i32, right: i32 }

        fun f() {
            let mut pair = Pair { left: 1, right: 2 };
            let field = &mut pair.left;
            let mut whole = &mut pair;
            *field = 3;
            whole.left = 4;
        }
        ",
        "E0302"
    ));
}

#[test]
fn known_array_indices_can_be_mutably_borrowed_separately() {
    is_clean(
        r"
        fun f() {
            let mut values = [1, 2, 3];
            let first = &mut values[0];
            let second = &mut values[1];
            *first = 4;
            *second = 5;
        }
        ",
    );
}

#[test]
fn dynamic_array_indices_are_conservatively_overlapping() {
    assert!(has_code(
        r"
        fun f(index: usize) {
            let mut values = [1, 2, 3];
            let first = &mut values[index];
            let second = &mut values[index];
            *first = 4;
            *second = 5;
        }
        ",
        "E0302"
    ));
}

#[test]
fn mutable_reference_move_transfers_its_loan() {
    assert!(has_code(
        r"
        fun f() {
            let mut value = 1;
            let first = &mut value;
            let second = first;
            *first = 2;
            *second = 3;
        }
        ",
        "E0100"
    ));
}

#[test]
fn shared_reference_copy_keeps_both_names_usable() {
    is_clean(
        r"
        fun f() {
            let value = 1;
            let first = &value;
            let second = first;
            *first;
            *second;
        }
        ",
    );
}

#[test]
fn mutable_reference_by_value_parameter_moves_it() {
    assert!(has_code(
        r"
        fun consume<T>(value: T) {}

        fun f() {
            let mut value = 1;
            let reference = &mut value;
            consume(reference);
            *reference = 2;
        }
        ",
        "E0100"
    ));
}

#[test]
fn mutable_reference_ref_parameter_reborrows_it() {
    is_clean(
        r"
        fun update(value: &mut i32) { *value += 1; }

        fun f() {
            let mut value = 1;
            let reference = &mut value;
            update(reference);
            update(reference);
        }
        ",
    );
}

#[test]
fn shared_reborrow_blocks_mutation_until_last_shared_use() {
    assert!(has_code(
        r"
        fun update(value: &mut i32) { *value += 1; }

        fun f() {
            let mut value = 1;
            let reference = &mut value;
            let shared = &*reference;
            update(reference);
            *shared;
        }
        ",
        "E0300"
    ));
}

#[test]
fn shared_reborrow_allows_parent_after_last_use() {
    is_clean(
        r"
        fun update(value: &mut i32) { *value += 1; }

        fun f() {
            let mut value = 1;
            let reference = &mut value;
            let shared = &*reference;
            *shared;
            update(reference);
        }
        ",
    );
}

#[test]
fn mutable_reborrow_keeps_parent_frozen() {
    assert!(has_code(
        r"
        fun update(value: &mut i32) { *value += 1; }

        fun f() {
            let mut value = 1;
            let parent = &mut value;
            let child = &mut *parent;
            update(parent);
            *child = 3;
        }
        ",
        "E0302"
    ));
}

#[test]
fn mutable_reborrow_allows_parent_after_child_last_use() {
    is_clean(
        r"
        fun update(value: &mut i32) { *value += 1; }

        fun f() {
            let mut value = 1;
            let parent = &mut value;
            let child = &mut *parent;
            *child = 3;
            update(parent);
        }
        ",
    );
}

#[test]
fn shared_method_borrow_conflicts_with_live_mutable_return() {
    assert!(has_code(
        r"
        struct Boxed { value: i32 }

        impl Boxed {
            fun get_mut(&mut self) -> &mut i32 { &mut self.value }
            fun read(&self) -> i32 { self.value }
        }

        fun f() {
            let mut boxed = Boxed { value: 1 };
            let reference = boxed.get_mut();
            boxed.read();
            *reference;
        }
        ",
        "E0301"
    ));
}

#[test]
fn mutable_method_borrow_conflicts_with_live_shared_return() {
    assert!(has_code(
        r"
        struct Boxed { value: i32 }

        impl Boxed {
            fun get(&self) -> &i32 { &self.value }
            fun set(&mut self) { self.value = 2; }
        }

        fun f() {
            let mut boxed = Boxed { value: 1 };
            let reference = boxed.get();
            boxed.set();
            *reference;
        }
        ",
        "E0300"
    ));
}

#[test]
fn returned_reference_from_free_function_keeps_argument_borrowed() {
    assert!(has_code(
        r"
        fun identity(value: &mut i32) -> &mut i32 { value }
        fun update(value: &mut i32) { *value += 1; }

        fun f() {
            let mut value = 1;
            let reference = identity(&mut value);
            update(&mut value);
            *reference;
        }
        ",
        "E0302"
    ));
}

#[test]
fn returned_shared_reference_from_free_function_keeps_argument_shared() {
    assert!(has_code(
        r"
        fun identity(value: &i32) -> &i32 { value }

        fun f() {
            let mut value = 1;
            let reference = identity(&value);
            let mutable = &mut value;
            *reference;
            *mutable;
        }
        ",
        "E0300"
    ));
}

#[test]
fn returned_reference_through_nested_array_is_tracked() {
    assert!(has_code(
        r"
        fun nested(value: &mut i32) -> [[&mut i32; 1]; 1] { [[value]] }
        fun update(value: &mut i32) { *value += 1; }

        fun f() {
            let mut value = 1;
            let result = nested(&mut value);
            update(&mut value);
            *result[0][0];
        }
        ",
        "E0302"
    ));
}

#[test]
fn returned_reference_through_array_is_tracked() {
    assert!(has_code(
        r"
        fun array(value: &mut i32) -> [&mut i32; 1] { [value] }
        fun update(value: &mut i32) { *value += 1; }

        fun f() {
            let mut value = 1;
            let result = array(&mut value);
            update(&mut value);
            *result[0];
        }
        ",
        "E0302"
    ));
}

#[test]
fn branch_return_reference_keeps_all_possible_sources_borrowed() {
    assert!(has_code(
        r"
        fun choose(flag: bool, left: &mut i32, right: &mut i32) -> &mut i32 {
            if flag { left } else { right }
        }
        fun update(value: &mut i32) { *value += 1; }

        fun f(flag: bool) {
            let mut left = 1;
            let mut right = 2;
            let reference = choose(flag, &mut left, &mut right);
            update(&mut left);
            *reference;
        }
        ",
        "E0302"
    ));
}

#[test]
fn match_return_reference_keeps_possible_source_borrowed() {
    assert!(has_code(
        r"
        enum Choice<T> { Some(T), None }

        impl<T> Choice<T> {
            fun unwrap_or(self, fallback: T) -> T {
                match self {
                    Choice::Some(value) => value,
                    Choice::None => fallback,
                }
            }
        }

        fun f() {
            let mut value = 1;
            let mut fallback = 2;
            let choice: Choice<&mut i32> = Choice::Some(&mut value);
            let reference = choice.unwrap_or(&mut fallback);
            let second = &mut value;
            *reference;
            *second;
        }
        ",
        "E0302"
    ));
}

#[test]
fn assignment_replaces_reference_provenance() {
    is_clean(
        r"
        fun update(value: &mut i32) { *value += 1; }

        fun f() {
            let mut left = 1;
            let mut right = 2;
            let mut reference = &mut left;
            reference = &mut right;
            *reference;
            update(&mut left);
        }
        ",
    );
}

#[test]
fn two_mutable_call_arguments_to_same_place_are_rejected() {
    assert!(has_code(
        r"
        fun pair(left: &mut i32, right: &mut i32) {}

        fun f() {
            let mut value = 1;
            pair(&mut value, &mut value);
        }
        ",
        "E0302"
    ));
}

#[test]
fn two_mutable_call_arguments_to_disjoint_fields_are_allowed() {
    is_clean(
        r"
        struct Pair { left: i32, right: i32 }
        fun pair(left: &mut i32, right: &mut i32) {}

        fun f() {
            let mut value = Pair { left: 1, right: 2 };
            pair(&mut value.left, &mut value.right);
        }
        ",
    );
}

#[test]
fn two_shared_call_arguments_to_same_place_are_allowed() {
    is_clean(
        r"
        fun pair(left: &i32, right: &i32) {}

        fun f() {
            let value = 1;
            pair(&value, &value);
        }
        ",
    );
}

#[test]
fn inner_block_borrow_does_not_escape_without_a_value() {
    is_clean(
        r"
        fun update(value: &mut i32) { *value += 1; }

        fun f() {
            let mut value = 1;
            {
                let reference = &mut value;
                *reference = 2;
            }
            update(&mut value);
        }
        ",
    );
}

#[test]
fn inner_block_returned_borrow_does_escape() {
    assert!(has_code(
        r"
        fun update(value: &mut i32) { *value += 1; }

        fun f() {
            let mut value = 1;
            let reference = {
                &mut value
            };
            update(&mut value);
            *reference;
        }
        ",
        "E0302"
    ));
}

#[test]
fn unknown_external_reference_return_is_conservative() {
    let result = analyze(
        r#"
        unsafe extern "C" {
            fun choose(value: &mut i32) -> &mut i32;
        }
        fun update(value: &mut i32) { *value += 1; }

        fun f() {
            let mut value = 1;
            let reference = unsafe { choose(&mut value) };
            update(&mut value);
            *reference;
        }
        "#,
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0302"),
        "{:?}",
        result.diagnostics
    );
}

#[test]
fn dyn_callable_returned_reference_keeps_argument_borrowed() {
    let result = analyze(
        r#"
        fun call(callback: &dyn Fn(&mut i32) -> &mut i32, value: &mut i32) -> &mut i32 {
            callback(value)
        }
        fun update(value: &mut i32) { *value += 1; }

        fun f(callback: &dyn Fn(&mut i32) -> &mut i32) {
            let mut value = 1;
            let reference = call(callback, &mut value);
            update(&mut value);
            *reference;
        }
        "#,
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0302"),
        "expected the dynamic callback result to keep `value` borrowed: {:#?}",
        result.diagnostics
    );
}

#[test]
fn moving_one_field_into_a_closure_leaves_the_other_field_available() {
    let result = analyze(
        r"
        struct Token { value: i32 }
        struct Pair { left: Token, right: Token }
        fun consume(value: Token) -> i32 { value.value }
        fun main() -> i32 {
            let pair = Pair {
                left: Token { value: 1 },
                right: Token { value: 2 },
            };
            let take_left = [ -> consume(pair.left)];
            consume(pair.right)
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn borrow_conflict_reports_the_original_borrow_as_secondary() {
    let result = analyze(
        r"
        fun f() {
            let mut value = 1;
            let first = &mut value;
            let second = &mut value;
            *first;
            *second;
        }
        ",
    );
    let diagnostic = result
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "E0302")
        .expect("missing E0302");
    assert!(diagnostic.labels.len() >= 2, "{:?}", diagnostic.labels);
    assert!(diagnostic.labels.iter().any(|label| {
        label.style == type_checker::LabelStyle::Secondary && label.message.contains("first borrow")
    }));
}

#[test]
fn reference_held_by_closure_blocks_assignment_until_last_call() {
    // A copy of `&value` captured into the closure keeps the shared loan of
    // `value` alive for the closure's lifetime: assigning between creation
    // and call would leave the captured reference dangling.
    let result = analyze(
        r"
        fun f() {
            let mut value = 1;
            let r = &value;
            let closure = [ -> { *r } ];
            value = 2;
            closure();
        }
        ",
    );

    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("cannot assign") && d.message.contains("value")),
        "{:?}",
        result.diagnostics
    );
}

#[test]
fn shared_capture_of_borrowed_binding_blocks_assignment() {
    let result = analyze(
        r"
        fun inspect(holder: &(&i32, i32)) -> i32 { *holder.0 }

        fun f() {
            let mut value = 1;
            let r = &value;
            let holder = (r, 10);
            let closure = [ -> { inspect(&holder) } ];
            value = 2;
            closure();
        }
        ",
    );

    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("cannot assign") && d.message.contains("value")),
        "{:?}",
        result.diagnostics
    );
}

#[test]
fn mutation_after_closure_last_use_is_allowed() {
    let result = analyze(
        r"
        fun f() {
            let mut value = 1;
            let r = &value;
            let closure = [ -> { *r } ];
            let out = closure();
            value = 2;
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn mutable_capture_conflicts_with_outstanding_borrow_held_by_closure() {
    // The closure captures `value` mutably while the shared loan captured
    // from `r` is still outstanding — rejected at the capture site.
    let result = analyze(
        r"
        fun f() {
            let mut value = 1;
            let r = &value;
            let mut closure = [ -> { value = 2; } ];
            closure();
            let out = *r;
        }
        ",
    );

    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("cannot capture") && d.message.contains("mutably")),
        "{:?}",
        result.diagnostics
    );
}

#[test]
fn immediately_invoked_closure_releases_its_captured_borrows() {
    let result = analyze(
        r"
        fun f() {
            let mut value = 1;
            let r = &value;
            [ -> { *r } ]();
            value = 2;
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn container_loan_held_by_closure_blocks_structural_mutation() {
    // The CHANGELOG's headline shape: a borrow of the container's interior
    // captured into a closure keeps its loan alive, so the `&mut self` call
    // in between is E0300. Without the fix the loan expired at the lambda's
    // closing bracket and `bag.set(2)` compiled against a stale interior
    // pointer.
    let result = analyze(
        r"
        struct Bag { value: i32 }

        impl Bag {
            fun borrow(&self) -> &i32 { &self.value }
            fun set(&mut self, next: i32) { self.value = next; }
        }

        fun f() {
            let mut bag = Bag { value: 1 };
            let r = bag.borrow();
            let closure = [ -> { *r } ];
            bag.set(2);
            let out = closure();
        }
        ",
    );

    assert!(
        result.diagnostics.iter().any(|d| d.code == "E0300"),
        "{:?}",
        result.diagnostics
    );
}

#[test]
fn container_loan_released_after_closures_last_call() {
    let result = analyze(
        r"
        struct Bag { value: i32 }

        impl Bag {
            fun borrow(&self) -> &i32 { &self.value }
            fun set(&mut self, next: i32) { self.value = next; }
        }

        fun f() {
            let mut bag = Bag { value: 1 };
            let r = bag.borrow();
            let closure = [ -> { *r } ];
            let out = closure();
            bag.set(2);
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn derived_read_blocks_write_through_reference() {
    // Writing through a `&mut` (`m.value = 2`) writes the referent: the
    // shared loan derived from `&m.value` conflicts, even though the
    // assignment's own place is rooted at the reference binding.
    let result = analyze(
        r"
        struct Bag { value: i32 }

        fun f() {
            let mut bag = Bag { value: 1 };
            let mut m = &mut bag;
            let r = &m.value;
            m.value = 2;
            let out = *r;
        }
        ",
    );

    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.code == "E0300" && d.message.contains("as mutable")),
        "{:?}",
        result.diagnostics
    );
}

#[test]
fn closure_held_derived_read_blocks_write_through_reference() {
    // Same shape with the derived read kept alive by a closure capture:
    // the write between creation and last call must be rejected.
    let result = analyze(
        r"
        struct Bag { value: i32 }

        fun f() {
            let mut bag = Bag { value: 1 };
            let mut m = &mut bag;
            let r = &m.value;
            let closure = [ -> { *r } ];
            m.value = 2;
            closure();
        }
        ",
    );

    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.code == "E0300" && d.message.contains("as mutable")),
        "{:?}",
        result.diagnostics
    );
}

#[test]
fn write_through_reference_after_last_derived_use_is_allowed() {
    // Once the derived reference's last use has passed, writing through the
    // `&mut` is fine — the loan is no longer held.
    let result = analyze(
        r"
        struct Bag { value: i32 }

        fun f() {
            let mut bag = Bag { value: 1 };
            let mut m = &mut bag;
            let r = &m.value;
            let closure = [ -> { *r } ];
            let out = closure();
            m.value = 2;
        }
        ",
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn whole_deref_assignment_conflicts_with_derived_loan() {
    // `*m = v` writes the whole referent, conflicting with a loan on any
    // part of it.
    let result = analyze(
        r"
        struct Bag { value: i32 }

        fun f() {
            let mut bag = Bag { value: 1 };
            let mut m = &mut bag;
            let r = &m.value;
            *m = Bag { value: 2 };
            let out = *r;
        }
        ",
    );

    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.code == "E0300" && d.message.contains("as mutable")),
        "{:?}",
        result.diagnostics
    );
}

#[test]
fn consecutive_next_on_stored_reference_iterator_is_allowed() {
    // `next` borrows the reference stored in the iterator's `values` field,
    // not the iterator itself: the returned element reference carries that
    // stored loan, the `&mut self` receiver loan expires at each call, and
    // two consecutive `next` calls no longer report E0302.
    let result = analyze(
        r#"
        enum Step {
            Item(&i32),
            Done,
        }

        struct Iter {
            values: &[i32; 2],
            index: usize,
        }

        trait StepIter {
            fun next(&mut self) -> Step;
        }

        impl StepIter for Iter {
            fun next(&mut self) -> Step {
                if self.index < 2usize {
                    let index = self.index;
                    self.index += 1usize;
                    Step::Item(&self.values[index])
                } else {
                    Step::Done
                }
            }
        }

        fun f() -> i32 {
            let data = [7, 9];
            let mut iter = Iter { values: &data, index: 0usize };
            let a = iter.next();
            let b = iter.next();
            match a { Step::Item(first) => *first, _ => 0 }
        }
        "#,
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn next_element_reference_blocks_underlying_mutation() {
    // The element reference carries the loan stored in the iterator's
    // `values` field, so mutably aliasing what that loan covers while the
    // element is alive is still a conflict.
    let result = analyze(
        r#"
        enum Step {
            Item(&i32),
            Done,
        }

        struct Iter {
            values: &[i32; 2],
            index: usize,
        }

        trait StepIter {
            fun next(&mut self) -> Step;
        }

        impl StepIter for Iter {
            fun next(&mut self) -> Step {
                if self.index < 2usize {
                    let index = self.index;
                    self.index += 1usize;
                    Step::Item(&self.values[index])
                } else {
                    Step::Done
                }
            }
        }

        fun f() -> i32 {
            let mut data = [7, 9];
            let mut iter = Iter { values: &data, index: 0usize };
            let a = iter.next();
            let m = &mut data;
            match a { Step::Item(first) => *first, _ => 0 }
        }
        "#,
    );

    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.code == "E0300" && d.message.contains("data")),
        "{:?}",
        result.diagnostics
    );
}

#[test]
fn generic_bound_consecutive_next_with_contract_is_allowed() {
    // Generic-bound dispatch has no concrete impl to summarize; the trait
    // method's `#[flow = "behind_reference"]` contract — verified against
    // every impl — licenses the returned references to live behind the
    // receiver's stored references, so consecutive `next` calls with live
    // results no longer report E0302.
    let result = analyze(
        r#"
        enum Step {
            Item(&i32),
            Done,
        }

        struct Iter {
            values: &[i32; 2],
            index: usize,
        }

        trait StepIter {
            #[flow = "behind_reference"]
            fun next(&mut self) -> Step;
        }

        impl StepIter for Iter {
            fun next(&mut self) -> Step {
                if self.index < 2usize {
                    let index = self.index;
                    self.index += 1usize;
                    Step::Item(&self.values[index])
                } else {
                    Step::Done
                }
            }
        }

        fun take_two<It: StepIter>(it: &mut It) -> i32 {
            let a = it.next();
            let b = it.next();
            match a { Step::Item(first) => *first, _ => 0 }
        }

        fun f() -> i32 {
            let data = [7, 9];
            let mut iter = Iter { values: &data, index: 0usize };
            take_two(&mut iter)
        }
        "#,
    );

    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn broken_contract_falls_back_to_conservative() {
    // An impl whose `next` borrows its own storage (the buffer field) fails
    // the contract verification; generic dispatch on that trait falls back
    // to the conservative whole-receiver mapping and consecutive calls
    // report again.
    let result = analyze(
        r#"
        enum Step {
            Item(&i32),
            Done,
        }

        struct OwnBuffer {
            buffer: [i32; 2],
            index: usize,
        }

        trait StepIter {
            #[flow = "behind_reference"]
            fun next(&mut self) -> Step;
        }

        impl StepIter for OwnBuffer {
            fun next(&mut self) -> Step {
                if self.index < 2usize {
                    let index = self.index;
                    self.index += 1usize;
                    Step::Item(&self.buffer[index])
                } else {
                    Step::Done
                }
            }
        }

        fun take_two<It: StepIter>(it: &mut It) -> i32 {
            let a = it.next();
            let b = it.next();
            match a { Step::Item(first) => *first, _ => 0 }
        }

        fun f() -> i32 {
            let mut iter = OwnBuffer { buffer: [7, 9], index: 0usize };
            take_two(&mut iter)
        }
        "#,
    );

    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.code == "E0300" || d.code == "E0301" || d.code == "E0302"),
        "{:?}",
        result.diagnostics
    );
}

fn assert_has_code(result: &move_checker::AnalysisResult, code: &str) {
    assert!(
        result.diagnostics.iter().any(|d| d.code == code),
        "expected {code}, got {:?}",
        result.diagnostics
    );
}

#[test]
fn rejects_assigning_a_field_through_a_shared_reference_parameter() {
    // `&T` says the referent is not writable. Field assignment through the
    // borrow went unchecked, so this mutated the caller's value.
    let result = analyze(
        r#"
        struct Sample { n: i32 }

        fun mutate(r: &Sample) {
            r.n = 5;
        }
        "#,
    );
    assert_has_code(&result, "E0309");
}

#[test]
fn allows_assigning_a_field_through_a_mutable_reference_parameter() {
    is_clean(
        r#"
        struct Sample { n: i32 }

        fun mutate(r: &mut Sample) {
            r.n = 5;
        }
        "#,
    );
}

#[test]
fn rejects_assigning_a_field_through_a_shared_self_receiver() {
    let result = analyze(
        r#"
        struct Counter { hits: i32 }

        impl Counter {
            fun bump(&self) {
                self.hits += 1;
            }
        }
        "#,
    );
    assert_has_code(&result, "E0309");
}

#[test]
fn rejects_a_mutable_method_call_on_a_field_of_a_shared_receiver() {
    // The receiver of `self.buffer.push(..)` is reached through `&self`, so the
    // `&mut` the method needs is taken through a shared borrow.
    let result = analyze(
        r#"
        struct Buffer { len: usize }

        impl Buffer {
            fun push(&mut self, value: i32) {
                self.len += value as usize;
            }
        }

        struct Holder { buffer: Buffer }

        impl Holder {
            fun record(&self) {
                self.buffer.push(1);
            }
        }
        "#,
    );
    assert_has_code(&result, "E0309");
}

#[test]
fn allows_writing_through_a_raw_pointer_derived_from_a_shared_reference() {
    // Raw pointers stay the documented `unsafe` escape hatch: reaching through
    // them is governed by the unsafe rules, not by the reference's mutability.
    is_clean(
        r#"
        struct Sample { n: i32 }

        fun mutate(r: &Sample) {
            unsafe {
                let p = r as *const Sample as *mut Sample;
                (*p).n = 5;
            }
        }
        "#,
    );
}

#[test]
fn casting_a_mutable_reference_to_a_pointer_keeps_the_reference_usable() {
    // A cast reads the pointer out of the reference; it does not consume it.
    // Recording a move here made the later `*r = 1` report E0100.
    is_clean(
        r#"
        fun write(r: &mut i32) -> i32 {
            let p = r as *const i32;
            *r = 1;
            unsafe {
                *p
            }
        }
        "#,
    );
}

#[test]
fn binding_a_mutable_reference_still_moves_it() {
    // The cast exemption must not turn into a general "references are Copy"
    // rule: binding one really does move it.
    let result = analyze(
        r#"
        fun take(r: &mut i32) {
            let forwarded = r;
            *forwarded = 1;
            *r = 2;
        }
        "#,
    );
    assert_has_code(&result, "E0100");
}

#[test]
fn a_diverging_branch_does_not_contribute_its_moves_to_the_join() {
    // `f` is only moved on the path that returns, so the fall-through call is
    // fine. Merging the `then` exit unconditionally reported E0100 here.
    is_clean(
        r#"
        fun apply<X>(flag: bool, f: impl FnOnce(i32) -> X) -> X {
            if flag {
                return f(1);
            }
            f(2)
        }
        "#,
    );
}

#[test]
fn a_falling_through_branch_still_contributes_its_moves_to_the_join() {
    // The same shape without the early return: the `then` path reaches the
    // second call with `f` already moved.
    let result = analyze(
        r#"
        fun apply<X>(flag: bool, f: impl FnOnce(i32) -> X) -> i32 {
            if flag {
                let _ = f(1);
            }
            let _ = f(2);
            0
        }
        "#,
    );
    assert_has_code(&result, "E0100");
}

#[test]
fn a_diverging_match_arm_does_not_contribute_its_moves_to_the_join() {
    // The same rule as the `if` case, one arm at a time: the arm that returns
    // never reaches the call after the `match`.
    is_clean(
        r#"
        fun pick<X>(flag: bool, g: impl FnOnce(i32) -> X) -> i32 {
            match flag {
                true => {
                    let _ = g(1);
                    return 0;
                }
                false => {}
            }
            let _ = g(2);
            1
        }
        "#,
    );
}

#[test]
fn a_falling_through_match_arm_still_contributes_its_moves_to_the_join() {
    let result = analyze(
        r#"
        fun pick<X>(flag: bool, g: impl FnOnce(i32) -> X) -> i32 {
            match flag {
                true => {
                    let _ = g(1);
                }
                false => {}
            }
            let _ = g(2);
            1
        }
        "#,
    );
    assert_has_code(&result, "E0100");
}

#[test]
fn a_mut_field_is_writable_through_a_shared_self_receiver() {
    is_clean(
        r#"
        struct Counter { mut hits: i32, plain: i32 }

        impl Counter {
            fun bump(&self) {
                self.hits += 1;
            }
        }
        "#,
    );
}

#[test]
fn a_plain_field_is_not_writable_through_a_shared_self_receiver() {
    let result = analyze(
        r#"
        struct Counter { mut hits: i32, plain: i32 }

        impl Counter {
            fun set(&self) {
                self.plain = 1;
            }
        }
        "#,
    );
    assert_has_code(&result, "E0309");
}

#[test]
fn a_mutable_method_call_on_a_mut_field_of_a_shared_receiver_is_allowed() {
    // `self.log.push(..)` has no `&mut` expression to reject: the borrow is the
    // call's own receiver argument, so it lives exactly as long as the call.
    is_clean(
        r#"
        struct Buffer { mut len: usize }

        impl Buffer {
            fun push(&mut self) {
                self.len += 1;
            }
        }

        struct Holder { mut buffer: Buffer }

        impl Holder {
            fun record(&self) {
                self.buffer.push();
            }
        }
        "#,
    );
}

#[test]
fn a_mut_field_borrow_cannot_be_bound() {
    // Binding the `&mut` would let it outlive the call, and two of them could
    // then alias. Only a call receiver or argument may take it.
    let result = analyze(
        r#"
        struct Counter { mut hits: i32 }

        fun bind(counter: &Counter) {
            let target = &mut counter.hits;
            *target = 1;
        }
        "#,
    );
    assert_has_code(&result, "E0309");
}

#[test]
fn a_mut_field_borrow_cannot_be_returned() {
    let result = analyze(
        r#"
        struct Counter { mut hits: i32 }

        fun leak(counter: &Counter) -> &mut i32 {
            &mut counter.hits
        }
        "#,
    );
    assert_has_code(&result, "E0309");
}

#[test]
fn a_mut_field_borrow_is_allowed_as_a_call_argument() {
    is_clean(
        r#"
        struct Counter { mut hits: i32 }

        fun add_to(target: &mut i32) {
            *target += 1;
        }

        fun pass(counter: &Counter) {
            add_to(&mut counter.hits);
        }
        "#,
    );
}

#[test]
fn a_shared_borrow_of_the_receiver_does_not_block_a_mut_field_write() {
    // A shared borrow of the whole value does not freeze a `mut` field: a
    // `&self` method may still update it, and the borrow observes the change.
    is_clean(
        r#"
        struct Counter { mut hits: i32 }

        impl Counter {
            fun bump(&self) {
                self.hits += 1;
            }

            fun read(&self) -> i32 {
                self.hits
            }
        }

        fun f() {
            let counter = Counter { hits: 0 };
            let view = &counter;
            counter.bump();
            let _ = view.read();
        }
        "#,
    );
}

#[test]
fn a_borrow_into_a_mut_field_blocks_a_shared_method_that_writes_it() {
    // The method's write can move what the borrow points at, and the callee's
    // own checks cannot see this caller's loan.
    let result = analyze(
        r#"
        struct Inner { value: i32 }

        struct Holder { mut inner: Inner }

        impl Holder {
            fun bump(&self) {
                self.inner.value += 1;
            }
        }

        fun f() {
            let holder = Holder { inner: Inner { value: 0 } };
            let held = &holder.inner.value;
            holder.bump();
            let _ = *held;
        }
        "#,
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.code == "E0300" || d.code == "E0303"),
        "{:?}",
        result.diagnostics
    );
}

#[test]
fn a_shared_method_writing_a_different_mut_field_does_not_conflict() {
    // The callee's interior-write summary names `a`, so a borrow into `b` is
    // unrelated. Claiming every `mut` field of the receiver rejected this.
    is_clean(
        r#"
        struct Inner { value: i32 }

        struct Holder { mut a: Inner, mut b: Inner }

        impl Holder {
            fun touch_a(&self) {
                self.a.value += 1;
            }
        }

        fun f() {
            let holder = Holder { a: Inner { value: 0 }, b: Inner { value: 0 } };
            let held = &holder.b.value;
            holder.touch_a();
            let _ = *held;
        }
        "#,
    );
}

#[test]
fn a_shared_method_writing_no_mut_field_does_not_conflict() {
    is_clean(
        r#"
        struct Inner { value: i32 }

        struct Holder { mut a: Inner, mut b: Inner }

        impl Holder {
            fun read_b(&self) -> i32 {
                self.b.value
            }
        }

        fun f() {
            let holder = Holder { a: Inner { value: 0 }, b: Inner { value: 0 } };
            let held = &holder.b.value;
            let _ = holder.read_b();
            let _ = *held;
        }
        "#,
    );
}

#[test]
fn a_free_function_writing_a_mut_field_blocks_a_borrow_into_it() {
    // The write travels through a `&T` argument, not a receiver: a free
    // function can move the field's storage just as a `&self` method can.
    let result = analyze(
        r#"
        struct Inner { value: i32 }

        struct Holder { mut a: Inner }

        fun write_a(holder: &Holder) {
            holder.a.value += 1;
        }

        fun f() {
            let holder = Holder { a: Inner { value: 0 } };
            let held = &holder.a.value;
            write_a(&holder);
            let _ = *held;
        }
        "#,
    );
    assert_has_code(&result, "E0300");
}

#[test]
fn another_types_method_writing_a_mut_field_blocks_a_borrow_into_it() {
    let result = analyze(
        r#"
        struct Inner { value: i32 }

        struct Holder { mut a: Inner }

        struct Sink { count: i32 }

        impl Sink {
            fun absorb(&mut self, holder: &Holder) {
                holder.a.value += 1;
                self.count += 1;
            }
        }

        fun f() {
            let holder = Holder { a: Inner { value: 0 } };
            let held = &holder.a.value;
            let mut sink = Sink { count: 0 };
            sink.absorb(&holder);
            let _ = *held;
        }
        "#,
    );
    assert_has_code(&result, "E0300");
}

#[test]
fn an_indirectly_written_mut_field_is_still_reported() {
    // `bump` writes nothing itself — it forwards to `helper`. The summary has
    // to follow the call, or the write would be invisible to the caller.
    let result = analyze(
        r#"
        struct Inner { value: i32 }

        struct Holder { mut inner: Inner }

        impl Holder {
            fun bump(&self) {
                self.helper();
            }

            fun helper(&self) {
                self.inner.value += 1;
            }
        }

        fun f() {
            let holder = Holder { inner: Inner { value: 0 } };
            let held = &holder.inner.value;
            holder.bump();
            let _ = *held;
        }
        "#,
    );
    assert_has_code(&result, "E0300");
}

#[test]
fn a_mut_field_write_through_a_mutable_borrow_reaches_the_caller() {
    // `push` takes `&mut self`, so the callee's own summary is empty — the
    // write is implied by the borrow the caller handed over.
    let result = analyze(
        r#"
        struct Buffer { len: usize }

        impl Buffer {
            fun push(&mut self) {
                self.len += 1;
            }
        }

        struct Holder { mut buffer: Buffer }

        fun forward(holder: &Holder) {
            holder.buffer.push();
        }

        fun f() {
            let holder = Holder { buffer: Buffer { len: 0 } };
            let held = &holder.buffer.len;
            forward(&holder);
            let _ = *held;
        }
        "#,
    );
    assert_has_code(&result, "E0300");
}

#[test]
fn a_reference_stored_through_a_parameter_keeps_its_loan() {
    // Storing through a `&mut &i32` parameter goes through the assignment to
    // the dereferenced place, so this shape is already caught. The neighbouring
    // shapes that are *not* caught — a container element, a `mut` field, a
    // trait object — are in `stored_reference.rs`.
    let result = analyze(
        r"
        struct Buf { mut data: i32 }

        impl Buf {
            fun write(&mut self, value: i32) {
                self.data = value;
            }
        }

        fun store(slot: &mut &i32, value: &i32) {
            *slot = value;
        }

        fun f() {
            let mut buf = Buf { data: 1 };
            let mut slot = &buf.data;
            store(&mut slot, &buf.data);
            buf.write(40);
            let _ = *slot;
        }
        ",
    );
    assert_has_code(&result, "E0300");
}
