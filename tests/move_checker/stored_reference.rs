//! A reference stored into a place that outlives the call must keep its loan.
//!
//! The cases below exercise calls that store a reference into a place
//! that outlives the call. Each must report the borrow conflict at the later
//! mutation.
//!
//! ```text
//! cargo test -p riddle --test move_checker stored_reference
//! ```
//!
//! A separate shape that the fix must preserve — a reference stored through a
//! `&mut &i32` parameter — already reports correctly and lives in
//! `tests/move_checker/borrow.rs`.
//!
//! # The regression
//!
//! `ReferenceFlow` summarises where a callee's borrows return to. A callable
//! that keeps a reference inside a parameter — `self.held = value`,
//! `self.items.push(value)` — has no return value to carry that provenance.
//! The storage effect must still keep the source loan active:
//!
//! ```riddle
//! let mut buf = Buf { data: 1 };
//! let mut refs: Vector<&i32> = Vector::new();
//! refs.push(&buf.data);
//! buf.write(40);          // E0300
//! let _ = *refs[0];       // reads 40
//! ```
//!
//! `tests/move_checker/borrow.rs` covers the neighbouring direct aliases and
//! the `&mut &i32` parameter shape.
//!
//! # What a fix has to get right
//!
//! Recording the store is the easy half: `analyze_binary` notes that the
//! right-hand side's provenance is written into a parameter path. The call
//! site maps that path onto the destination and keeps the loan through the
//! destination's scope. The summary is consulted for all calls, including
//! `()`-returning calls, while the return provenance remains unchanged.

use crate::analyze;

#[test]
fn proc_macro_standard_library_owned_results_do_not_retain_borrows() {
    // Proc-macro hosts inject their API as package source, so its bodies are
    // checked rather than using the cached standard-library check.
    let result = riddlec::pipeline::check_with_options(
        concat!(
            include_str!("../../std/std/proc_macro.rid"),
            "\n",
            include_str!("../../std/std/syn.rid"),
        ),
        riddlec::pipeline::CompileOptions::default(),
    );
    assert!(
        result.success(),
        "parse: {:#?}\nhir: {:#?}\ntypes: {:#?}\nborrows: {:#?}",
        result.parse_errors,
        result.hir_diagnostics,
        result.type_result.diagnostics,
        result.analysis_diagnostics,
    );
}

fn assert_has_code(result: &move_checker::AnalysisResult, code: &str) {
    assert!(
        result.diagnostics.iter().any(|d| d.code == code),
        "expected {code}, got {:?}",
        result.diagnostics
    );
}

/// The shape from the report: a reference pushed into a generic container.
/// Mirrors `Vector::push(&mut self, value: T)`.
#[test]
fn a_reference_pushed_into_a_generic_container_keeps_its_loan() {
    let result = analyze(
        r"
        struct Buf { mut data: i32 }

        impl Buf {
            fun write(&mut self, value: i32) {
                self.data = value;
            }
        }

        struct Bag<T> { mut item: T }

        impl<T> Bag<T> {
            fun store(&mut self, value: T) {
                self.item = value;
            }

            fun held(&self) -> &T {
                &self.item
            }
        }

        fun f() {
            let mut buf = Buf { data: 1 };
            let mut bag = Bag { item: &buf.data };
            bag.store(&buf.data);
            let _ = *bag.held();
            buf.write(40);
        }
        ",
    );
    assert_has_code(&result, "E0300");
}

/// The same shape without generics: a concrete reference parameter.
#[test]
fn a_reference_stored_by_a_concrete_value_method_keeps_its_loan() {
    let result = analyze(
        r"
        struct Buf { mut data: i32 }

        impl Buf {
            fun write(&mut self, value: i32) {
                self.data = value;
            }
        }

        struct Slot { held: &i32 }

        impl Slot {
            fun store(&mut self, value: &i32) {
                self.held = value;
            }
        }

        fun f() {
            let mut buf = Buf { data: 1 };
            let mut slot = Slot { held: &buf.data };
            slot.store(&buf.data);
            buf.write(40);
        }
        ",
    );
    assert_has_code(&result, "E0300");
}

/// Two fields deep, to show the path is not what makes it work.
#[test]
fn a_reference_stored_in_a_nested_field_keeps_its_loan() {
    let result = analyze(
        r"
        struct Buf { mut data: i32 }

        impl Buf {
            fun write(&mut self, value: i32) {
                self.data = value;
            }
        }

        struct Inner { held: &i32 }
        struct Outer { inner: Inner }

        impl Outer {
            fun store(&mut self, value: &i32) {
                self.inner.held = value;
            }
        }

        fun f() {
            let mut buf = Buf { data: 1 };
            let mut outer = Outer { inner: Inner { held: &buf.data } };
            outer.store(&buf.data);
            buf.write(40);
        }
        ",
    );
    assert_has_code(&result, "E0300");
}

/// Reached through a trait object, the path `&dyn Source` takes.
#[test]
fn a_reference_stored_through_a_trait_object_keeps_its_loan() {
    let result = analyze(
        r"
        trait Sink {
            fun put(&mut self, value: &i32);
        }

        struct Buf { mut data: i32 }

        impl Buf {
            fun write(&mut self, value: i32) {
                self.data = value;
            }
        }

        struct Slot { held: &i32 }

        impl Sink for Slot {
            fun put(&mut self, value: &i32) {
                self.held = value;
            }
        }

        fun f() {
            let mut buf = Buf { data: 1 };
            let mut slot = Slot { held: &buf.data };
            let sink: &mut dyn Sink = &mut slot;
            sink.put(&buf.data);
            buf.write(40);
        }
        ",
    );
    assert_has_code(&result, "E0300");
}

#[test]
fn a_forwarding_function_preserves_the_storage_effect() {
    let result = analyze(
        r"
        struct Buf { mut data: i32 }

        impl Buf {
            fun write(&mut self, value: i32) {
                self.data = value;
            }
        }

        struct Slot { held: &i32 }

        impl Slot {
            fun store(&mut self, value: &i32) {
                self.held = value;
            }
        }

        fun forward(slot: &mut Slot, value: &i32) {
            slot.store(value);
        }

        fun f() {
            let mut buf = Buf { data: 1 };
            let mut slot = Slot { held: &buf.data };
            forward(&mut slot, &buf.data);
            buf.write(40);
        }
        ",
    );
    assert_has_code(&result, "E0300");
}
