//! Escape-analysis behavior tests: values that stay on the stack vs values
//! promoted to the conservative GC heap, and the `E0310` diagnostic that
//! fires when a reference would escape in a no-GC build.

use riddlec::pipeline::{self, CompileOptions};
use type_checker::Diagnostic;

fn escape_diagnostics(source: &str, gc: bool) -> Vec<Diagnostic> {
    let result = pipeline::compile_with_options_and_gc(
        source,
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
        gc,
    );
    result.analysis_diagnostics
}

#[test]
fn local_scalar_stays_on_the_stack() {
    let diagnostics = escape_diagnostics(
        r"
        fun compute() -> i32 {
            let value = 7i32;
            value * 2i32
        }
        ",
        true,
    );
    assert!(
        diagnostics.is_empty(),
        "unexpected diagnostics: {diagnostics:#?}"
    );
}

#[test]
fn reference_returned_from_function_escapes() {
    let diagnostics = escape_diagnostics(
        r"
        struct Data { value: i32 }

        fun escaped() -> &Data {
            let local = Data { value: 1 };
            &local
        }
        ",
        true,
    );
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.code != "E0310"),
        "GC builds must promote the value instead of rejecting: {diagnostics:#?}"
    );
}

#[test]
fn reference_stored_in_struct_field_escapes() {
    let diagnostics = escape_diagnostics(
        r"
        struct Inner { value: i32 }
        struct Holder { reference: &Inner }

        fun make() -> Holder {
            let inner = Inner { value: 2 };
            Holder { reference: &inner }
        }
        ",
        true,
    );
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.code != "E0310"),
        "GC builds must promote through fields: {diagnostics:#?}"
    );
}

#[test]
fn no_gc_rejects_returned_reference_to_local() {
    let diagnostics = escape_diagnostics(
        r"
        struct Data { value: i32 }

        fun escaped() -> &Data {
            let local = Data { value: 1 };
            &local
        }
        ",
        false,
    );
    let diagnostic = diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "E0310")
        .expect("no-GC escape should report E0310");
    assert!(diagnostic.message.contains("GC is disabled"));
}

#[test]
fn no_gc_rejects_reference_stored_in_field() {
    let diagnostics = escape_diagnostics(
        r"
        struct Inner { value: i32 }
        struct Holder { reference: &Inner }

        fun make() -> Holder {
            let inner = Inner { value: 2 };
            Holder { reference: &inner }
        }
        ",
        false,
    );
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0310"),
        "field escape should report E0310 under no-GC: {diagnostics:#?}"
    );
}

#[test]
fn borrowing_within_a_frame_does_not_escape() {
    let diagnostics = escape_diagnostics(
        r"
        struct Data { value: i32 }

        fun read(data: &Data) -> i32 {
            data.value
        }

        fun compute() -> i32 {
            let local = Data { value: 3 };
            read(&local)
        }
        ",
        false,
    );
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.code != "E0310"),
        "same-frame borrows must stay on the stack under no-GC: {diagnostics:#?}"
    );
}

#[test]
fn bound_dispatch_receiver_escape_is_tracked_through_impl_summaries() {
    // `impl Save for Node::save(&self)` stores the receiver in its result.
    // Through the generic bound dispatch `item.save()`, the union of impl
    // summaries must mark the receiver as returning/escaping so a stack
    // reference passed into `generic` is rejected in no-GC builds (it used
    // to be treated as merely borrowed, producing a dangling pointer).
    let diagnostics = escape_diagnostics(
        r"
        struct Node { value: i32 }
        struct Holder { kept: &Node }

        trait Save {
            fun save(&self) -> Holder;
        }

        impl Save for Node {
            fun save(&self) -> Holder {
                Holder { kept: self }
            }
        }

        fun generic<T: Save>(item: &T) -> Holder {
            item.save()
        }

        fun leak() -> Holder {
            let node = Node { value: 1 };
            generic(&node)
        }
        ",
        false,
    );
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0310"),
        "expected E0310 for a bound-dispatch receiver escape in no-GC mode, got {diagnostics:#?}"
    );
}

#[test]
fn bound_dispatch_receiver_borrow_only_impl_stays_on_stack() {
    // An impl that only reads the receiver must not force promotion of the
    // caller's value: the impl-union summary keeps precision.
    let diagnostics = escape_diagnostics(
        r"
        struct Node { value: i32 }

        trait Read {
            fun read(&self) -> i32;
        }

        impl Read for Node {
            fun read(&self) -> i32 {
                self.value
            }
        }

        fun generic<T: Read>(item: &T) -> i32 {
            item.read()
        }

        fun use_it(node: &Node) -> i32 {
            generic(node)
        }
        ",
        false,
    );
    assert!(
        !diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0310"),
        "a borrow-only bound dispatch must stay on the stack, got {diagnostics:#?}"
    );
}

#[test]
fn reference_param_root_stored_in_result_escapes() {
    // The callee stores the reference *parameter* into its return value;
    // the caller's stack reference must then be rejected in no-GC builds
    // through the callee's function summary.
    let diagnostics = escape_diagnostics(
        r"
        struct Data { value: i32 }
        struct Holder { kept: &Data }

        fun store(data: &Data) -> Holder {
            Holder { kept: data }
        }

        fun leak() -> Holder {
            let local = Data { value: 1 };
            store(&local)
        }
        ",
        false,
    );
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0310"),
        "expected E0310 for a stack reference escaping through a reference param, got {diagnostics:#?}"
    );
}

#[test]
fn escape_through_an_inner_block_is_still_promoted() {
    // The reference is born inside a nested block expression but the value
    // outlives it; promotion decisions must not be scoped to one block.
    let diagnostics = escape_diagnostics(
        r"
        struct Data { value: i32 }
        struct Holder { kept: &Data }

        fun leak() -> Holder {
            let local = Data { value: 1 };
            let holder = {
                let made = Holder { kept: &local };
                made
            };
            holder
        }
        ",
        false,
    );
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0310"),
        "expected E0310 for a cross-block escaping reference in no-GC mode, got {diagnostics:#?}"
    );
}

#[test]
fn lambda_capturing_local_and_escaping_promotes_it() {
    // The returned lambda's environment holds a shared capture of `local`,
    // so `local` escapes with the callable in no-GC builds.
    let diagnostics = escape_diagnostics(
        r"
        struct Data { value: i32 }

        fun make_reader() -> impl Fn() -> i32 {
            let local = Data { value: 5 };
            [ -> local.value ]
        }
        ",
        false,
    );
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0310"),
        "expected E0310 for a local captured by an escaping lambda, got {diagnostics:#?}"
    );
}

#[test]
fn tuple_and_array_projection_references_escape() {
    // References to a tuple element and an array element stored in the
    // return value: the projections root at the whole local, so it must be
    // promoted rather than kept on the stack in no-GC builds.
    let diagnostics = escape_diagnostics(
        r"
        struct Data { value: i32 }
        struct Pair { first: &Data, second: &Data }

        fun leak() -> Pair {
            let tuple = (Data { value: 1 }, Data { value: 2 });
            let array = [Data { value: 3 }, Data { value: 4 }];
            Pair { first: &tuple.1, second: &array[0] }
        }
        ",
        false,
    );
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0310"),
        "expected E0310 for tuple/array projection escapes in no-GC mode, got {diagnostics:#?}"
    );
}
