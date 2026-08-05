use crate::{compile, lower};
use mir::Backend;
use mir::backend::c::CBackend;
use std::{fmt::Write as _, fs, process::Command};

fn c_symbol(kind: char, name: &str) -> String {
    let mut suffix = String::with_capacity(name.len() * 2);
    for byte in name.bytes() {
        write!(&mut suffix, "{byte:02x}").unwrap();
    }
    format!("riddle_{kind}_{suffix}")
}

fn c_function(name: &str) -> String {
    c_symbol('f', name)
}

fn c_type(name: &str) -> String {
    c_symbol('t', name)
}

fn c_member(name: &str) -> String {
    c_symbol('m', name)
}

fn c_variable(name: &str) -> String {
    c_symbol('v', name)
}

#[test]
fn c_simple_function() {
    let module = lower(
        r"
        fun main() {
            let x = 42;
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(result.contains("main"), "missing main: {result}");
    assert!(result.contains("return"), "missing return: {result}");
    // ponytail: const 42 is inlined; since x is dead, no int32_t variable is emitted.
}

#[test]
fn c_export_uses_c_string_abi_and_preserves_internal_str_layout() {
    let module = lower(
        r"
        #[c_export]
        fun macro_wrapper(input: &str) {}

        fun ordinary(input: &str) {}
        ",
    );

    let generated = CBackend::new().compile(&module).unwrap();
    assert!(
        generated.contains("void macro_wrapper(const char *p0);"),
        "{generated}"
    );
    assert!(
        generated.contains("void macro_wrapper (const char *"),
        "{generated}"
    );
    assert!(
        generated.contains("riddle_str export_str") && generated.contains("= (riddle_str){"),
        "{generated}"
    );
    assert!(
        generated.contains(&format!("void {}(riddle_str p0);", c_function("ordinary"))),
        "{generated}"
    );
}

#[test]
fn c_slice_borrow_carries_length_and_indexes_elements() {
    let (_, type_result, analysis, module) = compile(
        r"
        struct Item { value: i32 }

        fun second(values: &[Item]) -> i32 {
            values[1].value
        }

        fun main() -> i32 {
            let values = [
                Item { value: 10 },
                Item { value: 20 },
                Item { value: 30 },
            ];
            let slice: &[Item] = &values;
            second(slice)
        }
        ",
    );
    assert_eq!(type_result.diagnostics, vec![]);
    assert_eq!(analysis.diagnostics, vec![]);

    let generated = CBackend::new().compile(&module).unwrap();
    assert!(generated.contains("riddle_slice"), "{generated}");
    assert!(generated.contains(&c_type("Item")), "{generated}");
    assert!(generated.contains(".ptr"), "{generated}");
    assert!(generated.contains("UINT64_C(3)"), "{generated}");
}

#[test]
fn std_slices_support_safe_access_and_borrowed_iteration() {
    let result = riddlec::pipeline::compile(
        r"
        fun sum(values: &[i32]) -> i32 {
            let mut total = 0;
            for value in values {
                total += *value;
            }
            total
        }

        fun increment(values: &mut [i32]) {
            let length = values.len();
            for value in values {
                *value += 1;
            }
        }

        fun main() -> i32 {
            let mut values = [10, 20, 30];
            let slice: &[i32] = &values;
            let fallback = 0;
            let before = *slice.get(1usize).unwrap_or(&fallback);
            let length_ok = slice.len() == 3usize && !slice.is_empty();
            increment(&mut values);
            let after: &[i32] = &values;
            if before == 20 && length_ok && sum(after) == 63 { 0 } else { 1 }
        }
        ",
    );
    assert!(result.success(), "{:#?}", result.type_result.diagnostics);

    let generated = riddlec::pipeline::generate_c(result.mir_module.as_ref().unwrap()).unwrap();
    assert!(generated.contains("riddle_slice"), "{generated}");
    assert!(generated.contains(".len"), "{generated}");
    assert!(generated.contains(".ptr"), "{generated}");
}

#[test]
fn moving_a_mutable_reference_still_respects_live_reborrows() {
    let (_, _, analysis, _) = compile(
        r"
        fun consume<T>(value: T) {}

        fun invalid(value: &mut i32) {
            let child = &mut *value;
            consume(value);
            *child = 1;
        }
        ",
    );
    assert!(
        analysis
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0304"),
        "{:#?}",
        analysis.diagnostics
    );
}

#[test]
fn bare_slice_values_are_rejected() {
    let result = riddlec::pipeline::compile_with_options(
        "fun invalid(value: [i32]) -> [i32] { value }",
        riddlec::pipeline::CompileOptions { use_std: false },
    );
    assert!(
        result
            .type_result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "E0043"),
        "{:#?}",
        result.type_result.diagnostics
    );
}

#[test]
fn c_extern_slice_abi_is_rejected() {
    let module = lower(
        r#"
        unsafe extern "C" {
            fun consume(values: &[i32]);
        }

        fun main() {}
        "#,
    );
    let error = CBackend::new().compile(&module).unwrap_err();
    assert!(error.contains("pointer and length separately"), "{error}");
}

#[test]
fn c_backend_does_not_abort_after_never_extern() {
    let module = lower(
        r#"
        unsafe extern "C" {
            safe fun panic(message: &str) -> !;
        }

        fun main() {
            panic("boom");
        }
        "#,
    );
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(
        generated.contains("extern void panic(const char*, size_t)"),
        "{generated}"
    );
    assert!(!generated.contains("riddle_panic"), "{generated}");
    assert!(!generated.contains("static void panic"), "{generated}");
    assert!(!generated.contains("abort()"), "{generated}");
    assert!(generated.contains("for (;;) {}"), "{generated}");
    assert!(!generated.contains("return 0;"), "{generated}");
}

#[test]
fn c_tuple_types_are_named_and_reusable() {
    let module = lower(
        r"
        enum Foo { A(i32, (i64, i32)) }

        fun main() {
            let value = match Foo::A(1, (2, 3)) {
                Foo::A(_, pair) => pair,
            };
            let sink = value;
        }
        ",
    );
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(
        generated.contains(&format!("typedef struct {}", c_type("riddle_tuple_"))),
        "{generated}"
    );
    assert!(
        !generated.contains("struct { int64_t f0; int32_t f1; }"),
        "{generated}"
    );
}

#[test]
fn c_tuple_comparison_is_lowered_elementwise() {
    let module = lower(
        r#"
        #[lang = "partial_eq"]
        trait PartialEq<Rhs = Self> {
            fun eq(&self, other: &Rhs) -> bool;
        }

        impl PartialEq for i32 {
            fun eq(&self, other: &i32) -> bool {
                *self == *other
            }
        }

        fun main() -> bool {
            let left: (i32, i32) = (1, 2);
            let right: (i32, i32) = (1, 2);
            left == right
        }
        "#,
    );
    let generated = CBackend::new().compile(&module).unwrap();

    let f0 = c_member("f0");
    let f1 = c_member("f1");
    assert!(generated.contains(&format!(".{f0} ==")), "{generated}");
    assert!(generated.contains(&format!(".{f1} ==")), "{generated}");
    assert!(
        !generated.lines().any(|line| {
            let line = line.trim_start();
            line.starts_with("if (tup")
                && !line.contains(&format!(".{f0}"))
                && !line.contains(&format!(".{f1}"))
        }),
        "tuple values must not be compared as C structs:\n{generated}"
    );
}

#[test]
fn c_composite_comparison_dispatches_element_trait_impls() {
    let module = lower(
        r#"
        #[lang = "partial_eq"]
        trait PartialEq<Rhs = Self> {
            fun eq(&self, other: &Rhs) -> bool;
        }

        struct Point { value: i32 }

        impl PartialEq for Point {
            fun eq(&self, other: &Point) -> bool {
                self.value == other.value
            }
        }

        impl PartialEq for i32 {
            fun eq(&self, other: &i32) -> bool {
                *self == *other
            }
        }

        fun main() -> bool {
            let left: (Point, i32) = (Point { value: 1 }, 2);
            let right: (Point, i32) = (Point { value: 1 }, 2);
            left == right
        }
        "#,
    );
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(
        generated.matches(&c_function("eq__Point")).count() >= 3,
        "user-defined elements must call PartialEq::eq:\n{generated}"
    );
}

#[test]
fn c_array_comparison_is_lowered_elementwise() {
    let module = lower(
        r#"
        #[lang = "partial_eq"]
        trait PartialEq<Rhs = Self> {
            fun eq(&self, other: &Rhs) -> bool;
        }

        impl PartialEq for i32 {
            fun eq(&self, other: &i32) -> bool {
                *self == *other
            }
        }

        fun main() -> bool {
            let left: [i32; 2] = [1, 2];
            let right: [i32; 2] = [1, 2];
            left == right
        }
        "#,
    );
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(generated.contains("arr2["), "{generated}");
    assert!(generated.contains("arr3["), "{generated}");
    assert!(
        !generated
            .lines()
            .any(|line| line.trim_start().starts_with("if (arr") && line.contains("==")),
        "arrays must not be compared by C pointer decay:\n{generated}"
    );
}

#[test]
fn c_anonymous_function_uses_typed_function_pointer() {
    let module = lower(
        r"
        fun apply(f: impl Fn(i32) -> i32, value: i32) -> i32 {
            f(value)
        }

        fun main() -> i32 {
            let inc = fun(x) { x + 1 };
            apply(inc, 41)
        }
        ",
    );
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(
        generated.contains("typedef int32_t (*riddle_fn_"),
        "{generated}"
    );
    assert!(
        generated.contains(&c_function("__riddle_lambda_")),
        "{generated}"
    );
    assert!(
        generated.contains(&format!(".{}(", c_member("call")))
            && generated.contains(&format!(".{}", c_member("env"))),
        "{generated}"
    );
}

#[test]
fn c_non_escaping_closure_capture_uses_stack_environment() {
    let module = lower(
        r"
        fun main() -> i32 {
            let base = 40;
            let add = fun(value: i32) { base + value };
            add(2)
        }
        ",
    );
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(!generated.contains("rgc_alloc"), "{generated}");
    assert!(
        generated.contains(&c_type("__riddle_lambda_1_env")),
        "{generated}"
    );
    assert!(
        generated.contains(&c_member("capture_0_base")),
        "{generated}"
    );
    assert!(
        generated.contains(&format!(".{}(", c_member("call"))),
        "{generated}"
    );
}

#[test]
fn c_returned_closure_keeps_parameter_alive() {
    let module = lower(
        r"
        fun make_adder(base: i32) -> impl Fn(i32) -> i32 {
            fun(value: i32) { base + value }
        }

        fun main() -> i32 {
            let add = make_adder(40);
            add(2)
        }
        ",
    );
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(generated.contains(&c_function("make_adder")), "{generated}");
    assert!(generated.contains("rgc_alloc"), "{generated}");
    assert!(
        generated.contains(&c_member("capture_0_base")),
        "{generated}"
    );
}

#[test]
fn c_gc_closure_environment_has_deterministic_drop_glue() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard {}
        impl Drop for Guard { fun drop(&mut self) {} }
        fun consume(value: Guard) {}

        fun make() -> impl FnOnce() -> () {
            let guard = Guard {};
            fun() { consume(guard); }
        }

        fun main() {
            let closure = make();
        }
        "#,
    );
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(generated.contains("rgc_alloc"), "{generated}");
    assert!(
        generated.contains(&c_function("__riddle_lambda_1_drop")),
        "{generated}"
    );
    assert!(
        generated.contains(&c_function("drop__Guard")),
        "{generated}"
    );
    assert!(
        generated.contains(&format!(".{}(", c_member("drop"))),
        "{generated}"
    );
}

#[test]
fn c_named_function_value_uses_empty_environment_adapter() {
    let module = lower(
        r"
        fun inc(value: i32) -> i32 { value + 1 }
        fun apply(f: impl Fn(i32) -> i32, value: i32) -> i32 { f(value) }
        fun main() -> i32 { apply(inc, 41) }
        ",
    );
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(
        generated.contains(&c_function("__riddle_fn_adapter_inc")),
        "{generated}"
    );
    assert!(generated.contains(&c_function("apply__")), "{generated}");
}

#[test]
fn c_backend_unit_main_returns_zero() {
    let module = lower(
        r"
        fun main() {}
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(
        result.contains("int main"),
        "main should return int:\n{result}"
    );
    assert!(
        result.contains("return 0;"),
        "unit main should return zero:\n{result}"
    );
}

#[test]
fn c_return_value() {
    let module = lower(
        r"
        fun answer() -> i32 {
            return 42;
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(result.contains("int32_t"), "missing int type: {result}");
    assert!(result.contains("return"), "missing return: {result}");
}

#[test]
fn c_backend_preserves_unsigned_and_pointer_sized_types() {
    let module = lower(
        r"
        fun values(a: u8, b: u16, c: u32, d: u64, e: usize, f: isize) -> u64 {
            a as u64 + b as u64 + c as u64 + d + e as u64 + f as u64
        }
        ",
    );
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(
        generated.contains(&format!("uint8_t {}", c_variable("a"))),
        "{generated}"
    );
    assert!(
        generated.contains(&format!("uint16_t {}", c_variable("b"))),
        "{generated}"
    );
    assert!(
        generated.contains(&format!("uint32_t {}", c_variable("c"))),
        "{generated}"
    );
    assert!(
        generated.contains(&format!("uint64_t {}", c_variable("d"))),
        "{generated}"
    );
    assert!(
        generated.contains(&format!("size_t {}", c_variable("e"))),
        "{generated}"
    );
    assert!(
        generated.contains(&format!("ptrdiff_t {}", c_variable("f"))),
        "{generated}"
    );
}

#[test]
fn c_backend_preserves_u64_max_literal() {
    let module = lower("fun max() -> u64 { 18446744073709551615u64 }");
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(
        generated.contains("UINT64_C(18446744073709551615)"),
        "{generated}"
    );
}

#[test]
fn c_backend_preserves_i64_min_literal() {
    let module = lower("fun min() -> i64 { -9223372036854775808i64 }");
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(generated.contains("INT64_MIN"), "{generated}");
}

#[test]
fn c_backend_emits_unicode_char_as_u32_code_point() {
    let module = lower("fun ideograph() -> char { '\u{4e2d}' }");
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(
        generated.contains(&format!("uint32_t {}", c_function("ideograph"))),
        "{generated}"
    );
    assert!(generated.contains("UINT32_C(20013)"), "{generated}");
    assert!(!generated.contains("'\u{4e2d}'"), "{generated}");
}

#[test]
fn c_backend_casts_char_to_u32_code_point() {
    let module = lower("fun code_point() -> u32 { '\u{4e2d}' as u32 }");
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(
        generated.contains(&format!("uint32_t {}", c_function("code_point"))),
        "{generated}"
    );
    assert!(generated.contains("UINT32_C(20013)"), "{generated}");
}

#[test]
fn c_backend_casts_u8_to_char_code_point() {
    let module = lower("fun code_point() -> char { 65u8 as char }");
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(
        generated.contains(&format!("uint32_t {}", c_function("code_point"))),
        "{generated}"
    );
    assert!(
        generated.contains("((uint32_t)((uint8_t)UINT64_C(65)))"),
        "{generated}"
    );
}

#[test]
fn c_arithmetic() {
    let module = lower(
        r"
        fun add(a: i32, b: i32) -> i32 {
            return a + b;
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(result.contains('+'), "missing +: {result}");
}

#[test]
fn c_compound_assignment_uses_updated_value() {
    let module = lower(
        r"
        fun main() {
            let mut n: i32 = 1;
            n += 2;
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(result.contains('+'), "missing compound add:\n{result}");
    assert!(
        !result.contains("= 0;"),
        "compound assignment should not lower to zero:\n{result}"
    );
}

#[test]
fn c_basic_blocks() {
    let module = lower(
        r"
        fun choose(flag: bool) -> i32 {
            if flag {
                return 1;
            }
            return 0;
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(result.contains("if"), "missing if: {result}");
    assert!(result.contains("goto"), "missing goto: {result}");
}

#[test]
fn c_comparison() {
    let module = lower(
        r"
        fun lt(a: i32, b: i32) -> bool {
            return a < b;
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(result.contains("bool"), "missing bool type: {result}");
    assert!(result.contains('<'), "missing <: {result}");
}

#[test]
fn c_function_call() {
    let module = lower(
        r"
        fun square(n: i32) -> i32 {
            return n * n;
        }

        fun main() -> i32 {
            return square(5);
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(
        result.contains(&c_function("square")),
        "missing callee: {result}"
    );
}

#[test]
fn c_static_impl_method_call_uses_mangled_name() {
    let module = lower(
        r"
        struct Point {
            x: i32,
        }

        impl Point {
            fun new(x: i32) -> Point {
                Point { x }
            }
        }

        fun main() -> i32 {
            let p = Point::new(1);
            return p.x;
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();

    assert!(
        result.contains(&c_function("new__Point")),
        "static impl method should be mangled:\n{result}"
    );
    assert!(
        !result.contains(" new("),
        "static impl method call used bare name:\n{result}"
    );
}

#[test]
fn c_heap_alloc() {
    let module = lower(
        r"
        struct Data { value: i32 }

        fun escape() -> &Data {
            let local = Data { value: 1 };
            return &local;
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    // GC promotion: escaping local -> runtime allocation ABI.
    assert!(result.contains("rgc_alloc"), "missing rgc_alloc: {result}");
    assert!(
        result.contains("void rgc_collect(void)"),
        "missing runtime ABI declaration: {result}"
    );
    assert!(
        !result.contains("struct RgcHeader")
            && !result.contains("GC_MALLOC")
            && !result.contains("#include <gc.h>"),
        "runtime implementation should not be emitted by the backend: {result}"
    );
}

#[test]
fn c_backend_no_gc_uses_the_owned_allocator_without_gc_symbols() {
    let module = lower(
        r"
        struct Data { value: i32 }

        fun escape() -> &Data {
            let local = Data { value: 1 };
            &local
        }

        fun main() { escape(); }
        ",
    );
    let generated = CBackend::without_gc().compile(&module).unwrap();

    assert!(generated.contains("riddle_alloc"), "{generated}");
    assert!(!generated.contains("rgc_"), "{generated}");
    assert!(!generated.contains("RgcHeader"), "{generated}");
    assert!(!generated.contains("stack_anchor"), "{generated}");
}

#[test]
fn c_backend_no_gc_maps_the_standard_allocator_abi() {
    let module = lower(
        r#"
        unsafe extern "C" {
            safe fun rgc_realloc(data: *mut u8, size: usize) -> *mut u8;
            safe fun rgc_free(data: *mut u8);
        }

        fun main() {
            let data = rgc_realloc(0usize as *mut u8, 16usize);
            rgc_free(data);
        }
        "#,
    );
    let generated = CBackend::without_gc().compile(&module).unwrap();

    assert!(generated.contains("riddle_realloc"), "{generated}");
    assert!(generated.contains("riddle_free"), "{generated}");
    assert!(!generated.contains("rgc_"), "{generated}");
}

#[test]
fn c_backend_no_gc_rejects_collector_symbols() {
    let module = lower(
        r#"
        unsafe extern "C" { safe fun rgc_collect(); }
        fun main() { rgc_collect(); }
        "#,
    );
    let error = CBackend::without_gc().compile(&module).unwrap_err();
    assert!(error.contains("GC is disabled"), "{error}");
}

#[test]
fn c_backend_heap_allocates_escaping_reference_temporaries() {
    let module = lower(
        r"
        fun nested() -> &&i32 {
            let value = 1;
            &&value
        }

        fun literal() -> &i32 { &2 }
        ",
    );
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(
        generated.contains("rgc_alloc(sizeof(int32_t*))"),
        "nested reference temporary was not heap allocated:\n{generated}"
    );
    assert_eq!(
        generated.matches("rgc_alloc(sizeof(int32_t))").count(),
        2,
        "both referenced i32 values must escape:\n{generated}"
    );
    assert!(!generated.contains("return ref_tmp"), "{generated}");
    assert!(!generated.contains("return (&ref_tmp"), "{generated}");
}

#[test]
fn c_heap_alloc_wraps_main_with_a_gc_stack_boundary() {
    let module = lower(
        r"
        struct Data { value: i32 }

        fun escape() -> &Data {
            let local = Data { value: 1 };
            &local
        }

        fun main() { escape(); }
        ",
    );
    let generated = CBackend::new().compile(&module).unwrap();

    let user_main = c_function("main");
    assert!(
        generated.contains(&format!("int {user_main} (void)")),
        "{generated}"
    );
    assert!(
        generated.contains(&format!(
            "int main(void) {{\n  int rgc_stack_anchor = 0;\n  rgc_init(&rgc_stack_anchor);\n  return (int){user_main}();"
        )),
        "missing GC entry wrapper: {generated}"
    );
}

#[test]
fn c_multiple_functions() {
    let module = lower(
        r"
        fun a() {}
        fun b() {}
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(
        result.contains(&format!(" {} (void)", c_function("a"))),
        "missing a: {result}"
    );
    assert!(
        result.contains(&format!(" {} (void)", c_function("b"))),
        "missing b: {result}"
    );
}

#[test]
fn c_backend_local_var_has_init_value() {
    let module = lower(
        r"
        fun main() -> i32 {
            let x = 42;
            return x;
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(
        result.contains("42"),
        "local should be initialized with 42, got:\n{result}"
    );
}

#[test]
fn c_backend_alloca_for_non_escaping_struct() {
    let module = lower(
        r"
        struct Point { x: i32, y: i32 }

        fun use_point() -> i32 {
            let p = Point { x: 1, y: 2 };
            return p.x;
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    // Non-escaping struct should stay on the stack and not pull in the GC runtime.
    assert!(
        !result.contains("rgc_alloc"),
        "non-escaping struct should not use rgc_alloc, got:\n{result}"
    );
    assert!(result.contains("return"), "missing return");
}

#[test]
fn c_str_slice_return() {
    let module = lower(
        r#"
        fun hello() -> &str {
            return "world";
        }
        "#,
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(
        result.contains("struct { const char* ptr; size_t len; }"),
        "fat pointer struct not found:\n{result}"
    );
    assert!(result.contains("world"), "missing string:\n{result}");
}

#[test]
fn c_raw_string_return_escapes_content() {
    let module = lower(
        r####"
        fun hello() -> &str {
            return r###"say "hi"
"###;
        }
        "####,
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();

    assert!(
        result.contains("(riddle_str){ \"say \\\"hi\\\"\\n\", 9 }"),
        "raw string not escaped as C string:\n{result}"
    );
}

#[test]
fn c_string_escape_length_uses_decoded_utf8_bytes() {
    let module = lower(
        r#"
        fun text() -> &str {
            "a\nb"
        }
        "#,
    );
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(
        generated.contains("(riddle_str){ \"a\\nb\", 3 }"),
        "{generated}"
    );
}

#[test]
fn c_source_names_use_prefixed_collision_free_encoding() {
    use mir::instr::Terminator;
    use mir::types::{IntTy, StructType, Type};

    let mut module = mir::Module::new("identifiers");
    for name in ["int", "a::b", "a_b"] {
        let mut function = mir::Function::new(name.into(), Type::Unit);
        function.set_terminator(function.entry, Terminator::Return(None));
        module.add_function(function);
    }
    let mut typed = mir::Function::new("switch".into(), Type::Unit);
    typed.add_param(
        "auto".into(),
        Type::Struct(StructType {
            name: "union".into(),
            fields: vec![("case".into(), Type::Int(IntTy::I32))],
        }),
    );
    typed.set_terminator(typed.entry, Terminator::Return(None));
    module.add_function(typed);
    let generated = CBackend::new().compile(&module).unwrap();

    for encoded in [
        "riddle_f_696e74",
        "riddle_f_613a3a62",
        "riddle_f_615f62",
        "riddle_f_737769746368",
        "riddle_t_756e696f6e",
        "riddle_v_6175746f",
        "riddle_m_63617365",
    ] {
        assert!(generated.contains(encoded), "{encoded}:\n{generated}");
    }
    assert!(!generated.contains("void int("), "{generated}");
}

#[test]
fn c_str_slice_let() {
    let module = lower(
        r#"
        fun show(s: &str) { }
        fun main() {
            let s: &str = "hello";
            show(s);
        }
        "#,
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(
        result.contains("struct { const char* ptr; size_t len; }"),
        "fat pointer struct not found for &str local:\n{result}"
    );
    assert!(result.contains("hello"), "missing string:\n{result}");
}

#[test]
fn c_backend_preserves_returning_branches_and_mut_locals() {
    let module = lower(
        r"
        fun starts_a(ch: char) -> bool {
            if ch == 'a' { return true; }
            false
        }

        fun main() {
            let mut go: bool = true;
            while go {
                go = false;
            }
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(
        result.contains("UINT32_C(97)"),
        "char literal lowered wrong:\n{result}"
    );
    assert!(!result.contains("if ()"), "empty condition:\n{result}");
    assert!(
        !result.contains("if (("),
        "condition has redundant parentheses:\n{result}"
    );
    assert!(
        !result
            .lines()
            .any(|line| line.starts_with("block_") && line.ends_with(':')),
        "block label is not followed by a C11 statement:\n{result}"
    );
    assert!(!result.contains("= ;"), "empty assignment rhs:\n{result}");
    assert!(
        !result.contains("0 = false"),
        "unit fallback used as lvalue:\n{result}"
    );
}

#[test]
fn c_backend_assigns_if_phi_inputs_before_branching() {
    let module = lower(
        r"
        fun choose(flag: bool) -> bool {
            if flag { true } else { false }
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(result.contains("phi"), "missing phi variable:\n{result}");
    assert!(
        result.contains(" = true;") && result.contains(" = false;"),
        "phi inputs should be assigned on predecessor edges:\n{result}"
    );
}

#[test]
fn c_backend_emits_string_externs() {
    let module = lower(
        r#"
        unsafe extern "C" {
            safe fun str_len(s: &str) -> usize;
            fun str_byte(s: &str, idx: usize) -> u8;
        }

        fun main() -> u8 {
            let s: &str = "abc";
            let _len = str_len(s);
            return unsafe { str_byte(s, 1usize) };
        }
        "#,
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(
        result.contains("extern size_t str_len(const char*, size_t)"),
        "{result}"
    );
    assert!(
        result.contains("extern uint8_t str_byte(const char*, size_t, size_t)"),
        "{result}"
    );
    assert!(!result.contains("static inline size_t str_len"), "{result}");
    assert!(
        !result.contains("static inline uint8_t str_byte"),
        "{result}"
    );
}

#[test]
fn c_backend_wraps_string_extern_returns() {
    let module = lower(
        r#"
        unsafe extern "C" {
            fun greeting() -> &str;
        }

        fun hello() -> &str {
            return unsafe { greeting() };
        }
        "#,
    );
    let result = CBackend::new().compile(&module).unwrap();
    assert!(
        result.contains("extern const char* greeting(void);")
            && result.contains("const char* ffi_str")
            && result.contains("(riddle_str){ ffi_str"),
        "extern string return was not wrapped:\n{result}"
    );
}

#[test]
fn c_backend_separates_defined_extern_string_abi_from_imports() {
    let module = lower(
        r#"
        extern "C" fun echo(value: &str) -> &str {
            value
        }

        fun call_echo() -> &str {
            echo("hello")
        }
        "#,
    );
    assert!(module.externs.iter().all(|ext| ext.name != "echo"));

    let result = CBackend::new().compile(&module).unwrap();
    assert!(
        result.contains("riddle_str echo(riddle_str p0);")
            && !result.contains("extern const char* echo")
            && !result.contains("const char* ffi_str"),
        "defined extern string function used the import ABI:\n{result}"
    );
}

#[test]
fn c_backend_compares_string_pattern_by_contents() {
    let module = lower(
        r#"
        fun is_hello(value: &str) -> bool {
            match value {
                "hello" => true,
                _ => false,
            }
        }
        "#,
    );
    let result = CBackend::new().compile(&module).unwrap();
    assert!(
        result.contains("memcmp("),
        "string comparison missing:\n{result}"
    );
}

#[test]
fn c_backend_assigns_struct_field_with_associated_type_cast() {
    let module = lower(
        r"
        struct Foo {
            x: i32,
            y: i64,
        }

        trait Bar {
            type X;
        }

        impl Bar for Foo {
            type X = i32;
        }

        fun main() {
            let mut q = Foo { x: 10, y: 20 };
            let r = 10 as Foo::X;
            q.x = r;
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(
        result.contains(&format!(".{} =", c_member("x"))),
        "field store should use .x:\n{result}"
    );
    assert!(
        !result.contains(&format!(".{}", c_member("f0"))),
        "field name fallback leaked:\n{result}"
    );
    assert!(
        !result.contains("((void)"),
        "associated type cast lowered to void:\n{result}"
    );
}

#[test]
fn c_backend_monomorphizes_generic_structs() {
    let module = lower(
        r"
        struct Box<T> {
            value: T,
        }

        fun main() {
            let a: Box<i32> = Box { value: 1 };
            let b: Box<bool> = Box { value: true };
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(
        result.contains(&format!("struct {} {{", c_type("Box_i32"))),
        "missing i32 monomorph:\n{result}"
    );
    assert!(
        result.contains(&format!("struct {} {{", c_type("Box_bool"))),
        "missing bool monomorph:\n{result}"
    );
    assert!(
        result.contains(&format!("int32_t {};", c_member("value")))
            && result.contains(&format!("bool {};", c_member("value"))),
        "field types were not substituted:\n{result}"
    );
}

#[test]
fn c_backend_accepts_nested_generic_type_args_without_spaces() {
    let module = lower(
        r"
        struct Box<T> {
            value: T,
        }

        impl<T> Box<T> {
            fun get(&self) -> T {
                self.value
            }
        }

        fun main() {
            let b: Box<Box<i32>> = Box { value: Box { value: 1 } };
            let n = b.value.get();
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(
        result.contains(&c_type("Box_Box_i32")),
        "missing nested monomorph:\n{result}"
    );
    assert!(
        result.contains(&c_function("get__Box_i32")),
        "missing monomorphized generic method:\n{result}"
    );
    assert!(
        !result.contains("0.f0"),
        "method receiver lowering lost outer function state:\n{result}"
    );
}

#[test]
fn c_backend_monomorphizes_generic_functions() {
    let module = lower(
        r"
        fun id<T>(value: T) -> T {
            value
        }

        fun main() -> i32 {
            let a = id(1);
            let b = id(true);
            return a;
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(
        result.contains(&c_function("id__i32")),
        "missing i32 instance:\n{result}"
    );
    assert!(
        result.contains(&c_function("id__bool")),
        "missing bool instance:\n{result}"
    );
    assert!(
        !result.contains(" id ("),
        "generic template should not be emitted directly:\n{result}"
    );
}

#[test]
fn c_backend_monomorphizes_explicit_generic_method_arguments() {
    let module = lower(
        r"
        struct Helper {}

        impl Helper {
            fun id<T>(&self, value: T) -> T {
                value
            }
        }

        fun main() -> i32 {
            let helper = Helper {};
            helper.id::<i32>(1)
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();

    assert!(
        result.contains(&c_function("id__Helper_i32")),
        "missing explicit generic method instance:\n{result}"
    );
}

#[test]
fn c_backend_separates_shadowed_impl_and_method_generics() {
    let module = lower(
        r"
        struct C<T> {
            value: T,
        }

        impl<T> C<T> {
            fun test<T>(&self, value: T) -> T {
                value
            }
        }

        fun main() -> i32 {
            let c = C { value: true };
            c.test(1)
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();

    assert!(
        result.contains(&c_function("test__C_bool_i32")),
        "impl and method generic arguments were conflated:\n{result}"
    );
}

#[test]
fn c_backend_dispatches_trait_bound_method_in_generic_function() {
    let module = lower(
        r"
        trait Named {
            fun name(&self) -> i32;
        }

        trait Tagged {
            fun tag(&self) -> i32;
        }

        struct User {
            id: i32,
            tag_value: i32,
        }

        impl Named for User {
            fun name(&self) -> i32 {
                self.id
            }
        }

        impl Tagged for User {
            fun tag(&self) -> i32 {
                self.tag_value
            }
        }

        fun read<T: Named + Tagged>(value: T) -> i32 {
            value.name() + value.tag()
        }

        fun main() -> i32 {
            let user = User { id: 7, tag_value: 2 };
            return read(user);
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(
        result.contains(&c_function("read__User")),
        "missing generic function monomorph:\n{result}"
    );
    assert!(
        result.contains(&format!("{}(", c_function("name__User"))),
        "generic body should call concrete Named impl method:\n{result}"
    );
    assert!(
        result.contains(&format!("{}(", c_function("tag__User"))),
        "generic body should call concrete Tagged impl method:\n{result}"
    );
}

#[test]
fn c_backend_dispatches_generic_raw_pointer_trait_impl() {
    let module = lower(
        r"
        trait Value {
            fun value(&self) -> i32;
        }

        impl<T> Value for *mut T {
            fun value(&self) -> i32 { 7 }
        }

        fun read<T: Value>(value: T) -> i32 {
            value.value()
        }

        fun main() -> i32 {
            let pointer = 0usize as *mut i32;
            read(pointer)
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();

    assert!(
        result.contains(&c_function("value__ptr_i32")),
        "missing raw pointer impl monomorph:\n{result}"
    );
    assert!(
        result.contains(&format!("{}(", c_function("value__ptr_i32"))),
        "generic body should call the concrete raw pointer impl:\n{result}"
    );
}

#[test]
fn c_backend_uses_trait_default_method_unless_overridden() {
    let module = lower(
        r"
        trait Value {
            fun base(&self) -> i32;

            fun value(&self) -> i32 {
                self.base() + 1
            }
        }

        struct Defaulted {}
        struct Overridden {}

        impl Value for Defaulted {
            fun base(&self) -> i32 {
                6
            }
        }
        impl Value for Overridden {
            fun base(&self) -> i32 {
                0
            }

            fun value(&self) -> i32 {
                9
            }
        }

        fun main() -> i32 {
            let defaulted = Defaulted {};
            let overridden = Overridden {};
            defaulted.value() + overridden.value()
        }
        ",
    );
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(
        generated.contains(&format!("{}(", c_function("value__Defaulted"))),
        "{generated}"
    );
    assert!(
        generated.contains(&format!("{}(", c_function("value__Overridden"))),
        "{generated}"
    );
}

#[test]
fn c_backend_lowers_non_copy_array_into_iterator() {
    let module = lower(
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
            for item in [Token { value: 1 }, Token { value: 2 }] {
                let next = item.value + 1;
            }
        }
        ",
    );
    let mut backend = CBackend::new();
    let result = backend.compile(&module).unwrap();
    assert!(
        result.contains(&c_function("into_iter__arr2_Token")),
        "missing array IntoIterator monomorph:\n{result}"
    );
    assert!(
        result.contains(&c_function("next__ArrayIter_Token_2")),
        "missing ArrayIter::next monomorph:\n{result}"
    );
    assert!(
        result.contains(&format!("  {} s", c_type("ArrayIter_Token_2"))),
        "array iterator construction lost its const argument:\n{result}"
    );
    assert!(
        result.contains(&format!("{}((&", c_function("next__ArrayIter_Token_2"))),
        "Iterator::next should receive the iterator slot by reference:\n{result}"
    );
    assert!(
        result.contains(&format!("{} {}[2];", c_type("Token"), c_member("values"))),
        "array field should use C array declarator:\n{result}"
    );
    assert!(
        result.contains("memcpy("),
        "array field initialization should copy array storage:\n{result}"
    );
}

#[test]
fn c_backend_heap_allocates_an_escaping_array() {
    let module = lower(
        r"
        struct Data { value: i32 }

        fun index_ref() -> &Data {
            let items = [Data { value: 1 }, Data { value: 2 }];
            &items[0]
        }

        fun nested_index_ref() -> &Data {
            let items = [
                [Data { value: 3 }, Data { value: 4 }, Data { value: 5 }],
                [Data { value: 6 }, Data { value: 7 }, Data { value: 8 }],
            ];
            &items[1][2]
        }

        fun parameter_index_ref(items: [Data; 2]) -> &Data {
            &items[1]
        }

        fun copy_parameter(items: [Data; 2]) -> i32 {
            let mut copied = items;
            copied[0].value
        }

        struct Boxed { items: [Data; 2] }

        fun field_index_ref() -> &Data {
            let boxed = Boxed {
                items: [Data { value: 9 }, Data { value: 10 }],
            };
            &boxed.items[1]
        }

        struct Grid { items: [[Data; 3]; 2] }

        fun nested_field_index_ref() -> &Data {
            let grid = Grid {
                items: [
                    [Data { value: 11 }, Data { value: 12 }, Data { value: 13 }],
                    [Data { value: 14 }, Data { value: 15 }, Data { value: 16 }],
                ],
            };
            &grid.items[1][2]
        }
        ",
    );
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(
        generated.contains(&format!("rgc_alloc(sizeof({}[2]))", c_type("Data"))),
        "{generated}"
    );
    assert!(generated.contains("memcpy(h"), "{generated}");
    assert!(generated.contains("(&h"), "{generated}");
    assert!(
        generated.contains(&format!("rgc_alloc(sizeof({}[2][3]))", c_type("Data"))),
        "{generated}"
    );
    assert!(generated.contains("* 3)"), "{generated}");
    assert!(!generated.contains("[3]*"), "{generated}");
    assert!(
        generated.contains(&format!(
            ", {}, sizeof({}[2]))",
            c_variable("items"),
            c_type("Data")
        )),
        "{generated}"
    );
    assert!(
        !generated.contains(&format!("sizeof({})", c_variable("items"))),
        "{generated}"
    );
    assert!(
        !generated.contains(&format!("&{}", c_variable("items"))),
        "{generated}"
    );
    assert!(
        generated.contains(&format!("->{}[0]", c_member("items"))),
        "{generated}"
    );
    assert!(
        generated.contains(&format!("->{}[0][0]", c_member("items"))),
        "{generated}"
    );
}

#[test]
fn c_backend_inlines_checked_const_values() {
    let (_, type_result, analysis, module) = compile(
        r"
        const ANSWER: i32 = 40 + 2;

        fun answer() -> i32 {
            ANSWER
        }
        ",
    );
    assert_eq!(type_result.diagnostics, vec![]);
    assert_eq!(analysis.diagnostics, vec![]);

    let generated = CBackend::new().compile(&module).unwrap();
    assert!(
        generated.contains("40") && generated.contains('2'),
        "{generated}"
    );
    assert!(!generated.contains("return 0;"), "{generated}");
}

#[test]
fn c_backend_checks_safe_array_and_slice_indexes() {
    let (_, type_result, analysis, module) = compile(
        r"
        fun array_get(values: [i32; 2], index: usize) -> i32 {
            values[index]
        }

        fun slice_get(values: &[i32], index: usize) -> i32 {
            values[index]
        }

        fun generic_get<const N: usize>(values: [i32; N], index: usize) -> i32 {
            values[index]
        }

        fun instantiate_generic_get() -> i32 {
            generic_get([10, 20], 99usize)
        }

        fun raw_get(values: *const i32, index: usize) -> i32 {
            unsafe { values[index] }
        }
        ",
    );
    assert_eq!(type_result.diagnostics, vec![]);
    assert_eq!(analysis.diagnostics, vec![]);

    let generated = CBackend::new().compile(&module).unwrap();
    assert_eq!(
        generated.matches("index out of bounds").count(),
        3,
        "{generated}"
    );
}

#[test]
fn c_backend_uses_strict_c11_representations_for_zero_sized_values() {
    let (_, type_result, analysis, module) = compile(
        r"
        struct Empty {}

        fun take_unit(value: ()) {}

        fun main() {
            let empty = Empty {};
            let values: [i32; 0] = [];
            let unit = ();
            let units = [(), ()];
            take_unit(unit);
        }
        ",
    );
    assert_eq!(type_result.diagnostics, vec![]);
    assert_eq!(analysis.diagnostics, vec![]);

    let generated = CBackend::new().compile(&module).unwrap();
    assert!(
        generated.contains("unsigned char _riddle_zst;"),
        "{generated}"
    );
    assert!(!generated.contains("[0]"), "{generated}");
    assert!(!generated.contains("{  }"), "{generated}");
    assert!(
        generated.contains("typedef unsigned char riddle_unit;"),
        "{generated}"
    );
    assert!(
        generated.contains(&format!(
            "void {}(riddle_unit p0);",
            c_function("take_unit")
        )),
        "{generated}"
    );
}

#[test]
fn c_backend_defines_integer_and_float_cast_edge_semantics() {
    let (_, type_result, analysis, module) = compile(
        r"
        fun add(left: i32, right: i32) -> i32 { left + right }
        fun divide(left: i32, right: i32) -> i32 { left / right }
        fun shift(left: i32, right: i32) -> i32 { left << right }
        fun convert(value: f64) -> i32 { value as i32 }
        ",
    );
    assert_eq!(type_result.diagnostics, vec![]);
    assert_eq!(analysis.diagnostics, vec![]);

    let generated = CBackend::new().compile(&module).unwrap();
    assert!(generated.contains("(uint32_t)"), "{generated}");
    assert!(generated.contains("division by zero"), "{generated}");
    assert!(generated.contains("isnan("), "{generated}");
    assert!(generated.contains("INT32_MAX"), "{generated}");
}

#[test]
fn c_backend_numeric_semantics_compile_and_run_as_strict_c11() {
    let compiler = std::env::var_os("CC").unwrap_or_else(|| "cc".into());
    if !Command::new(&compiler)
        .arg("--version")
        .output()
        .is_ok_and(|output| output.status.success())
    {
        eprintln!("skipping strict C11 numeric test: no usable C compiler");
        return;
    }

    let (_, type_result, analysis, module) = compile(
        r"
        fun wrap(left: i32, right: i32) -> i32 { left + right }

        fun wrap8(left: u8, right: u8) -> u8 { left + right }

        fun shift(value: i32, count: i32) -> i32 { value >> count }

        fun cast(value: f64) -> i32 { value as i32 }

        fun main() -> i32 {
            let zero = 0.0f64;
            let wrapped = wrap(2147483647i32, 1i32);
            let wrapped8 = wrap8(255u8, 1u8);
            let shifted = shift(-4i32, 1i32);
            let nan = cast(zero / zero);
            if wrapped == -2147483648i32 && wrapped8 == 0u8 && shifted == -2i32 && nan == 0 {
                0
            } else {
                1
            }
        }
        ",
    );
    assert_eq!(type_result.diagnostics, vec![]);
    assert_eq!(analysis.diagnostics, vec![]);
    let generated = CBackend::new().compile(&module).unwrap();

    let root = std::env::temp_dir().join(format!(
        "riddle-c-numeric-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&root).unwrap();
    let source = root.join("main.c");
    let executable = root.join(if cfg!(windows) { "main.exe" } else { "main" });
    fs::write(&source, generated).unwrap();

    let compile_output = Command::new(&compiler)
        .args(["-std=c11", "-pedantic-errors", "-Wall", "-Werror"])
        .arg(&source)
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        compile_output.status.success(),
        "C11 compile failed:\n{}",
        String::from_utf8_lossy(&compile_output.stderr)
    );
    let run = Command::new(&executable).output().unwrap();
    assert!(
        run.status.success(),
        "native program exited with {}",
        run.status
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn c_closure_environment_stores_only_the_captured_field() {
    let module = lower(
        r"
        struct First { value: i32 }
        struct Second { value: i32 }
        struct Pair { first: First, second: Second }
        fun read(value: First) -> i32 { value.value }
        fun main() -> i32 {
            let pair = Pair {
                first: First { value: 1 },
                second: Second { value: 2 },
            };
            let take_first = fun() { read(pair.first) };
            pair.second.value
        }
        ",
    );
    let generated = CBackend::new().compile(&module).unwrap();
    let member = c_member("capture_0_pair_first");
    let capture_line = generated
        .lines()
        .find(|line| line.contains(&member))
        .expect("missing projected capture field");

    assert!(capture_line.contains(&c_type("First")), "{capture_line}");
    assert!(!capture_line.contains(&c_type("Pair")), "{capture_line}");
}

#[test]
fn c_impl_fn_supports_closures_named_functions_and_opaque_returns() {
    let module = lower(
        r"
        fun apply(f: impl Fn(i32) -> i32, value: i32) -> i32 { f(value) }
        fun increment(value: i32) -> i32 { value + 1 }
        fun make(base: i32) -> impl Fn(i32) -> i32 {
            move fun(value: i32) { base + value }
        }
        fun main() -> i32 {
            apply(increment, 1) + apply(make(38), 2)
        }
        ",
    );
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(generated.contains(&c_function("apply__")), "{generated}");
    assert!(
        generated.contains(&c_function("__riddle_fn_adapter_increment")),
        "{generated}"
    );
    assert!(
        generated.contains(&c_function("__riddle_lambda_")),
        "{generated}"
    );
    assert!(!generated.contains("vtable"), "{generated}");
}

#[test]
fn c_generic_function_item_adapter_targets_one_instantiation() {
    let module = lower(
        r"
        fun identity<T>(value: T) -> T { value }
        fun apply(f: impl Fn(i32) -> i32, value: i32) -> i32 { f(value) }
        fun main() -> i32 { apply(identity, 42) }
        ",
    );
    let generated = CBackend::new().compile(&module).unwrap();

    assert!(
        generated.contains(&c_function("identity__i32")),
        "{generated}"
    );
    assert!(
        generated.contains(&c_function("__riddle_fn_adapter_identity__i32")),
        "{generated}"
    );
}
