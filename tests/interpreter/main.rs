//! Interpreter integration tests: compile Riddle sources through the same
//! entry point `riddle run` uses and execute the MIR with the in-process
//! interpreter — no C toolchain required. The scenarios mirror
//! `tests/mir/std_behavior.rs` so interpreter semantics stay aligned with the
//! C backend.

use interpreter::{self, Config};
use riddlec::pipeline;

/// Compiles `source` and interprets `main`. Returns the exit code (or trap
/// debug form), captured stdout, and captured stderr including trap output.
fn run(source: &str) -> (Result<i32, String>, String, String) {
    let result = pipeline::compile_for_interpretation(source, "test.rid");
    assert!(
        result.success(),
        "riddle diagnostics: {:#?}
hir: {:#?}
macro: {:#?}
parse: {:#?}
analysis: {:#?}",
        result.type_result.diagnostics,
        result.hir_diagnostics,
        result.macro_diagnostics,
        result.parse_errors,
        result.analysis_diagnostics
    );
    let module = result.mir_module.expect("successful compile produced MIR");
    let config = Config {
        rng_seed: 0x5EED,
        ..Config::default()
    };
    let outcome = interpreter::run_with(&module, result.source_files, config);
    let mut stderr = String::from_utf8_lossy(&outcome.stderr).into_owned();
    if let Err(trap) = &outcome.result {
        let mut rendered = Vec::new();
        trap.render(&mut rendered);
        if !rendered.is_empty() {
            stderr.push_str(&String::from_utf8_lossy(&rendered));
        }
    }
    let code = outcome.result.clone().map_err(|trap| format!("{trap:?}"));
    (
        code,
        String::from_utf8_lossy(&outcome.stdout).into_owned(),
        stderr,
    )
}

fn assert_ok(source: &str, expected_stdout: &str) {
    let (code, stdout, stderr) = run(source);
    assert_eq!(code, Ok(0), "stdout: {stdout}stderr: {stderr}");
    assert_eq!(stdout, expected_stdout, "stderr: {stderr}");
}

#[test]
fn interpreter_runs_hello_world() {
    assert_ok(
        r#"
        fun main() -> i32 {
            println!("hello, world");
            0
        }
        "#,
        "hello, world\n",
    );
}

#[test]
fn interpreter_format_placeholders() {
    assert_ok(
        r#"
        fun main() -> i32 {
            let name = "riddle";
            let version = 42i32;
            println!("hello {name} v{version}!");
            println!("{0} then {1} then {0} and {}", 1i32, 2i32, 3i32);
            println!("debug: {version:?} hex {}", 7i32);
            println!("{}", format!("{}+{}={}", 1i32, 2i32, 3i32).as_str());
            0
        }
        "#,
        "hello riddle v42!\n1 then 2 then 1 and 1\ndebug: 42 hex 7\n1+2=3\n",
    );
}

#[test]
fn interpreter_integer_arithmetic_matches_c_semantics() {
    assert_ok(
        r#"
        fun main() -> i32 {
            println!("{}", 7i32 / 2i32);
            println!("{}", -7i32 / 2i32);
            println!("{}", -7i32 % 3i32);
            println!("{}", 1i64 << 40);
            println!("{}", -8i32 >> 1);
            println!("{}", 255u8 + 1u8);
            println!("{}", -128i8);
            0
        }
        "#,
        "3\n-3\n-1\n1099511627776\n-4\n0\n-128\n",
    );
}

#[test]
fn interpreter_float_arithmetic_rounds_through_f32() {
    assert_ok(
        r#"
        fun main() -> i32 {
            println!("{}", 3.5f64 + 1.5f64);
            println!("{}", 0.1f32 + 0.2f32);
            println!("{}", 2.5f64 * 4.0f64);
            0
        }
        "#,
        "5.000000\n0.300000\n10.000000\n",
    );
}

#[test]
fn interpreter_division_by_zero_aborts() {
    let (code, _stdout, stderr) = run("fun main() -> i32 { let x = 1i32 / 0i32; x }");
    assert!(
        code.as_ref().unwrap_err().contains("division by zero"),
        "code: {code:?} stderr: {stderr}"
    );
    assert!(
        stderr.contains("riddle: division by zero"),
        "stderr: {stderr}"
    );
}

#[test]
fn interpreter_signed_division_overflow_aborts() {
    let source = r#"
        fun main() -> i32 {
            let min = -9223372036854775808i64;
            let quotient = min / -1i64;
            quotient as i32
        }
        "#;
    let (code, _stdout, stderr) = run(source);
    assert!(
        code.as_ref()
            .unwrap_err()
            .contains("integer division overflow"),
        "code: {code:?}"
    );
    assert!(
        stderr.contains("riddle: integer division overflow"),
        "stderr: {stderr}"
    );
}

#[test]
fn interpreter_panic_reports_location() {
    let (code, _stdout, stderr) = run(r#"fun main() -> i32 { crate::std::panic::panic("boom") }"#);
    assert!(code.is_err());
    assert!(
        stderr.starts_with("thread 'main' panicked at "),
        "stderr: {stderr}"
    );
    assert!(stderr.ends_with(":\nboom\n"), "stderr: {stderr}");
}

#[test]
fn interpreter_panic_inside_std_reports_std_region() {
    // `Option::unwrap` panics inside the bundled std, whose code lives in
    // the region appended after the user source; the site must resolve to
    // the std segment instead of falling back to the module name.
    let source = r#"
        fun main() -> i32 {
            let value: crate::std::option::Option<i32> = crate::std::option::Option::None;
            value.unwrap();
            0
        }
    "#;
    let (_code, _stdout, stderr) = run(source);
    assert!(stderr.contains("at std:"), "stderr: {stderr}");
}

#[test]
fn interpreter_panic_after_macro_expansion_maps_to_user_source() {
    // `println!`/`assert!` rewrite the user source before lowering, so the
    // panic site's offset no longer matches the original text; the source
    // files must map it back to the user region.
    let source = r#"
        fun main() -> i32 {
            println!("before");
            assert!(1i32 == 2i32);
            0
        }
    "#;
    let (_code, stdout, stderr) = run(source);
    assert_eq!(stdout, "before\n", "stderr: {stderr}");
    assert!(stderr.contains("assertion failed"), "stderr: {stderr}");
    // The site must be the `assert!` call in the user region — the file name the
    // runner was given, as `riddle run` passes it — not the std region or the
    // fallback module name.
    assert!(stderr.contains("at test.rid:"), "stderr: {stderr}");
    assert!(!stderr.contains("at std:"), "stderr: {stderr}");
}

#[test]
fn interpreter_index_out_of_bounds_aborts() {
    let source = r#"
        fun main() -> i32 {
            let values = [1i32, 2i32, 3i32];
            println!("{}", values[3usize]);
            0
        }
        "#;
    let (code, _stdout, stderr) = run(source);
    assert!(
        code.as_ref().unwrap_err().contains("index out of bounds"),
        "code: {code:?}"
    );
    assert!(
        stderr.contains("riddle: index out of bounds"),
        "stderr: {stderr}"
    );
}

#[test]
fn interpreter_indexes_through_array_references() {
    // Indexing a `&[T; N]` must stride by the element size, not the whole
    // array's size, and keep the bounds check on the array's length.
    assert_ok(
        r#"
        struct Pair { a: [i32; 2] }

        fun main() -> i32 {
            let values = [5i32, 6i32, 7i32];
            let view = &values;
            let mut total = 0i32;
            let mut index = 0usize;
            while index < 3usize {
                total += view[index];
                index += 1usize;
            }
            if total != 18i32 { return 1; }
            if view[2usize] != 7i32 { return 2; }

            let pair = Pair { a: [1i32, 2i32] };
            let inner = &pair.a;
            if inner[0usize] + inner[1usize] != 3i32 { return 3; }
            0
        }
        "#,
        "",
    );

    let out_of_bounds = r#"
        fun main() -> i32 {
            let values = [1i32, 2i32];
            let view = &values;
            println!("{}", view[2usize]);
            0
        }
        "#;
    let (code, _stdout, stderr) = run(out_of_bounds);
    assert!(
        code.as_ref().unwrap_err().contains("index out of bounds"),
        "code: {code:?}"
    );
    assert!(
        stderr.contains("riddle: index out of bounds"),
        "stderr: {stderr}"
    );
}

#[test]
fn interpreter_float_to_int_cast_saturates() {
    assert_ok(
        r#"
        fun main() -> i32 {
            println!("{}", 1e300 as i32);
            println!("{}", -1e300 as i32);
            println!("{}", (0.0 / 0.0) as i32);
            println!("{}", 2.9 as i32);
            0
        }
        "#,
        "2147483647\n-2147483648\n0\n2\n",
    );
}

#[test]
fn interpreter_recursion_and_match_guards() {
    assert_ok(
        r#"
        fun fib(n: i32) -> i32 {
            if n < 2i32 { return n; }
            fib(n - 1i32) + fib(n - 2i32)
        }

        fun classify(n: i32) -> &str {
            match n {
                0 => "zero",
                2 => "small",
                n if n < 10 => "medium",
                _ => "large",
            }
        }

        fun main() -> i32 {
            println!("{}", fib(10));
            println!("{}", classify(0));
            println!("{}", classify(2));
            println!("{}", classify(7));
            println!("{}", classify(100));
            0
        }
        "#,
        "55\nzero\nsmall\nmedium\nlarge\n",
    );
}

#[test]
fn interpreter_deep_recursion_is_supported() {
    assert_ok(
        r#"
        fun count(n: i64) -> i64 {
            if n == 0i64 { return 0i64; }
            count(n - 1i64)
        }

        fun main() -> i32 {
            println!("{}", count(20000i64));
            0
        }
        "#,
        "0\n",
    );
}

#[test]
fn interpreter_bracket_lambdas_run() {
    // Ported from tests/mir/std_behavior.rs `std_bracket_lambdas_run`.
    let source = r#"
        use crate::std::iter::{Iterator, IntoIterator};

        struct Counter {
            index: usize,
            limit: usize,
        }

        impl Iterator for Counter {
            type Item = i32;

            fun next(&mut self) -> Option<i32> {
                if self.index < self.limit {
                    self.index += 1usize;
                    Option::Some(self.index as i32)
                } else {
                    Option::None
                }
            }
        }

        fun invoke(action: impl Fn() -> i32) -> i32 { action() }

        fun main() -> i32 {
            let sum = Counter { index: 0usize, limit: 5usize }
                .fold(0i32, [acc, v -> acc + v]);
            if sum != 15i32 { return 1; }

            let mut counter2 = Counter { index: 0usize, limit: 5usize };
            if counter2.find([it -> *it == 4i32]).unwrap_or(0i32) != 4i32 { return 2; }

            let chained = Counter { index: 0usize, limit: 5usize }
                .map [v -> v + 1i32]
                .filter [it -> *it > 3i32];
            let mut total = 0i32;
            for value in chained {
                total += value;
            }
            if total != 15i32 { return 3; }

            let base = 10i32;
            let offset = move [ -> base + 5i32];
            if invoke(offset) != 15i32 { return 4; }

            let double = [it -> it * 2i32];
            if double(21i32) != 42i32 { return 5; }

            let mut count = 0i32;
            let mut bump = [ -> { count += 1i32; count }];
            bump();
            bump();
            if count != 2i32 { return 6; }

            0
        }
        "#;
    assert_ok(source, "");
}

#[test]
fn interpreter_question_operator_supports_from_and_option() {
    // Ported from tests/mir/std_behavior.rs.
    let source = r#"
        use crate::std::convert::From;
        use crate::std::option::Option;
        use crate::std::result::Result;

        enum ParseError {
            Empty,
        }

        enum AppError {
            Wrapped(ParseError),
        }

        impl From<ParseError> for AppError {
            fun from(value: ParseError) -> AppError {
                AppError::Wrapped(value)
            }
        }

        fun parse(flag: bool) -> Result<i32, ParseError> {
            if flag {
                Result::Ok(40i32)
            } else {
                Result::Err(ParseError::Empty)
            }
        }

        fun run(flag: bool) -> Result<i32, AppError> {
            let value = parse(flag)?;
            Result::Ok(value + 2i32)
        }

        fun find(flag: bool) -> Option<i32> {
            if flag {
                Option::Some(8i32)
            } else {
                Option::None
            }
        }

        fun run_option(flag: bool) -> Option<i32> {
            let value = find(flag)?;
            Option::Some(value * 3i32)
        }

        fun main() -> i32 {
            let a = run(true).unwrap_or(0i32);
            let b = run_option(true).unwrap_or(0i32);
            if a == 42i32 && b == 24i32 {
                0
            } else {
                1
            }
        }
        "#;
    assert_ok(source, "");
}

#[test]
fn interpreter_vector_operations_roundtrip() {
    // Ported from tests/mir/std_behavior.rs
    // `vector_insert_remove_sort_contains_retain_roundtrip`.
    let source = r#"
        use crate::std::vector::Vector;

        fun main() -> i32 {
            let mut v = Vector::new();
            v.push(3i32);
            v.push(1i32);
            v.insert(1usize, 2i32);
            if *v.get(0usize).unwrap_or(&0) != 3i32 { return 1; }
            if *v.get(1usize).unwrap_or(&0) != 2i32 { return 2; }
            if *v.get(2usize).unwrap_or(&0) != 1i32 { return 3; }
            v.sort();
            if *v.get(0usize).unwrap_or(&0) != 1i32 { return 4; }
            if *v.get(2usize).unwrap_or(&0) != 3i32 { return 5; }
            if !v.contains(&2i32) { return 6; }
            let removed = v.remove(0usize);
            if removed != 1i32 || v.len() != 2usize { return 7; }
            v.retain([x -> *x >= 2i32]);
            if v.len() != 2usize { return 8; }
            0
        }
        "#;
    assert_ok(source, "");
}

#[test]
fn interpreter_hash_map_get_or_insert_counts_once() {
    // Ported from tests/mir/std_behavior.rs.
    let source = r#"
        use crate::std::collections::hash_map::HashMap;

        fun main() -> i32 {
            let mut counts = HashMap::new();
            let slot = counts.get_or_insert(7i32, 0i32);
            *slot += 1i32;
            let slot2 = counts.get_or_insert(7i32, 100i32);
            if *slot2 != 1i32 { return 1; }
            if !counts.contains_key(&7i32) { return 2; }
            if counts.len() != 1usize { return 3; }
            0
        }
        "#;
    assert_ok(source, "");
}

#[test]
fn interpreter_string_split_replace_and_ascii_case() {
    // Ported from tests/mir/std_behavior.rs
    // `string_split_replace_and_ascii_case_roundtrip`.
    let source = r#"
        use crate::std::string::String;

        fun main() -> i32 {
            let csv = String::from_str("alpha,beta,,gamma");
            let parts = csv.split(",");
            if parts.len() != 4usize { return 1; }
            if parts.get(0usize).unwrap_or(&String::new()).as_str() != "alpha" { return 2; }
            if parts.get(2usize).unwrap_or(&String::new()).as_str() != "" { return 3; }
            if csv.replace(",", ";").as_str() != "alpha;beta;;gamma" { return 4; }
            if String::from_str("no-sep").replace(",", "x").as_str() != "no-sep" { return 5; }
            if String::from_str("MixEd123!").to_ascii_uppercase().as_str() != "MIXED123!" { return 6; }
            if String::from_str("MixEd123!").to_ascii_lowercase().as_str() != "mixed123!" { return 7; }
            0
        }
        "#;
    assert_ok(source, "");
}

#[test]
fn interpreter_fs_roundtrip_in_temp_dir() {
    // Ported from tests/mir/std_behavior.rs `std_fs_roundtrips_file_content`,
    // redirected into a unique temp directory so parallel test runs never
    // collide.
    let dir = std::env::temp_dir().join(format!(
        "riddle-interp-fs-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir_all(&dir).unwrap();
    let target = dir.join("riddle_fs_e2e.tmp");
    let target = target.display().to_string().replace('\\', "/");
    let source = format!(
        r#"
        use crate::std::fs::{{read_to_string, write, FsFile}};
        use crate::std::result::Result;

        fun main() -> i32 {{
            match write("{target}", "hello fs") {{
                Result::Ok(()) => {{}},
                Result::Err(_) => {{ return 1; }},
            }}
            let content = match read_to_string("{target}") {{
                Result::Ok(text) => text,
                Result::Err(_) => {{ return 2; }},
            }};
            if content.len() != 8usize {{
                return 3;
            }}
            match FsFile::open("{target}") {{
                Result::Ok(mut file) => {{
                    let mut buffer = [0u8; 16];
                    let read = file.read(&mut buffer).unwrap_or(0usize);
                    if read != 8usize {{
                        return 4;
                    }}
                    if buffer[0usize] != 104u8 {{
                        return 5;
                    }}
                }},
                Result::Err(_) => {{ return 6; }},
            }}
            0
        }}
        "#
    );
    assert_ok(&source, "");
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn interpreter_drops_run_deterministically() {
    assert_ok(
        r#"
        struct Guard { label: &str }

        impl crate::std::ops::Drop for Guard {
            fun drop(&mut self) {
                println!("drop {}", self.label);
            }
        }

        fun make(label: &str) -> Guard {
            Guard { label }
        }

        fun main() -> i32 {
            let outer = make("outer");
            {
                let inner = make("inner");
                println!("mid");
            }
            println!("end");
            crate::std::mem::drop(outer);
            0
        }
        "#,
        "mid\ndrop inner\nend\ndrop outer\n",
    );
}

#[test]
fn interpreter_moves_and_borrowed_access() {
    assert_ok(
        r#"
        struct Point { x: i32, y: i32 }

        fun magnitude(point: &Point) -> i32 {
            point.x * point.x + point.y * point.y
        }

        fun main() -> i32 {
            let mut point = Point { x: 3, y: 4 };
            point.x = -3;
            println!("{}", magnitude(&point));
            let other = point;
            println!("{} {}", other.x, other.y);
            0
        }
        "#,
        "25\n-3 4\n",
    );
}

#[test]
fn interpreter_dyn_trait_dispatch() {
    assert_ok(
        r#"
        trait Shape {
            fun area(&self) -> i32;
        }

        struct Square { side: i32 }
        struct Rect { width: i32, height: i32 }

        impl Shape for Square {
            fun area(&self) -> i32 { self.side * self.side }
        }

        impl Shape for Rect {
            fun area(&self) -> i32 { self.width * self.height }
        }

        fun describe(shape: &dyn Shape) -> &str {
            if shape.area() > 15 { "big" } else { "small" }
        }

        fun main() -> i32 {
            let square = Square { side: 4 };
            let rect = Rect { width: 3, height: 5 };
            println!("{}", square.area());
            println!("{}", describe(&square));
            println!("{}", describe(&rect));
            0
        }
        "#,
        "16\nbig\nsmall\n",
    );
}

#[test]
fn interpreter_generics_monomorphize_and_call() {
    assert_ok(
        r#"
        fun pick<T>(a: T, b: T) -> T
        where T: crate::std::cmp::PartialOrd {
            if a < b { a } else { b }
        }

        struct Pair<A, B> { first: A, second: B }

        impl<A, B> Pair<A, B> {
            fun swap(self) -> Pair<B, A> {
                Pair { first: self.second, second: self.first }
            }
        }

        fun main() -> i32 {
            println!("{}", pick(3i32, 7i32));
            println!("{}", pick(2.5f64, 1.5f64));
            let pair = Pair { first: 1i32, second: "two" };
            let swapped = pair.swap();
            println!("{}", swapped.first);
            0
        }
        "#,
        "3\n1.500000\ntwo\n",
    );
}

#[test]
fn interpreter_for_loops_over_ranges() {
    assert_ok(
        r#"
        fun main() -> i32 {
            let mut sum = 0i32;
            for i in 0..10 {
                sum += i;
            }
            for _ in 0..0 {
                sum += 100;
            }
            for i in 1..=3 {
                sum *= i;
            }
            println!("{}", sum);
            0
        }
        "#,
        "270\n",
    );
}

#[test]
fn interpreter_process_exit_code() {
    let (code, _stdout, _stderr) = run("fun main() -> i32 { crate::std::process::exit(7) }");
    assert!(
        code.as_ref().unwrap_err().contains("ProcessExit(7)"),
        "code: {code:?}"
    );
}

#[test]
fn interpreter_unknown_extern_is_reported() {
    let source = r#"
        unsafe extern "C" {
            fun mysterious_runtime_call(value: i32) -> i32;
        }

        fun main() -> i32 {
            unsafe { mysterious_runtime_call(1i32) }
        }
        "#;
    let (code, _stdout, stderr) = run(source);
    assert!(
        code.as_ref().unwrap_err().contains("UnsupportedExtern"),
        "code: {code:?}"
    );
    assert!(
        stderr.contains("interpreter does not support extern"),
        "stderr: {stderr}"
    );
}

#[test]
fn interpreter_bool_bitwise_ops_are_eager() {
    // `match` lowering folds pattern tests and guards into eager
    // `BitAnd`/`BitOr`/`BitXor` on `bool`, so the interpreter must evaluate
    // them; `&&`/`||` stay short-circuit control flow and must not.
    assert_ok(
        r#"
        fun main() -> i32 {
            let mut calls = 0i32;
            let mut bump = [ -> { calls += 1i32; true }];
            let t = true;
            let f = false;

            if t & f { return 1; }
            if !(t | f) { return 2; }
            if !(t ^ f) { return 3; }
            if t && f { return 4; }
            if !(t || f) { return 5; }

            if f && bump() { return 6; }
            if calls != 0i32 { return 7; }

            if f & bump() { return 8; }
            if calls != 1i32 { return 9; }
            0
        }
        "#,
        "",
    );
}

#[test]
fn interpreter_prints_uints_and_chars() {
    assert_ok(
        r#"
        fun main() -> i32 {
            println!("{}", 42u8);
            println!("{}", -1isize);
            println!("{}", 'r');
            println!("{}", true);
            0
        }
        "#,
        "42\n-1\nr\ntrue\n",
    );
}

#[test]
fn interpreter_hash_set_iterates_in_insertion_order() {
    assert_ok(
        r#"
        use std::collections::HashSet;

        fun main() -> i32 {
            let mut set: HashSet<i32> = HashSet::new();
            set.insert(3);
            set.insert(1);
            set.insert(4);
            set.insert(1);
            let mut out = 0usize;
            for value in &set {
                out = out * 10usize + (*value as usize);
            }
            println!("{}", out);
            0
        }
        "#,
        "314\n",
    );
}

#[test]
fn interpreter_tree_set_iterates_sorted() {
    assert_ok(
        r#"
        use std::collections::TreeSet;

        fun main() -> i32 {
            let mut set: TreeSet<i32> = TreeSet::new();
            set.insert(5);
            set.insert(1);
            set.insert(9);
            set.insert(3);
            let mut out = 0usize;
            for value in &set {
                out = out * 10usize + (*value as usize);
            }
            println!("{}", out);
            0
        }
        "#,
        "1359\n",
    );
}

#[test]
fn interpreter_tree_map_keys_values_and_range() {
    assert_ok(
        r#"
        use std::collections::TreeMap;

        fun main() -> i32 {
            let mut map: TreeMap<i32, i32> = TreeMap::new();
            map.insert(10, 1);
            map.insert(20, 2);
            map.insert(30, 3);
            map.insert(40, 4);
            map.insert(50, 5);

            let mut keys = 0usize;
            for key in map.keys() {
                keys = keys * 100usize + (*key as usize);
            }
            println!("keys={}", keys);

            let mut values = 0usize;
            for value in map.values() {
                values = values * 10usize + (*value as usize);
            }
            println!("values={}", values);

            let mut range_keys = 0usize;
            for (key, value) in map.range(&20, &50) {
                range_keys = range_keys * 100usize + (*key as usize);
                if *value != *key / 10 { return 1; }
            }
            println!("range={}", range_keys);

            let mut empty = 0usize;
            for (key, _) in map.range(&35, &40) {
                empty = empty * 100usize + (*key as usize);
            }
            println!("empty={}", empty);
            0
        }
        "#,
        "keys=1020304050\nvalues=12345\nrange=203040\nempty=0\n",
    );
}

#[test]
fn interpreter_vector_sorts_merge_sorted_and_stable() {
    assert_ok(
        r#"
        use std::cmp::{Ord, Ordering, PartialEq, PartialOrd};
        use std::option::Option;

        struct Pair { key: i32, tag: i32 }

        impl PartialEq for Pair {
            fun eq(&self, other: &Self) -> bool {
                self.key == other.key && self.tag == other.tag
            }
        }

        impl PartialOrd for Pair {
            fun partial_cmp(&self, other: &Self) -> Option<Ordering> {
                if self.key < other.key {
                    Option::Some(Ordering::Less)
                } else if self.key > other.key {
                    Option::Some(Ordering::Greater)
                } else {
                    Option::Some(Ordering::Equal)
                }
            }
        }

        fun main() -> i32 {
            let mut v: Vector<i32> = Vector::new();
            let mut seed = 7i32;
            let mut i = 0usize;
            while i < 100usize {
                // Deterministic LCG spread over positive and negative keys.
                seed = seed * 1103515245i32 + 12345i32;
                v.push(seed / 65536i32);
                i += 1usize;
            }
            v.sort();
            let mut j: usize = 1usize;
            while j < v.len() {
                if v[j - 1usize] > v[j] { return 1; }
                j += 1usize;
            }

            // Stability: equal keys keep insertion order (tags ascending
            // within equal keys in the order pushed).
            let mut pairs: Vector<Pair> = Vector::new();
            pairs.push(Pair { key: 1, tag: 1 });
            pairs.push(Pair { key: 0, tag: 2 });
            pairs.push(Pair { key: 1, tag: 3 });
            pairs.push(Pair { key: 0, tag: 4 });
            pairs.sort();
            if pairs[0].key != 0 || pairs[0].tag != 2 { return 2; }
            if pairs[1].key != 0 || pairs[1].tag != 4 { return 3; }
            if pairs[2].key != 1 || pairs[2].tag != 1 { return 4; }
            if pairs[3].key != 1 || pairs[3].tag != 3 { return 5; }
            println!("sorted");
            0
        }
        "#,
        "sorted\n",
    );
}

#[test]
fn interpreter_tuple_comparison_and_hash_dispatch() {
    assert_ok(
        r#"
        use std::collections::{HashMap, HashSet};

        fun main() -> i32 {
            // Direct method calls through the std tuple impls.
            let a = (1, 2);
            let b = (0, 9);
            if !b.lt(&a) { return 1; }
            if a.ge(&a) != true { return 2; }
            if a.eq(&b) { return 3; }
            match a.partial_cmp(&b) {
                Option::Some(std::cmp::Ordering::Greater) => {},
                _ => { return 4; },
            }
            match a.cmp(&a) {
                std::cmp::Ordering::Equal => {},
                _ => { return 5; },
            }

            // Operators still agree with the impls.
            let same = (1, 2);
            if !(a > b && a == same && a != b) { return 6; }

            // Generic dispatch: sort vectors of tuples and pairs-of-tuples.
            let mut v: Vector<(i32, i32)> = Vector::new();
            v.push((3, 1));
            v.push((1, 2));
            v.push((1, 1));
            v.sort();
            if v[0].0 != 1 || v[0].1 != 1 { return 7; }
            if v[1].0 != 1 || v[1].1 != 2 { return 8; }
            if v[2].0 != 3 { return 9; }

            // Tuples as map keys (Hash + Eq) and ordered keys (Ord).
            let mut counts: HashMap<(i32, i32), i32> = HashMap::new();
            counts.insert((1, 2), 10);
            counts.insert((1, 2), 20);
            counts.insert((2, 1), 30);
            match counts.get(&(1, 2)) {
                Option::Some(value) => { if *value != 20 { return 10; } },
                Option::None => { return 11; },
            }
            let mut ordered: std::collections::TreeMap<(i32, i32), i32> = std::collections::TreeMap::new();
            ordered.insert((2, 0), 1);
            ordered.insert((1, 9), 2);
            let mut first_key = (9, 9);
            let low = (0, 0);
            let high = (2, 0);
            for (key, _) in ordered.range(&low, &high) {
                first_key = *key;
                break;
            }
            let want = (1, 9);
            if first_key != want { return 12; }
            0
        }
        "#,
        "",
    );
}

#[test]
fn interpreter_match_or_patterns_dispatch() {
    assert_ok(
        r#"
        enum Color { Red, Green, Blue, Custom(i32) }

        fun band(x: i32) -> &str {
            match x {
                1 | 2 | 3 => "low",
                10 => "ten",
                _ => "other",
            }
        }

        fun name(color: Color) -> &str {
            match color {
                Color::Red | Color::Green => "warm",
                Color::Custom(_) | Color::Blue => "cool",
            }
        }

        fun guarded(x: i32) -> &str {
            match x {
                1 | 2 | 3 if x > 2 => "big-small",
                1 | 2 | 3 => "small",
                y if y > 100 => "huge",
                _ => "rest",
            }
        }

        fun main() -> i32 {
            println!("{}{}{}", band(2), band(10), band(99));
            println!("{}{}", name(Color::Red), name(Color::Blue));
            println!("{}{}", name(Color::Custom(7)), name(Color::Green));
            // `1 | 2 | 3 if x > 2` matches 3; `1 | 2 | 3` catches 1; the
            // binding arm `y if y > 100` still sees its `y`.
            println!("{}{}{}{}", guarded(3), guarded(1), guarded(200), guarded(50));
            0
        }
        "#,
        "lowtenother\nwarmcool\ncoolwarm\nbig-smallsmallhugerest\n",
    );
}

#[test]
fn interpreter_tuple_dispatch_through_nested_and_generic_paths() {
    assert_ok(
        r#"
        fun main() -> i32 {
            // 3-tuples sort through the element-wise PartialOrd impl.
            let mut v: Vector<(i32, i32, i32)> = Vector::new();
            v.push((1, 2, 3));
            v.push((1, 2, 1));
            v.push((0, 9, 9));
            v.sort();
            if v[0].2 != 9 { return 1; }
            if v[2].2 != 3 { return 2; }

            // Nested tuples: the outer impl dispatches `.lt` into the inner
            // tuple impl — recursion through the same generic machinery.
            let mut nested: Vector<((i32, i32), i32)> = Vector::new();
            nested.push(((1, 0), 5));
            nested.push(((0, 9), 5));
            nested.push(((1, 0), 2));
            nested.sort();
            if (nested[0].0).0 != 0 { return 3; }
            if nested[1].1 != 2 { return 4; }
            if nested[2].1 != 5 { return 5; }
            0
        }
        "#,
        "",
    );
}

#[test]
fn interpreter_tuple_arities_four_to_six_sort_and_compare() {
    // The std tuple impls cover arities 2..=6; the other tuple tests only
    // exercise 2 and 3, so the long forms stay unproven without this.
    assert_ok(
        r#"
        use std::cmp::Ordering;

        fun main() -> i32 {
            let mut four: Vector<(i32, i32, i32, i32)> = Vector::new();
            four.push((0, 0, 1, 0));
            four.push((0, 0, 0, 9));
            four.push((0, 0, 0, 1));
            four.sort();
            if !four[0].eq(&(0, 0, 0, 1)) { return 1; }
            if !four[2].eq(&(0, 0, 1, 0)) { return 2; }

            let mut five: Vector<(i32, i32, i32, i32, i32)> = Vector::new();
            five.push((1, 0, 0, 0, 0));
            five.push((0, 9, 9, 9, 9));
            five.sort();
            if five[0].0 != 0 || five[1].0 != 1 { return 3; }

            let mut six: Vector<(i32, i32, i32, i32, i32, i32)> = Vector::new();
            six.push((0, 0, 0, 0, 0, 5));
            six.push((0, 0, 0, 0, 0, 2));
            six.push((0, 0, 0, 0, 1, 0));
            six.sort();
            if !six[0].eq(&(0, 0, 0, 0, 0, 2)) { return 4; }
            if !six[2].eq(&(0, 0, 0, 0, 1, 0)) { return 5; }

            // Lexicographic tie-breaking reaches the last element of a 6-tuple.
            if (0, 0, 0, 0, 0, 2).lt(&(0, 0, 0, 0, 0, 5)) != true { return 6; }
            // `Ordering` has no `PartialEq`, so `cmp` results match by variant.
            match (1, 2, 3, 4, 5, 6).cmp(&(1, 2, 3, 4, 5, 6)) {
                Ordering::Equal => {},
                _ => { return 7; },
            }
            if (0, 0, 0, 0, 0, 9).gt(&(9, 0, 0, 0, 0, 0)) { return 8; }
            0
        }
        "#,
        "",
    );
}

#[test]
fn interpreter_tuple_display_and_debug_render_parenthesized() {
    // `std::fmt` ships Display/Debug for arities 2..=6; elements format
    // through their own impls, and nested tuples recurse.
    assert_ok(
        r#"
        fun main() -> i32 {
            println!("{}", (1, 2));
            println!("{:?}", (1, true));
            println!("{}", ('a', 2u8, 3.5f64, "s"));
            println!("{}", (1, 2, 3, 4, 5, 6));
            println!("{}", ((1, 2), 3));
            println!("{:?}", ((1, 2), 3));
            0
        }
        "#,
        "(1, 2)\n(1, true)\n(a, 2, 3.500000, s)\n(1, 2, 3, 4, 5, 6)\n((1, 2), 3)\n((1, 2), 3)\n",
    );
}

#[test]
fn interpreter_float_hashes_distinguish_values_by_bit_pattern() {
    // Port of the C-backend scenario: the interpreter has to agree that
    // fractions below 1.0 no longer collapse onto one hash.
    assert_ok(
        r#"
        use crate::std::hash::Hash;

        fun main() -> i32 {
            let small = 0.1f64;
            let large = 0.9f64;
            let half = 0.5f64;
            if small.hash() == large.hash() { return 1; }
            if half.hash() == small.hash() { return 2; }
            let one = 1.5f64;
            let two = 2.5f64;
            if one.hash() == two.hash() { return 3; }
            let quarter = 0.25f64;
            let quarter_again = 0.25f64;
            if quarter.hash() != quarter_again.hash() { return 4; }
            let fsmall = 0.1f32;
            let flarge = 0.9f32;
            if fsmall.hash() == flarge.hash() { return 5; }
            0
        }
        "#,
        "",
    );
}

#[test]
fn interpreter_float_hashes_fold_the_exact_bit_pattern() {
    // Port of the C-backend pin: the interpreter walks the same `&[u8]` view, so
    // it must produce the identical bit-exact fold. The hashes are printed and
    // compared here rather than checked inside the program, because the expected
    // fold needs a `usize` literal wider than `usize` on a 32-bit target — the
    // checker rejects that as E0011 — while the interpreter itself keeps `usize`
    // at one 8-byte word on every host, so the full pattern is what comes out.
    let source = r#"
        use crate::std::hash::Hash;

        fun main() -> i32 {
            println!("{} {}", 1.5f64.hash(), 2.25f64.hash());
            let zero = 0.0f64;
            let neg_zero = -0.0f64;
            if zero != neg_zero { return 3; }
            if zero.hash() == neg_zero.hash() { return 4; }
            0
        }
        "#;
    let (code, stdout, stderr) = run(source);
    assert_eq!(code, Ok(0), "stdout: {stdout}stderr: {stderr}");
    let hashes = stdout
        .split_whitespace()
        .map(|text| text.parse::<u64>().expect("hash printed as a number"))
        .collect::<Vec<_>>();
    assert_eq!(
        hashes,
        vec![9_826_234_843_501_278_960, 2_653_818_900_198_545_032],
        "stdout: {stdout}stderr: {stderr}"
    );
}

#[test]
fn interpreter_float_display_handles_nan_infinity_and_negative_zero() {
    // Port of the C-backend scenario: both backends must render the same
    // non-finite and exact-big-value forms.
    let expected = format!(
        "nan=NaN dbg=NaN pos=inf neg=-inf\nzero=-0.000000 f32=0.250000\n\
         big=100000000000000000000.000000\ndmax={:.0}.000000\n",
        f64::MAX
    );
    assert_ok(
        r#"
        fun main() -> i32 {
            let nan = 0.0f64 / 0.0f64;
            let positive_infinity = 1.0f64 / 0.0f64;
            let negative_infinity = -1.0f64 / 0.0f64;
            let negative_zero = -0.0f64;
            let quarter = 0.25f32;
            let big = 1.0e20f64;
            let dmax = 1.7976931348623157e308f64;
            println!("nan={nan} dbg={nan:?} pos={positive_infinity} neg={negative_infinity}");
            println!("zero={negative_zero} f32={quarter}");
            println!("big={big}");
            println!("dmax={dmax}");
            0
        }
        "#,
        &expected,
    );
}

#[test]
fn interpreter_path_rejects_unterminated_block_comment() {
    // `riddle run` compiles through the macro-expansion pipeline, which builds
    // its green tree from a token stream instead of a fresh lex. A stray `/*`
    // must still stop the program there — silently dropping every item after
    // it makes the run path disagree with `riddlec` and `clue check`.
    let source = "fun main() { println!(\"hi\"); }\n/* stray\n";
    let result = pipeline::compile(source);
    assert!(!result.success(), "parse: {:#?}", result.parse_errors);
    let reported = result
        .parse_errors
        .iter()
        .filter(|error| error.message.contains("unterminated block comment"))
        .count();
    assert_eq!(
        reported, 1,
        "expected exactly one diagnostic, got: {:#?}",
        result.parse_errors
    );
}

#[test]
fn interpreter_compares_ordering_values() {
    // Port of the C-backend scenario: `Ordering` used to carry only `Copy`,
    // so `x.cmp(&y) == Ordering::Equal` — the ordinary way to test a
    // comparison — had no `PartialEq` to dispatch through.
    let source = r#"
        use crate::std::cmp::Ordering;

        fun main() -> i32 {
            let a = 1i32;
            let b = 2i32;
            if a.cmp(&a) != Ordering::Equal { return 1; }
            if a.cmp(&b) != Ordering::Less { return 2; }
            if b.cmp(&a) != Ordering::Greater { return 3; }
            if a.cmp(&b) == Ordering::Equal { return 4; }
            if !(Ordering::Less < Ordering::Equal) { return 5; }
            if !(Ordering::Equal < Ordering::Greater) { return 6; }
            if !(Ordering::Greater > Ordering::Less) { return 7; }
            let mut results = vec![Ordering::Greater, Ordering::Less, Ordering::Equal];
            results.sort();
            if results[0] != Ordering::Less { return 8; }
            if results[1] != Ordering::Equal { return 9; }
            if results[2] != Ordering::Greater { return 10; }
            0
        }
        "#;
    assert_ok(source, "");
}

#[test]
fn references_compare_through_blanket_impls() {
    let source = r#"
        use crate::std::cmp::Ordering;

        fun ref_eq<T: crate::std::cmp::PartialEq>(a: &T, b: &T) -> bool {
            a == b
        }

        fun main() -> i32 {
            let p = 10;
            let q = 10;
            let a = &p;
            let b = &q;
            if !(a == b) { return 1; }
            if a != b { return 2; }
            let small = 3;
            let big = 9;
            if !(&small < &big) { return 3; }
            if !(&big > &small) { return 4; }
            if &small >= &big { return 5; }
            if !a.eq(b) { return 6; }
            if a.cmp(b) != Ordering::Equal { return 7; }
            if small.cmp(&big) != Ordering::Less { return 8; }
            if !ref_eq(&p, &q) { return 9; }
            if ref_eq(&small, &big) { return 10; }
            0
        }
        "#;
    assert_ok(source, "");
}

#[test]
fn debug_formats_ordering_and_nested_options() {
    let source = r#"
        use crate::std::cmp::Ordering;
        use crate::std::option::Option;
        use crate::std::vector::Vector;

        fun main() -> i32 {
            let mut v: Vector<i32> = Vector::new();
            v.push(1);
            v.push(2);
            let none: Option<i32> = Option::None;
            println!("{:?}|{:?}|{:?}|{:?}", Option::Some(v), none, Ordering::Greater, Option::Some(Ordering::Less));
            0
        }
        "#;
    assert_ok(source, "Some([1, 2])|None|Greater|Some(Less)\n");
}

#[test]
fn slice_iter_supports_manual_consecutive_next_calls() {
    let source = r#"
        use crate::std::option::Option;
        use crate::std::vector::Vector;

        fun main() -> i32 {
            let mut v: Vector<i32> = Vector::new();
            v.push(10);
            v.push(20);
            v.push(30);
            let mut it = v.iter();
            let a = it.next();
            let b = it.next();
            let c = it.next();
            let done = it.next();
            let av = match a { Option::Some(x) => *x, _ => -1 };
            let bv = match b { Option::Some(x) => *x, _ => -1 };
            let cv = match c { Option::Some(x) => *x, _ => -1 };
            let dn = match done { Option::Some(_) => 1, _ => 0 };
            if av == 10 && bv == 20 && cv == 30 && dn == 0 { 0 } else { 1 }
        }
        "#;
    assert_ok(source, "");
}

#[test]
fn generic_iterator_consumption_and_tree_next_calls() {
    let source = r#"
        use crate::std::collections::TreeMap;
        use crate::std::iter::Iterator;
        use crate::std::option::Option;

        fun count_all<I: Iterator>(it: &mut I) -> usize {
            let mut n = 0usize;
            loop {
                match it.next() {
                    Option::Some(_) => { n += 1usize; },
                    Option::None => { break; },
                }
            }
            n
        }

        fun main() -> i32 {
            let mut tree: TreeMap<i32, i32> = TreeMap::new();
            tree.insert(10i32, 1);
            tree.insert(20i32, 2);
            // Generic-bound consumption (dead results) plus manual
            // consecutive `next` on the tree iterator (live results).
            let mut iter = tree.iter();
            let n = count_all(&mut iter);
            let mut iter2 = tree.iter();
            let a = iter2.next();
            let b = iter2.next();
            let av = match a { Option::Some((k, _)) => *k, _ => -1 };
            let bv = match b { Option::Some((k, _)) => *k, _ => -1 };
            if n == 2usize && av == 10 && bv == 20 { 0 } else { 1 }
        }
        "#;
    assert_ok(source, "");
}

// ---------------------------------------------------------------------------
// Recursive types
//
// A struct or enum that reaches itself through a reference used to blow the
// compiler's stack while lowering: the MIR type of `next: Option<&Node>` was
// expanded field by field, so the expansion never ended. These tests pin the
// closed-cycle representation end to end.

#[test]
fn recursive_struct_reference_chain_sums() {
    assert_ok(
        r#"
        struct Node {
            data: i32,
            next: Option<&Node>,
        }

        fun sum(node: &Node) -> i32 {
            match node.next {
                Option::Some(next) => node.data + sum(next),
                Option::None => node.data,
            }
        }

        fun main() {
            let tail = Node { data: 2, next: Option::None };
            let middle = Node { data: 3, next: Option::Some(&tail) };
            let head = Node { data: 1, next: Option::Some(&middle) };
            println!("{}", sum(&head));
        }
        "#,
        "6\n",
    );
}

#[test]
fn recursive_struct_field_access_crosses_two_back_edges() {
    // Each `next` hop reads a field through the recursive occurrence, so this
    // pins the offsets the back edge resolves to, not just the first level.
    assert_ok(
        r#"
        struct Node {
            data: i32,
            next: Option<&Node>,
        }

        fun tail_data(node: &Node) -> i32 {
            match node.next {
                Option::Some(middle) => match middle.next {
                    Option::Some(tail) => tail.data,
                    Option::None => middle.data,
                },
                Option::None => node.data,
            }
        }

        fun main() {
            let tail = Node { data: 30, next: Option::None };
            let middle = Node { data: 20, next: Option::Some(&tail) };
            let head = Node { data: 10, next: Option::Some(&middle) };
            println!("{}", tail_data(&head));
        }
        "#,
        "30\n",
    );
}

#[test]
fn recursively_typed_value_survives_heap_promotion() {
    // `make` returns a reference to its own local, so the nodes are promoted to
    // the heap; traversal has to keep working across the frame boundary.
    assert_ok(
        r#"
        struct Node {
            data: i32,
            next: Option<&Node>,
        }

        fun make() -> &Node {
            let tail = Node { data: 2, next: Option::None };
            let head = Node { data: 1, next: Option::Some(&tail) };
            &head
        }

        fun sum(node: &Node) -> i32 {
            match node.next {
                Option::Some(next) => node.data + sum(next),
                Option::None => node.data,
            }
        }

        fun main() {
            println!("{}", sum(make()));
        }
        "#,
        "3\n",
    );
}

#[test]
fn mutually_recursive_structs_stay_finite() {
    assert_ok(
        r#"
        struct EvenStep {
            next: Option<&OddStep>,
        }

        struct OddStep {
            next: Option<&EvenStep>,
        }

        fun odd_len(node: &OddStep) -> i32 {
            match node.next {
                Option::Some(next) => 1 + even_len(next),
                Option::None => 0,
            }
        }

        fun even_len(node: &EvenStep) -> i32 {
            match node.next {
                Option::Some(next) => 1 + odd_len(next),
                Option::None => 0,
            }
        }

        fun main() {
            let odd = OddStep { next: Option::None };
            let even = EvenStep { next: Option::Some(&odd) };
            println!("{}", even_len(&even));
        }
        "#,
        "1\n",
    );
}

#[test]
fn recursive_enum_payload_traverses() {
    assert_ok(
        r#"
        enum LinkChain {
            End,
            Link(i32, &LinkChain),
        }

        fun total(chain: &LinkChain) -> i32 {
            match chain {
                LinkChain::End => 0,
                LinkChain::Link(value, next) => *value + total(*next),
            }
        }

        fun main() {
            let a = LinkChain::End;
            let b = LinkChain::Link(2, &a);
            let c = LinkChain::Link(1, &b);
            println!("{}", total(&c));
        }
        "#,
        "3\n",
    );
}

#[test]
fn recursive_type_may_share_a_name_with_a_std_type() {
    // `Chain` is also an iterator adapter in std. The self-reference inside the
    // declaration has to keep pointing at the declaration: lowering used to
    // resolve field and signature types by name alone, so `&Chain` picked up
    // std's `Chain` and the program silently read the wrong layout.
    assert_ok(
        r#"
        enum Chain {
            End,
            Link(i32, &Chain),
        }

        fun total(chain: &Chain) -> i32 {
            match chain {
                Chain::End => 0,
                Chain::Link(value, next) => *value + total(*next),
            }
        }

        fun main() {
            let a = Chain::End;
            let b = Chain::Link(2, &a);
            let c = Chain::Link(1, &b);
            println!("{}", total(&c));
        }
        "#,
        "3\n",
    );
}

#[test]
fn a_type_may_share_a_name_with_a_std_type() {
    // std declares an iterator adapter named `Chain`, and every package's items
    // share one scope in the graph. `impl` blocks used to be attached to the
    // first struct with that name, so std's `impl Iterator for Chain<I, J, T>`
    // landed on this type and std's own sources reported a bogus `E0032`.
    assert_ok(
        r#"
        struct Chain {
            data: i32,
        }

        fun main() {
            let c = Chain { data: 7 };
            println!("{}", c.data);
        }
        "#,
        "7\n",
    );
}

#[test]
fn interpreter_binds_let_else_enum_payload() {
    // The let-else binding path only handled tuple/struct patterns, so an
    // enum payload pattern left the binding uninitialize and reading it
    // trapped on a zero-sized allocation.
    assert_ok(
        r#"
        use std::option::Option;

        fun main() -> i32 {
            let v: Option<i32> = Option::Some(41);
            let Option::Some(x) = v else {
                return 1;
            };
            println!("x={}", x + 1);
            0
        }
        "#,
        "x=42\n",
    );
}

#[test]
fn interpreter_runs_let_else_diverging_arm() {
    assert_ok(
        r#"
        use std::option::Option;

        fun main() -> i32 {
            let v: Option<i32> = Option::None;
            let Option::Some(x) = v else {
                println!("no value");
                return 0;
            };
            println!("unreachable {}", x);
            1
        }
        "#,
        "no value\n",
    );
}

#[test]
fn interpreter_calls_slice_methods_on_array_values() {
    // `a.len()` used to pass a thin `&[T; N]` where the slice instance reads a
    // fat pointer.
    assert_ok(
        r#"
        fun main() -> i32 {
            let a = [1, 2, 3];
            let mut total = 0;
            let mut index = 0usize;
            while index < a.len() {
                total += a[index];
                index += 1usize;
            }
            println!("{} {} {}", a.len(), a.is_empty(), total);
            0
        }
        "#,
        "3 false 6\n",
    );
}

#[test]
fn interpreter_assigns_through_destructuring_assignment() {
    assert_ok(
        r#"
        struct Point { x: i32, y: i32 }

        fun main() -> i32 {
            let mut a = 0;
            let mut b = 0;
            (a, b) = (1, 2);
            println!("{} {}", a, b);
            let mut c = 0;
            let mut d = 0;
            (c, (a, b)) = (3, (4, 5));
            println!("{} {} {} {}", c, d, a, b);
            let mut point = Point { x: 0, y: 0 };
            Point { x: point.x, y: point.y } = Point { x: 6, y: 7 };
            println!("{} {}", point.x, point.y);
            Point { y: d, x: c } = point;
            println!("{c} {d}");
            0
        }
        "#,
        "1 2
3 0 4 5
6 7
6 7
",
    );
}

#[test]
fn interpreter_hashes_str_keys() {
    assert_ok(
        r#"
        use std::collections::HashMap;

        fun main() -> i32 {
            let mut counts: HashMap<&str, i32> = HashMap::new();
            counts.insert("alpha", 1);
            counts.insert("beta", 2);
            counts.insert("alpha", 3);
            let alpha = match counts.get(&"alpha") {
                Option::Some(value) => *value,
                Option::None => -1,
            };
            println!("{} {} {}", alpha, counts.len(), counts.contains_key(&"beta"));
            0
        }
        "#,
        "3 2 true\n",
    );
}

#[test]
fn interpreter_drops_a_partially_moved_owner_once() {
    // A `let` destructure moved the field but left the owner's drop glue armed
    // for it too, so the buffer was freed twice.
    assert_ok(
        r#"
        use std::string::String;

        struct Pair { a: String, b: i32 }

        fun main() -> i32 {
            let mut total = 0;
            let mut round = 0;
            while round < 64 {
                let p = Pair { a: String::from("payload"), b: round };
                let Pair { a } = p;
                total += a.len() as i32 + p.b;
                round += 1;
            }
            println!("{}", total);
            0
        }
        "#,
        "2464\n",
    );
}

#[test]
fn interpreter_folds_negative_float_and_text_constants() {
    assert_ok(
        r#"
        const NEG: i32 = -7;
        const HALF: f64 = 2.5;
        const NAME: &str = "riddle";

        fun main() -> i32 {
            println!("{} {} {}", NEG, HALF, NAME);
            0
        }
        "#,
        "-7 2.500000 riddle\n",
    );
}

#[test]
fn interpreter_returns_self_from_inherent_impl() {
    assert_ok(
        r#"
        struct Point { x: i32, y: i32 }

        impl Point {
            fun make(x: i32, y: i32) -> Self {
                Point { x: x, y: y }
            }

            fun sum(&self) -> i32 {
                self.x + self.y
            }
        }

        fun main() -> i32 {
            let p = Point::make(20, 22);
            println!("{}", p.sum());
            0
        }
        "#,
        "42\n",
    );
}

#[test]
fn interpreter_reclaims_scratch_between_calls() {
    // Long-lived sessions (notably the REPL) call `main` repeatedly; frame
    // storage used to accumulate in the arena forever.
    let source = r#"
        use std::vector::Vector;

        fun total(seed: i32) -> i32 {
            let mut values: Vector<i32> = Vector::new();
            let mut index = 0;
            while index < 32 {
                values.push(seed + index);
                index += 1;
            }
            let mut sum = 0;
            let mut scan = 0usize;
            while scan < values.len() {
                sum += values[scan];
                scan += 1usize;
            }
            sum
        }

        fun main() -> i32 {
            println!("{}", total(1));
            0
        }
        "#;
    assert_ok(source, "528\n");
}

#[test]
fn interpreter_block_value_owns_the_moved_buffer() {
    // A block's tail moves its local, so the buffer is freed once — by the
    // block value's own drop glue. The C backend read freed memory silently;
    // a strict allocator traps.
    let source = r#"
        use crate::std::string::String;

        fun main() -> i32 {
            println!("{}", {
                let mut buffer = String::new();
                buffer.push_str("abc");
                buffer
            });
            0
        }
        "#;
    assert_ok(source, "abc\n");
}
