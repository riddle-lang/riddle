//! End-to-end behavior tests for the standard library surface: collection
//! `remove`, the iterator combinator functions, `?` with `From`/`Option`, and
//! `std::fs` file I/O. Each test compiles a Riddle program to C, links it with
//! the runtime, runs it, and asserts on the exit code and output.

use riddlec::pipeline;
use std::io::Write;
use std::process::Stdio;
use std::{fs, path::Path, process::Command};

/// Compiles `source` with the full pipeline, emits C, builds it with the
/// system C compiler plus the selected runtime, and runs it with empty
/// standard input. Returns the exit code and captured stdout.
fn compile_and_run(source: &str, gc: bool) -> (i32, String) {
    compile_and_run_with_stdin(source, gc, &[])
}

/// Same as `compile_and_run`, but feeds `stdin` bytes to the program and
/// closes the pipe so standard input reads observe end of stream.
fn compile_and_run_with_stdin(source: &str, gc: bool, stdin: &[u8]) -> (i32, String) {
    let result = pipeline::compile(source);
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
    let generated = pipeline::generate_c_with_gc_and_source(
        result.mir_module.as_ref().unwrap(),
        gc,
        "src/main.rid",
    )
    .unwrap();

    let runtime = if gc {
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/crates/gc/src/runtime.c"
        ))
    } else {
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/crates/gc/src/no_gc_runtime.c"
        ))
    };
    let args_runtime = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/crates/gc/src/args_runtime.c"
    ));

    let compiler = std::env::var_os("CC").unwrap_or_else(|| "cc".into());
    let compiler_name = Path::new(&compiler)
        .file_stem()
        .unwrap_or_default()
        .to_string_lossy()
        .to_ascii_lowercase();
    let is_msvc = compiler_name == "cl";

    let dir = std::env::temp_dir().join(format!(
        "riddle-std-e2e-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&dir).unwrap();
    let c_source = dir.join("main.c");
    let executable = dir.join(if cfg!(windows) { "main.exe" } else { "main" });
    fs::write(&c_source, format!("{generated}\n{runtime}\n{args_runtime}")).unwrap();

    let mut command = Command::new(&compiler);
    if is_msvc {
        command
            .args(["/std:c11", "/W4"])
            .arg(&c_source)
            .arg(format!("/Fo{}.obj", executable.display()))
            .arg(format!("/Fe{}", executable.display()));
    } else {
        command
            .args(["-std=c11"])
            .arg(&c_source)
            .arg("-o")
            .arg(&executable);
    }
    let compile_output = command.output().unwrap();
    assert!(
        compile_output.status.success(),
        "C compile failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&compile_output.stdout),
        String::from_utf8_lossy(&compile_output.stderr)
    );

    let mut child = Command::new(&executable)
        .current_dir(&dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|error| panic!("failed to launch {executable:?}: {error}"));
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(stdin)
        .unwrap_or_else(|error| panic!("failed to feed stdin: {error}"));
    // `wait_with_output` closes stdin first, so reads observe end of stream.
    let run = child.wait_with_output().unwrap();
    let stdout = String::from_utf8_lossy(&run.stdout).into_owned();
    let _ = fs::remove_dir_all(&dir);
    (run.status.code().unwrap_or(-1), stdout)
}

/// The low bits of a 64-bit hash, as the host's `usize` keeps them. A `usize`
/// literal wider than the target is rejected as E0011 on 32-bit hosts, where
/// `hash()` can only return the truncated fold.
fn hash_fold(value: u64) -> usize {
    (value & (usize::MAX as u64)) as usize
}

#[test]
fn std_collections_remove_elements_and_keep_lookups() {
    let (code, stdout) = compile_and_run(
        r#"
        use crate::std::collections::{HashMap, HashSet, TreeMap, TreeSet};

        fun main() -> i32 {
            let mut map: HashMap<i32, i32> = HashMap::new();
            map.insert(1i32, 10i32);
            map.insert(2i32, 20i32);
            map.insert(3i32, 30i32);
            let removed = map.remove(&2i32);
            let gone = map.remove(&2i32);
            let map_ok = map.len() == 2usize
                && map.contains_key(&1i32)
                && map.contains_key(&3i32)
                && !map.contains_key(&2i32)
                && removed.unwrap_or(0i32) == 20i32
                && gone.is_none();
            if !map_ok { return 1; }

            // Removing through a probe cluster keeps later lookups working.
            map.insert(4i32, 40i32);
            map.remove(&1i32);
            map.insert(5i32, 50i32);
            if map.get(&3i32).is_none() || map.get(&4i32).is_none() || map.get(&5i32).is_none() {
                return 2;
            }

            let mut set: HashSet<i32> = HashSet::new();
            set.insert(5i32);
            set.insert(6i32);
            if !set.remove(&5i32) || set.contains(&5i32) || !set.contains(&6i32) {
                return 3;
            }

            let mut tree: TreeMap<i32, i32> = TreeMap::new();
            tree.insert(1i32, 10i32);
            tree.insert(2i32, 20i32);
            tree.insert(3i32, 30i32);
            tree.insert(4i32, 40i32);
            if tree.remove(&2i32).unwrap_or(0i32) != 20i32 {
                return 4;
            }
            if tree.len() != 3usize
                || !tree.contains_key(&1i32)
                || !tree.contains_key(&3i32)
                || !tree.contains_key(&4i32)
                || tree.contains_key(&2i32)
            {
                return 5;
            }
            // The tree stays usable for lookups and further inserts.
            tree.insert(5i32, 50i32);
            if tree.get(&5i32).is_none() || tree.remove(&9i32).is_some() {
                return 6;
            }

            let mut tset: TreeSet<i32> = TreeSet::new();
            tset.insert(7i32);
            tset.insert(8i32);
            if !tset.remove(&7i32) || tset.contains(&7i32) || !tset.contains(&8i32) {
                return 7;
            }
            0
        }
        "#,
        true,
    );
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn std_iterator_combinators_and_eager_maps_run() {
    let (code, stdout) = compile_and_run(
        r#"
        use crate::std::iter::{Iterator, IntoIterator, map_into, filter_into};

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

        fun main() -> i32 {
            let counter = Counter { index: 0usize, limit: 5usize };
            let sum = counter.fold(0i32, [acc: i32, value: i32 -> acc + value]);
            if sum != 15i32 { return 1; }

            let mut counter2 = Counter { index: 0usize, limit: 5usize };
            if counter2.count() != 5usize { return 2; }

            let mut counter3 = Counter { index: 0usize, limit: 5usize };
            if counter3.nth(2usize).unwrap_or(0i32) != 3i32 { return 3; }

            let mut counter4 = Counter { index: 0usize, limit: 5usize };
            if !counter4.all([v: i32 -> v > 0i32]) { return 4; }

            let mut counter5 = Counter { index: 0usize, limit: 5usize };
            if !counter5.any([v: i32 -> v > 3i32]) { return 5; }

            let mut counter6 = Counter { index: 0usize, limit: 5usize };
            if counter6.find([v: &i32 -> *v == 4i32]).unwrap_or(0i32) != 4i32 {
                return 6;
            }

            let mut counter7 = Counter { index: 0usize, limit: 5usize };
            if counter7.position([v: &i32 -> *v == 4i32]).unwrap_or(9usize) != 3usize {
                return 7;
            }

            let mut counter8 = Counter { index: 0usize, limit: 4usize };
            let doubled = map_into(&mut counter8, [v: i32 -> v * 2i32]);
            let mut total = 0i32;
            for value in doubled {
                total += value;
            }
            if total != 20i32 { return 8; }

            let mut counter9 = Counter { index: 0usize, limit: 6usize };
            let evens = filter_into(&mut counter9, [v: &i32 -> *v % 2i32 == 0i32]);
            let mut even_total = 0i32;
            for value in evens {
                even_total += value;
            }
            if even_total != 12i32 { return 9; }

            let mut counter10 = Counter { index: 0usize, limit: 4usize };
            let collected = Vector::from_iterator(&mut counter10);
            if collected.len() != 4usize { return 10; }

            // Lazy map / filter with chaining.
            let mapped = Counter { index: 0usize, limit: 4usize }
                .map([v: i32 -> v * 10i32]);
            let mut mapped_total = 0i32;
            for value in mapped {
                mapped_total += value;
            }
            if mapped_total != 100i32 { return 11; }

            let chained = Counter { index: 0usize, limit: 5usize }
                .map([v: i32 -> v + 1i32])
                .filter([v: &i32 -> *v > 3i32]);
            let mut chained_total = 0i32;
            for value in chained {
                chained_total += value;
            }
            if chained_total != 15i32 { return 12; }
            0
        }
        "#,
        true,
    );
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn std_iterator_lazy_helpers_chain_and_short_circuit() {
    let (code, stdout) = compile_and_run(
        r#"
        use crate::std::iter::{Iterator, IntoIterator};

        fun main() -> i32 {
            let chained = crate::std::ops::range(0, 3).into_iter()
                .chain(crate::std::ops::range(3, 5).into_iter());
            let chained_total = chained.fold(0i32, [acc: i32, value: i32 -> acc + value]);
            if chained_total != 10i32 { return 1; }

            let taken = crate::std::ops::range(0, 10).into_iter()
                .take_while([value: &i32 -> *value < 4i32]);
            if taken.fold(0i32, [acc: i32, value: i32 -> acc + value]) != 6i32 { return 2; }

            let skipped = crate::std::ops::range(0, 6).into_iter()
                .skip_while([value: &i32 -> *value < 3i32]);
            if skipped.fold(0i32, [acc: i32, value: i32 -> acc + value]) != 12i32 { return 3; }

            let mut inspected_total = 0i32;
            let inspected = crate::std::ops::range(1, 4).into_iter()
                .inspect([value: &i32 -> { inspected_total += *value; }]);
            let copied_total = inspected.fold(0i32, [acc: i32, value: i32 -> acc + value]);
            if inspected_total != 6i32 || copied_total != 6i32 { return 4; }
            0
        }
        "#,
        true,
    );
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn std_bracket_lambdas_run() {
    let (code, stdout) = compile_and_run(
        r#"
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
            // Bracket lambda passed inside the argument list; parameter
            // types inferred from the expected callable signature.
            let sum = Counter { index: 0usize, limit: 5usize }
                .fold(0i32, [acc, v -> acc + v]);
            if sum != 15i32 { return 1; }

            // `it` convention on a reference parameter.
            let mut counter2 = Counter { index: 0usize, limit: 5usize };
            if counter2.find([it -> *it == 4i32]).unwrap_or(0i32) != 4i32 { return 2; }

            // Lazy chain with trailing method bracket lambdas.
            let chained = Counter { index: 0usize, limit: 5usize }
                .map [v -> v + 1i32]
                .filter [it -> *it > 3i32];
            let mut total = 0i32;
            for value in chained {
                total += value;
            }
            if total != 15i32 { return 3; }

            // Zero-parameter lambda with move capture.
            let base = 10i32;
            let offset = move [ -> base + 5i32];
            if invoke(offset) != 15i32 { return 4; }

            // Bracket lambda bound to a variable and called directly.
            let double = [it -> it * 2i32];
            if double(21i32) != 42i32 { return 5; }

            // Mutable capture through a zero-parameter bracket lambda (FnMut).
            let mut count = 0i32;
            let mut bump = [ -> { count += 1i32; count }];
            bump();
            bump();
            if count != 2i32 { return 6; }

            0
        }
        "#,
        true,
    );
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn std_question_operator_supports_from_and_option() {
    let (code, stdout) = compile_and_run(
        r#"
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
        "#,
        true,
    );
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn std_fs_roundtrips_file_content() {
    let source = r#"
        use crate::std::fs::{read_to_string, write, FsFile};
        use crate::std::result::Result;

        fun main() -> i32 {
            match write("riddle_fs_e2e.tmp", "hello fs") {
                Result::Ok(()) => {},
                Result::Err(_) => { return 1; },
            }
            let content = match read_to_string("riddle_fs_e2e.tmp") {
                Result::Ok(text) => text,
                Result::Err(_) => { return 2; },
            };
            if content.len() != 8usize {
                return 3;
            }
            match FsFile::open("riddle_fs_e2e.tmp") {
                Result::Ok(mut file) => {
                    let mut buffer = [0u8; 16];
                    let read = file.read(&mut buffer).unwrap_or(0usize);
                    if read != 8usize {
                        return 4;
                    }
                    if buffer[0usize] != 104u8 {
                        return 5;
                    }
                },
                Result::Err(_) => { return 6; },
            }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn std_option_result_combinators_compile_and_run() {
    let source = r#"
        use crate::std::option::Option;
        use crate::std::result::Result;

        enum AppError {
            Bad,
        }

        fun main() -> i32 {
            let fallback = Option::Some(5i32).unwrap_or_else([ -> 0i32 ]);
            let none: Option<i32> = Option::None;
            let mapped = none.map_or(9i32, [v -> v * 2i32]);
            let recovered = none.or_else([ -> Option::Some(3i32) ]).unwrap();
            let chained = Option::Some(1i32).and(Option::Some(2i32)).unwrap();
            let converted = match Result::Err(AppError::Bad).map_err([_ -> 7i32]) {
                Result::Ok(_) => 0i32,
                Result::Err(code) => code,
            };
            let errored = Result::Ok(4i32).map_or(0i32, [v -> v + 1i32]);
            if fallback == 5i32 && mapped == 9i32 && recovered == 3i32
                && chained == 2i32 && converted == 7i32 && errored == 5i32
            { 0 } else { 1 }
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn range_syntax_iterates_exclusive_and_inclusive() {
    let source = r#"
        fun main() -> i32 {
            let mut total = 0;
            for i in 0..5 {
                total += i;
            }
            for i in 0..=4 {
                total += i * 10;
            }
            let mut parts = 0;
            for i in 1 + 1..4 + 1 {
                parts += i;
            }
            let mut empty = 0;
            for _i in 5..5 {
                empty += 1;
            }
            let mut single = 0;
            for i in 3..=3 {
                single = i;
            }
            if total == 110 && parts == 9 && empty == 0 && single == 3 { 0 } else { 1 }
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn string_split_replace_and_ascii_case_roundtrip() {
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
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn vector_insert_remove_sort_contains_retain_roundtrip() {
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
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn iterator_collect_skip_min_max_roundtrip() {
    let source = r#"
        use crate::std::iter::{Iterator, IntoIterator};

        fun main() -> i32 {
            let collected = crate::std::ops::range(0, 10).collect();
            if collected.len() != 10usize { return 1; }
            if *collected.get(3usize).unwrap_or(&0) != 3i32 { return 2; }
            let mut skipped_sum = 0;
            for value in crate::std::iter::skip(crate::std::ops::range(0, 10).into_iter(), 8usize) {
                skipped_sum += value;
            }
            if skipped_sum != 17 { return 3; }
            let mut taken = crate::std::iter::take(crate::std::ops::range(0, 10).into_iter(), 2usize);
            let taken_count = taken.count();
            if taken_count != 2usize { return 4; }
            let smallest = crate::std::iter::min(crate::std::ops::range(0, 10).collect().into_iter());
            match smallest {
                Option::Some(0i32) => {},
                _ => { return 5; },
            }
            let largest = crate::std::iter::max(crate::std::ops::range(0, 10).collect().into_iter());
            match largest {
                Option::Some(9i32) => {},
                _ => { return 6; },
            }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn hash_map_get_or_insert_counts_once() {
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
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn parse_wide_integers_and_radix_roundtrip() {
    let source = r#"
        use crate::std::parse::{ParseIntErrorKind, parse_i64, parse_u64, parse_usize, parse_with_radix};
        use crate::std::result::Result;

        fun main() -> i32 {
            match parse_i64("-9223372036854775808") {
                Result::Ok(v) => { if v != -9223372036854775807i64 - 1i64 { return 1; } },
                Result::Err(_) => { return 2; },
            }
            match parse_i64("9223372036854775808") {
                Result::Err(error) => match *error.kind() {
                    ParseIntErrorKind::PosOverflow => {},
                    _ => { return 3; },
                },
                Result::Ok(_) => { return 3; },
            }
            match parse_u64("18446744073709551615") {
                Result::Ok(v) => { if v != 18446744073709551615u64 { return 4; } },
                Result::Err(_) => { return 5; },
            }
            match parse_with_radix("ff", 16) {
                Result::Ok(v) => { if v != 255i64 { return 6; } },
                Result::Err(_) => { return 7; },
            }
            match parse_with_radix("-2a", 16) {
                Result::Ok(v) => { if v != -42i64 { return 8; } },
                Result::Err(_) => { return 9; },
            }
            match parse_with_radix("1010", 2) {
                Result::Ok(v) => { if v != 10i64 { return 10; } },
                Result::Err(_) => { return 11; },
            }
            match parse_with_radix("1", 37) {
                Result::Err(error) => match *error.kind() {
                    ParseIntErrorKind::InvalidDigit => {},
                    _ => { return 12; },
                },
                Result::Ok(_) => { return 12; },
            }
            match parse_usize("12345") {
                Result::Ok(v) => { if v != 12345usize { return 13; } },
                Result::Err(_) => { return 14; },
            }
            match parse_i64("") {
                Result::Err(error) => match *error.kind() {
                    ParseIntErrorKind::Empty => {},
                    _ => { return 15; },
                },
                Result::Ok(_) => { return 15; },
            }
            match parse_i64("12x") {
                Result::Err(error) => match *error.kind() {
                    ParseIntErrorKind::InvalidDigit => {},
                    _ => { return 16; },
                },
                Result::Ok(_) => { return 16; },
            }
            match parse_i64("-9223372036854775809") {
                Result::Err(error) => match *error.kind() {
                    ParseIntErrorKind::NegOverflow => {},
                    _ => { return 17; },
                },
                Result::Ok(_) => { return 17; },
            }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn format_placeholders_positional_named_and_debug() {
    let source = r#"
        fun main() -> i32 {
            let name = "riddle";
            let version = 42i32;
            println!("hello {name} v{version}!");
            println!("{0} then {1} then {0} and {}", 1i32, 2i32, 3i32);
            println!("debug: {version:?} hex {}", 7i32);
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
    assert!(stdout.contains("hello riddle v42!"), "stdout: {stdout}");
    assert!(stdout.contains("1 then 2 then 1 and 1"), "stdout: {stdout}");
    assert!(stdout.contains("debug: 42 hex 7"), "stdout: {stdout}");
}

#[test]
fn generic_bound_dispatch_compiles_and_runs() {
    // Regression: trait-method calls on a generic `T` inside a generic
    // function used to miscompile when the operand was a match payload
    // binding (its storage was lazily materialized inside one arm and read
    // from sibling arms that never executed it).
    let source = r#"
        use crate::std::iter::{Iterator, IntoIterator};
        use crate::std::option::Option;

        fun pick<I, T>(mut iterator: I) -> Option<T>
        where I: Iterator<Item = T>,
              T: crate::std::cmp::PartialOrd {
            let mut best: Option<T> = Option::None;
            loop {
                match iterator.next() {
                    Option::Some(value) => {
                        match best {
                            Option::Some(current) => {
                                let is_smaller = value.lt(&current);
                                if is_smaller {
                                    best = Option::Some(value);
                                }
                            },
                            Option::None => { best = Option::Some(value); },
                        }
                    },
                    Option::None => { break; },
                }
            }
            best
        }

        fun compare_only<T: crate::std::cmp::PartialOrd>(a: T, b: T) -> bool {
            a.lt(&b)
        }

        fun main() -> i32 {
            let smallest = pick(crate::std::ops::range(0, 10).collect().into_iter());
            match smallest {
                Option::Some(0i32) => {},
                _ => { return 1; },
            }
            if compare_only(3i32, 5i32) { 0 } else { 2 }
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn adapter_methods_resolve_and_run() {
    // Regression: method calls directly on adapter types (`taken.count()`)
    // used to report E0013 because the receiver type still contained a
    // pending inference variable at method-lookup time.
    let source = r#"
        use crate::std::iter::IntoIterator;

        fun main() -> i32 {
            let mut taken = crate::std::iter::take(crate::std::ops::range(0, 10).into_iter(), 2usize);
            let taken_count = taken.count();
            let mut skipped = crate::std::iter::skip(crate::std::ops::range(0, 10).into_iter(), 8usize);
            let skipped_count = skipped.count();
            let mut enumerated = crate::std::iter::enumerate(crate::std::ops::range(0, 10).into_iter());
            let enumerated_count = enumerated.count();
            if taken_count == 2usize
                && skipped_count == 2usize
                && enumerated_count == 10usize
            { 0 } else { 1 }
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn vec_macro_builds_lists_repeats_and_empty_vectors() {
    let source = r#"
        fun main() -> i32 {
            let list = vec![1, 2, 3];
            let mut total = 0i32;
            for value in list {
                total += value;
            }
            if total != 6i32 { return 1; }

            // The repeat form clones a non-Copy element into every slot.
            let strings = vec![String::from_str("x"); 3usize];
            let mut joined = String::new();
            for value in strings {
                joined.push_str(value.as_str());
            }
            if joined.as_str() != "xxx" { return 2; }

            // The empty form infers its element type from the binding.
            let mut empty: Vector<String> = vec![];
            empty.push(String::from_str("first"));
            empty.push(String::from_str("second"));
            if empty.len() != 2usize { return 3; }
            if empty.get(1usize).unwrap_or(&String::new()).as_str() != "second" { return 4; }

            // Nested vectors and a zero-count repeat.
            let nested = vec![vec![1, 2], vec![3]];
            let mut nested_total = 0i32;
            for outer in nested {
                for value in outer {
                    nested_total += value;
                }
            }
            if nested_total != 6i32 { return 5; }
            if vec![9; 0usize].len() != 0usize { return 6; }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn random_values_stay_in_requested_bounds() {
    let source = r#"
        use crate::std::random::{random_u32, random_bool, random_below};

        fun main() -> i32 {
            // random_below(1) must collapse to the only in-range value.
            let mut index = 0usize;
            while index < 64usize {
                if random_below(1u32) != 0u32 { return 1; }
                if random_below(0u32) != 0u32 { return 2; }
                let value = random_below(7u32);
                if value >= 7u32 { return 3; }
                index += 1usize;
            }
            // Bounded by the full range: any result is representable; just
            // exercise both generators so a broken binding shows up.
            let _ = random_u32();
            let coin = random_bool();
            if coin != true && coin != false { return 4; }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn time_now_is_monotonic_across_reads() {
    let source = r#"
        use crate::std::time::time_now;

        fun main() -> i32 {
            let first = time_now();
            let mut second = time_now();
            // Adjacent reads never go backwards; retry a few times so a
            // coarse clock still produces an advancing pair.
            let mut tries = 0;
            while second == first && tries < 1000 {
                second = time_now();
                tries += 1;
            }
            if second < first { return 1; }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn osstring_roundtrips_and_args_stay_consistent() {
    let source = r#"
        use crate::std::ffi::OsString;
        use crate::std::env;
        use crate::std::result::Result;
        use crate::std::option::Option;

        fun main() -> i32 {
            // str -> OsString -> str roundtrip preserves bytes.
            let original = OsString::from_str("hello world");
            match original.clone().into_string() {
                Result::Ok(text) => { if text.as_str() != "hello world" { return 1; } },
                Result::Err(_) => { return 2; },
            }
            if original.len() != 11usize || original.is_empty() { return 3; }

            // Encoded bytes survive the unsafe copy constructor.
            let copy = original.clone();
            let left = original.as_encoded_bytes();
            let right = copy.as_encoded_bytes();
            if left.len() != right.len() { return 4; }
            let mut index = 0usize;
            while index < left.len() {
                if left[index] != right[index] { return 5; }
                index += 1usize;
            }

            // The process always has at least its own executable name, and
            // args_os/args agree on the count.
            let wide = env::args_os();
            if wide.is_empty() { return 6; }
            let narrow = env::args();
            if narrow.len() != wide.len() { return 7; }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn tree_map_iterates_sorted_with_boundary_keys() {
    let source = r#"
        use crate::std::collections::TreeMap;
        use crate::std::option::Option;
        use crate::std::iter::Iterator;

        fun main() -> i32 {
            let mut tree: TreeMap<i32, i32> = TreeMap::new();
            // Insert in scrambled order including both i32 extremes.
            tree.insert(0i32, 0i32);
            tree.insert(-2147483648i32, 1i32);
            tree.insert(2147483647i32, 2i32);
            tree.insert(5i32, 3i32);
            tree.insert(-5i32, 4i32);
            if tree.len() != 5usize { return 1; }

            // In-order walk: strictly ascending, extremes first and last.
            let mut iter = tree.iter();
            let mut previous: Option<i32> = Option::None;
            let mut count = 0usize;
            let mut first_key = 0i32;
            let mut last_key = 0i32;
            loop {
                match iter.next() {
                    Option::Some((key, _value)) => {
                        if count == 0usize { first_key = *key; }
                        last_key = *key;
                        match previous {
                            Option::Some(p) => { if *key <= p { return 2; } },
                            Option::None => {},
                        }
                        previous = Option::Some(*key);
                        count += 1usize;
                    },
                    Option::None => { break; },
                }
            }
            if count != 5usize { return 3; }
            if first_key != -2147483648i32 || last_key != 2147483647i32 { return 4; }

            // Removing either extreme keeps the middle ordered and lookupable.
            match tree.remove(&-2147483648i32) {
                Option::Some(value) => { if value != 1i32 { return 5; } },
                Option::None => { return 6; },
            }
            match tree.remove(&2147483647i32) {
                Option::Some(value) => { if value != 2i32 { return 7; } },
                Option::None => { return 8; },
            }
            if tree.len() != 3usize || !tree.contains_key(&0i32) { return 9; }

            // A missing lookup and reinsert after boundary removals.
            if tree.get(&-2147483648i32).is_some() { return 10; }
            tree.insert(-2147483648i32, 9i32);
            if tree.get(&-2147483648i32).is_none() { return 11; }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn tree_set_orders_and_removes_boundary_values() {
    let source = r#"
        use crate::std::collections::TreeSet;

        fun main() -> i32 {
            let mut set: TreeSet<i32> = TreeSet::new();
            set.insert(10i32);
            set.insert(-10i32);
            set.insert(-2147483648i32);
            set.insert(2147483647i32);
            set.insert(0i32);
            if set.len() != 5usize { return 1; }

            // Re-inserting an existing value is a no-op.
            set.insert(0i32);
            if set.len() != 5usize { return 2; }

            if !set.contains(&-2147483648i32) || !set.contains(&2147483647i32) { return 3; }
            if !set.remove(&-2147483648i32) { return 4; }
            if set.contains(&-2147483648i32) || set.len() != 4usize { return 5; }
            if set.remove(&-2147483648i32) { return 6; }

            // Boundary removal keeps the rest addressable.
            if !set.contains(&0i32) || !set.contains(&10i32) { return 7; }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn hash_map_entry_runs_the_rust_idiom_end_to_end() {
    let source = r#"
        use crate::std::collections::HashMap;

        fun main() -> i32 {
            let mut counts: HashMap<i32, i32> = HashMap::new();

            // Vacant path: inserts 0, then the slot is writable.
            let slot = counts.entry(7i32).or_insert(0i32);
            *slot = *slot + 1i32;
            let observed = match counts.get(&7i32) { Option::Some(v) => *v, Option::None => -1i32 };
            if observed != 1i32 { return 1; }

            // Occupied path: the default is dropped, the stored value stays.
            let again = counts.entry(7i32).or_insert(100i32);
            if *again != 1i32 { return 2; }
            *again = *again + 1i32;

            // Lazy default: the lambda must not run for occupied entries.
            let mut produced = 0i32;
            let third = counts.entry(7i32).or_insert_with([ -> { produced = produced + 1i32; 5i32 }]);
            if *third != 2i32 { return 3; }
            if produced != 0i32 { return 4; }

            // A vacant entry runs the default exactly once.
            let fourth = counts.entry(9i32).or_insert_with([ -> { produced = produced + 1i32; 5i32 }]);
            if *fourth != 5i32 || produced != 1i32 { return 5; }

            if counts.len() != 2usize { return 6; }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn slice_iter_next_back_walks_both_directions() {
    // Regression: `next_back` used to return the final element without a
    // back cursor, so a backward walk looped on the last value forever.
    let source = r#"
        use crate::std::iter::{DoubleEndedIterator, Iterator};

        fun main() -> i32 {
            let values = [3i32, 7i32, 4i32, 9i32, 5i32];
            let slice: &[i32] = &values;

            // A pure backward walk visits every element exactly once, last
            // to first.
            let mut iter = slice.iter();
            let mut count = 0usize;
            let mut first_seen = 0i32;
            let mut last_seen = 0i32;
            loop {
                match iter.next_back() {
                    Option::Some(value) => {
                        if count == 0usize { last_seen = *value; }
                        first_seen = *value;
                        count += 1usize;
                    },
                    Option::None => { break; },
                }
            }
            if count != 5usize || last_seen != 5i32 || first_seen != 3i32 { return 1; }

            // The exhausted iterator stays exhausted from both ends.
            if iter.next().is_some() || iter.next_back().is_some() { return 2; }

            // Interleaved ends never overlap or repeat.
            let mut mixed = slice.iter();
            if *mixed.next().unwrap_or(&0i32) != 3i32 { return 3; }
            if *mixed.next_back().unwrap_or(&0i32) != 5i32 { return 4; }
            if *mixed.next().unwrap_or(&0i32) != 7i32 { return 5; }
            if *mixed.next_back().unwrap_or(&0i32) != 9i32 { return 6; }
            if *mixed.next().unwrap_or(&0i32) != 4i32 { return 7; }
            if mixed.next().is_some() || mixed.next_back().is_some() { return 8; }

            // Empty and single-element slices terminate immediately.
            let empty = [0i32; 0usize];
            let empty_slice: &[i32] = &empty;
            if empty_slice.iter().next_back().is_some() { return 9; }
            let single = [42i32];
            let single_slice: &[i32] = &single;
            let mut single_iter = single_slice.iter();
            if *single_iter.next_back().unwrap_or(&0i32) != 42i32 { return 10; }
            if single_iter.next_back().is_some() { return 11; }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn float_hashes_distinguish_values_by_bit_pattern() {
    // Regression: `Hash for f32/f64` used to cast the value to `usize`,
    // truncating every fraction below 1.0 onto the same hash.
    let source = r#"
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
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn float_hashes_fold_the_exact_bit_pattern() {
    // `f64_bits` reads the value's bytes through a `&[u8]` view, so a wrong byte
    // order or a short fold still leaves "these two hashes differ" true for most
    // pairs. Pinning the exact hash of `1.5` / `2.25` (fmix64 of
    // 0x3FF8000000000000 / 0x4002000000000000) catches that. The last pair
    // records the documented `-0.0` / `0.0` split: they compare equal yet hash
    // apart, which is safe only while floats implement `PartialEq` but not `Eq`.
    // The two folds are printed and compared here because the expected pattern
    // is 64 bits wide while `usize` is 32 bits on some targets.
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
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
    let hashes = stdout
        .split_whitespace()
        .map(|text| text.parse::<usize>().expect("hash printed as a number"))
        .collect::<Vec<_>>();
    assert_eq!(
        hashes,
        vec![
            hash_fold(9_826_234_843_501_278_960),
            hash_fold(2_653_818_900_198_545_032)
        ],
        "stdout: {stdout}"
    );
}

#[test]
fn float_display_handles_nan_infinity_and_negative_zero() {
    // Regression: `write_float` used to print NaN as `0.000000` and the
    // infinities as `±18446744073709551615.000000`, dropped the sign of
    // negative zero, and mangled any finite value at or above 2^64.
    let source = r#"
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
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
    assert!(stdout.contains("nan=NaN"), "stdout: {stdout}");
    assert!(stdout.contains("dbg=NaN"), "stdout: {stdout}");
    assert!(stdout.contains("pos=inf"), "stdout: {stdout}");
    assert!(stdout.contains("neg=-inf"), "stdout: {stdout}");
    assert!(stdout.contains("zero=-0.000000"), "stdout: {stdout}");
    assert!(stdout.contains("f32=0.250000"), "stdout: {stdout}");
    assert!(
        stdout.contains("big=100000000000000000000.000000"),
        "stdout: {stdout}"
    );
    assert!(
        // The exact decimal expansion (`{:.0}`), not the shortest round-trip
        // form `Display` picks; the Riddle formatter prints exact digits.
        stdout.contains(&format!("dmax={:.0}.000000", f64::MAX)),
        "stdout: {stdout}"
    );
}

#[test]
fn stdin_read_line_decodes_utf8_and_rejects_invalid_bytes() {
    let source = r#"
        use crate::std::io;
        use crate::std::result::Result;
        use crate::std::string::String;

        fun main() -> i32 {
            let mut line = String::new();
            match io::read_line(&mut line) {
                Result::Ok(()) => {},
                Result::Err(_) => { return 2; },
            }
            println!("got={line}");
            0
        }
    "#;
    let (code, stdout) = compile_and_run_with_stdin(source, true, "你好，riddle\n".as_bytes());
    assert_eq!(code, 0, "stdout: {stdout}");
    assert!(stdout.contains("got=你好，riddle"), "stdout: {stdout}");

    let invalid = r#"
        use crate::std::io;
        use crate::std::io::ReadError;
        use crate::std::result::Result;
        use crate::std::string::String;

        fun main() -> i32 {
            let mut line = String::new();
            match io::read_line(&mut line) {
                Result::Ok(()) => { return 3; },
                Result::Err(error) => {
                    match error {
                        ReadError::InvalidUtf8 => {},
                        ReadError::EndOfFile => { return 4; },
                    }
                },
            }
            if !line.is_empty() { return 5; }
            println!("rejected");
            0
        }
    "#;
    let (code, stdout) = compile_and_run_with_stdin(invalid, true, b"\xff\xfe bad \xff\n");
    assert_eq!(code, 0, "stdout: {stdout}");
    assert!(stdout.contains("rejected"), "stdout: {stdout}");
}

#[test]
fn buf_reader_read_line_decodes_utf8_across_lines() {
    let source = r#"
        use crate::std::fs::write;
        use crate::std::io::{BufReader, ReadError};
        use crate::std::result::Result;
        use crate::std::string::String;

        fun main() -> i32 {
            match write("riddle_utf8_lines.tmp", "héllo wörld 你好\nsecond line") {
                Result::Ok(()) => {},
                Result::Err(_) => { return 1; },
            }
            let mut reader = match BufReader::open("riddle_utf8_lines.tmp") {
                Result::Ok(reader) => reader,
                Result::Err(_) => { return 2; },
            };
            let mut line = String::new();
            match reader.read_line(&mut line) {
                Result::Ok(()) => {},
                Result::Err(_) => { return 3; },
            }
            println!("first={line}");
            match reader.read_line(&mut line) {
                Result::Ok(()) => {},
                Result::Err(_) => { return 4; },
            }
            println!("second={line}");
            match reader.read_line(&mut line) {
                Result::Err(ReadError::EndOfFile) => {},
                _ => { return 5; },
            }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
    assert!(
        stdout.contains("first=héllo wörld 你好"),
        "stdout: {stdout}"
    );
    assert!(stdout.contains("second=second line"), "stdout: {stdout}");
}

#[test]
fn collections_sets_and_map_views_iterate() {
    let source = r#"
        use crate::std::collections::{HashMap, HashSet, TreeMap, TreeSet};

        fun main() -> i32 {
            let mut hash: HashSet<i32> = HashSet::new();
            hash.insert(3);
            hash.insert(1);
            hash.insert(4);
            hash.insert(1);
            if hash.len() != 3usize { return 1; }

            let mut hash_out = 0usize;
            for value in &hash {
                hash_out = hash_out * 10usize + (*value as usize);
            }
            if hash_out != 314usize { return 2; }

            let mut tree: TreeSet<i32> = TreeSet::new();
            tree.insert(5);
            tree.insert(1);
            tree.insert(9);
            tree.insert(3);
            let mut tree_out = 0usize;
            for value in &tree {
                tree_out = tree_out * 10usize + (*value as usize);
            }
            if tree_out != 1359usize { return 3; }

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
            if keys != 1020304050usize { return 4; }

            let mut values = 0usize;
            for value in map.values() {
                values = values * 10usize + (*value as usize);
            }
            if values != 12345usize { return 5; }

            let mut range_out = 0usize;
            for (key, value) in map.range(&20, &50) {
                range_out = range_out * 100usize + (*key as usize);
                if *value != *key / 10 { return 6; }
            }
            if range_out != 203040usize { return 7; }

            let mut none = 0usize;
            for (key, _) in map.range(&35, &40) {
                none = none * 100usize + (*key as usize);
            }
            if none != 0usize { return 8; }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn vector_sorts_large_inputs_in_n_log_n_moves() {
    let source = r#"
        use crate::std::cmp::{Ord, Ordering, PartialEq, PartialOrd};
        use crate::std::option::Option;

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
            while i < 500usize {
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

            // Stability: equal keys keep push order (tag ascending within
            // equal keys exactly as inserted).
            let mut pairs: Vector<Pair> = Vector::new();
            let mut k = 0usize;
            while k < 64usize {
                pairs.push(Pair { key: (k % 4usize) as i32, tag: k as i32 });
                k += 1usize;
            }
            pairs.sort();
            let mut m: usize = 1usize;
            while m < pairs.len() {
                if pairs[m - 1usize].key > pairs[m].key { return 2; }
                if pairs[m - 1usize].key == pairs[m].key
                    && pairs[m - 1usize].tag > pairs[m].tag { return 3; }
                m += 1usize;
            }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn tuple_trait_methods_dispatch_through_std_impls() {
    let source = r#"
        use crate::std::collections::{HashMap, TreeMap};

        fun main() -> i32 {
            let a = (1, 2);
            let b = (0, 9);
            if !b.lt(&a) { return 1; }
            if a.eq(&b) { return 2; }
            match a.cmp(&a) {
                crate::std::cmp::Ordering::Equal => {},
                _ => { return 3; },
            }
            let same = (1, 2);
            if !(a == same && a != b && a > b) { return 4; }

            // Generic dispatch through tuple impls: sort and ordered maps.
            let mut v: Vector<(i32, i32)> = Vector::new();
            v.push((3, 1));
            v.push((1, 2));
            v.push((1, 1));
            v.sort();
            if v[0].0 != 1 || v[0].1 != 1 { return 5; }
            if v[1].0 != 1 || v[1].1 != 2 { return 6; }
            if v[2].0 != 3 { return 7; }

            // Nested tuples: the outer impl dispatches `.lt` into the inner
            // tuple impl, recursing through the same generic machinery.
            let mut nested: Vector<((i32, i32), i32)> = Vector::new();
            nested.push(((1, 0), 5));
            nested.push(((0, 9), 5));
            nested.push(((1, 0), 2));
            nested.sort();
            if (nested[0].0).0 != 0 { return 20; }
            if nested[1].1 != 2 { return 21; }
            if nested[2].1 != 5 { return 22; }

            let mut counts: HashMap<(i32, i32), i32> = HashMap::new();
            counts.insert((1, 2), 10);
            counts.insert((1, 2), 20);
            counts.insert((2, 1), 30);
            match counts.get(&(1, 2)) {
                crate::std::option::Option::Some(value) => { if *value != 20 { return 8; } },
                crate::std::option::Option::None => { return 9; },
            }

            let mut ordered: TreeMap<(i32, i32), i32> = TreeMap::new();
            ordered.insert((2, 0), 1);
            ordered.insert((1, 9), 2);
            let mut first: (i32, i32) = (9, 9);
            let low = (0, 0);
            let high = (2, 0);
            for (key, _) in ordered.range(&low, &high) {
                first = *key;
                break;
            }
            if first.0 != 1 || first.1 != 9 { return 10; }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn match_or_patterns_dispatch_through_c() {
    let source = r#"
        enum Color { Red, Green, Blue, Custom(i32) }

        fun band(x: i32) -> i32 {
            match x {
                1 | 2 | 3 => 10,
                10 => 20,
                _ => 30,
            }
        }

        fun name(color: Color) -> i32 {
            match color {
                Color::Red | Color::Green => 1,
                Color::Custom(_) | Color::Blue => 2,
            }
        }

        fun guarded(x: i32) -> i32 {
            match x {
                1 | 2 | 3 if x > 2 => 1,
                1 | 2 | 3 => 2,
                y if y > 100 => 3,
                _ => 4,
            }
        }

        fun main() -> i32 {
            if band(2) != 10 { return 1; }
            if band(10) != 20 { return 2; }
            if band(99) != 30 { return 3; }
            if name(Color::Red) != 1 { return 4; }
            if name(Color::Blue) != 2 { return 5; }
            if name(Color::Custom(7)) != 2 { return 6; }
            if name(Color::Green) != 1 { return 7; }
            if guarded(3) != 1 { return 8; }
            if guarded(1) != 2 { return 9; }
            if guarded(200) != 3 { return 10; }
            if guarded(50) != 4 { return 11; }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn bool_bitwise_ops_are_eager_through_c() {
    // The interpreter must agree with the C backend here: `&`/`|`/`^` on
    // `bool` are eager operators, while `&&`/`||` short-circuit.
    let source = r#"
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
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn indexing_through_array_references_through_c() {
    let source = r#"
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
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn ordering_values_compare_through_c() {
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
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
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
            // Value equality through distinct addresses.
            if !(a == b) { return 1; }
            if a != b { return 2; }
            // Ordering operators on references.
            let small = 3;
            let big = 9;
            if !(&small < &big) { return 3; }
            if !(&big > &small) { return 4; }
            if &small >= &big { return 5; }
            // Method syntax resolves through the pointee impl.
            if !a.eq(b) { return 6; }
            // cmp on references and on the comparison result.
            if a.cmp(b) != Ordering::Equal { return 7; }
            if small.cmp(&big) != Ordering::Less { return 8; }
            // Generic context goes through the trait bound.
            if !ref_eq(&p, &q) { return 9; }
            if ref_eq(&small, &big) { return 10; }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn debug_formats_std_sum_types() {
    let source = r#"
        use crate::std::cmp::Ordering;
        use crate::std::option::Option;
        use crate::std::string::String;
        use crate::std::vector::Vector;

        fun main() -> i32 {
            let some = Option::Some(42);
            let none: Option<i32> = Option::None;
            let ok: crate::std::result::Result<i32, String> = crate::std::result::Result::Ok(7);
            let err: crate::std::result::Result<i32, String> =
                crate::std::result::Result::Err(String::from("boom"));
            let mut v: Vector<i32> = Vector::new();
            v.push(1);
            v.push(2);
            println!(
                "{:?}|{:?}|{:?}|{:?}|{:?}|{:?}|{:?}",
                some, none, ok, err, Ordering::Less, Ordering::Equal, Option::Some(v),
            );
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
    assert_eq!(
        stdout.trim(),
        r#"Some(42)|None|Ok(7)|Err("boom")|Less|Equal|Some([1, 2])"#,
        "stdout: {stdout}"
    );
}

#[test]
fn private_helper_with_std_name_compiles() {
    let source = r#"
        use crate::std::collections::hash_map::HashMap;

        fun mix64(value: u64) -> u64 {
            value ^ 0xdead_beef_dead_beefu64
        }

        fun main() -> i32 {
            // std's own private `mix64` (hash.rid) must not collide with the
            // user's same-named helper in the generated C symbols.
            let mut map: HashMap<i32, i32> = HashMap::new();
            map.insert(1, 10);
            map.insert(2, 20);
            let probe = mix64(7u64);
            if map.len() == 2usize && probe != 0 { 0 } else { 1 }
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
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
            // Manual `next` calls on an `Item = &T` iterator: the returned
            // element references borrow the stored buffer loan, not the
            // iterator, so consecutive calls are fine.
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
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn tree_map_iter_supports_manual_consecutive_next_calls() {
    let source = r#"
        use crate::std::collections::TreeMap;
        use crate::std::option::Option;
        use crate::std::iter::Iterator;

        fun main() -> i32 {
            let mut tree: TreeMap<i32, i32> = TreeMap::new();
            tree.insert(10i32, 1);
            tree.insert(20i32, 2);
            tree.insert(30i32, 3);
            // Tree iterators reach their elements through `Vector::get`,
            // whose provenance is trackable again after the raw-pointer
            // detour was replaced by the slice accessor.
            let mut iter = tree.iter();
            let a = iter.next();
            let b = iter.next();
            let c = iter.next();
            let done = iter.next();
            let av = match a { Option::Some((k, _)) => *k, _ => -1 };
            let bv = match b { Option::Some((k, _)) => *k, _ => -1 };
            let cv = match c { Option::Some((k, _)) => *k, _ => -1 };
            let dn = match done { Option::Some(_) => 1, _ => 0 };
            if av == 10 && bv == 20 && cv == 30 && dn == 0 { 0 } else { 1 }
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

/// A struct that reaches itself through a reference lowers to a shared
/// definition whose own field points back at it. The C backend has to emit the
/// forward `typedef`, keep the back edge a pointer, and still compute field
/// offsets for the recursive occurrence.
#[test]
fn recursive_struct_runs_through_the_c_backend() {
    let (code, stdout) = compile_and_run(
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

        fun main() -> i32 {
            let tail = Node { data: 2, next: Option::None };
            let middle = Node { data: 3, next: Option::Some(&tail) };
            let head = Node { data: 1, next: Option::Some(&middle) };
            sum(&head) - 6
        }
        "#,
        true,
    );

    assert_eq!(code, 0, "stdout: {stdout}");
    assert_eq!(stdout, "");
}

/// The same shape without the GC: owner-scope freeing still has to walk the
/// recursive definition.
#[test]
fn recursive_struct_runs_without_gc() {
    let (code, stdout) = compile_and_run(
        r#"
        struct Node {
            data: i32,
            next: Option<&Node>,
        }

        fun depth(node: &Node) -> i32 {
            match node.next {
                Option::Some(next) => 1 + depth(next),
                Option::None => 0,
            }
        }

        fun main() -> i32 {
            let tail = Node { data: 2, next: Option::None };
            let middle = Node { data: 3, next: Option::Some(&tail) };
            let head = Node { data: 1, next: Option::Some(&middle) };
            depth(&head) - 2
        }
        "#,
        false,
    );

    assert_eq!(code, 0, "stdout: {stdout}");
    assert_eq!(stdout, "");
}

#[test]
fn std_fs_read_on_write_only_handle_reports_read_failed() {
    // `fread` returns a short count for both end of stream and failure; the
    // stream's error indicator is what tells them apart. A write-only handle
    // is a portable way to force a real read failure.
    let source = r#"
        use crate::std::fs::{write, FsError, FsFile};
        use crate::std::result::Result;

        fun main() -> i32 {
            match write("riddle_fs_readfail.tmp", "content") {
                Result::Ok(()) => {},
                Result::Err(_) => { return 1; },
            }
            let mut file = match FsFile::create("riddle_fs_readfail.tmp") {
                Result::Ok(file) => file,
                Result::Err(_) => { return 2; },
            };
            let mut buffer = [0u8; 8];
            match file.read(&mut buffer) {
                Result::Err(FsError::ReadFailed) => {},
                Result::Err(_) => { return 3; },
                Result::Ok(count) => {
                    // Empty reads are legal and must stay distinguishable.
                    if count != 0usize { return 4; }
                    return 5;
                },
            }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    let _ = fs::remove_file("riddle_fs_readfail.tmp");
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn slice_methods_run_on_array_values() {
    // Codegen used to hand the slice instance a thin `&[T; N]`, which the C
    // compiler rejected; the array now fattens into `{ptr, len}`.
    let source = r#"
        fun main() -> i32 {
            let a = [4, 5, 6];
            if a.len() != 3usize { return 1; }
            if a.is_empty() { return 2; }
            let mut total = 0;
            let mut index = 0usize;
            while index < a.len() {
                total += a[index];
                index += 1usize;
            }
            if total != 15 { return 3; }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn hash_map_accepts_str_slice_keys() {
    let source = r#"
        use crate::std::collections::HashMap;
        use crate::std::hash::Hash;
        use crate::std::string::String;

        fun main() -> i32 {
            let mut counts: HashMap<&str, i32> = HashMap::new();
            counts.insert("alpha", 1);
            counts.insert("beta", 2);
            counts.insert("alpha", 3);
            if counts.len() != 2usize { return 1; }
            let alpha = match counts.get(&"alpha") {
                Option::Some(value) => *value,
                Option::None => -1,
            };
            if alpha != 3 { return 2; }
            if !counts.contains_key(&"beta") { return 3; }
            // `String` and `&str` fold the same bytes with the same algorithm.
            let owned = String::from("alpha");
            if owned.as_str().hash() != "alpha".hash() { return 4; }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn reference_self_trait_impl_methods_resolve() {
    // `impl Clone for &str` spells `Self` as a reference, so a borrowed
    // receiver only reaches it by borrowing itself; the lookup used to strip
    // the borrow unconditionally and report the method as unknown.
    let source = r#"
        use crate::std::clone::Clone;
        use crate::std::string::String;

        fun main() -> i32 {
            let owned = String::from("alpha");
            let text: &str = owned.as_str();
            let copied = text.clone();
            if copied.len() != 5usize { return 1; }
            if copied != "alpha" { return 2; }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}

#[test]
fn destructuring_assignment_writes_through_to_targets() {
    let source = r#"
        struct Point { x: i32, y: i32 }

        fun main() -> i32 {
            let mut a = 0;
            let mut b = 0;
            (a, b) = (7, 9);
            if a != 7 || b != 9 { return 1; }
            let mut swapped_a = 0;
            let mut swapped_b = 0;
            (swapped_a, swapped_b) = (b, a);
            if swapped_a != 9 || swapped_b != 7 { return 2; }
            let mut point = Point { x: 0, y: 0 };
            Point { x: point.x, y: point.y } = Point { x: 3, y: 4 };
            if point.x != 3 || point.y != 4 { return 3; }
            // Fields pair by name, not by the order they are written in.
            let mut first = 0;
            let mut second = 0;
            Point { y: second, x: first } = point;
            if first != 3 || second != 4 { return 4; }
            let (mut inner_left, mut inner_right) = (0, 0);
            ((inner_left, inner_right), a) = ((5, 6), 7);
            if inner_left != 5 || inner_right != 6 || a != 7 { return 5; }
            0
        }
    "#;
    let (code, stdout) = compile_and_run(source, true);
    assert_eq!(code, 0, "stdout: {stdout}");
}
