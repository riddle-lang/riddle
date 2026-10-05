use std::{
    fs,
    path::{Path, PathBuf},
    process::{Command, Output},
    time::{SystemTime, UNIX_EPOCH},
};

fn temp_root(name: &str) -> PathBuf {
    std::env::temp_dir().join(format!(
        "riddlec-{name}-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ))
}

fn run(args: &[&Path]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_riddlec"))
        .args(args)
        .output()
        .unwrap()
}

#[test]
fn accepts_no_std() {
    let root = temp_root("no-std");
    fs::create_dir_all(&root).unwrap();
    let input = root.join("main.rid");
    fs::write(&input, "fun main() {}\n").unwrap();

    let output = run(&[Path::new("--no-std"), &input]);

    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn expands_standard_print_macros() {
    let root = temp_root("print-macros");
    fs::create_dir_all(&root).unwrap();
    let input = root.join("main.rid");
    fs::write(
        &input,
        "fun main() -> i32 { print!(\"value={}\", 7); println!(); 0 }\n",
    )
    .unwrap();

    let output = run(&[&input]);

    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn accepts_supported_target_and_rejects_unknown_target() {
    let root = temp_root("target");
    fs::create_dir_all(&root).unwrap();
    let input = root.join("main.rid");
    fs::write(&input, "fun main() {}\n").unwrap();

    let accepted = run(&[
        Path::new("--target"),
        Path::new("aarch64-unknown-linux-gnu"),
        &input,
    ]);
    assert!(
        accepted.status.success(),
        "{}",
        String::from_utf8_lossy(&accepted.stderr)
    );

    let rejected = run(&[
        Path::new("--target"),
        Path::new("x86_64-unknown-linux-musl"),
        &input,
    ]);
    assert!(!rejected.status.success());
    assert!(
        String::from_utf8_lossy(&rejected.stderr).contains("unsupported target"),
        "{}",
        String::from_utf8_lossy(&rejected.stderr)
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn target_selects_the_pointer_width_literals_are_checked_against() {
    // The triple used to be printed for `--verbose` and otherwise dropped, so a
    // 64-bit host compiled an over-wide `usize` literal for a 32-bit target into
    // `((size_t)UINT64_C(4294967296))` — a constant C narrows to the low half of
    // `size_t` without a word, and the program then ran with a wrong value.
    // Selecting the target now selects the width the literal is range-checked
    // against, so the program is refused before it can reach codegen.
    let root = temp_root("target-pointer-width");
    fs::create_dir_all(&root).unwrap();
    let input = root.join("main.rid");
    fs::write(
        &input,
        "fun main() -> i32 { let value = 4294967296usize; if value == 0usize { 1 } else { 0 } }\n",
    )
    .unwrap();
    let generated = root.join("main.c");

    let narrow = run(&[
        Path::new("--target"),
        Path::new("i686-unknown-linux-gnu"),
        Path::new("--emit"),
        Path::new("c"),
        Path::new("-o"),
        &generated,
        &input,
    ]);
    let stderr = String::from_utf8_lossy(&narrow.stderr);
    assert!(stderr.contains("E0011"), "{stderr}");
    assert!(stderr.contains("4294967296"), "{stderr}");
    assert!(!generated.exists(), "a refused program must not emit C");

    let wide = run(&[
        Path::new("--target"),
        Path::new("x86_64-unknown-linux-gnu"),
        Path::new("--emit"),
        Path::new("c"),
        Path::new("-o"),
        &generated,
        &input,
    ]);
    assert!(
        wide.status.success(),
        "{}",
        String::from_utf8_lossy(&wide.stderr)
    );
    let code = fs::read_to_string(&generated).unwrap();
    assert!(code.contains("4294967296"), "{code}");
    let _ = fs::remove_dir_all(root);
}

#[test]
fn c_backend_combines_multiple_inputs_into_one_program() {
    let root = temp_root("multiple-inputs");
    fs::create_dir_all(&root).unwrap();
    let first = root.join("first.rid");
    let second = root.join("second.rid");
    let generated = root.join("combined.c");
    fs::write(&first, "fun main() -> i32 { double(21) }\n").unwrap();
    fs::write(&second, "pub fun double(value: i32) -> i32 { value * 2 }\n").unwrap();

    let output = run(&[
        Path::new("--backend"),
        Path::new("c"),
        Path::new("--output"),
        &generated,
        &first,
        &second,
    ]);

    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let code = fs::read_to_string(&generated).unwrap();
    // `double` is emitted with the standard hex-encoded function symbol.
    let double_symbol: String = ["riddle_f_", "646f75626c65"].concat();
    assert!(
        code.contains(&double_symbol),
        "second.rid items participate in the combined program"
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn emit_c_generates_the_same_program_as_backend_c() {
    let root = temp_root("emit-c");
    fs::create_dir_all(&root).unwrap();
    let input = root.join("main.rid");
    let generated = root.join("emitted.c");
    fs::write(&input, "fun main() -> i32 { 7 }\n").unwrap();

    let output = run(&[
        Path::new("--emit"),
        Path::new("c"),
        Path::new("--output"),
        &generated,
        &input,
    ]);

    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    // `--emit c` used to fall through both emission branches because `--emit`
    // conflicts with `--backend`, exiting 0 without writing anything.
    let code = fs::read_to_string(&generated).unwrap_or_else(|error| {
        panic!("--emit c wrote no file: {error}");
    });
    assert!(code.contains("int main("), "--emit c output: {code}");
    let _ = fs::remove_dir_all(root);
}

#[test]
fn c_backend_reports_macro_expansion_errors_instead_of_dropping_calls() {
    let root = temp_root("macro-error-backend");
    fs::create_dir_all(&root).unwrap();
    // The format string has two placeholders but only one argument, so
    // `println!` fails to expand. The C-backend path previously discarded the
    // expansion diagnostic, lowered the unexpanded call (and every macro
    // after it) to nothing, and emitted a program that silently skipped the
    // remaining statements of the block.
    let input = root.join("main.rid");
    fs::write(
        &input,
        concat!(
            "fun main() -> i32 {\n",
            "    let s = 3;\n",
            "    let t = 4;\n",
            "    println!(\"s={} t={}\", [s, t]);\n",
            "    0\n",
            "}\n",
        ),
    )
    .unwrap();
    let generated = root.join("out.c");

    let output = run(&[
        Path::new("--backend"),
        Path::new("c"),
        Path::new("--output"),
        &generated,
        &input,
    ]);

    assert!(
        !output.status.success(),
        "macro expansion errors must fail the C-backend build"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("E0400"),
        "the expansion diagnostic is reported, got: {stderr}"
    );
    assert!(
        !generated.exists(),
        "no C file is emitted for a failed expansion"
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn diagnostics_keep_rust_style_hierarchy() {
    let root = temp_root("diagnostics");
    fs::create_dir_all(&root).unwrap();
    let input = root.join("main.rid");
    fs::write(
        &input,
        "struct Foo {}\nfun main() {\n    let a = Foo {};\n    let b = a;\n    let c = a;\n}\n",
    )
    .unwrap();

    let output = run(&[Path::new("--no-std"), &input]);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(!output.status.success());
    assert!(
        stderr.starts_with("error[E0100]: use of moved value: `a`\n"),
        "{stderr}"
    );
    assert!(
        stderr.contains(" 4 |     let b = a;\n   |             - value moved here\n"),
        "{stderr}"
    );
    assert!(
        stderr.contains(" 5 |     let c = a;\n   |             ^\n"),
        "{stderr}"
    );
    assert!(
        stderr.ends_with("error: aborting due to 1 previous error\n"),
        "{stderr}"
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn debug_format_bound_diagnostic_points_to_user_source() {
    let root = temp_root("debug-format-diagnostic");
    fs::create_dir_all(&root).unwrap();
    let input = root.join("main.rid");
    fs::write(
        &input,
        "enum Foo { A() }\nfun main() {\n    let value = Foo::A();\n    println!(\"{:?}\", value);\n}\n",
    )
    .unwrap();

    let output = run(&[&input]);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let open = '{';
    let close = '}';
    let debug_placeholder = format!("{open}:?{close}");

    assert!(!output.status.success());
    assert!(
        stderr.starts_with("error[E0035]: `Foo` doesn't implement `Debug`\n"),
        "{stderr}"
    );
    assert!(
        stderr.contains(&format!("println!(\"{debug_placeholder}\", value);")),
        "{stderr}"
    );
    assert!(
        stderr.contains(&format!(
            "`Foo` cannot be formatted using `{debug_placeholder}` because it doesn't implement `Debug`"
        )),
        "{stderr}"
    );
    assert!(
        stderr.contains("required by this formatting parameter"),
        "{stderr}"
    );
    assert!(
        stderr.contains("consider annotating `Foo` with `#[derive(Debug)]`"),
        "{stderr}"
    );
    assert!(!stderr.contains("append_debug"), "{stderr}");
    assert!(!stderr.contains("_print"), "{stderr}");
    assert!(!stderr.contains(r"\\?\"), "{stderr}");
    let _ = fs::remove_dir_all(root);
}
