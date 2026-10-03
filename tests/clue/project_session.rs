use clue::{ProjectSession, resolve_project_with_session};
use riddlec::pipeline::CompileOptions;
use std::{
    collections::HashMap,
    ffi::OsString,
    fs,
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
    time::{SystemTime, UNIX_EPOCH},
};

fn has_c_compiler() -> bool {
    std::env::var_os("CC")
        .into_iter()
        .chain(
            ["cc", "gcc", "clang", "clang-cl", "cl"]
                .into_iter()
                .map(OsString::from),
        )
        .any(|compiler| {
            let is_msvc = Path::new(&compiler)
                .file_stem()
                .is_some_and(|name| name == "cl" || name == "clang-cl");
            Command::new(&compiler)
                .arg(if is_msvc { "/?" } else { "--version" })
                .output()
                .is_ok_and(|output| output.status.success())
        })
}

#[test]
fn project_session_reuses_analysis_for_unchanged_inputs() {
    let root = temp_root("analysis-cache");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"analysis-cache\"\n\n[dependencies]\n",
    )
    .unwrap();
    fs::write(root.join("src/main.rid"), "fun main() -> i32 { 1 }\n").unwrap();

    let mut session = ProjectSession::default();
    let first = resolve_project_with_session(
        &root,
        &HashMap::new(),
        CompileOptions::default(),
        &mut session,
    )
    .unwrap();
    let second = resolve_project_with_session(
        &root,
        &HashMap::new(),
        CompileOptions::default(),
        &mut session,
    )
    .unwrap();

    assert!(Arc::ptr_eq(&first.result, &second.result));
    let checked = clue::check_project_with_session(
        &root,
        &HashMap::new(),
        CompileOptions::default(),
        &mut session,
    )
    .unwrap();
    let resolved_after_check = resolve_project_with_session(
        &root,
        &HashMap::new(),
        CompileOptions::default(),
        &mut session,
    )
    .unwrap();
    assert!(Arc::ptr_eq(&checked.result, &resolved_after_check.result));
    let _ = fs::remove_dir_all(root);
}

#[test]
fn a_one_character_edit_rechecks_only_the_edited_body() {
    // The editor's warm path: a project is already analysed, the user types one
    // character, and the project is checked again. Only the function that
    // changed may be re-type-checked — everything else, above all the standard
    // library's ~827 bodies, has to come from the incremental cache.
    let root = temp_root("keystroke-reuse");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"keystroke-reuse\"\n\n[dependencies]\n",
    )
    .unwrap();
    let main = root.join("src/main.rid");
    fs::write(&main, "fun main() -> i32 { 1 }\n").unwrap();

    let mut session = ProjectSession::default();
    let mut overlays = HashMap::from([(main.clone(), "fun main() -> i32 { 1 }\n".to_string())]);
    clue::check_project_with_session(&root, &overlays, CompileOptions::default(), &mut session)
        .unwrap();

    let cold = session.last_check_stats();
    assert!(
        cold.checked_bodies > 100,
        "the first check has to build the standard library's cache, got {cold:?}"
    );

    // One character longer, inside the body only.
    overlays.insert(main, "fun main() -> i32 { 100 }\n".to_string());
    clue::check_project_with_session(&root, &overlays, CompileOptions::default(), &mut session)
        .unwrap();

    let warm = session.last_check_stats();
    assert_eq!(
        warm.checked_bodies, 1,
        "one edited body must re-check one body, not {}: {warm:?}",
        warm.checked_bodies
    );
    assert_eq!(
        warm.missed_context, 0,
        "the type context must not move: {warm:?}"
    );
    assert_eq!(
        warm.missed_body, 1,
        "only the edited body changed: {warm:?}"
    );
    assert!(!warm.rebuilt_globals, "globals must be reused: {warm:?}");

    let _ = fs::remove_dir_all(root);
}

fn temp_root(name: &str) -> PathBuf {
    std::env::temp_dir().join(format!(
        "clue-{name}-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ))
}

#[test]
fn project_session_drops_removed_overlays() {
    let root = temp_root("removed-overlay");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"cached\"\n\n[dependencies]\n",
    )
    .unwrap();
    fs::write(
        root.join("src/main.rid"),
        "mod util; fun main() -> i32 { util::value() }\n",
    )
    .unwrap();
    let util = root.join("src/util.rid");
    fs::write(&util, "pub fun value() -> i32 { 1 }\n").unwrap();
    let mut session = ProjectSession::default();
    let stale = HashMap::from([(util, "pub fun other() -> i32 { 1 }\n".to_string())]);

    let first =
        resolve_project_with_session(&root, &stale, CompileOptions::default(), &mut session)
            .unwrap();
    assert!(first.result.hir_diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message
            .contains("unresolved name: `util::value`")
    }));

    let second = resolve_project_with_session(
        &root,
        &HashMap::new(),
        CompileOptions::default(),
        &mut session,
    )
    .unwrap();
    assert!(second.result.hir_diagnostics.iter().all(|diagnostic| {
        !diagnostic
            .message
            .contains("unresolved name: `util::value`")
    }));
    let _ = fs::remove_dir_all(root);
}

#[test]
fn project_accepts_an_unsaved_module_overlay() {
    let root = temp_root("unsaved-module");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"overlay\"\n\n[dependencies]\n",
    )
    .unwrap();
    fs::write(
        root.join("src/main.rid"),
        "mod fresh; fun main() -> i32 { fresh::value() }\n",
    )
    .unwrap();
    let fresh = root.join("src/fresh.rid");
    let overlays = HashMap::from([(fresh, "pub fun value() -> i32 { 7 }\n".to_string())]);

    let analysis = resolve_project_with_session(
        &root,
        &overlays,
        CompileOptions::default(),
        &mut ProjectSession::default(),
    )
    .unwrap();

    assert!(
        analysis.result.success(),
        "{:#?}",
        analysis.result.hir_diagnostics
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn project_session_invalidates_proc_macro_overlays() {
    if !has_c_compiler() {
        eprintln!("skipping proc-macro overlay test: no C compiler found");
        return;
    }
    let root = temp_root("proc-macro-overlay");
    fs::create_dir_all(root.join("macros/src")).unwrap();
    fs::create_dir_all(root.join("app/src")).unwrap();
    fs::write(
        root.join("macros/Clue.toml"),
        "[package]\nname = \"macros\"\n\n[lib]\nproc-macro = true\n\n[dependencies]\n",
    )
    .unwrap();
    let macro_source = root.join("macros/src/lib.rid");
    fs::write(
        &macro_source,
        "#[proc_macro_derive(Value)]\npub fun derive(_input: TokenStream) -> TokenStream { TokenStream::new() }\n",
    )
    .unwrap();
    fs::write(
        root.join("app/Clue.toml"),
        "[package]\nname = \"app\"\n\n[[bin]]\npath = \"src/main.rid\"\n\n[dependencies]\nmacros = { path = \"../macros\" }\n",
    )
    .unwrap();
    fs::write(
        root.join("app/src/main.rid"),
        "#[derive(macros::Value)]\nstruct Value {}\nfun main() -> i32 { generated() }\n",
    )
    .unwrap();

    let macro_version = |value| {
        format!(
            "#[proc_macro_derive(Value)]\npub fun derive(_input: TokenStream) -> TokenStream {{ TokenStream::from_str(\"fun generated() -> i32 {{ {value} }}\").unwrap_or(TokenStream::new()) }}\n"
        )
    };
    let mut session = ProjectSession::default();
    let first = resolve_project_with_session(
        &root.join("app"),
        &HashMap::from([(macro_source.clone(), macro_version(1))]),
        CompileOptions::default(),
        &mut session,
    )
    .unwrap();
    let first_revision = session.revision();
    assert!(first.source.source.contains("generated () -> i32 {1}"));

    let second = resolve_project_with_session(
        &root.join("app"),
        &HashMap::from([(macro_source, macro_version(2))]),
        CompileOptions::default(),
        &mut session,
    )
    .unwrap();
    assert!(session.revision() > first_revision);
    assert!(second.source.source.contains("generated () -> i32 {2}"));
    let _ = fs::remove_dir_all(root);
}
