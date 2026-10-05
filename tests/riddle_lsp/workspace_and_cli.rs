use super::*;

/// The incremental type checker only pays for itself if the session that owns
/// it survives between edits. A standalone document analysed twice through the
/// same session must go through one session, not two — a fresh session re-checks
/// all ~827 standard-library bodies (measured at ~700 ms in a release build).
#[test]
fn standalone_analyses_reuse_the_incremental_checker_across_runs() {
    let uri = lsp_types::Url::parse("untitled:riddle-incremental-reuse.rid").unwrap();
    let mut sessions = DiagnosticSessions::default();

    let first = collect_workspace_diagnostics_with_sessions(
        &HashMap::from([(
            uri.clone(),
            Document::new("fun main() { let value = 1; }\n", Some(1)),
        )]),
        CompileOptions::default(),
        &mut sessions,
    );
    assert_eq!(first.len(), 1);
    assert_eq!(sessions.standalone_checks(&uri), Some(1));

    let second = collect_workspace_diagnostics_with_sessions(
        &HashMap::from([(
            uri.clone(),
            Document::new("fun main() { let value = 2; }\n", Some(2)),
        )]),
        CompileOptions::default(),
        &mut sessions,
    );
    assert_eq!(second.len(), 1);
    assert_eq!(
        sessions.standalone_checks(&uri),
        Some(2),
        "the second analysis must run in the same session, keeping the \
         standard library's incremental type check"
    );
}

fn write_workspace_project(root: &std::path::Path, name: &str) {
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        format!("[package]\nname = \"{name}\"\n"),
    )
    .unwrap();
    fs::write(root.join("src/main.rid"), "fun main() {}\n").unwrap();
}

#[test]
fn workspace_discovers_nested_clue_projects_and_skips_generated_trees() {
    let root = temp_root("workspace-discovery");
    write_workspace_project(&root.join("app"), "app");
    write_workspace_project(&root.join("libs/math"), "math");
    for ignored in [".git", ".clue", "target", "node_modules", "dist"] {
        write_workspace_project(&root.join(ignored).join("ignored"), "ignored");
    }

    let projects = discover_projects(&root).unwrap();

    assert_eq!(
        projects,
        [
            fs::canonicalize(root.join("app")).unwrap(),
            fs::canonicalize(root.join("libs/math")).unwrap(),
        ]
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn workspace_manifest_limits_lsp_discovery_to_registered_crates() {
    let root = temp_root("workspace-manifest-discovery");
    write_workspace_project(&root.join("app"), "app");
    write_workspace_project(&root.join("libs/math"), "math");
    write_workspace_project(&root.join("extra"), "extra");
    fs::write(
        root.join("Clue.toml"),
        "[workspace]\ncrates = [\"app\", \"libs/math\"]\n",
    )
    .unwrap();

    let projects = discover_projects(&root).unwrap();

    assert_eq!(
        projects,
        [
            fs::canonicalize(root.join("app")).unwrap(),
            fs::canonicalize(root.join("libs/math")).unwrap(),
        ]
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn workspace_roots_can_be_added_and_removed_without_dropping_overlap() {
    let root = temp_root("workspace-roots");
    let cleanup = root.clone();
    let nested = root.join("nested");
    write_workspace_project(&root.join("app"), "app");
    write_workspace_project(&nested.join("library"), "library");
    let workspace = WorkspaceState::default();

    workspace.add_roots([root.clone(), nested.clone()]).unwrap();
    assert_eq!(workspace.projects().len(), 2);

    workspace.remove_roots([nested]);
    assert_eq!(workspace.projects().len(), 2);

    workspace.remove_roots([root]);
    assert!(workspace.projects().is_empty());
    let _ = fs::remove_dir_all(cleanup);
}

#[test]
fn project_diagnostics_follow_peer_overlay_removal() {
    let root = temp_root("peer-overlay-removal");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    let main = root.join("src/main.rid");
    let main_text = "mod util;\nfun main() -> i32 { util::value() }\n".to_string();
    fs::write(&main, &main_text).unwrap();
    let util = root.join("src/util.rid");
    fs::write(&util, "pub fun value() -> i32 { 1 }\n").unwrap();
    let main_uri = lsp_types::Url::from_file_path(&main).unwrap();
    let util_uri = lsp_types::Url::from_file_path(&util).unwrap();
    let mut docs = HashMap::from([
        (main_uri.clone(), Document::new(main_text, Some(1))),
        (
            util_uri.clone(),
            Document::new("pub fun other() -> i32 { 1 }\n", Some(1)),
        ),
    ]);

    let mut sessions = DiagnosticSessions::default();
    let stale = collect_workspace_diagnostics_with_sessions(
        &docs,
        CompileOptions::default(),
        &mut sessions,
    )
    .into_iter()
    .find(|published| published.uri == main_uri)
    .unwrap()
    .diagnostics;
    assert!(
        stale
            .iter()
            .any(|diagnostic| diagnostic.message.contains("unresolved")),
        "{stale:#?}"
    );

    docs.remove(&util_uri);
    let refreshed = collect_workspace_diagnostics_with_sessions(
        &docs,
        CompileOptions::default(),
        &mut sessions,
    )
    .into_iter()
    .find(|published| published.uri == main_uri)
    .unwrap()
    .diagnostics;
    assert!(refreshed.is_empty(), "{refreshed:#?}");
    let _ = fs::remove_dir_all(root);
}

#[test]
fn parse_args_accepts_no_std() {
    let args = vec!["riddle-lsp".into(), "--no-std".into()];
    let opts = parse_args(&args).unwrap();

    assert!(!opts.compile_options.use_std);
}

#[test]
fn parse_args_accepts_completion_delay_ms() {
    let args = vec![
        "riddle-lsp".into(),
        "--completion-delay-ms".into(),
        "25".into(),
    ];
    let opts = parse_args(&args).unwrap();

    assert_eq!(opts.completion_delay, Duration::from_millis(25));
}

#[test]
fn workspace_analysis_can_be_cancelled_between_documents() {
    let docs = HashMap::from([
        (
            lsp_types::Url::parse("untitled:first.rid").unwrap(),
            Document::new("fun first() {}", Some(1)),
        ),
        (
            lsp_types::Url::parse("untitled:second.rid").unwrap(),
            Document::new("fun second() {}", Some(1)),
        ),
    ]);
    let polls = Cell::new(0);
    let result = collect_workspace_diagnostics_cancellable(
        &docs,
        CompileOptions::default(),
        &mut DiagnosticSessions::default(),
        || {
            let next = polls.get() + 1;
            polls.set(next);
            next > 1
        },
    );

    assert!(result.is_none());
    assert_eq!(polls.get(), 2);
}

#[test]
fn document_analysis_can_be_cancelled_before_work_starts() {
    let uri = lsp_types::Url::parse("untitled:cancelled.rid").unwrap();
    let source = "fun value() -> i32 { 1 }";
    let docs = HashMap::from([(uri.clone(), Document::new(source, Some(1)))]);

    let hover = hover_for_document_cancellable(
        &uri,
        &docs,
        Position::new(0, 5),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
        &AnalysisSessions::default(),
        &|| true,
    )
    .unwrap();

    assert!(hover.is_none());
}

#[test]
fn workspace_sessions_observe_project_disk_edits() {
    let root = temp_root("project-session-reuse");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    let main_path = root.join("src/main.rid");
    let main_source = "mod util;\nfun main() { util::value(); }\n";
    fs::write(&main_path, main_source).unwrap();
    let util_path = root.join("src/util.rid");
    fs::write(&util_path, "pub fun value() {}\n").unwrap();
    let main_uri = lsp_types::Url::from_file_path(fs::canonicalize(&main_path).unwrap()).unwrap();
    let docs = HashMap::from([(main_uri, Document::new(main_source, Some(1)))]);
    let mut sessions = DiagnosticSessions::default();

    collect_workspace_diagnostics_with_sessions(&docs, CompileOptions::default(), &mut sessions);

    fs::write(&util_path, "pub fun value() { missing; }\n").unwrap();
    let published = collect_workspace_diagnostics_with_sessions(
        &docs,
        CompileOptions::default(),
        &mut sessions,
    );
    let util_uri = lsp_types::Url::from_file_path(fs::canonicalize(&util_path).unwrap()).unwrap();
    assert!(published.iter().any(|item| {
        item.uri == util_uri
            && item
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.message.contains("unresolved name: `missing`"))
    }));
    let _ = fs::remove_dir_all(root);
}
