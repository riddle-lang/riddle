use super::*;

#[test]
fn workspace_index_warms_project_without_open_documents() {
    let root = temp_root("workspace-index-warm");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    fs::write(
        root.join("src/main.rid"),
        "pub fun workspace_value() -> i32 { 1 }\n",
    )
    .unwrap();

    let index = project_index_for_root(
        &root,
        &HashMap::new(),
        CompileOptions::default(),
        &AnalysisSessions::default(),
    )
    .unwrap()
    .unwrap();

    assert!(
        index
            .symbols
            .iter()
            .any(|symbol| symbol.name == "workspace_value")
    );
    let _ = fs::remove_dir_all(root);
}

#[test]
fn project_index_contains_unopened_module_symbols_and_files() {
    let root = temp_root("workspace-index-project");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    let main_path = root.join("src/main.rid");
    let main_source = "mod util;\nfun main() { util::helper(); }\n";
    fs::write(&main_path, main_source).unwrap();
    let util_path = root.join("src/util.rid");
    fs::write(&util_path, "pub fun helper() {}\n").unwrap();
    let main_uri = lsp_types::Url::from_file_path(fs::canonicalize(&main_path).unwrap()).unwrap();
    let docs = HashMap::from([(main_uri.clone(), Document::new(main_source, Some(1)))]);

    let index = project_index_for_document(
        &main_uri,
        &docs,
        CompileOptions::default(),
        &AnalysisSessions::default(),
    )
    .unwrap()
    .unwrap();

    assert!(index.revision > 0);
    assert!(index.files.contains(&fs::canonicalize(util_path).unwrap()));
    assert!(index.symbols.iter().any(|symbol| symbol.name == "main"));
    assert!(index.symbols.iter().any(|symbol| symbol.name == "helper"));
    let symbols = workspace_symbols_for_index(&index, "helper");
    assert!(symbols.iter().any(|symbol| symbol.name == "helper"));
    assert!(symbols.iter().all(|symbol| symbol.location.uri != main_uri));
    let _ = fs::remove_dir_all(root);
}

#[test]
fn workspace_index_preserves_nested_members_and_public_visibility() {
    let root = temp_root("workspace-index-members");
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    fs::write(
        root.join("src/main.rid"),
        r#"
        pub mod api {
            pub struct Widget { pub value: i32, hidden: i32 }
            pub enum Choice { Ready, Waiting }
            pub trait Read { pub fun read(&self) -> i32; }
            impl Widget {
                pub fun read(&self) -> i32 { self.value }
            }
        }
        fun private_helper() {}
        "#,
    )
    .unwrap();

    let index = project_index_for_root(
        &root,
        &HashMap::new(),
        CompileOptions {
            use_std: false,
            ..Default::default()
        },
        &AnalysisSessions::default(),
    )
    .unwrap()
    .unwrap();

    let value = workspace_symbols_for_index(&index, "value");
    assert_eq!(value.len(), 1);
    assert_eq!(value[0].container_name.as_deref(), Some("api::Widget"));
    let ready = workspace_symbols_for_index(&index, "ready");
    assert_eq!(ready.len(), 1);
    assert_eq!(ready[0].container_name.as_deref(), Some("api::Choice"));
    let reads = workspace_symbols_for_index(&index, "read");
    assert!(reads.iter().any(|symbol| {
        symbol.container_name.as_deref() == Some("api::Read")
            && symbol.kind == lsp_types::SymbolKind::FUNCTION
    }));
    assert!(workspace_symbols_for_index(&index, "hidden").is_empty());
    assert!(workspace_symbols_for_index(&index, "private_helper").is_empty());
    let _ = fs::remove_dir_all(root);
}

#[test]
fn stale_project_index_generation_is_not_installed() {
    let workspace = WorkspaceState::default();
    let project = PathBuf::from("project");
    let stale = workspace.begin_rebuild(&project);
    let current = workspace.begin_rebuild(&project);

    assert!(!workspace.install(
        stale,
        ProjectIndex::empty(project.clone(), 1, std::iter::empty())
    ));
    assert!(workspace.install(
        current,
        ProjectIndex::empty(project.clone(), 2, std::iter::empty())
    ));
    assert_eq!(workspace.snapshot(&project).unwrap().revision, 2);
}

#[test]
fn shared_dependency_change_invalidates_every_dependent_snapshot() {
    let workspace = WorkspaceState::default();
    let shared = PathBuf::from("shared/src/lib.rid");
    let first = PathBuf::from("first");
    let second = PathBuf::from("second");

    let first_token = workspace.begin_rebuild(&first);
    assert!(workspace.install(
        first_token,
        ProjectIndex::empty(first.clone(), 1, [shared.clone()])
    ));
    let second_token = workspace.begin_rebuild(&second);
    assert!(workspace.install(
        second_token,
        ProjectIndex::empty(second.clone(), 1, [shared.clone()])
    ));

    assert_eq!(workspace.invalidate_path(&shared), [first, second]);
    assert!(workspace.snapshots().is_empty());
}
