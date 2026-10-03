use super::*;

fn hierarchy_index(name: &str, source: &str) -> (PathBuf, ProjectIndex) {
    let root = temp_root(name);
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(
        root.join("Clue.toml"),
        "[package]\nname = \"app\"\n\n[dependencies]\n",
    )
    .unwrap();
    fs::write(root.join("src/main.rid"), source).unwrap();
    let index = project_index_for_root(
        &root,
        &HashMap::new(),
        CompileOptions { use_std: false },
        &AnalysisSessions::default(),
    )
    .unwrap()
    .unwrap();
    (root, index)
}

#[test]
fn trait_call_targets_trait_method_declaration() {
    let source = "trait Render { fun draw(&self); }\nstruct Canvas {}\nimpl Render for Canvas { fun draw(&self) {} }\nfun caller(value: Canvas) { value.draw(); }\n";
    let (root, index) = hierarchy_index("call-hierarchy-trait", source);
    let caller = index
        .symbols
        .iter()
        .find(|symbol| symbol.name == "caller")
        .unwrap();
    let edge = index
        .calls
        .iter()
        .find(|edge| edge.caller == caller.key)
        .unwrap();
    let target = index
        .symbols
        .iter()
        .find(|symbol| symbol.key == edge.target)
        .unwrap();

    assert_eq!(target.name, "draw");
    // `key` covers the whole declaration while the identifier is remembered
    // separately, so "where the symbol is" is the name range.
    assert_eq!(
        target.name_source_range.start,
        u32::try_from(source.find("draw").unwrap()).unwrap()
    );
    assert!(target.key.start < target.name_source_range.start);
    assert_eq!(edge.sites.len(), 1);
    let _ = fs::remove_dir_all(root);
}

#[test]
fn type_hierarchy_connects_traits_children_and_implementers() {
    let source = "trait Base {}\ntrait Child: Base {}\nstruct Value {}\nimpl Base for Value {}\n";
    let (root, index) = hierarchy_index("type-hierarchy-relations", source);
    let base = &index
        .symbols
        .iter()
        .find(|symbol| symbol.name == "Base")
        .unwrap()
        .key;
    let mut subtype_names = index.types.subtypes[base]
        .iter()
        .filter_map(|key| index.symbols.iter().find(|symbol| symbol.key == *key))
        .map(|symbol| symbol.name.as_str())
        .collect::<Vec<_>>();
    subtype_names.sort_unstable();

    assert_eq!(subtype_names, ["Child", "Value"]);
    let _ = fs::remove_dir_all(root);
}
