//! `clue doc` — generates static HTML API documentation for the current
//! package from HIR item trees and doc comments.
//!
//! Output layout under `.clue/doc/`: one `index.html` per module plus a
//! shared stylesheet. Documentation prose is the item's attached `///`,
//! `/** */`, and trailing `//<` comments, rendered as markdown-lite
//! (headings, `code`, ```` ``` ```` fences, and `-` lists).

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use anyhow::{Context, bail};
use hir::item_tree::{ItemTree, TopLevelItem};
use hir::render;
use hir::{HirFile, Name};
use rowan::TextRange;

/// One documented item, already rendered for HTML output.
struct DocEntry {
    /// Signature block, e.g. `pub fun push(&mut self, value: T)`.
    signature: String,
    /// Rendered markdown-lite documentation, if any.
    docs: Option<String>,
    /// Anchor id within the page.
    anchor: String,
    kind: &'static str,
    /// Methods of an impl or trait block with their rendered docs, listed
    /// under the section header.
    methods: Vec<(String, Option<String>)>,
}

impl DocEntry {
    fn new(signature: String, docs: Option<String>, anchor: String, kind: &'static str) -> Self {
        Self {
            signature,
            docs,
            anchor,
            kind,
            methods: Vec::new(),
        }
    }
}

/// One page of the documentation site.
struct DocPage {
    /// Module path from the crate root, e.g. `collections` (root is "").
    path: String,
    entries: Vec<DocEntry>,
}

pub fn generate(
    root: &Path,
    package: Option<&str>,
    document_private: bool,
    open: bool,
    no_std: bool,
) -> anyhow::Result<PathBuf> {
    let root = if let Some(name) = package {
        let selected = crate::selected_target_packages(root, Some(name), false)?;
        selected
            .into_iter()
            .next()
            .with_context(|| format!("clue doc: no package named `{name}`"))?
    } else {
        root.to_path_buf()
    };
    let root = root.as_path();
    // Documentation has to be checked at the same pointer width a build of
    // this project would use, or an integer literal that only fits a 64-bit
    // `size_t` would pass `clue doc` and fail `clue build`.
    let triple = crate::target::resolve(None, None)?;
    let options = riddlec::pipeline::CompileOptions {
        use_std: !no_std,
        ..crate::analysis_options(triple)
    };
    let analysis = crate::check_project_with_options(root, &HashMap::new(), options)?;
    let hir = analysis
        .result
        .hir
        .as_ref()
        .context("compilation produced no HIR")?;
    if !analysis.result.success() {
        let errors = riddlec::diagnostics::report_mapped(
            &analysis.result,
            &analysis.source,
            &analysis.entry.display().to_string(),
        );
        if errors == 0 {
            eprintln!("clue doc: documentation requires a clean check");
        }
        bail!("documentation requires a clean check");
    }

    let crate_name = analysis.package_name.clone();
    let pages = collect_pages(hir, &crate_name, analysis.package_index, document_private);
    let out_dir = root.join(".clue").join("doc");
    let index = out_dir.join("index.html");
    std::fs::create_dir_all(&out_dir)?;
    write_pages(&out_dir, &crate_name, &pages)?;

    println!("clue: documented `{crate_name}` at {}", index.display());
    if open {
        open_in_browser(&index)?;
    }
    Ok(index)
}

/// Walks the item tree, grouping items by module path.
fn collect_pages(
    hir: &HirFile,
    _crate_name: &str,
    package_index: usize,
    document_private: bool,
) -> Vec<DocPage> {
    let mut pages: Vec<DocPage> = Vec::new();
    let tree = &hir.item_tree;
    walk(
        hir,
        tree,
        &tree.top_level,
        String::new(),
        &mut pages,
        package_index,
        document_private,
    );
    pages
}

fn walk(
    hir: &HirFile,
    tree: &ItemTree,
    items: &[TopLevelItem],
    module_path: String,
    pages: &mut Vec<DocPage>,
    package_index: usize,
    document_private: bool,
) {
    let in_package = |name_range: TextRange| {
        hir.package_for_range(name_range)
            .is_some_and(|index| index == package_index)
    };
    let visible =
        |visibility: &hir::item_tree::Visibility| document_private || visibility.is_public();

    let mut entries: Vec<DocEntry> = Vec::new();
    // rustfmt collapses this type to one line, which trips type_complexity.
    #[allow(clippy::type_complexity)]
    let mut impls: Vec<(String, Option<String>, Vec<(String, Option<String>)>)> = Vec::new();
    for item in items {
        match item {
            TopLevelItem::Function(id) => {
                let function = &tree.functions[*id];
                if !visible(&function.visibility) || !in_package(function.name_range) {
                    continue;
                }
                entries.push(DocEntry::new(
                    render::format_function(function),
                    docs_for(hir, function.name_range),
                    anchor("fun", &function.name),
                    "function",
                ));
            }
            TopLevelItem::Struct(id) => {
                let strukt = &tree.structs[*id];
                if !visible(&strukt.visibility) || !in_package(strukt.name_range) {
                    continue;
                }
                entries.push(DocEntry::new(
                    render::format_struct(strukt),
                    docs_for(hir, strukt.name_range),
                    anchor("struct", &strukt.name),
                    "struct",
                ));
            }
            TopLevelItem::Enum(id) => {
                let enumeration = &tree.enums[*id];
                if !visible(&enumeration.visibility) || !in_package(enumeration.name_range) {
                    continue;
                }
                entries.push(DocEntry::new(
                    render::format_enum(enumeration),
                    docs_for(hir, enumeration.name_range),
                    anchor("enum", &enumeration.name),
                    "enum",
                ));
            }
            TopLevelItem::Trait(id) => {
                let trait_decl = &tree.traits[*id];
                if !visible(&trait_decl.visibility) || !in_package(trait_decl.name_range) {
                    continue;
                }
                let methods = trait_decl
                    .methods
                    .iter()
                    .map(|method| {
                        (
                            render::format_function(method),
                            docs_for(hir, method.name_range),
                        )
                    })
                    .collect();
                entries.push(DocEntry {
                    signature: render::format_trait(trait_decl),
                    docs: docs_for(hir, trait_decl.name_range),
                    anchor: anchor("trait", &trait_decl.name),
                    kind: "trait",
                    methods,
                });
            }
            TopLevelItem::Const(id) => {
                let constant = &tree.consts[*id];
                if !visible(&constant.visibility) || !in_package(constant.name_range) {
                    continue;
                }
                entries.push(DocEntry::new(
                    render::format_const(constant),
                    docs_for(hir, constant.name_range),
                    anchor("const", &constant.name),
                    "const",
                ));
            }
            TopLevelItem::TypeAlias(id) => {
                let alias = &tree.type_aliases[*id];
                if !visible(&alias.visibility) || !in_package(alias.name_range) {
                    continue;
                }
                entries.push(DocEntry::new(
                    render::format_type_alias(alias),
                    docs_for(hir, alias.name_range),
                    anchor("type", &alias.name),
                    "type",
                ));
            }
            TopLevelItem::Module(id) => {
                let module = &tree.modules[*id];
                if !visible(&module.visibility) || !in_package(module.name_range) {
                    continue;
                }
                let Some(nested) = &module.items else {
                    // `mod foo;` without inline items: nothing to document
                    // at this position.
                    continue;
                };
                let child_path = if module_path.is_empty() {
                    module.name.0.clone()
                } else {
                    format!("{}::{}", module_path, module.name.0)
                };
                walk(
                    hir,
                    tree,
                    nested,
                    child_path,
                    pages,
                    package_index,
                    document_private,
                );
            }
            TopLevelItem::Impl(id) => {
                let imp = &tree.impls[*id];
                if !in_package(imp.self_ty_range) {
                    continue;
                }
                let mut methods = Vec::new();
                for method_id in &imp.methods {
                    let function = &tree.functions[*method_id];
                    if !in_package(function.name_range) {
                        continue;
                    }
                    methods.push((
                        render::format_function(function),
                        docs_for(hir, function.name_range),
                    ));
                }
                if !methods.is_empty() {
                    impls.push((
                        render::format_impl(imp),
                        docs_for(hir, imp.self_ty_range),
                        methods,
                    ));
                }
            }
            TopLevelItem::Use(_) => {}
        }
    }

    // Impl blocks render as one section each, listing their methods with
    // their doc comments.
    for (header, docs, methods) in impls {
        entries.push(DocEntry {
            signature: header,
            docs,
            anchor: format!("impl-{}", entries.len()),
            kind: "impl",
            methods,
        });
    }

    pages.push(DocPage {
        path: module_path,
        entries,
    });
}

fn docs_for(hir: &HirFile, name_range: TextRange) -> Option<String> {
    render::doc_comment_for_range(&hir.doc_comments, name_range)
}

fn anchor(kind: &str, name: &Name) -> String {
    format!("{kind}-{}", name.0)
}

fn write_pages(out_dir: &Path, crate_name: &str, pages: &[DocPage]) -> anyhow::Result<()> {
    let files: Vec<(&DocPage, String)> = pages
        .iter()
        .map(|page| {
            let file = if page.path.is_empty() {
                "index.html".to_string()
            } else {
                format!("{}.html", page.path.replace("::", "-"))
            };
            (page, file)
        })
        .collect();

    for (page, file) in &files {
        let mut html = String::new();
        html.push_str("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n");
        html.push_str(&format!(
            "<title>{} — {}</title>\n",
            escape_html(crate_name),
            escape_html(&page.path)
        ));
        html.push_str("<style>");
        html.push_str(STYLE);
        html.push_str("</style>\n</head>\n<body>\n");
        html.push_str("<nav>");
        for (other, other_file) in &files {
            let label = if other.path.is_empty() {
                crate_name.to_string()
            } else {
                other.path.clone()
            };
            html.push_str(&format!(
                "<a href=\"{}\">{}</a>",
                escape_html(other_file),
                escape_html(&label)
            ));
        }
        html.push_str("</nav>\n<main>\n");
        let heading = if page.path.is_empty() {
            crate_name.to_string()
        } else {
            page.path.clone()
        };
        html.push_str(&format!("<h1>{}</h1>\n", escape_html(&heading)));
        if page.entries.is_empty() {
            html.push_str("<p class=\"empty\">No documented items in this module.</p>\n");
        }
        for entry in &page.entries {
            html.push_str(&format!(
                "<section class=\"{}\" id=\"{}\">\n",
                escape_html(entry.kind),
                escape_html(&entry.anchor)
            ));
            html.push_str(&format!(
                "<h2><code>{}</code></h2>\n",
                escape_html(&entry.signature)
            ));
            if let Some(docs) = &entry.docs {
                html.push_str(&render_markdown(docs));
            }
            for (signature, method_docs) in &entry.methods {
                html.push_str("<div class=\"method\"><code>");
                html.push_str(&escape_html(signature));
                html.push_str("</code></div>\n");
                if let Some(docs) = method_docs {
                    html.push_str(&render_markdown(docs));
                }
            }
            html.push_str("</section>\n");
        }
        html.push_str("</main>\n</body>\n</html>\n");
        let path = out_dir.join(file);
        std::fs::write(&path, html).with_context(|| format!("writing {}", path.display()))?;
    }
    Ok(())
}

/// Minimal markdown: headings, code fences, inline code is left as-is
/// (already escaped), bullet lists, paragraphs. Everything is escaped
/// first, so the output is inert HTML.
fn render_markdown(docs: &str) -> String {
    let mut html = String::new();
    let mut in_code = false;
    let mut in_list = false;
    for line in docs.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("```") {
            if in_code {
                html.push_str("</code></pre>\n");
                in_code = false;
            } else {
                html.push_str("<pre><code>");
                in_code = true;
            }
            continue;
        }
        if in_code {
            html.push_str(&escape_html(line));
            html.push('\n');
            continue;
        }
        if let Some(heading) = trimmed.strip_prefix("# ") {
            close_list(&mut html, &mut in_list);
            html.push_str(&format!("<h3>{}</h3>\n", escape_html(heading)));
        } else if let Some(item) = trimmed.strip_prefix("- ") {
            if !in_list {
                html.push_str("<ul>\n");
                in_list = true;
            }
            html.push_str(&format!("<li>{}</li>\n", escape_html(item)));
        } else if trimmed.is_empty() {
            close_list(&mut html, &mut in_list);
        } else {
            close_list(&mut html, &mut in_list);
            html.push_str(&format!("<p>{}</p>\n", escape_html(trimmed)));
        }
    }
    if in_code {
        html.push_str("</code></pre>\n");
    }
    close_list(&mut html, &mut in_list);
    html
}

fn close_list(html: &mut String, in_list: &mut bool) {
    if *in_list {
        html.push_str("</ul>\n");
        *in_list = false;
    }
}

fn escape_html(text: &str) -> String {
    text.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

fn open_in_browser(path: &Path) -> anyhow::Result<()> {
    // `start` treats the first quoted argument as the window title, so an
    // empty title must precede the path.
    #[cfg(target_os = "windows")]
    let status = std::process::Command::new("cmd")
        .args(["/C", "start", ""])
        .arg(path)
        .status();
    #[cfg(target_os = "macos")]
    let status = std::process::Command::new("open").arg(path).status();
    #[cfg(all(unix, not(target_os = "macos")))]
    let status = std::process::Command::new("xdg-open").arg(path).status();
    match status {
        Ok(_) => Ok(()),
        Err(error) => bail!("cannot open browser: {error}"),
    }
}

const STYLE: &str = r#"
body { font-family: system-ui, sans-serif; margin: 0; display: flex; }
nav { width: 220px; padding: 1rem; border-right: 1px solid #ddd; }
nav a { display: block; color: #356; text-decoration: none; padding: 2px 0; }
main { padding: 1rem 2rem; flex: 1; max-width: 60rem; }
section { border-bottom: 1px solid #eee; padding: 1rem 0; }
section h2 code { background: none; font-size: 0.95rem; white-space: pre-wrap; }
section.impl h2 code { color: #667; }
.method { margin: 0.6rem 0 0.2rem; }
.method code { background: none; font-size: 0.95rem; white-space: pre-wrap; display: block; }
code { background: #f4f4f4; padding: 1px 4px; border-radius: 3px; }
pre code { display: block; padding: 0.75rem; overflow-x: auto; }
.empty { color: #889; }
"#;
