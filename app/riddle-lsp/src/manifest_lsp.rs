//! Language features for `Clue.toml` manifests: schema-aware diagnostics,
//! completions, hover, and document symbols. The schema mirrors
//! `clue`'s manifest parser; deep checks (missing entry files, dependency
//! resolution) stay with the project analysis pipeline (`CLUE0001`).

use lsp_types::{
    CompletionItem, CompletionItemKind, Diagnostic, DiagnosticSeverity, DocumentSymbol,
    DocumentSymbolResponse, Hover, HoverContents, MarkupContent, MarkupKind, NumberOrString,
    Position, Range,
};
use toml_edit::{Document, Item, TableLike, TomlError};

use crate::text::{offset_for_position, text_range};

/// The owned, immutable parse result — the only form that keeps key/value
/// spans (`DocumentMut::from_str` strips them via `despan`).
type TomlDocument = Document<String>;

const DIAGNOSTIC_SOURCE: &str = "clue";

/// Top-level tables of a Clue manifest.
const SECTIONS: &[(&str, &str)] = &[
    ("package", "package identity and entry configuration"),
    (
        "dependencies",
        "packages this package depends on (version string or table)",
    ),
    (
        "dev-dependencies",
        "dependencies only used by tests, examples, and benches",
    ),
    ("features", "named feature sets forwarded to dependencies"),
    (
        "bin",
        "binary targets (array of tables: name, path, required-features)",
    ),
    ("lib", "library target configuration"),
    ("test", "test targets (array of tables)"),
    ("example", "example targets (array of tables)"),
    ("bench", "bench targets (array of tables)"),
    ("workspace", "virtual workspace configuration (crates)"),
    (
        "runtime",
        "runtime selection for binary packages (gc, source)",
    ),
    ("build", "build configuration (target triple)"),
];

const PACKAGE_KEYS: &[(&str, &str)] = &[
    (
        "name",
        "the package name (required, used as the crate name)",
    ),
    ("version", "package version (semver, defaults to \"0.1.0\")"),
    ("license", "SPDX license expression"),
    (
        "entry",
        "explicit entry file path (defaults to src/main.rid)",
    ),
    (
        "publish",
        "false to disable publishing, or an array of allowed registries",
    ),
];

const DEPENDENCY_KEYS: &[(&str, &str)] = &[
    ("package", "real package name when the alias differs"),
    ("version", "semver version requirement"),
    ("path", "filesystem path dependency"),
    ("git", "git repository URL"),
    ("registry", "alternate registry name"),
    ("branch", "git branch (exclusive with tag and rev)"),
    ("tag", "git tag (exclusive with branch and rev)"),
    ("rev", "git commit revision (exclusive with branch and tag)"),
    ("optional", "only compiled in when a feature enables it"),
    ("features", "features to enable for this dependency"),
    (
        "default-features",
        "set to false to disable default features",
    ),
];

const LIB_KEYS: &[(&str, &str)] = &[
    ("name", "library name (defaults to the package name)"),
    ("path", "library root (defaults to src/lib.rid)"),
    (
        "proc-macro",
        "build this library as a procedural macro crate",
    ),
    (
        "crate-type",
        "library types to build: riddlelib, staticlib, cdylib",
    ),
];

const TARGET_KEYS: &[(&str, &str)] = &[
    ("name", "target name"),
    ("path", "target source path"),
    ("required-features", "features that must be active to build"),
];

const WORKSPACE_KEYS: &[(&str, &str)] = &[("crates", "member crate paths of the workspace")];
const RUNTIME_KEYS: &[(&str, &str)] = &[
    ("gc", "enable the garbage-collecting runtime (default true)"),
    ("source", "explicit runtime object file"),
];
const BUILD_KEYS: &[(&str, &str)] = &[("target", "target triple to build for")];

const LIBRARY_TYPES: &[&str] = &["riddlelib", "staticlib", "cdylib"];

#[must_use]
pub fn manifest_diagnostics(source: &str) -> Vec<Diagnostic> {
    let line_index = crate::text::LineIndex::new(source);
    let document = match source.parse::<TomlDocument>() {
        Ok(document) => document,
        Err(error) => {
            return vec![syntax_error(source, &line_index, &error)];
        }
    };
    let mut diagnostics = Vec::new();
    check_top_level(&document, source, &line_index, &mut diagnostics);
    diagnostics.sort_by_key(|diagnostic| (diagnostic.range.start, diagnostic.range.end));
    diagnostics
}

fn check_top_level(
    document: &TomlDocument,
    source: &str,
    line_index: &crate::text::LineIndex,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let table = document.as_table();
    let mut package_seen = false;
    for (key, item) in table.iter() {
        let span = key_span(table, key);
        let Some(section) = SECTIONS.iter().find(|(name, _)| *name == key) else {
            push_warning(
                source,
                line_index,
                span,
                diagnostics,
                &format!("unknown manifest key `{key}`"),
            );
            continue;
        };
        match section.0 {
            "package" => {
                package_seen = true;
                check_section(
                    item,
                    "package",
                    PACKAGE_KEYS,
                    source,
                    line_index,
                    diagnostics,
                );
                if let Some(package) = item.as_table_like() {
                    check_package_values(package, source, line_index, diagnostics);
                }
            }
            "dependencies" | "dev-dependencies" => {
                check_dependencies(item, section.0, source, line_index, diagnostics);
            }
            "features" => check_features(item, source, line_index, diagnostics),
            "lib" => {
                check_section(item, "lib", LIB_KEYS, source, line_index, diagnostics);
                if let Some(lib) = item.as_table_like() {
                    check_lib_values(lib, source, line_index, diagnostics);
                }
            }
            "bin" | "test" | "example" | "bench" => {
                check_target_array(item, section.0, source, line_index, diagnostics);
            }
            "workspace" => {
                check_section(
                    item,
                    "workspace",
                    WORKSPACE_KEYS,
                    source,
                    line_index,
                    diagnostics,
                );
            }
            "runtime" => {
                check_section(
                    item,
                    "runtime",
                    RUNTIME_KEYS,
                    source,
                    line_index,
                    diagnostics,
                );
            }
            "build" => {
                check_section(item, "build", BUILD_KEYS, source, line_index, diagnostics);
            }
            _ => {}
        }
    }
    if !package_seen {
        let span = table
            .iter()
            .next()
            .and_then(|(key, _)| key_span(table, key))
            .or(Some(0..source.len()));
        push_error(
            source,
            line_index,
            span,
            diagnostics,
            "missing required table `[package]`",
        );
    }
}

/// Unknown keys within a known section.
fn check_section(
    item: &Item,
    section: &str,
    keys: &[(&str, &str)],
    source: &str,
    line_index: &crate::text::LineIndex,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(table) = item.as_table_like() else {
        push_error(
            source,
            line_index,
            value_span(item),
            diagnostics,
            &format!("`{section}` must be a table (`[{section}]`)"),
        );
        return;
    };
    for (key, _) in table.iter() {
        if !keys.iter().any(|(name, _)| *name == key) {
            push_warning(
                source,
                line_index,
                key_span(table, key),
                diagnostics,
                &format!("unknown key `{section}.{key}`"),
            );
        }
    }
}

fn check_package_values(
    package: &dyn TableLike,
    source: &str,
    line_index: &crate::text::LineIndex,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Some(name) = package.get("name") {
        if !value_is_string(name) {
            push_type_error(
                source,
                line_index,
                name,
                package,
                "name",
                diagnostics,
                "a string",
            );
        }
    } else {
        push_error(
            source,
            line_index,
            key_span(package, "package"),
            diagnostics,
            "missing required key `package.name`",
        );
    }
    if let Some(version) = package.get("version")
        && let Some(text) = version.as_value().and_then(|value| value.as_str())
        && semver::Version::parse(text).is_err()
    {
        push_error(
            source,
            line_index,
            value_span(version),
            diagnostics,
            &format!("`package.version` is not a valid semver version: `{text}`"),
        );
    }
    if let Some(publish) = package.get("publish") {
        let valid = publish.as_value().is_some_and(|value| {
            value.as_bool().is_some()
                || value
                    .as_array()
                    .is_some_and(|entries| entries.iter().all(|entry| entry.as_str().is_some()))
        });
        if !valid {
            push_error(
                source,
                line_index,
                value_span(publish),
                diagnostics,
                "`package.publish` must be a boolean or an array of registry names",
            );
        }
    }
    for key in ["license", "entry"] {
        if let Some(value) = package.get(key)
            && !value_is_string(value)
        {
            push_type_error(
                source,
                line_index,
                value,
                package,
                key,
                diagnostics,
                "a string",
            );
        }
    }
}

fn check_dependencies(
    item: &Item,
    section: &str,
    source: &str,
    line_index: &crate::text::LineIndex,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(table) = item.as_table_like() else {
        push_error(
            source,
            line_index,
            value_span(item),
            diagnostics,
            &format!("`{section}` must be a table (`[{section}]`)"),
        );
        return;
    };
    for (alias, value) in table.iter() {
        if let Some(version) = value.as_value().and_then(|value| value.as_str()) {
            if semver::VersionReq::parse(version).is_err() {
                push_error(
                    source,
                    line_index,
                    value_span(value),
                    diagnostics,
                    &format!("invalid version requirement `{version}` for dependency `{alias}`"),
                );
            }
            continue;
        }
        let Some(config) = value.as_table_like() else {
            push_error(
                source,
                line_index,
                value_span(value),
                diagnostics,
                &format!("dependency `{alias}` must be a version string or table"),
            );
            continue;
        };
        for (key, _) in config.iter() {
            if !DEPENDENCY_KEYS.iter().any(|(name, _)| *name == key) {
                push_warning(
                    source,
                    line_index,
                    key_span(config, key),
                    diagnostics,
                    &format!("unknown key `{section}.{alias}.{key}`"),
                );
            }
        }
        if config.get("path").is_some() && config.get("git").is_some() {
            push_error(
                source,
                line_index,
                key_span(table, alias),
                diagnostics,
                &format!("dependency `{alias}` cannot specify both `path` and `git`"),
            );
        }
        let references = ["branch", "tag", "rev"]
            .iter()
            .filter(|key| config.get(key).is_some())
            .count();
        if references > 1 {
            push_error(
                source,
                line_index,
                key_span(table, alias),
                diagnostics,
                &format!("dependency `{alias}` may specify only one of `branch`, `tag`, or `rev`"),
            );
        }
        if let Some(version) = config.get("version")
            && let Some(text) = version.as_value().and_then(|value| value.as_str())
            && semver::VersionReq::parse(text).is_err()
        {
            push_error(
                source,
                line_index,
                value_span(version),
                diagnostics,
                &format!("invalid version requirement `{text}` for dependency `{alias}`"),
            );
        }
        for key in ["optional", "default-features"] {
            if let Some(value) = config.get(key)
                && !value
                    .as_value()
                    .is_some_and(|value| value.as_bool().is_some())
            {
                push_type_error(
                    source,
                    line_index,
                    value,
                    config,
                    key,
                    diagnostics,
                    "a boolean",
                );
            }
        }
        if let Some(features) = config.get("features")
            && !value_is_string_array(features)
        {
            push_type_error(
                source,
                line_index,
                features,
                config,
                "features",
                diagnostics,
                "an array of strings",
            );
        }
    }
}

fn check_features(
    item: &Item,
    source: &str,
    line_index: &crate::text::LineIndex,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(table) = item.as_table() else {
        push_error(
            source,
            line_index,
            value_span(item),
            diagnostics,
            "`features` must be a table (`[features]`)",
        );
        return;
    };
    for (name, value) in table.iter() {
        if !value_is_string_array(value) {
            push_type_error(
                source,
                line_index,
                value,
                table,
                name,
                diagnostics,
                "an array of strings",
            );
        }
    }
}

fn check_lib_values(
    lib: &dyn TableLike,
    source: &str,
    line_index: &crate::text::LineIndex,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Some(value) = lib.get("proc-macro")
        && !value
            .as_value()
            .is_some_and(|value| value.as_bool().is_some())
    {
        push_type_error(
            source,
            line_index,
            value,
            lib,
            "proc-macro",
            diagnostics,
            "a boolean",
        );
    }
    for key in ["name", "path"] {
        if let Some(value) = lib.get(key)
            && !value_is_string(value)
        {
            push_type_error(source, line_index, value, lib, key, diagnostics, "a string");
        }
    }
    if let Some(crate_type) = lib.get("crate-type") {
        let Some(entries) = crate_type
            .as_value()
            .and_then(|value| value.as_array())
            .map(|entries| {
                entries
                    .iter()
                    .filter_map(|entry| entry.as_str().map(str::to_owned))
                    .collect::<Vec<_>>()
            })
        else {
            push_type_error(
                source,
                line_index,
                crate_type,
                lib,
                "crate-type",
                diagnostics,
                "an array of strings",
            );
            return;
        };
        for entry in entries {
            if !LIBRARY_TYPES.contains(&entry.as_str()) {
                push_error(
                    source,
                    line_index,
                    value_span(crate_type),
                    diagnostics,
                    &format!(
                        "unsupported library crate type `{entry}` (expected one of {LIBRARY_TYPES:?})"
                    ),
                );
            }
        }
    }
}

fn check_target_array(
    item: &Item,
    section: &str,
    source: &str,
    line_index: &crate::text::LineIndex,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(entries) = item.as_array_of_tables() else {
        push_error(
            source,
            line_index,
            value_span(item),
            diagnostics,
            &format!("`{section}` must be an array of target tables"),
        );
        return;
    };
    for entry in entries.iter() {
        for (key, _) in entry.iter() {
            if !TARGET_KEYS.iter().any(|(name, _)| *name == key) {
                push_warning(
                    source,
                    line_index,
                    key_span(entry, key),
                    diagnostics,
                    &format!("unknown key `{section}[].{key}`"),
                );
            }
        }
        for key in ["name", "path"] {
            if let Some(value) = entry.get(key)
                && !value_is_string(value)
            {
                push_type_error(
                    source,
                    line_index,
                    value,
                    entry,
                    key,
                    diagnostics,
                    "a string",
                );
            }
        }
        if let Some(value) = entry.get("required-features")
            && !value_is_string_array(value)
        {
            push_type_error(
                source,
                line_index,
                value,
                entry,
                "required-features",
                diagnostics,
                "an array of strings",
            );
        }
    }
}

fn syntax_error(
    source: &str,
    line_index: &crate::text::LineIndex,
    error: &TomlError,
) -> Diagnostic {
    let span = error.span().unwrap_or(0..source.len());
    diagnostic(
        source,
        line_index,
        Some(span),
        DiagnosticSeverity::ERROR,
        "CLUE0002",
        format!("invalid TOML: {}", error.message().trim()),
    )
}

fn push_warning(
    source: &str,
    line_index: &crate::text::LineIndex,
    span: Option<std::ops::Range<usize>>,
    diagnostics: &mut Vec<Diagnostic>,
    message: &str,
) {
    diagnostics.push(diagnostic(
        source,
        line_index,
        span,
        DiagnosticSeverity::WARNING,
        "CLUE0003",
        message.to_string(),
    ));
}

fn push_error(
    source: &str,
    line_index: &crate::text::LineIndex,
    span: Option<std::ops::Range<usize>>,
    diagnostics: &mut Vec<Diagnostic>,
    message: &str,
) {
    diagnostics.push(diagnostic(
        source,
        line_index,
        span,
        DiagnosticSeverity::ERROR,
        "CLUE0004",
        message.to_string(),
    ));
}

fn push_type_error(
    source: &str,
    line_index: &crate::text::LineIndex,
    value: &Item,
    table: &dyn TableLike,
    key: &str,
    diagnostics: &mut Vec<Diagnostic>,
    expected: &str,
) {
    let span = value_span(value).or_else(|| key_span(table, key));
    push_error(
        source,
        line_index,
        span,
        diagnostics,
        &format!("`{key}` must be {expected}"),
    );
}

fn diagnostic(
    source: &str,
    line_index: &crate::text::LineIndex,
    span: Option<std::ops::Range<usize>>,
    severity: DiagnosticSeverity,
    code: &str,
    message: String,
) -> Diagnostic {
    let range = span
        .and_then(|span| {
            let range = text_range(span.start, span.end);
            crate::text::LineIndex::range(line_index, source, range)
        })
        .unwrap_or_else(|| Range::new(Position::new(0, 0), Position::new(0, 0)));
    Diagnostic {
        range,
        severity: Some(severity),
        code: Some(NumberOrString::String(code.into())),
        source: Some(DIAGNOSTIC_SOURCE.into()),
        message,
        ..Diagnostic::default()
    }
}

fn key_span(table: &dyn TableLike, key: &str) -> Option<std::ops::Range<usize>> {
    table.get_key_value(key).and_then(|(key, _)| key.span())
}

fn value_span(item: &Item) -> Option<std::ops::Range<usize>> {
    match item {
        Item::Value(value) => value.span(),
        Item::Table(table) => table.span(),
        Item::ArrayOfTables(array) => array.span(),
        Item::None => None,
    }
}

fn value_is_string(item: &Item) -> bool {
    item.as_value()
        .is_some_and(|value| value.as_str().is_some())
}

fn value_is_string_array(item: &Item) -> bool {
    item.as_value().is_some_and(|value| {
        value
            .as_array()
            .is_some_and(|entries| entries.iter().all(|entry| entry.as_str().is_some()))
    })
}

/// Whether a URI refers to a Clue manifest (`Clue.toml`), for both `file://`
/// and `untitled:` schemes.
#[must_use]
pub fn is_manifest_uri(uri: &lsp_types::Url) -> bool {
    uri.path()
        .rsplit('/')
        .next()
        .is_some_and(|name| name == "Clue.toml")
}

#[must_use]
pub fn manifest_completions(source: &str, position: Position) -> Vec<CompletionItem> {
    let Some(offset) = offset_for_position(source, position) else {
        return Vec::new();
    };
    let line_start = source[..offset].rfind('\n').map_or(0, |pos| pos + 1);
    let before_cursor = &source[line_start..offset];
    let (section, existing_keys) = enclosing_section(source, line_start);

    if let Some((key, typed)) = assignment_before(before_cursor) {
        return value_completions(section.as_deref(), key, typed);
    }
    if before_cursor.trim_start().starts_with('[') {
        let typed = before_cursor.trim_start().trim_start_matches('[');
        return section_completions(typed);
    }
    match section.as_deref() {
        None => section_completions(""),
        Some("package") => key_completions(PACKAGE_KEYS, &existing_keys, ""),
        Some("lib") => key_completions(LIB_KEYS, &existing_keys, ""),
        Some("workspace") => key_completions(WORKSPACE_KEYS, &existing_keys, ""),
        Some("runtime") => key_completions(RUNTIME_KEYS, &existing_keys, ""),
        Some("build") => key_completions(BUILD_KEYS, &existing_keys, ""),
        Some(section) => nested_section_completions(section, &existing_keys),
    }
}

/// Keys for sections nested under a table, such as `[dependencies.foo]` or
/// `[[bin]]`.
///
/// The *names* in these sections are user-chosen (`foo` above), but their keys
/// are not: `path`, `version`, `git` and friends are a fixed schema, and
/// leaving them out meant the manifest was the one file in the project with no
/// completions at all.
fn nested_section_completions(section: &str, existing_keys: &[String]) -> Vec<CompletionItem> {
    let section = section.trim_matches('"');
    if section.starts_with("dependencies.") || section.starts_with("dev-dependencies.") {
        return key_completions(DEPENDENCY_KEYS, existing_keys, "");
    }
    for prefix in ["bin", "example", "test", "bench"] {
        if section == prefix || section.starts_with(&format!("{prefix}.")) {
            return key_completions(TARGET_KEYS, existing_keys, "");
        }
    }
    // Feature and dependency names themselves are user-chosen; only their
    // values are completable, and `[features]` keys are plain booleans or lists.
    Vec::new()
}

/// The `[section]` header governing the line starting at `line_start`, plus
/// the keys already present under it.
///
/// Nested and array headers are normalised to their dotted name: `[[bin]]`
/// becomes `bin` and `[dependencies.foo]` becomes `dependencies.foo`, which is
/// what [`nested_section_completions`] matches on.
fn enclosing_section(source: &str, line_start: usize) -> (Option<String>, Vec<String>) {
    let mut section = None;
    let mut existing = Vec::new();
    for line in source[..=line_start.saturating_sub(1)]
        .lines()
        .chain(source[line_start..].lines().next())
    {
        let trimmed = line.trim_start();
        if let Some(header) = trimmed.strip_prefix('[') {
            let name = header.split(']').next().unwrap_or("").trim();
            if !name.starts_with('[') {
                section = Some(name.trim_matches('"').to_string());
                existing.clear();
                continue;
            }
        }
        if section.is_some()
            && let Some((key, _)) = line.split_once('=')
        {
            let key = key.trim().trim_matches('"');
            if !key.is_empty() {
                existing.push(key.to_string());
            }
        }
    }
    (section, existing)
}

/// `key = value-prefix` on the current line before the cursor.
fn assignment_before(before_cursor: &str) -> Option<(&str, &str)> {
    let (key, value) = before_cursor.split_once('=')?;
    let key = key.trim();
    let value = value.trim_start();
    (!key.is_empty() && !key.starts_with('[')).then_some((key, value))
}

fn section_completions(typed: &str) -> Vec<CompletionItem> {
    SECTIONS
        .iter()
        .filter(|(name, _)| name.starts_with(typed))
        .map(|(name, doc)| CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::STRUCT),
            detail: Some((*doc).into()),
            insert_text: Some(format!("[{name}]")),
            ..CompletionItem::default()
        })
        .collect()
}

fn key_completions(keys: &[(&str, &str)], existing: &[String], typed: &str) -> Vec<CompletionItem> {
    keys.iter()
        .filter(|(name, _)| name.starts_with(typed) && !existing.iter().any(|key| key == name))
        .map(|(name, doc)| CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some((*doc).into()),
            ..CompletionItem::default()
        })
        .collect()
}

fn value_completions(section: Option<&str>, key: &str, typed: &str) -> Vec<CompletionItem> {
    let values: &[&str] = match (section, key) {
        (Some("runtime"), "gc")
        | (Some("lib"), "proc-macro")
        | (_, "optional")
        | (_, "default-features")
        | (Some("package"), "publish") => &["true", "false"],
        (Some("lib"), "crate-type") => LIBRARY_TYPES,
        _ => return Vec::new(),
    };
    values
        .iter()
        .filter(|value| value.starts_with(typed))
        .map(|value| CompletionItem {
            label: (*value).into(),
            kind: Some(CompletionItemKind::CONSTANT),
            ..CompletionItem::default()
        })
        .collect()
}

#[must_use]
pub fn manifest_hover(source: &str, position: Position) -> Option<Hover> {
    let offset = offset_for_position(source, position)?;
    let document = source.parse::<TomlDocument>().ok()?;
    let table = document.as_table();
    for (section, item) in table.iter() {
        let Some(section_table) = item.as_table() else {
            continue;
        };
        for (key, _) in section_table.iter() {
            let Some(span) = key_span(section_table, key) else {
                continue;
            };
            if !span.contains(&offset) {
                continue;
            }
            let keys = section_keys(section)?;
            let doc = keys
                .iter()
                .find(|(name, _)| *name == key)
                .map(|(_, doc)| *doc)?;
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!(
                        "**{section}.{key}** — {doc}\n\n```toml\n[{section}]\n{key} = …\n```"
                    ),
                }),
                range: None,
            });
        }
        // Keys of a dependency entry (`alias.path`, …).
        if matches!(section, "dependencies" | "dev-dependencies")
            && let Some(dep) = item.as_table()
        {
            for (alias, value) in dep.iter() {
                let Some(config) = value.as_table_like() else {
                    continue;
                };
                for (key, _) in config.iter() {
                    let Some(span) = key_span(config, key) else {
                        continue;
                    };
                    if !span.contains(&offset) {
                        continue;
                    }
                    let doc = DEPENDENCY_KEYS
                        .iter()
                        .find(|(name, _)| *name == key)
                        .map(|(_, doc)| *doc)?;
                    return Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: format!(
                                "**{section}.{alias}.{key}** — {doc}\n\n```toml\n[{section}]\n{alias} = {{ {key} = … }}\n```"
                            ),
                        }),
                        range: None,
                    });
                }
            }
        }
    }
    None
}

fn section_keys(section: &str) -> Option<&'static [(&'static str, &'static str)]> {
    match section {
        "package" => Some(PACKAGE_KEYS),
        "lib" => Some(LIB_KEYS),
        "workspace" => Some(WORKSPACE_KEYS),
        "runtime" => Some(RUNTIME_KEYS),
        "build" => Some(BUILD_KEYS),
        _ => None,
    }
}

#[must_use]
#[allow(deprecated)]
pub fn manifest_document_symbols(source: &str) -> DocumentSymbolResponse {
    let Ok(document) = source.parse::<TomlDocument>() else {
        return DocumentSymbolResponse::Nested(Vec::new());
    };
    let mut symbols = Vec::new();
    for (section, item) in document.as_table().iter() {
        let Some(span) = key_span(document.as_table(), section) else {
            continue;
        };
        let Some(range) =
            crate::text::LineIndex::new(source).range(source, text_range(span.start, span.end))
        else {
            continue;
        };
        let mut children = Vec::new();
        if let Some(table) = item.as_table() {
            for (key, _) in table.iter() {
                let Some(key_span) = key_span(table, key) else {
                    continue;
                };
                let Some(child_range) = crate::text::LineIndex::new(source)
                    .range(source, text_range(key_span.start, key_span.end))
                else {
                    continue;
                };
                children.push(DocumentSymbol {
                    name: key.to_string(),
                    kind: lsp_types::SymbolKind::FIELD,
                    range: child_range,
                    selection_range: child_range,
                    children: None,
                    detail: None,
                    tags: None,
                    deprecated: None,
                });
            }
        } else if let Some(entries) = item.as_array_of_tables() {
            for entry in entries.iter() {
                let name = entry
                    .get("name")
                    .and_then(|value| value.as_value())
                    .and_then(|value| value.as_str())
                    .map(str::to_owned);
                let Some(key_span) = entry.span() else {
                    continue;
                };
                let Some(child_range) = crate::text::LineIndex::new(source)
                    .range(source, text_range(key_span.start, key_span.end))
                else {
                    continue;
                };
                children.push(DocumentSymbol {
                    name: name.unwrap_or_else(|| section.to_string()),
                    kind: lsp_types::SymbolKind::FIELD,
                    range: child_range,
                    selection_range: child_range,
                    children: None,
                    detail: None,
                    tags: None,
                    deprecated: None,
                });
            }
        }
        symbols.push(DocumentSymbol {
            name: section.to_string(),
            kind: lsp_types::SymbolKind::STRUCT,
            range,
            selection_range: range,
            children: Some(children),
            detail: None,
            tags: None,
            deprecated: None,
        });
    }
    DocumentSymbolResponse::Nested(symbols)
}
