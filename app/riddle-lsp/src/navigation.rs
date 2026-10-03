use std::{
    collections::{BTreeMap, HashMap},
    hash::BuildHasher,
    path::Path,
};

use hir::{
    HirFile, Name,
    body::{BodyId, Expr, ExprId, Pattern, PatternBindingId, ResolvedName, Stmt},
    item_tree::{
        FunctionId, HirEnum, HirEnumVariant, HirFunction, HirImpl, HirStruct, HirStructField,
        HirTypeRef, HirUseTree, HirUseTreeKind, HirVariantKind, TraitId,
    },
};
use lsp_types::{
    DocumentChanges, DocumentHighlight, DocumentHighlightKind, Documentation,
    GotoDefinitionResponse, Hover, HoverContents, Location, MarkupContent, MarkupKind, OneOf,
    OptionalVersionedTextDocumentIdentifier, ParameterInformation, ParameterLabel, Position,
    PrepareRenameResponse, SignatureHelp, SignatureInformation, TextDocumentEdit, TextEdit,
    WorkspaceEdit,
};
use riddlec::{
    pipeline::CompileOptions,
    proc_macro::{
        ProcMacroKind, ProcMacroOccurrence, STANDARD_DERIVE_MACROS, STANDARD_FUNCTION_MACROS,
    },
};
use rowan::{TextRange, TextSize};
use scope_graph::{
    DefRef, Node, RefOrigin, ScopeGraph,
    resolve::{
        resolve_path_at_reference, resolve_path_from, resolve_reference, visible_definitions,
    },
};
use syntax::SyntaxKind;
use type_checker::{CallableSignature, Type, TypeCheckResult};

use crate::{
    analysis::{AnalysisDepth, DocumentAnalysis, analyze_document_cancellable},
    completion::BUILTIN_TYPES,
    server::Document,
    session::AnalysisSessions,
    text::{LineIndex, normalized_path, offset_for_position, range_is_in_source, text_range},
};

struct Symbol {
    origin: TextRange,
    detail: String,
    definition: Option<TextRange>,
    implementations: Vec<TextRange>,
}

const HOVER_DECLARATION_ITEM_LIMIT: usize = 5;

/// Computes hover information for an open document.
///
/// # Errors
///
/// Returns an error when the document is unavailable or project analysis fails.
#[cfg(feature = "test")]
pub fn hover_for_document<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: Position,
    options: CompileOptions,
    sessions: &AnalysisSessions,
) -> Result<Option<Hover>, String> {
    hover_for_document_cancellable(uri, docs, position, options, sessions, &|| false)
}

/// Computes hover information and supports cooperative cancellation.
///
/// # Errors
///
/// Returns an error when the document is unavailable or project analysis fails.
pub fn hover_for_document_cancellable<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: Position,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> Result<Option<Hover>, String> {
    let document = docs
        .get(uri)
        .ok_or_else(|| "document is not open".to_string())?;
    let Some(analysis) = analyze_document_cancellable(
        uri,
        docs,
        options,
        sessions,
        AnalysisDepth::Infer,
        cancelled,
    )?
    else {
        return Ok(None);
    };
    Ok(hover_from_analysis(document, &analysis, position))
}

pub fn signature_help_for_document_cancellable<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: Position,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> Result<Option<SignatureHelp>, String> {
    let document = docs
        .get(uri)
        .ok_or_else(|| "document is not open".to_string())?;
    let Some(analysis) = analyze_document_cancellable(
        uri,
        docs,
        options,
        sessions,
        AnalysisDepth::Infer,
        cancelled,
    )?
    else {
        return Ok(None);
    };
    Ok(signature_help_from_analysis(
        &document.text,
        &analysis,
        position,
    ))
}

pub fn document_highlights_for_document_cancellable<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: Position,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> Result<Option<Vec<DocumentHighlight>>, String> {
    let Some(locations) = references_for_document_cancellable(
        uri, docs, position, true, options, sessions, cancelled,
    )?
    else {
        return Ok(None);
    };
    Ok(Some(
        locations
            .into_iter()
            .filter(|location| same_document_uri(&location.uri, uri))
            .map(|location| DocumentHighlight {
                range: location.range,
                kind: Some(DocumentHighlightKind::TEXT),
            })
            .collect(),
    ))
}

/// Finds the definition at a position in an open document.
///
/// # Errors
///
/// Returns an error when the document is unavailable or project analysis fails.
#[cfg(feature = "test")]
pub fn definition_for_document<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: Position,
    options: CompileOptions,
    sessions: &AnalysisSessions,
) -> Result<Option<GotoDefinitionResponse>, String> {
    definition_for_document_cancellable(uri, docs, position, options, sessions, &|| false)
}

pub fn definition_for_document_cancellable<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: Position,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> Result<Option<GotoDefinitionResponse>, String> {
    declaration_for_document_cancellable(uri, docs, position, options, sessions, cancelled)
}

pub fn declaration_for_document_cancellable<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: Position,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> Result<Option<GotoDefinitionResponse>, String> {
    let document = docs
        .get(uri)
        .ok_or_else(|| "document is not open".to_string())?;
    let Some(analysis) = analyze_document_cancellable(
        uri,
        docs,
        options,
        sessions,
        AnalysisDepth::Infer,
        cancelled,
    )?
    else {
        return Ok(None);
    };
    Ok(definition_from_analysis(uri, document, &analysis, position))
}

pub fn type_definition_for_document_cancellable<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: Position,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> Result<Option<GotoDefinitionResponse>, String> {
    let document = docs
        .get(uri)
        .ok_or_else(|| "document is not open".to_string())?;
    let Some(analysis) = analyze_document_cancellable(
        uri,
        docs,
        options,
        sessions,
        AnalysisDepth::Infer,
        cancelled,
    )?
    else {
        return Ok(None);
    };
    Ok(type_definition_from_analysis(
        uri, document, &analysis, position,
    ))
}

pub fn implementation_for_document_cancellable<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: Position,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> Result<Option<GotoDefinitionResponse>, String> {
    let document = docs
        .get(uri)
        .ok_or_else(|| "document is not open".to_string())?;
    let Some(analysis) = analyze_document_cancellable(
        uri,
        docs,
        options,
        sessions,
        AnalysisDepth::Infer,
        cancelled,
    )?
    else {
        return Ok(None);
    };
    Ok(implementation_from_analysis(
        uri, document, &analysis, position,
    ))
}

/// Finds references at a position in an open document.
///
/// # Errors
///
/// Returns an error when the document is unavailable or project analysis fails.
#[cfg(feature = "test")]
pub fn references_for_document<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: Position,
    include_declaration: bool,
    options: CompileOptions,
    sessions: &AnalysisSessions,
) -> Result<Option<Vec<Location>>, String> {
    references_for_document_cancellable(
        uri,
        docs,
        position,
        include_declaration,
        options,
        sessions,
        &|| false,
    )
}

pub fn references_for_document_cancellable<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: Position,
    include_declaration: bool,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> Result<Option<Vec<Location>>, String> {
    let document = docs
        .get(uri)
        .ok_or_else(|| "document is not open".to_string())?;
    let Some(analysis) = analyze_document_cancellable(
        uri,
        docs,
        options,
        sessions,
        AnalysisDepth::Infer,
        cancelled,
    )?
    else {
        return Ok(None);
    };
    Ok(references_from_analysis(
        uri,
        document,
        &analysis,
        position,
        include_declaration,
    ))
}

/// Prepares a rename at a position in an open document.
///
/// # Errors
///
/// Returns an error when the document is unavailable or project analysis fails.
#[cfg(feature = "test")]
pub fn prepare_rename_for_document<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: Position,
    options: CompileOptions,
    sessions: &AnalysisSessions,
) -> Result<Option<PrepareRenameResponse>, String> {
    prepare_rename_for_document_cancellable(uri, docs, position, options, sessions, &|| false)
}

pub fn prepare_rename_for_document_cancellable<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: Position,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> Result<Option<PrepareRenameResponse>, String> {
    let document = docs
        .get(uri)
        .ok_or_else(|| "document is not open".to_string())?;
    let Some(analysis) = analyze_document_cancellable(
        uri,
        docs,
        options,
        sessions,
        AnalysisDepth::Infer,
        cancelled,
    )?
    else {
        return Ok(None);
    };
    // A rejection other than "nothing here" is reported as "nothing here":
    // `prepareRename` has no error channel beyond `null`, and the client will
    // learn the reason from `rename` if the user tries anyway.
    Ok(prepare_rename_from_analysis(document, &analysis, position).unwrap_or(None))
}

/// Renames a symbol at a position in an open document.
///
/// # Errors
///
/// Returns an error when the new name is invalid, the document is unavailable, or analysis fails.
#[cfg(feature = "test")]
pub fn rename_for_document<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: Position,
    new_name: &str,
    options: CompileOptions,
    sessions: &AnalysisSessions,
) -> Result<Option<WorkspaceEdit>, RenameError> {
    rename_for_document_cancellable(uri, docs, position, new_name, options, sessions, &|| false)
}

pub fn rename_for_document_cancellable<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: Position,
    new_name: &str,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> Result<Option<WorkspaceEdit>, RenameError> {
    validate_identifier(new_name).map_err(RenameError::InvalidName)?;
    let document = docs
        .get(uri)
        .ok_or_else(|| RenameError::Unavailable("document is not open".into()))?;
    let Some(analysis) = analyze_document_cancellable(
        uri,
        docs,
        options,
        sessions,
        AnalysisDepth::Infer,
        cancelled,
    )
    .map_err(RenameError::Unavailable)?
    else {
        return Ok(None);
    };
    Ok(Some(rename_from_analysis(
        uri, docs, document, &analysis, position, new_name,
    )?))
}

#[cfg(feature = "test")]
#[must_use]
pub fn hover_for_source(
    source: &str,
    position: Position,
    options: CompileOptions,
) -> Option<Hover> {
    let document = Document::new(source, Some(1));
    let analysis = standalone_analysis(source, options);
    hover_from_analysis(&document, &analysis, position)
}

#[cfg(feature = "test")]
#[must_use]
/// Finds a definition in standalone source.
///
/// # Panics
///
/// Panics if the fixed test document URL cannot be parsed.
pub fn definition_for_source(
    source: &str,
    position: Position,
    options: CompileOptions,
) -> Option<GotoDefinitionResponse> {
    let uri = lsp_types::Url::parse("file:///riddle-navigation.rid").unwrap();
    let document = Document::new(source, Some(1));
    let analysis = standalone_analysis(source, options);
    definition_from_analysis(&uri, &document, &analysis, position)
}

#[cfg(feature = "test")]
#[must_use]
/// Finds a type definition in standalone source.
///
/// # Panics
///
/// Panics if the fixed test document URL cannot be parsed.
pub fn type_definition_for_source(
    source: &str,
    position: Position,
    options: CompileOptions,
) -> Option<GotoDefinitionResponse> {
    let uri = lsp_types::Url::parse("file:///riddle-navigation.rid").unwrap();
    let document = Document::new(source, Some(1));
    let analysis = standalone_analysis(source, options);
    type_definition_from_analysis(&uri, &document, &analysis, position)
}

#[cfg(feature = "test")]
#[must_use]
/// Finds implementations in standalone source.
///
/// # Panics
///
/// Panics if the fixed test document URL cannot be parsed.
pub fn implementation_for_source(
    source: &str,
    position: Position,
    options: CompileOptions,
) -> Option<GotoDefinitionResponse> {
    let uri = lsp_types::Url::parse("file:///riddle-navigation.rid").unwrap();
    let document = Document::new(source, Some(1));
    let analysis = standalone_analysis(source, options);
    implementation_from_analysis(&uri, &document, &analysis, position)
}

#[cfg(feature = "test")]
#[must_use]
/// Finds references in standalone source.
///
/// # Panics
///
/// Panics if the fixed test document URL cannot be parsed.
pub fn references_for_source(
    source: &str,
    position: Position,
    include_declaration: bool,
    options: CompileOptions,
) -> Option<Vec<Location>> {
    let uri = lsp_types::Url::parse("file:///riddle-navigation.rid").unwrap();
    let document = Document::new(source, Some(1));
    let analysis = standalone_analysis(source, options);
    references_from_analysis(&uri, &document, &analysis, position, include_declaration)
}

#[cfg(feature = "test")]
#[must_use]
pub fn prepare_rename_for_source(
    source: &str,
    position: Position,
    options: CompileOptions,
) -> Option<PrepareRenameResponse> {
    let document = Document::new(source, Some(1));
    let analysis = standalone_analysis(source, options);
    prepare_rename_from_analysis(&document, &analysis, position).unwrap_or(None)
}

#[cfg(feature = "test")]
/// Renames a symbol in standalone source.
///
/// # Errors
///
/// Returns an error when `new_name` is not a valid Riddle identifier.
///
/// # Panics
///
/// Panics if the fixed test document URL cannot be parsed.
pub fn rename_for_source(
    source: &str,
    position: Position,
    new_name: &str,
    options: CompileOptions,
) -> Result<Option<WorkspaceEdit>, RenameError> {
    validate_identifier(new_name).map_err(RenameError::InvalidName)?;
    let uri = lsp_types::Url::parse("file:///riddle-navigation.rid").unwrap();
    let document = Document::new(source, Some(1));
    let docs = HashMap::from([(uri.clone(), document.clone())]);
    let analysis = standalone_analysis(source, options);
    Ok(Some(rename_from_analysis(
        &uri, &docs, &document, &analysis, position, new_name,
    )?))
}

#[cfg(feature = "test")]
#[must_use]
pub fn signature_help_for_source(source: &str, position: Position) -> Option<SignatureHelp> {
    let analysis = standalone_analysis(source, CompileOptions { use_std: false });
    signature_help_from_analysis(source, &analysis, position)
}

#[cfg(feature = "test")]
#[must_use]
pub fn document_highlights_for_source(
    source: &str,
    position: Position,
) -> Option<Vec<DocumentHighlight>> {
    let locations =
        references_for_source(source, position, true, CompileOptions { use_std: false })?;
    Some(
        locations
            .into_iter()
            .map(|location| DocumentHighlight {
                range: location.range,
                kind: Some(DocumentHighlightKind::TEXT),
            })
            .collect(),
    )
}

#[cfg(feature = "test")]
fn standalone_analysis(source: &str, options: CompileOptions) -> DocumentAnalysis {
    let mut session = riddlec::pipeline::CheckSession::new();
    let result = session
        .infer_with_options_cancellable(source, options, || false)
        .expect("inference should not be cancelled");
    DocumentAnalysis {
        result: std::sync::Arc::new(result),
        source: source.into(),
        source_map: None,
        macro_occurrences: Vec::new(),
        macro_source_map: None,
        path: None,
        project_root: None,
        project_revision: 0,
        files: Vec::new(),
    }
}

fn signature_help_from_analysis(
    source: &str,
    analysis: &DocumentAnalysis,
    position: Position,
) -> Option<SignatureHelp> {
    let (callee, active_parameter) = call_context(source, position)?;
    let callee_position = LineIndex::new(source).position(source, usize::from(callee.start()))?;
    let symbol = symbol_at(source, analysis, callee_position)?;
    let label = symbol.detail;
    label.starts_with("fun ").then_some(())?;
    let parameters = signature_parameters(&label);
    let documentation = symbol
        .definition
        .and_then(|range| documentation_for_symbol(analysis, range));
    let active_parameter =
        active_parameter + u32::from(parameters.first().is_some_and(is_receiver_parameter));
    let active_parameter = u32::try_from(parameters.len())
        .ok()
        .is_some_and(|length| active_parameter < length)
        .then_some(active_parameter);
    Some(SignatureHelp {
        signatures: vec![SignatureInformation {
            label,
            documentation: documentation.map(|value| {
                Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value,
                })
            }),
            parameters: Some(parameters),
            active_parameter: None,
        }],
        active_signature: Some(0),
        active_parameter,
    })
}

pub fn call_parameter_names_at(
    source: &str,
    analysis: &DocumentAnalysis,
    position: Position,
) -> Option<Vec<String>> {
    let symbol = symbol_at(source, analysis, position)?;
    Some(
        signature_parameters(&symbol.detail)
            .into_iter()
            .filter(|parameter| !is_receiver_parameter(parameter))
            .filter_map(|parameter| match parameter.label {
                ParameterLabel::Simple(label) => label
                    .split_once(':')
                    .map(|(name, _)| name.trim().to_string()),
                ParameterLabel::LabelOffsets(_) => None,
            })
            .collect(),
    )
}

fn call_context(source: &str, position: Position) -> Option<(TextRange, u32)> {
    let offset = offset_for_position(source, position)?;
    let tokens = frontend::lexer::lex(source)
        .into_iter()
        .filter(|token| {
            token.kind != SyntaxKind::Whitespace && token.kind != SyntaxKind::LineComment
        })
        .collect::<Vec<_>>();
    let mut opens = Vec::new();
    for (index, token) in tokens.iter().enumerate() {
        if token.span.start >= offset {
            break;
        }
        match token.kind {
            SyntaxKind::LParen => opens.push(index),
            SyntaxKind::RParen => {
                opens.pop();
            }
            _ => {}
        }
    }
    let open = *opens.last()?;
    let callee = tokens.get(open.checked_sub(1)?)?;
    (callee.kind == SyntaxKind::Ident).then_some(())?;
    let mut nested = 0usize;
    let mut active_parameter = 0u32;
    for token in tokens.iter().skip(open + 1) {
        if token.span.start >= offset {
            break;
        }
        match token.kind {
            SyntaxKind::LParen | SyntaxKind::LBracket | SyntaxKind::LBrace => nested += 1,
            SyntaxKind::RParen | SyntaxKind::RBracket | SyntaxKind::RBrace => {
                nested = nested.saturating_sub(1);
            }
            SyntaxKind::Comma if nested == 0 => active_parameter += 1,
            _ => {}
        }
    }
    Some((
        text_range(callee.span.start, callee.span.end),
        active_parameter,
    ))
}

fn signature_parameters(label: &str) -> Vec<ParameterInformation> {
    let Some(start) = label.find('(') else {
        return Vec::new();
    };
    let mut parameters = Vec::new();
    let content_start = start + 1;
    let mut parameter_start = content_start;
    let mut depth = 0usize;
    for (offset, character) in label[content_start..].char_indices() {
        let index = content_start + offset;
        match character {
            '(' | '[' | '{' | '<' => depth += 1,
            ')' if depth == 0 => {
                push_signature_parameter(&mut parameters, &label[parameter_start..index]);
                break;
            }
            ')' | ']' | '}' | '>' => depth = depth.saturating_sub(1),
            ',' if depth == 0 => {
                push_signature_parameter(&mut parameters, &label[parameter_start..index]);
                parameter_start = index + character.len_utf8();
            }
            _ => {}
        }
    }
    parameters
}

fn push_signature_parameter(parameters: &mut Vec<ParameterInformation>, parameter: &str) {
    let parameter = parameter.trim();
    if !parameter.is_empty() {
        parameters.push(ParameterInformation {
            label: ParameterLabel::Simple(parameter.into()),
            documentation: None,
        });
    }
}

fn is_receiver_parameter(parameter: &ParameterInformation) -> bool {
    let ParameterLabel::Simple(label) = &parameter.label else {
        return false;
    };
    let label = label.trim();
    matches!(label, "self" | "&self" | "&mut self" | "mut self") || label.starts_with("self:")
}

fn hover_from_analysis(
    document: &Document,
    analysis: &DocumentAnalysis,
    position: Position,
) -> Option<Hover> {
    if let Some(occurrence) = macro_at(&document.text, analysis, position) {
        let origin = analysis.local_macro_range(&occurrence.range)?;
        let standard = occurrence.package == "std"
            && (STANDARD_FUNCTION_MACROS.contains(&occurrence.macro_name.as_str())
                || STANDARD_DERIVE_MACROS.contains(&occurrence.macro_name.as_str()));
        let kind = match occurrence.kind {
            ProcMacroKind::Derive => "derive",
            ProcMacroKind::Attribute => "attribute",
            ProcMacroKind::FunctionLike => "function-like",
        };
        let value = if standard && occurrence.kind == ProcMacroKind::Derive {
            format!(
                "```riddle\nstandard derive macro {}\n```\n\n`#[derive({})]`",
                occurrence.name, occurrence.macro_name
            )
        } else if standard {
            format!(
                "```riddle\nstandard macro {}!(...)\n```\n\n`std::{}!`",
                occurrence.name, occurrence.macro_name
            )
        } else {
            format!(
                "```riddle\n{kind} proc macro {}\n```\n\n`{}::{}`",
                occurrence.name, occurrence.package, occurrence.macro_name
            )
        };
        return Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value,
            }),
            range: Some(LineIndex::new(&document.text).range(&document.text, origin)?),
        });
    }
    let symbol = symbol_at(&document.text, analysis, position)?;
    let range = LineIndex::new(&document.text).range(&document.text, symbol.origin)?;
    let documentation = symbol
        .definition
        .and_then(|range| documentation_for_symbol(analysis, range));
    let value = documentation.map_or_else(
        || format!("```riddle\n{}\n```", symbol.detail),
        |documentation| format!("```riddle\n{}\n```\n\n{}", symbol.detail, documentation),
    );
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: Some(range),
    })
}

fn definition_from_analysis(
    uri: &lsp_types::Url,
    document: &Document,
    analysis: &DocumentAnalysis,
    position: Position,
) -> Option<GotoDefinitionResponse> {
    if let Some(occurrence) = macro_at(&document.text, analysis, position) {
        let location = macro_definition_location(uri, analysis, occurrence)?;
        return Some(GotoDefinitionResponse::Scalar(location));
    }
    let symbol = symbol_at(&document.text, analysis, position)?;
    let location = location_for_range(uri, analysis, symbol.definition?)?;
    Some(GotoDefinitionResponse::Scalar(location))
}

fn type_definition_from_analysis(
    uri: &lsp_types::Url,
    document: &Document,
    analysis: &DocumentAnalysis,
    position: Position,
) -> Option<GotoDefinitionResponse> {
    let offset = offset_for_position(&document.text, position)?;
    let origin = identifier_range_at(&document.text, offset)?;
    let hir = analysis.result.hir.as_ref()?;
    let definition = explicit_type_definition(hir, analysis, origin)
        .or_else(|| declared_type_definition(hir, analysis, origin))
        .or_else(|| {
            inferred_type_definition(hir, &analysis.result.type_result, analysis, origin)
        })?;
    Some(GotoDefinitionResponse::Scalar(location_for_range(
        uri, analysis, definition,
    )?))
}

fn explicit_type_definition(
    hir: &HirFile,
    analysis: &DocumentAnalysis,
    origin: TextRange,
) -> Option<TextRange> {
    let graph = analysis.result.scope_graph.as_ref()?;
    graph
        .nodes
        .iter()
        .filter_map(|(reference, node)| {
            let Node::Reference {
                origin: RefOrigin::Type { range },
                ..
            } = node
            else {
                return None;
            };
            let local = analysis.local_range(*range)?;
            range_contains(local, origin).then_some((local.len(), reference))
        })
        .min_by_key(|(length, _)| *length)
        .and_then(|(_, reference)| {
            resolve_reference(graph, reference)
                .into_iter()
                .find_map(|definition| definition_name_range(hir, &definition))
        })
}

fn declared_type_definition(
    hir: &HirFile,
    analysis: &DocumentAnalysis,
    origin: TextRange,
) -> Option<TextRange> {
    for (_, function) in hir.item_tree.functions.iter() {
        for parameter in &function.params {
            if analysis
                .local_range(parameter.name_range)
                .is_some_and(|range| range_contains(range, origin))
            {
                return hir_type_definition(hir, &parameter.ty);
            }
        }
    }
    for (_, strukt) in hir.item_tree.structs.iter() {
        for field in &strukt.fields {
            if analysis
                .local_range(field.name_range)
                .is_some_and(|range| range_contains(range, origin))
            {
                return hir_type_definition(hir, &field.ty);
            }
        }
    }
    for (body_id, body) in hir.bodies.iter() {
        for (_, statement) in body.stmts.iter() {
            let Stmt::Let { pat, ty, .. } = statement else {
                continue;
            };
            let Some(range) = body
                .source_map
                .pat_ranges
                .get(pat)
                .and_then(|range| analysis.local_range(*range))
            else {
                continue;
            };
            if range_contains(range, origin) {
                return hir_type_definition(hir, ty).or_else(|| {
                    types_for_pattern(&analysis.result.type_result, body_id, *pat)
                        .and_then(|ty| nominal_type_definition(hir, ty))
                });
            }
        }
    }
    None
}

fn inferred_type_definition(
    hir: &HirFile,
    types: &TypeCheckResult,
    analysis: &DocumentAnalysis,
    origin: TextRange,
) -> Option<TextRange> {
    let expression = types
        .expr_types
        .iter()
        .filter_map(|((body, expression), ty)| {
            let range = hir.bodies[*body].source_map.expr_ranges.get(expression)?;
            let local = analysis.local_range(*range)?;
            range_contains(local, origin).then_some((local.len(), ty))
        })
        .min_by_key(|(length, _)| *length)
        .and_then(|(_, ty)| nominal_type_definition(hir, ty));
    if expression.is_some() {
        return expression;
    }
    types
        .pattern_binding_types
        .iter()
        .find_map(|((body, binding), ty)| {
            let range = pattern_binding_name_range(hir, *body, *binding, &analysis.source)?;
            let local = analysis.local_range(range)?;
            range_contains(local, origin)
                .then(|| nominal_type_definition(hir, ty))
                .flatten()
        })
}

fn types_for_pattern(
    types: &TypeCheckResult,
    body: BodyId,
    pattern: hir::body::PatId,
) -> Option<&Type> {
    types
        .pattern_binding_types
        .iter()
        .find_map(|((candidate_body, binding), ty)| {
            (*candidate_body == body && binding.pattern == pattern).then_some(ty)
        })
}

fn nominal_type_definition(hir: &HirFile, ty: &Type) -> Option<TextRange> {
    match ty {
        Type::Ref(inner, _) | Type::Ptr { inner, .. } => nominal_type_definition(hir, inner),
        Type::Struct(id, _) => Some(hir.item_tree.structs[*id].name_range),
        Type::Enum(id, _) => Some(hir.item_tree.enums[*id].name_range),
        _ => None,
    }
}

fn hir_type_definition(hir: &HirFile, ty: &HirTypeRef) -> Option<TextRange> {
    match ty {
        HirTypeRef::Named(path) => match hir.type_resolutions.get(&path.range) {
            Some(ResolvedName::Struct(id)) => Some(hir.item_tree.structs[*id].name_range),
            Some(ResolvedName::Enum(id)) => Some(hir.item_tree.enums[*id].name_range),
            Some(ResolvedName::Trait(id)) => Some(hir.item_tree.traits[*id].name_range),
            Some(ResolvedName::TypeAlias(id)) => Some(hir.item_tree.type_aliases[*id].name_range),
            _ => None,
        },
        HirTypeRef::Ref(inner, _)
        | HirTypeRef::Ptr { inner, .. }
        | HirTypeRef::Slice(inner)
        | HirTypeRef::Array(inner, _) => hir_type_definition(hir, inner),
        HirTypeRef::ImplTrait { trait_ty, .. } | HirTypeRef::DynTrait { trait_ty, .. } => {
            hir_type_definition(hir, trait_ty)
        }
        HirTypeRef::Never
        | HirTypeRef::Tuple(_)
        | HirTypeRef::Const(_)
        | HirTypeRef::Unknown
        | HirTypeRef::Error => None,
    }
}

fn definition_name_range(hir: &HirFile, definition: &DefRef) -> Option<TextRange> {
    match definition {
        DefRef::Struct(id) => Some(hir.item_tree.structs[*id].name_range),
        DefRef::Enum(id) => Some(hir.item_tree.enums[*id].name_range),
        DefRef::Trait(id) => Some(hir.item_tree.traits[*id].name_range),
        DefRef::TypeAlias(id) => Some(hir.item_tree.type_aliases[*id].name_range),
        _ => None,
    }
}

fn range_contains(range: TextRange, inner: TextRange) -> bool {
    range.start() <= inner.start() && inner.end() <= range.end()
}

fn documentation_for_symbol(analysis: &DocumentAnalysis, target: TextRange) -> Option<String> {
    let hir = analysis.result.hir.as_ref()?;
    hir.doc_comments
        .iter()
        .filter(|(range, _)| range_contains(*range, target))
        .min_by_key(|(range, _)| range.len())
        .map(|(_, comments)| {
            comments
                .iter()
                .map(|comment| normalize_doc_comment(comment))
                .filter(|comment| !comment.is_empty())
                .collect::<Vec<_>>()
                .join("\n")
        })
        .filter(|documentation| !documentation.is_empty())
}

fn normalize_doc_comment(comment: &str) -> String {
    let comment = comment.trim();
    if let Some(comment) = comment
        .strip_prefix("///")
        .or_else(|| comment.strip_prefix("//!"))
        .or_else(|| comment.strip_prefix("//<"))
    {
        return comment.trim_start().to_string();
    }
    let Some(comment) = comment
        .strip_prefix("/**")
        .or_else(|| comment.strip_prefix("/*!"))
    else {
        return comment.to_string();
    };
    let comment = comment.strip_suffix("*/").unwrap_or(comment);
    comment
        .lines()
        .map(str::trim)
        .map(|line| line.strip_prefix('*').map_or(line, str::trim_start))
        .collect::<Vec<_>>()
        .join("\n")
        .trim()
        .to_string()
}

fn implementation_from_analysis(
    uri: &lsp_types::Url,
    document: &Document,
    analysis: &DocumentAnalysis,
    position: Position,
) -> Option<GotoDefinitionResponse> {
    let symbol = symbol_at(&document.text, analysis, position)?;
    let mut locations = symbol
        .implementations
        .into_iter()
        .filter_map(|range| location_for_range(uri, analysis, range))
        .collect::<Vec<_>>();
    locations.sort_by(|left, right| {
        left.uri
            .as_str()
            .cmp(right.uri.as_str())
            .then_with(|| left.range.start.line.cmp(&right.range.start.line))
            .then_with(|| left.range.start.character.cmp(&right.range.start.character))
    });
    locations.dedup();
    (!locations.is_empty()).then_some(GotoDefinitionResponse::Array(locations))
}

#[derive(Clone, Debug)]
struct ShorthandField {
    name: String,
    definition: TextRange,
}

#[derive(Clone, Debug)]
struct SymbolOccurrence {
    range: TextRange,
    is_declaration: bool,
    shorthand: Option<ShorthandField>,
}

fn references_from_analysis(
    uri: &lsp_types::Url,
    document: &Document,
    analysis: &DocumentAnalysis,
    position: Position,
    include_declaration: bool,
) -> Option<Vec<Location>> {
    if let Some(target) = macro_at(&document.text, analysis, position) {
        let mut locations = analysis
            .macro_occurrences
            .iter()
            .filter(|occurrence| same_macro_binding(target, occurrence))
            .filter(|occurrence| include_declaration || !occurrence.is_declaration)
            .filter_map(|occurrence| macro_occurrence_location(uri, analysis, occurrence))
            .collect::<Vec<_>>();
        sort_and_dedup_locations(&mut locations);
        return Some(locations);
    }
    let target = symbol_at(&document.text, analysis, position)?.definition?;
    let mut locations = symbol_occurrences(analysis, target)
        .into_iter()
        .filter(|occurrence| include_declaration || !occurrence.is_declaration)
        .filter_map(|occurrence| location_for_range(uri, analysis, occurrence.range))
        .collect::<Vec<_>>();
    sort_and_dedup_locations(&mut locations);
    Some(locations)
}

/// Why a rename could not be produced.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RenameError {
    /// The requested new name is not a legal identifier.
    InvalidName(String),
    /// The target exists but cannot be renamed.
    Rejected(RenameRejection),
    /// The document or its analysis was unavailable.
    Unavailable(String),
}

impl std::fmt::Display for RenameError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidName(message) | Self::Unavailable(message) => formatter.write_str(message),
            Self::Rejected(rejection) => write!(formatter, "{rejection}"),
        }
    }
}

impl From<RenameRejection> for RenameError {
    fn from(rejection: RenameRejection) -> Self {
        Self::Rejected(rejection)
    }
}

/// Why a position cannot be renamed.
///
/// `prepareRename` and `rename` share this so a target one accepts can never be
/// silently refused by the other.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RenameRejection {
    /// Nothing nameable sits under the cursor.
    NoTarget,
    /// The position names something the project does not own, such as a
    /// standard-library item reached through a source map.
    NotOwned,
    /// A procedural-macro invocation with no import binding to rename.
    UnboundMacro,
}

impl std::fmt::Display for RenameRejection {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            Self::NoTarget => "there is nothing to rename at this position",
            Self::NotOwned => "this item is not defined in the project and cannot be renamed",
            Self::UnboundMacro => "this macro invocation has no import to rename",
        })
    }
}

/// What the cursor is on, resolved once for both rename entry points.
enum RenameTarget<'a> {
    Macro(&'a riddlec::proc_macro::ProcMacroOccurrence),
    Symbol {
        /// The identifier under the cursor, which is what the client replaces.
        origin: rowan::TextRange,
        /// The declaration the occurrence resolves to.
        definition: rowan::TextRange,
    },
}

fn rename_target<'a>(
    document: &Document,
    analysis: &'a DocumentAnalysis,
    position: Position,
) -> Result<RenameTarget<'a>, RenameRejection> {
    if let Some(occurrence) = macro_at(&document.text, analysis, position) {
        return if occurrence.binding.is_some()
            && analysis.local_macro_range(&occurrence.range).is_some()
        {
            Ok(RenameTarget::Macro(occurrence))
        } else {
            Err(RenameRejection::UnboundMacro)
        };
    }
    let Some(symbol) = symbol_at(&document.text, analysis, position) else {
        return Err(RenameRejection::NoTarget);
    };
    let Some(definition) = symbol.definition else {
        return Err(RenameRejection::NoTarget);
    };
    if renamable_target(analysis, definition).is_none() {
        return Err(RenameRejection::NotOwned);
    }
    Ok(RenameTarget::Symbol {
        origin: symbol.origin,
        definition,
    })
}

pub fn prepare_rename_from_analysis(
    document: &Document,
    analysis: &DocumentAnalysis,
    position: Position,
) -> Result<Option<PrepareRenameResponse>, RenameRejection> {
    match rename_target(document, analysis, position) {
        Ok(RenameTarget::Macro(occurrence)) => {
            let origin = analysis
                .local_macro_range(&occurrence.range)
                .ok_or(RenameRejection::UnboundMacro)?;
            let placeholder = document
                .text
                .get(usize::from(origin.start())..usize::from(origin.end()))
                .ok_or(RenameRejection::NoTarget)?;
            Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
                range: LineIndex::new(&document.text)
                    .range(&document.text, origin)
                    .ok_or(RenameRejection::NoTarget)?,
                placeholder: placeholder.into(),
            }))
        }
        Ok(RenameTarget::Symbol { origin, .. }) => {
            let placeholder = document
                .text
                .get(usize::from(origin.start())..usize::from(origin.end()))
                .ok_or(RenameRejection::NoTarget)?;
            Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
                range: LineIndex::new(&document.text)
                    .range(&document.text, origin)
                    .ok_or(RenameRejection::NoTarget)?,
                placeholder: placeholder.into(),
            }))
        }
        // "Nothing to rename here" is not an error for `prepareRename`: the
        // specification defines `null` as the answer and clients use it to
        // disable the rename affordance.
        Err(RenameRejection::NoTarget) => Ok(None),
        Err(rejection) => Err(rejection),
    }
}

fn rename_from_analysis<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    document: &Document,
    analysis: &DocumentAnalysis,
    position: Position,
    new_name: &str,
) -> Result<WorkspaceEdit, RenameRejection> {
    let mut documents = BTreeMap::<String, (lsp_types::Url, Vec<TextEdit>)>::new();
    match rename_target(document, analysis, position)? {
        RenameTarget::Macro(target) => {
            for occurrence in analysis
                .macro_occurrences
                .iter()
                .filter(|occurrence| same_macro_binding(target, occurrence))
            {
                let Some(location) = macro_occurrence_location(uri, analysis, occurrence) else {
                    continue;
                };
                documents
                    .entry(location.uri.as_str().into())
                    .or_insert_with(|| (location.uri.clone(), Vec::new()))
                    .1
                    .push(TextEdit::new(location.range, new_name.into()));
            }
        }
        RenameTarget::Symbol { definition, .. } => {
            for occurrence in symbol_occurrences(analysis, definition) {
                let Some(location) = location_for_range(uri, analysis, occurrence.range) else {
                    continue;
                };
                let replacement = match occurrence.shorthand {
                    Some(shorthand) if shorthand.definition == definition => {
                        format!("{new_name}: {}", shorthand.name)
                    }
                    Some(shorthand) => format!("{}: {new_name}", shorthand.name),
                    None => new_name.into(),
                };
                documents
                    .entry(location.uri.as_str().into())
                    .or_insert_with(|| (location.uri.clone(), Vec::new()))
                    .1
                    .push(TextEdit::new(location.range, replacement));
            }
        }
    }
    Ok(workspace_edit(documents, docs))
}

fn workspace_edit<S: BuildHasher>(
    documents: BTreeMap<String, (lsp_types::Url, Vec<TextEdit>)>,
    docs: &HashMap<lsp_types::Url, Document, S>,
) -> WorkspaceEdit {
    let edits = documents
        .into_values()
        .map(|(uri, mut edits)| {
            edits.sort_by_key(|edit| {
                (
                    edit.range.start.line,
                    edit.range.start.character,
                    edit.range.end.line,
                    edit.range.end.character,
                )
            });
            TextDocumentEdit {
                text_document: OptionalVersionedTextDocumentIdentifier {
                    version: document_version(docs, &uri),
                    uri,
                },
                edits: edits.into_iter().map(OneOf::Left).collect(),
            }
        })
        .collect();
    WorkspaceEdit {
        document_changes: Some(DocumentChanges::Edits(edits)),
        ..WorkspaceEdit::default()
    }
}

fn macro_at<'a>(
    document_source: &str,
    analysis: &'a DocumentAnalysis,
    position: Position,
) -> Option<&'a ProcMacroOccurrence> {
    let offset = offset_for_position(document_source, position)?;
    let origin = identifier_range_at(document_source, offset)?;
    analysis
        .macro_occurrences
        .iter()
        .find(|occurrence| analysis.local_macro_range(&occurrence.range) == Some(origin))
}

fn same_macro_binding(left: &ProcMacroOccurrence, right: &ProcMacroOccurrence) -> bool {
    match (&left.binding, &right.binding) {
        (Some(left), Some(right)) => left == right,
        (None, None) => {
            left.package == right.package
                && left.macro_name == right.macro_name
                && left.kind == right.kind
        }
        _ => false,
    }
}

fn macro_occurrence_location(
    current_uri: &lsp_types::Url,
    analysis: &DocumentAnalysis,
    occurrence: &ProcMacroOccurrence,
) -> Option<Location> {
    macro_source_location(current_uri, analysis, &occurrence.range)
}

fn macro_definition_location(
    current_uri: &lsp_types::Url,
    analysis: &DocumentAnalysis,
    occurrence: &ProcMacroOccurrence,
) -> Option<Location> {
    if let Some(definition) = &occurrence.definition {
        return Some(Location::new(
            source_uri(current_uri, &definition.path)?,
            LineIndex::new(&definition.source).range(
                &definition.source,
                text_range(definition.range.start, definition.range.end),
            )?,
        ));
    }
    macro_source_location(current_uri, analysis, occurrence.binding.as_ref()?)
}

fn macro_source_location(
    current_uri: &lsp_types::Url,
    analysis: &DocumentAnalysis,
    range: &std::ops::Range<usize>,
) -> Option<Location> {
    let source_map = analysis.macro_source_map.as_ref()?;
    let mapped = source_map.map_range(text_range(range.start, range.end))?;
    Some(Location::new(
        source_uri(current_uri, mapped.path)?,
        LineIndex::new(mapped.source).range(mapped.source, mapped.range)?,
    ))
}

fn sort_and_dedup_locations(locations: &mut Vec<Location>) {
    locations.sort_by(|left, right| {
        left.uri
            .as_str()
            .cmp(right.uri.as_str())
            .then_with(|| left.range.start.line.cmp(&right.range.start.line))
            .then_with(|| left.range.start.character.cmp(&right.range.start.character))
            .then_with(|| left.range.end.line.cmp(&right.range.end.line))
            .then_with(|| left.range.end.character.cmp(&right.range.end.character))
    });
    locations.dedup();
}

fn document_version<S: BuildHasher>(
    docs: &HashMap<lsp_types::Url, Document, S>,
    uri: &lsp_types::Url,
) -> Option<i32> {
    if let Some(document) = docs.get(uri) {
        return document.version;
    }
    let path = normalized_path(uri.to_file_path().ok()?);
    docs.iter().find_map(|(candidate, document)| {
        (candidate
            .to_file_path()
            .ok()
            .is_some_and(|candidate| normalized_path(candidate) == path))
        .then_some(document.version)
        .flatten()
    })
}

fn same_document_uri(left: &lsp_types::Url, right: &lsp_types::Url) -> bool {
    left == right
        || left
            .to_file_path()
            .ok()
            .zip(right.to_file_path().ok())
            .is_some_and(|(left, right)| normalized_path(left) == normalized_path(right))
}

pub fn validate_identifier(name: &str) -> Result<(), String> {
    let tokens = frontend::lexer::lex(name);
    if matches!(tokens.as_slice(), [token] if token.kind == SyntaxKind::Ident && token.span == (0..name.len()))
    {
        Ok(())
    } else {
        Err(format!("`{name}` is not a valid Riddle identifier"))
    }
}

fn renamable_target(analysis: &DocumentAnalysis, target: TextRange) -> Option<()> {
    analysis.source_map.as_ref().map_or_else(
        || range_is_in_source(target, analysis.source.len()).then_some(()),
        |source_map| source_map.map_range(target).map(|_| ()),
    )
}

fn location_for_range(
    current_uri: &lsp_types::Url,
    analysis: &DocumentAnalysis,
    range: TextRange,
) -> Option<Location> {
    if let Some(source_map) = &analysis.source_map {
        let mapped = source_map.map_range(range)?;
        return Some(Location::new(
            source_uri(current_uri, mapped.path)?,
            LineIndex::new(mapped.source).range(mapped.source, mapped.range)?,
        ));
    }
    Some(Location::new(
        current_uri.clone(),
        LineIndex::new(&analysis.source).range(&analysis.source, range)?,
    ))
}

pub fn source_uri(current_uri: &lsp_types::Url, path: &Path) -> Option<lsp_types::Url> {
    if path.as_os_str().is_empty() {
        Some(current_uri.clone())
    } else {
        #[cfg(target_arch = "wasm32")]
        {
            // wasm32-unknown-unknown has no host filesystem URL conversion.
            let _ = path;
            Some(current_uri.clone())
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            lsp_types::Url::from_file_path(path).ok()
        }
    }
}

#[derive(Clone)]
struct ResolvedFieldOccurrence {
    definition: TextRange,
    range: TextRange,
    shorthand: Option<ShorthandField>,
}

fn symbol_occurrences(analysis: &DocumentAnalysis, target: TextRange) -> Vec<SymbolOccurrence> {
    let Some(hir) = analysis.result.hir.as_ref() else {
        return Vec::new();
    };
    let Some(graph) = analysis.result.scope_graph.as_ref() else {
        return Vec::new();
    };
    let types = &analysis.result.type_result;
    let source = &analysis.source;
    let fields = resolved_field_occurrences(hir, types, source);
    let shorthand = fields
        .iter()
        .filter_map(|field| {
            field
                .shorthand
                .clone()
                .map(|shorthand| (field.range, shorthand))
        })
        .collect::<HashMap<_, _>>();
    let mut occurrences = fields
        .into_iter()
        .filter(|field| field.definition == target)
        .map(|field| SymbolOccurrence {
            range: field.range,
            is_declaration: false,
            shorthand: field.shorthand,
        })
        .collect::<Vec<_>>();

    for (reference, node) in graph.nodes.iter() {
        let Node::Reference {
            segments,
            origin: reference_origin,
            ..
        } = node
        else {
            continue;
        };
        let Some(path_range) = reference_path_range(hir, *reference_origin) else {
            continue;
        };
        let segment_ranges = path_segment_ranges(source, path_range, segments);
        let body = match reference_origin {
            RefOrigin::Expr { body, .. } => Some(*body),
            RefOrigin::Type { .. } => None,
        };
        for (index, range) in segment_ranges.into_iter().enumerate() {
            let Some(symbol) = symbol_for_reference_segment(
                hir, graph, types, body, reference, segments, index, range, source,
            ) else {
                continue;
            };
            if symbol.definition == Some(target) {
                occurrences.push(SymbolOccurrence {
                    range,
                    is_declaration: false,
                    shorthand: shorthand.get(&range).cloned(),
                });
            }
        }
    }

    for (body_id, body) in hir.bodies.iter() {
        for (expr_id, expr) in body.exprs.iter() {
            if !matches!(expr, Expr::FieldAccess { .. }) {
                continue;
            }
            let Some(range) = body
                .source_map
                .expr_ranges
                .get(&expr_id)
                .and_then(|range| last_identifier_range(source, *range))
            else {
                continue;
            };
            if field_access_symbol(hir, types, body_id, expr_id, range)
                .is_some_and(|symbol| symbol.definition == Some(target))
            {
                occurrences.push(SymbolOccurrence {
                    range,
                    is_declaration: false,
                    shorthand: None,
                });
            }
        }
    }

    for symbol in declaration_symbols(hir, types, source) {
        if symbol.definition == Some(target) {
            occurrences.push(SymbolOccurrence {
                range: symbol.origin,
                is_declaration: true,
                shorthand: shorthand.get(&symbol.origin).cloned(),
            });
        }
    }
    collect_use_path_occurrences(hir, graph, types, source, target, &mut occurrences);
    collect_pattern_path_occurrences(hir, types, source, target, &mut occurrences);

    deduplicate_symbol_occurrences(occurrences)
}

fn deduplicate_symbol_occurrences(mut occurrences: Vec<SymbolOccurrence>) -> Vec<SymbolOccurrence> {
    occurrences.sort_by_key(|occurrence| (occurrence.range.start(), occurrence.range.end()));
    let mut deduplicated: Vec<SymbolOccurrence> = Vec::with_capacity(occurrences.len());
    for occurrence in occurrences {
        if let Some(previous) = deduplicated
            .last_mut()
            .filter(|previous| previous.range == occurrence.range)
        {
            previous.is_declaration |= occurrence.is_declaration;
            if previous.shorthand.is_none() {
                previous.shorthand = occurrence.shorthand;
            }
        } else {
            deduplicated.push(occurrence);
        }
    }
    deduplicated
}

fn symbol_at(
    document_source: &str,
    analysis: &DocumentAnalysis,
    position: Position,
) -> Option<Symbol> {
    let offset = offset_for_position(document_source, position)?;
    let origin = identifier_range_at(document_source, offset)?;
    let hir = analysis.result.hir.as_ref()?;
    let graph = analysis.result.scope_graph.as_ref()?;

    method_or_field_symbol(hir, &analysis.result.type_result, analysis, origin)
        .or_else(|| reference_symbol(hir, graph, &analysis.result.type_result, analysis, origin))
        .or_else(|| use_path_symbol(hir, graph, &analysis.result.type_result, analysis, origin))
        .or_else(|| declaration_symbol(hir, &analysis.result.type_result, analysis, origin))
        .or_else(|| field_label_symbol(hir, &analysis.result.type_result, analysis, origin))
        .or_else(|| pattern_path_symbol(hir, &analysis.result.type_result, analysis, origin))
        .or_else(|| inferred_expression_symbol(hir, &analysis.result.type_result, analysis, origin))
        .or_else(|| {
            let text = &document_source[origin];
            BUILTIN_TYPES.contains(&text).then(|| Symbol {
                origin,
                detail: format!("builtin type {text}"),
                definition: None,
                implementations: Vec::new(),
            })
        })
}

fn method_or_field_symbol(
    hir: &HirFile,
    types: &TypeCheckResult,
    analysis: &DocumentAnalysis,
    origin: TextRange,
) -> Option<Symbol> {
    for (body_id, body) in hir.bodies.iter() {
        for (expr_id, expr) in body.exprs.iter() {
            let Expr::FieldAccess { .. } = expr else {
                continue;
            };
            let Some(range) = body.source_map.expr_ranges.get(&expr_id).copied() else {
                continue;
            };
            let Some(field_range) = last_identifier_range(&analysis.source, range) else {
                continue;
            };
            if analysis.local_range(field_range) != Some(origin) {
                continue;
            }
            return field_access_symbol(hir, types, body_id, expr_id, origin);
        }
    }
    None
}

fn field_access_symbol(
    hir: &HirFile,
    types: &TypeCheckResult,
    body_id: BodyId,
    expr_id: hir::body::ExprId,
    origin: TextRange,
) -> Option<Symbol> {
    let Expr::FieldAccess { base, field } = &hir.bodies[body_id].exprs[expr_id] else {
        return None;
    };
    if let Some(call) = types.trait_method_calls.get(&(body_id, expr_id)) {
        return trait_method_symbol(
            hir,
            types,
            body_id,
            expr_id,
            call.trait_id,
            &call.method,
            origin,
        );
    }
    if let Some(Type::FunctionItem { function, .. }) = types.expr_types.get(&(body_id, expr_id)) {
        let mut symbol = function_symbol(hir, *function, origin);
        if let Some(signature) = types.method_signatures.get(&(body_id, expr_id)) {
            symbol.detail =
                format_instantiated_method(hir, &hir.item_tree.functions[*function], signature)
                    .unwrap_or(symbol.detail);
        }
        return Some(symbol);
    }

    let struct_id = receiver_struct_id(types.expr_types.get(&(body_id, *base))?)?;
    let field = hir.item_tree.structs[struct_id]
        .fields
        .iter()
        .find(|candidate| candidate.name == *field)?;
    Some(Symbol {
        origin,
        detail: format!("field {}: {}", field.name.0, field.ty.display()),
        definition: Some(field.name_range),
        implementations: Vec::new(),
    })
}

fn reference_symbol(
    hir: &HirFile,
    graph: &ScopeGraph,
    types: &TypeCheckResult,
    analysis: &DocumentAnalysis,
    origin: TextRange,
) -> Option<Symbol> {
    for (reference, node) in graph.nodes.iter() {
        let Node::Reference {
            segments,
            origin: reference_origin,
            ..
        } = node
        else {
            continue;
        };
        let Some(path_range) = reference_path_range(hir, *reference_origin) else {
            continue;
        };
        let segment_ranges = path_segment_ranges(&analysis.source, path_range, segments);
        let Some(index) = segment_ranges
            .iter()
            .position(|range| analysis.local_range(*range) == Some(origin))
        else {
            continue;
        };
        let body = match reference_origin {
            RefOrigin::Expr { body, .. } => Some(*body),
            RefOrigin::Type { .. } => None,
        };
        return symbol_for_reference_segment(
            hir,
            graph,
            types,
            body,
            reference,
            segments,
            index,
            origin,
            &analysis.source,
        );
    }
    None
}

#[allow(clippy::too_many_arguments)]
fn symbol_for_reference_segment(
    hir: &HirFile,
    graph: &ScopeGraph,
    types: &TypeCheckResult,
    body: Option<BodyId>,
    reference: scope_graph::NodeId,
    segments: &[Name],
    index: usize,
    origin: TextRange,
    source: &str,
) -> Option<Symbol> {
    let mut symbol = resolve_path_at_reference(graph, reference, &segments[..=index])
        .into_iter()
        .find_map(|definition| {
            symbol_for_definition(hir, types, body, definition, origin, source)
        })?;
    if index == 0
        && let Some(alias_range) =
            explicit_alias_at_reference(hir, graph, reference, &segments[0], source)
    {
        symbol.definition = Some(alias_range);
    }
    Some(symbol)
}

fn explicit_alias_at_reference(
    hir: &HirFile,
    graph: &ScopeGraph,
    reference: scope_graph::NodeId,
    name: &Name,
    source: &str,
) -> Option<TextRange> {
    visible_definitions(graph, reference)
        .into_iter()
        .find_map(|(candidate, definition)| {
            if candidate != *name {
                return None;
            }
            let DefRef::UseAlias { use_range, .. } = definition else {
                return None;
            };
            explicit_alias_range(hir, source, use_range)
        })
}

fn declaration_symbol(
    hir: &HirFile,
    types: &TypeCheckResult,
    analysis: &DocumentAnalysis,
    origin: TextRange,
) -> Option<Symbol> {
    if let Some(mut symbol) = use_alias_symbols(hir, &analysis.source)
        .into_iter()
        .find(|symbol| analysis.local_range(symbol.origin) == Some(origin))
    {
        symbol.origin = origin;
        return Some(symbol);
    }
    for (trait_id, tr) in hir.item_tree.traits.iter() {
        if analysis.local_range(tr.name_range) == Some(origin) {
            return Some(trait_symbol(hir, trait_id, origin));
        }
        for method in &tr.methods {
            if analysis.local_range(method.name_range) == Some(origin) {
                return Some(trait_method_declaration_symbol(
                    hir, trait_id, method, origin,
                ));
            }
        }
    }
    for (function_id, function) in hir.item_tree.functions.iter() {
        if analysis.local_range(function.name_range) == Some(origin) {
            return Some(function_symbol(hir, function_id, origin));
        }
        for parameter in &function.params {
            if parameter.name.0 != "self"
                && analysis.local_range(parameter.name_range) == Some(origin)
            {
                return Some(Symbol {
                    origin,
                    detail: format!("parameter {}: {}", parameter.name.0, parameter.ty.display()),
                    definition: Some(parameter.name_range),
                    implementations: Vec::new(),
                });
            }
        }
    }
    if let Some(symbol) = nominal_declaration_symbol(hir, analysis, origin) {
        return Some(symbol);
    }
    for (_, konst) in hir.item_tree.consts.iter() {
        if analysis.local_range(konst.name_range) == Some(origin) {
            return Some(Symbol {
                origin,
                detail: format!("const {}: {}", konst.name.0, konst.ty.display()),
                definition: Some(konst.name_range),
                implementations: Vec::new(),
            });
        }
    }
    for (_, alias) in hir.item_tree.type_aliases.iter() {
        if analysis.local_range(alias.name_range) == Some(origin) {
            return Some(Symbol {
                origin,
                detail: alias.ty.as_ref().map_or_else(
                    || format!("type {}", alias.name.0),
                    |ty| format!("type {} = {}", alias.name.0, ty.display()),
                ),
                definition: Some(alias.name_range),
                implementations: Vec::new(),
            });
        }
    }
    for (_, module) in hir.item_tree.modules.iter() {
        if analysis.local_range(module.name_range) == Some(origin) {
            return Some(Symbol {
                origin,
                detail: format!("mod {}", module.name.0),
                definition: Some(module.name_range),
                implementations: Vec::new(),
            });
        }
    }
    body_declaration_symbol(hir, types, analysis, origin)
}

fn nominal_declaration_symbol(
    hir: &HirFile,
    analysis: &DocumentAnalysis,
    origin: TextRange,
) -> Option<Symbol> {
    for (_, strukt) in hir.item_tree.structs.iter() {
        if analysis.local_range(strukt.name_range) == Some(origin) {
            return Some(Symbol {
                origin,
                detail: format_struct(strukt),
                definition: Some(strukt.name_range),
                implementations: Vec::new(),
            });
        }
        for field in &strukt.fields {
            if analysis.local_range(field.name_range) == Some(origin) {
                return Some(field_symbol(field, origin));
            }
        }
    }
    for (_, enumeration) in hir.item_tree.enums.iter() {
        if analysis.local_range(enumeration.name_range) == Some(origin) {
            return Some(Symbol {
                origin,
                detail: format_enum(enumeration),
                definition: Some(enumeration.name_range),
                implementations: Vec::new(),
            });
        }
        for variant in &enumeration.variants {
            if analysis.local_range(variant.name_range) == Some(origin) {
                return Some(Symbol {
                    origin,
                    detail: format!(
                        "variant {}::{}",
                        enumeration.name.0,
                        format_enum_variant(variant)
                    ),
                    definition: Some(variant.name_range),
                    implementations: Vec::new(),
                });
            }
            if let HirVariantKind::Struct(fields) = &variant.kind {
                for field in fields {
                    if analysis.local_range(field.name_range) == Some(origin) {
                        return Some(field_symbol(field, origin));
                    }
                }
            }
        }
    }
    None
}

fn body_declaration_symbol(
    hir: &HirFile,
    types: &TypeCheckResult,
    analysis: &DocumentAnalysis,
    origin: TextRange,
) -> Option<Symbol> {
    for (body_id, body) in hir.bodies.iter() {
        for (pat_id, pattern) in body.pats.iter() {
            let mut bindings = Vec::new();
            match pattern {
                Pattern::Binding { name, .. } => bindings.push((
                    name,
                    PatternBindingId {
                        pattern: pat_id,
                        field: None,
                    },
                )),
                Pattern::Struct { fields, .. } => {
                    bindings.extend(fields.iter().enumerate().filter_map(|(index, field)| {
                        field.pat.is_none().then_some((
                            &field.name,
                            PatternBindingId {
                                pattern: pat_id,
                                field: Some(index),
                            },
                        ))
                    }));
                }
                _ => {}
            }
            for (name, binding) in bindings {
                let Some(name_range) =
                    pattern_binding_name_range(hir, body_id, binding, &analysis.source)
                else {
                    continue;
                };
                if analysis.local_range(name_range) == Some(origin) {
                    return Some(pattern_binding_symbol(
                        hir, types, body_id, binding, name, origin, name_range,
                    ));
                }
            }
        }
        for (lambda_expr, expr) in body.exprs.iter() {
            let Expr::Lambda { params, .. } = expr else {
                continue;
            };
            for (index, parameter) in params.iter().enumerate() {
                if parameter
                    .name_range
                    .is_some_and(|range| analysis.local_range(range) == Some(origin))
                {
                    let displayed =
                        inferred_lambda_parameter_type(hir, types, body_id, lambda_expr, index)
                            .unwrap_or_else(|| parameter.ty.display());
                    return Some(Symbol {
                        origin,
                        detail: format!("parameter {}: {}", parameter.name.0, displayed),
                        definition: parameter.name_range,
                        implementations: Vec::new(),
                    });
                }
            }
        }
    }
    None
}

fn declaration_symbols(hir: &HirFile, types: &TypeCheckResult, source: &str) -> Vec<Symbol> {
    let mut symbols = use_alias_symbols(hir, source);
    for (trait_id, tr) in hir.item_tree.traits.iter() {
        symbols.push(trait_symbol(hir, trait_id, tr.name_range));
        symbols.extend(tr.methods.iter().map(|method| {
            trait_method_declaration_symbol(hir, trait_id, method, method.name_range)
        }));
    }
    for (function_id, function) in hir.item_tree.functions.iter() {
        symbols.push(function_symbol(hir, function_id, function.name_range));
        symbols.extend(
            function
                .params
                .iter()
                .filter(|parameter| parameter.name.0 != "self")
                .map(|parameter| Symbol {
                    origin: parameter.name_range,
                    detail: format!("parameter {}: {}", parameter.name.0, parameter.ty.display()),
                    definition: Some(parameter.name_range),
                    implementations: Vec::new(),
                }),
        );
    }
    for (_, strukt) in hir.item_tree.structs.iter() {
        symbols.push(Symbol {
            origin: strukt.name_range,
            detail: format_struct(strukt),
            definition: Some(strukt.name_range),
            implementations: Vec::new(),
        });
        symbols.extend(
            strukt
                .fields
                .iter()
                .map(|field| field_symbol(field, field.name_range)),
        );
    }
    for (_, enumeration) in hir.item_tree.enums.iter() {
        symbols.push(Symbol {
            origin: enumeration.name_range,
            detail: format_enum(enumeration),
            definition: Some(enumeration.name_range),
            implementations: Vec::new(),
        });
        for variant in &enumeration.variants {
            symbols.push(Symbol {
                origin: variant.name_range,
                detail: format!(
                    "variant {}::{}",
                    enumeration.name.0,
                    format_enum_variant(variant)
                ),
                definition: Some(variant.name_range),
                implementations: Vec::new(),
            });
            if let HirVariantKind::Struct(fields) = &variant.kind {
                symbols.extend(
                    fields
                        .iter()
                        .map(|field| field_symbol(field, field.name_range)),
                );
            }
        }
    }
    symbols.extend(hir.item_tree.consts.iter().map(|(_, item)| Symbol {
        origin: item.name_range,
        detail: format!("const {}: {}", item.name.0, item.ty.display()),
        definition: Some(item.name_range),
        implementations: Vec::new(),
    }));
    symbols.extend(hir.item_tree.type_aliases.iter().map(|(_, item)| Symbol {
        origin: item.name_range,
        detail: item.ty.as_ref().map_or_else(
            || format!("type {}", item.name.0),
            |ty| format!("type {} = {}", item.name.0, ty.display()),
        ),
        definition: Some(item.name_range),
        implementations: Vec::new(),
    }));
    symbols.extend(hir.item_tree.modules.iter().map(|(_, item)| Symbol {
        origin: item.name_range,
        detail: format!("mod {}", item.name.0),
        definition: Some(item.name_range),
        implementations: Vec::new(),
    }));

    collect_body_declaration_symbols(hir, types, source, &mut symbols);
    symbols
}

fn collect_body_declaration_symbols(
    hir: &HirFile,
    types: &TypeCheckResult,
    source: &str,
    symbols: &mut Vec<Symbol>,
) {
    for (body_id, body) in hir.bodies.iter() {
        for (pat_id, pattern) in body.pats.iter() {
            let mut bindings = Vec::new();
            match pattern {
                Pattern::Binding { name, .. } => bindings.push((
                    name,
                    PatternBindingId {
                        pattern: pat_id,
                        field: None,
                    },
                )),
                Pattern::Struct { fields, .. } => {
                    bindings.extend(fields.iter().enumerate().filter_map(|(index, field)| {
                        field.pat.is_none().then_some((
                            &field.name,
                            PatternBindingId {
                                pattern: pat_id,
                                field: Some(index),
                            },
                        ))
                    }));
                }
                _ => {}
            }
            for (name, binding) in bindings {
                if let Some(name_range) = pattern_binding_name_range(hir, body_id, binding, source)
                {
                    symbols.push(pattern_binding_symbol(
                        hir, types, body_id, binding, name, name_range, name_range,
                    ));
                }
            }
        }
        for (_, expr) in body.exprs.iter() {
            let Expr::Lambda { params, .. } = expr else {
                continue;
            };
            symbols.extend(params.iter().filter_map(|parameter| {
                let range = parameter.name_range?;
                Some(parameter_symbol(
                    &parameter.name,
                    &parameter.ty,
                    range,
                    Some(range),
                ))
            }));
        }
    }
}

fn field_symbol(field: &HirStructField, origin: TextRange) -> Symbol {
    Symbol {
        origin,
        detail: format!("field {}: {}", field.name.0, field.ty.display()),
        definition: Some(field.name_range),
        implementations: Vec::new(),
    }
}

fn pattern_binding_symbol(
    hir: &HirFile,
    types: &TypeCheckResult,
    body: BodyId,
    binding: PatternBindingId,
    name: &Name,
    origin: TextRange,
    definition: TextRange,
) -> Symbol {
    let ty = types
        .pattern_binding_types
        .get(&(body, binding))
        .map_or_else(|| "_".into(), |ty| ty.display(hir));
    Symbol {
        origin,
        detail: format!("let {}: {ty}", name.0),
        definition: Some(definition),
        implementations: Vec::new(),
    }
}

fn pattern_binding_name_range(
    hir: &HirFile,
    body_id: BodyId,
    binding: PatternBindingId,
    source: &str,
) -> Option<TextRange> {
    let body = &hir.bodies[body_id];
    let pattern_range = body.source_map.pat_ranges.get(&binding.pattern).copied()?;
    let pattern = &body.pats[binding.pattern];
    let Some(field_index) = binding.field else {
        let Pattern::Binding { name, .. } = pattern else {
            return None;
        };
        return identifier_named_in_range(source, pattern_range, &name.0);
    };
    let Pattern::Struct { path, fields } = pattern else {
        return None;
    };
    let labels = field_labels_in_range(
        source,
        pattern_range,
        path.range.end(),
        fields.iter().map(|field| field.name.0.as_str()),
    );
    labels.get(field_index).map(|label| label.range)
}

fn use_alias_symbols(hir: &HirFile, source: &str) -> Vec<Symbol> {
    let mut symbols = Vec::new();
    for (_, item) in hir.item_tree.uses.iter() {
        collect_use_alias_symbols(&item.tree, source, &mut symbols);
    }
    symbols
}

fn collect_use_alias_symbols(tree: &HirUseTree, source: &str, symbols: &mut Vec<Symbol>) {
    match &tree.kind {
        HirUseTreeKind::Simple { alias: Some(alias) } => {
            if let Some(range) = last_identifier_named_in_range(source, tree.range, &alias.0) {
                symbols.push(Symbol {
                    origin: range,
                    detail: format!("use {} as {}", tree.prefix.display(), alias.0),
                    definition: Some(range),
                    implementations: Vec::new(),
                });
            }
        }
        HirUseTreeKind::List(children) => {
            for child in children {
                collect_use_alias_symbols(child, source, symbols);
            }
        }
        HirUseTreeKind::Simple { alias: None } | HirUseTreeKind::Glob => {}
    }
}

fn explicit_alias_range(hir: &HirFile, source: &str, use_range: TextRange) -> Option<TextRange> {
    hir.item_tree
        .uses
        .iter()
        .find_map(|(_, item)| explicit_alias_range_in_tree(&item.tree, source, use_range))
}

fn explicit_alias_range_in_tree(
    tree: &HirUseTree,
    source: &str,
    use_range: TextRange,
) -> Option<TextRange> {
    if tree.range == use_range
        && let HirUseTreeKind::Simple { alias: Some(alias) } = &tree.kind
    {
        return last_identifier_named_in_range(source, tree.range, &alias.0);
    }
    let HirUseTreeKind::List(children) = &tree.kind else {
        return None;
    };
    children
        .iter()
        .find_map(|child| explicit_alias_range_in_tree(child, source, use_range))
}

#[derive(Clone)]
struct FieldLabel {
    name: String,
    range: TextRange,
    shorthand: bool,
}

fn resolved_field_occurrences(
    hir: &HirFile,
    types: &TypeCheckResult,
    source: &str,
) -> Vec<ResolvedFieldOccurrence> {
    let mut occurrences = Vec::new();
    for (body_id, body) in hir.bodies.iter() {
        for (expr_id, expr) in body.exprs.iter() {
            let Expr::Struct { path, fields, .. } = expr else {
                continue;
            };
            let Some(definitions) = fields_for_struct_expression(hir, expr) else {
                continue;
            };
            let Some(range) = body.source_map.expr_ranges.get(&expr_id).copied() else {
                continue;
            };
            let labels = field_labels_in_range(
                source,
                range,
                path.range.end(),
                fields.iter().map(|field| field.name.0.as_str()),
            );
            push_resolved_field_labels(definitions, labels, &mut occurrences);
        }
        for (pat_id, pattern) in body.pats.iter() {
            let Pattern::Struct { path, fields } = pattern else {
                continue;
            };
            let Some(definitions) = fields_for_struct_pattern(hir, types, body_id, pat_id, path)
            else {
                continue;
            };
            let Some(range) = body.source_map.pat_ranges.get(&pat_id).copied() else {
                continue;
            };
            let labels = field_labels_in_range(
                source,
                range,
                path.range.end(),
                fields.iter().map(|field| field.name.0.as_str()),
            );
            push_resolved_field_labels(definitions, labels, &mut occurrences);
        }
    }
    occurrences
}

fn push_resolved_field_labels(
    definitions: &[HirStructField],
    labels: Vec<FieldLabel>,
    occurrences: &mut Vec<ResolvedFieldOccurrence>,
) {
    for label in labels {
        let Some(field) = definitions.iter().find(|field| field.name.0 == label.name) else {
            continue;
        };
        occurrences.push(ResolvedFieldOccurrence {
            definition: field.name_range,
            range: label.range,
            shorthand: label.shorthand.then_some(ShorthandField {
                name: label.name,
                definition: field.name_range,
            }),
        });
    }
}

pub(crate) fn fields_for_struct_expression<'a>(
    hir: &'a HirFile,
    expr: &Expr,
) -> Option<&'a [HirStructField]> {
    let Expr::Struct { resolved, .. } = expr else {
        return None;
    };
    match resolved.as_ref()? {
        ResolvedName::Struct(id) => Some(&hir.item_tree.structs[*id].fields),
        ResolvedName::EnumVariant(enum_id, index) => {
            let HirVariantKind::Struct(fields) =
                &hir.item_tree.enums[*enum_id].variants.get(*index)?.kind
            else {
                return None;
            };
            Some(fields)
        }
        _ => None,
    }
}

pub(crate) fn fields_for_struct_pattern<'a>(
    hir: &'a HirFile,
    types: &TypeCheckResult,
    body: BodyId,
    pattern: hir::body::PatId,
    path: &hir::item_tree::HirPath,
) -> Option<&'a [HirStructField]> {
    match dereferenced_type(types.pattern_types.get(&(body, pattern))?) {
        Type::Struct(id, _) => Some(&hir.item_tree.structs[*id].fields),
        Type::Enum(id, _) => {
            let name = &path.segments.last()?.0;
            let variant = hir.item_tree.enums[*id]
                .variants
                .iter()
                .find(|variant| variant.name.0 == *name)?;
            let HirVariantKind::Struct(fields) = &variant.kind else {
                return None;
            };
            Some(fields)
        }
        _ => None,
    }
}

fn dereferenced_type(mut ty: &Type) -> &Type {
    while let Type::Ref(inner, _) | Type::Ptr { inner, .. } = ty {
        ty = inner;
    }
    ty
}

fn field_label_symbol(
    hir: &HirFile,
    types: &TypeCheckResult,
    analysis: &DocumentAnalysis,
    origin: TextRange,
) -> Option<Symbol> {
    let field = resolved_field_occurrences(hir, types, &analysis.source)
        .into_iter()
        .find(|field| analysis.local_range(field.range) == Some(origin))?;
    field_symbol_for_definition(hir, field.definition, origin)
}

fn field_symbol_for_definition(
    hir: &HirFile,
    definition: TextRange,
    origin: TextRange,
) -> Option<Symbol> {
    for (_, strukt) in hir.item_tree.structs.iter() {
        if let Some(field) = strukt
            .fields
            .iter()
            .find(|field| field.name_range == definition)
        {
            return Some(field_symbol(field, origin));
        }
    }
    for (_, enumeration) in hir.item_tree.enums.iter() {
        for variant in &enumeration.variants {
            let HirVariantKind::Struct(fields) = &variant.kind else {
                continue;
            };
            if let Some(field) = fields.iter().find(|field| field.name_range == definition) {
                return Some(field_symbol(field, origin));
            }
        }
    }
    None
}

fn field_labels_in_range<'a>(
    source: &str,
    container: TextRange,
    fields_start: TextSize,
    expected: impl Iterator<Item = &'a str>,
) -> Vec<FieldLabel> {
    let start = usize::from(fields_start.max(container.start()));
    let end = usize::from(container.end());
    let Some(text) = source.get(start..end) else {
        return Vec::new();
    };
    let expected = expected.collect::<Vec<_>>();
    let tokens = frontend::lexer::lex(text);
    let mut labels = Vec::with_capacity(expected.len());
    let mut braces = 0usize;
    let mut parentheses = 0usize;
    let mut brackets = 0usize;
    let mut expecting_field = false;
    for (index, token) in tokens.iter().enumerate() {
        match token.kind {
            SyntaxKind::LBrace => {
                braces += 1;
                if braces == 1 {
                    expecting_field = true;
                }
            }
            SyntaxKind::RBrace => {
                if braces == 1 {
                    break;
                }
                braces = braces.saturating_sub(1);
            }
            SyntaxKind::LParen if braces > 0 => parentheses += 1,
            SyntaxKind::RParen if braces > 0 => parentheses = parentheses.saturating_sub(1),
            SyntaxKind::LBracket if braces > 0 => brackets += 1,
            SyntaxKind::RBracket if braces > 0 => brackets = brackets.saturating_sub(1),
            SyntaxKind::Comma if braces == 1 && parentheses == 0 && brackets == 0 => {
                expecting_field = true;
            }
            SyntaxKind::Ident
                if braces == 1
                    && parentheses == 0
                    && brackets == 0
                    && expecting_field
                    && expected.get(labels.len()).copied() == Some(token.text(text)) =>
            {
                let shorthand = tokens[index + 1..]
                    .iter()
                    .find(|next| {
                        !matches!(next.kind, SyntaxKind::Whitespace | SyntaxKind::LineComment)
                    })
                    .is_none_or(|next| next.kind != SyntaxKind::Colon);
                labels.push(FieldLabel {
                    name: token.text(text).into(),
                    range: text_range(start + token.span.start, start + token.span.end),
                    shorthand,
                });
                expecting_field = false;
            }
            _ => {}
        }
    }
    labels
}

fn pattern_path_symbol(
    hir: &HirFile,
    types: &TypeCheckResult,
    analysis: &DocumentAnalysis,
    origin: TextRange,
) -> Option<Symbol> {
    for (body_id, body) in hir.bodies.iter() {
        for (pat_id, pattern) in body.pats.iter() {
            let (Pattern::Path { path }
            | Pattern::TupleStruct { path, .. }
            | Pattern::Struct { path, .. }) = pattern
            else {
                continue;
            };
            let ranges = path_segment_ranges(&analysis.source, path.range, &path.segments);
            let Some(index) = ranges
                .iter()
                .position(|range| analysis.local_range(*range) == Some(origin))
            else {
                continue;
            };
            let Some(definition) =
                pattern_path_definition(hir, types, body_id, pat_id, path, index)
            else {
                continue;
            };
            return symbol_for_definition(
                hir,
                types,
                Some(body_id),
                definition,
                origin,
                &analysis.source,
            );
        }
    }
    None
}

fn pattern_path_definition(
    hir: &HirFile,
    types: &TypeCheckResult,
    body: BodyId,
    pattern: hir::body::PatId,
    path: &hir::item_tree::HirPath,
    index: usize,
) -> Option<DefRef> {
    let ty = dereferenced_type(types.pattern_types.get(&(body, pattern))?);
    match ty {
        Type::Struct(struct_id, _) if index + 1 == path.segments.len() => (path.segments.last()?
            == &hir.item_tree.structs[*struct_id].name)
            .then_some(DefRef::Struct(*struct_id)),
        Type::Enum(enum_id, _) => {
            let enumeration = &hir.item_tree.enums[*enum_id];
            if index + 1 == path.segments.len() {
                let variant = enumeration
                    .variants
                    .iter()
                    .position(|variant| Some(&variant.name) == path.segments.last())?;
                return Some(DefRef::EnumVariant {
                    enum_id: *enum_id,
                    index: variant,
                });
            }
            if path.segments.get(index) == Some(&enumeration.name) {
                return Some(DefRef::Enum(*enum_id));
            }
            None
        }
        _ => None,
    }
}

fn collect_pattern_path_occurrences(
    hir: &HirFile,
    types: &TypeCheckResult,
    source: &str,
    target: TextRange,
    occurrences: &mut Vec<SymbolOccurrence>,
) {
    for (body_id, body) in hir.bodies.iter() {
        for (pat_id, pattern) in body.pats.iter() {
            let (Pattern::Path { path }
            | Pattern::TupleStruct { path, .. }
            | Pattern::Struct { path, .. }) = pattern
            else {
                continue;
            };
            let ranges = path_segment_ranges(source, path.range, &path.segments);
            for (index, range) in ranges.into_iter().enumerate() {
                let Some(definition) =
                    pattern_path_definition(hir, types, body_id, pat_id, path, index)
                else {
                    continue;
                };
                let Some(symbol) =
                    symbol_for_definition(hir, types, Some(body_id), definition, range, source)
                else {
                    continue;
                };
                if symbol.definition == Some(target) {
                    occurrences.push(SymbolOccurrence {
                        range,
                        is_declaration: false,
                        shorthand: None,
                    });
                }
            }
        }
    }
}

fn collect_use_path_occurrences(
    hir: &HirFile,
    graph: &ScopeGraph,
    types: &TypeCheckResult,
    source: &str,
    target: TextRange,
    occurrences: &mut Vec<SymbolOccurrence>,
) {
    occurrences.extend(
        resolved_use_path_symbols(hir, graph, types, source)
            .into_iter()
            .filter(|symbol| symbol.definition == Some(target))
            .map(|symbol| SymbolOccurrence {
                range: symbol.origin,
                is_declaration: false,
                shorthand: None,
            }),
    );
}

fn use_path_symbol(
    hir: &HirFile,
    graph: &ScopeGraph,
    types: &TypeCheckResult,
    analysis: &DocumentAnalysis,
    origin: TextRange,
) -> Option<Symbol> {
    let mut symbol = resolved_use_path_symbols(hir, graph, types, &analysis.source)
        .into_iter()
        .find(|symbol| analysis.local_range(symbol.origin) == Some(origin))?;
    symbol.origin = origin;
    Some(symbol)
}

fn resolved_use_path_symbols(
    hir: &HirFile,
    graph: &ScopeGraph,
    types: &TypeCheckResult,
    source: &str,
) -> Vec<Symbol> {
    let mut symbols = Vec::new();
    for (_, item) in hir.item_tree.uses.iter() {
        collect_use_tree_path_symbols(&item.tree, &[], hir, graph, types, source, &mut symbols);
    }
    symbols
}

fn collect_use_tree_path_symbols(
    tree: &HirUseTree,
    inherited: &[(Name, TextRange)],
    hir: &HirFile,
    graph: &ScopeGraph,
    types: &TypeCheckResult,
    source: &str,
    symbols: &mut Vec<Symbol>,
) {
    let mut path = inherited.to_vec();
    let ranges = path_segment_ranges(source, tree.prefix.range, &tree.prefix.segments);
    path.extend(tree.prefix.segments.iter().cloned().zip(ranges));
    match &tree.kind {
        HirUseTreeKind::Simple { .. } => {
            let Some((anchor, rewrite_to)) = use_alias_definition(graph, tree.range) else {
                return;
            };
            if rewrite_to.len() != path.len() {
                return;
            }
            for (index, (_, range)) in path.iter().enumerate() {
                let Some(symbol) = resolve_path_from(graph, anchor, &rewrite_to[..=index])
                    .into_iter()
                    .find_map(|definition| {
                        symbol_for_definition(hir, types, None, definition, *range, source)
                    })
                else {
                    continue;
                };
                symbols.push(symbol);
            }
        }
        HirUseTreeKind::List(children) => {
            for child in children {
                collect_use_tree_path_symbols(child, &path, hir, graph, types, source, symbols);
            }
        }
        HirUseTreeKind::Glob => {}
    }
}

fn use_alias_definition(
    graph: &ScopeGraph,
    use_range: TextRange,
) -> Option<(scope_graph::NodeId, Vec<Name>)> {
    graph.nodes.iter().find_map(|(_, node)| {
        let Node::PopSymbol {
            define:
                DefRef::UseAlias {
                    rewrite_to,
                    anchor,
                    use_range: candidate,
                },
            ..
        } = node
        else {
            return None;
        };
        (*candidate == use_range).then(|| (*anchor, rewrite_to.clone()))
    })
}

fn inferred_expression_symbol(
    hir: &HirFile,
    types: &TypeCheckResult,
    analysis: &DocumentAnalysis,
    origin: TextRange,
) -> Option<Symbol> {
    let (_, ty) = types
        .expr_types
        .iter()
        .filter_map(|((body_id, expr_id), ty)| {
            let range = hir.bodies[*body_id].source_map.expr_ranges.get(expr_id)?;
            let local = analysis.local_range(*range)?;
            (local.start() <= origin.start() && origin.end() <= local.end())
                .then_some((local.len(), ty))
        })
        .min_by_key(|(length, _)| *length)?;
    Some(Symbol {
        origin,
        detail: ty.display(hir),
        definition: None,
        implementations: Vec::new(),
    })
}

fn symbol_for_definition(
    hir: &HirFile,
    types: &TypeCheckResult,
    body: Option<BodyId>,
    definition: DefRef,
    origin: TextRange,
    source: &str,
) -> Option<Symbol> {
    match definition {
        DefRef::Function(function) => Some(function_symbol(hir, function, origin)),
        DefRef::Struct(id) => {
            let item = &hir.item_tree.structs[id];
            Some(Symbol {
                origin,
                detail: format_struct(item),
                definition: Some(item.name_range),
                implementations: Vec::new(),
            })
        }
        DefRef::Enum(id) => {
            let item = &hir.item_tree.enums[id];
            Some(Symbol {
                origin,
                detail: format_enum(item),
                definition: Some(item.name_range),
                implementations: Vec::new(),
            })
        }
        DefRef::Trait(id) => Some(trait_symbol(hir, id, origin)),
        DefRef::Const(id) => {
            let item = &hir.item_tree.consts[id];
            Some(Symbol {
                origin,
                detail: format!("const {}: {}", item.name.0, item.ty.display()),
                definition: Some(item.name_range),
                implementations: Vec::new(),
            })
        }
        DefRef::TypeAlias(id) => {
            let item = &hir.item_tree.type_aliases[id];
            Some(Symbol {
                origin,
                detail: item.ty.as_ref().map_or_else(
                    || format!("type {}", item.name.0),
                    |ty| format!("type {} = {}", item.name.0, ty.display()),
                ),
                definition: Some(item.name_range),
                implementations: Vec::new(),
            })
        }
        DefRef::Module { id, .. } => {
            let item = &hir.item_tree.modules[id];
            Some(Symbol {
                origin,
                detail: format!("mod {}", item.name.0),
                definition: Some(item.name_range),
                implementations: Vec::new(),
            })
        }
        DefRef::PatternBinding { name, id } => {
            let body = body?;
            let name_range = pattern_binding_name_range(hir, body, id, source)?;
            Some(pattern_binding_symbol(
                hir, types, body, id, &name, origin, name_range,
            ))
        }
        DefRef::Param { fn_id, index } => {
            let parameter = &hir.item_tree.functions[fn_id].params[index];
            Some(parameter_symbol(
                &parameter.name,
                &parameter.ty,
                origin,
                Some(parameter.name_range),
            ))
        }
        DefRef::LambdaParam {
            body_id,
            lambda,
            index,
        } => lambda_parameter_symbol(hir, types, body_id, lambda, index, origin),
        DefRef::ConstParam { name } => Some(Symbol {
            origin,
            detail: format!("const parameter {}", name.0),
            definition: None,
            implementations: Vec::new(),
        }),
        DefRef::EnumVariant { enum_id, index } => {
            let enumeration = &hir.item_tree.enums[enum_id];
            let variant = enumeration.variants.get(index)?;
            Some(Symbol {
                origin,
                detail: format!(
                    "variant {}::{}",
                    enumeration.name.0,
                    format_enum_variant(variant)
                ),
                definition: Some(variant.name_range),
                implementations: Vec::new(),
            })
        }
        DefRef::UseAlias { .. } => None,
    }
}

fn lambda_parameter_symbol(
    hir: &HirFile,
    types: &TypeCheckResult,
    body_id: BodyId,
    lambda: ExprId,
    index: usize,
    origin: TextRange,
) -> Option<Symbol> {
    let Expr::Lambda { params, .. } = &hir.bodies[body_id].exprs[lambda] else {
        return None;
    };
    let parameter = params.get(index)?;
    let displayed = inferred_lambda_parameter_type(hir, types, body_id, lambda, index)
        .unwrap_or_else(|| parameter.ty.display());
    Some(Symbol {
        origin,
        detail: format!("parameter {}: {}", parameter.name.0, displayed),
        definition: parameter.name_range,
        implementations: Vec::new(),
    })
}

/// The type-checker-inferred type of an unannotated lambda parameter, rendered
/// for hover. Returns `None` when inference failed or the parameter is typed in
/// the source, in which case the declared type is displayed instead.
fn inferred_lambda_parameter_type(
    hir: &HirFile,
    types: &TypeCheckResult,
    body_id: BodyId,
    lambda: ExprId,
    index: usize,
) -> Option<String> {
    let signature = types
        .expr_types
        .get(&(body_id, lambda))?
        .callable_signature()?;
    let ty = signature.params.get(index)?;
    crate::inlay_hints::is_displayable_type(ty).then(|| ty.display(hir))
}

fn parameter_symbol(
    name: &Name,
    ty: &HirTypeRef,
    origin: TextRange,
    definition: Option<TextRange>,
) -> Symbol {
    Symbol {
        origin,
        detail: format!("parameter {}: {}", name.0, ty.display()),
        definition,
        implementations: Vec::new(),
    }
}

fn function_symbol(hir: &HirFile, function_id: FunctionId, origin: TextRange) -> Symbol {
    let function = &hir.item_tree.functions[function_id];
    if let Some((trait_id, _)) = trait_impl_for_function(hir, function_id) {
        let definition = trait_method(hir, trait_id, &function.name.0)
            .map(|method| method.name_range)
            .or(Some(function.name_range));
        return Symbol {
            origin,
            detail: format_function(function),
            definition,
            implementations: trait_method_implementations(hir, trait_id, &function.name.0),
        };
    }
    Symbol {
        origin,
        detail: format_function(function),
        definition: Some(function.name_range),
        implementations: Vec::new(),
    }
}

fn trait_symbol(hir: &HirFile, trait_id: TraitId, origin: TextRange) -> Symbol {
    let tr = &hir.item_tree.traits[trait_id];
    Symbol {
        origin,
        detail: format_nominal("trait", &tr.name, &tr.generics),
        definition: Some(tr.name_range),
        implementations: hir
            .item_tree
            .impls
            .iter()
            .filter_map(|(_, implementation)| {
                (trait_id_for_impl(hir, implementation) == Some(trait_id))
                    .then_some(implementation.self_ty_range)
            })
            .collect(),
    }
}

fn trait_method_symbol(
    hir: &HirFile,
    types: &TypeCheckResult,
    body_id: BodyId,
    expr_id: hir::body::ExprId,
    trait_id: TraitId,
    method_name: &str,
    origin: TextRange,
) -> Option<Symbol> {
    let method = trait_method(hir, trait_id, method_name)?;
    let actual = match types.expr_types.get(&(body_id, expr_id)) {
        Some(Type::FunctionItem { function, .. })
            if trait_impl_for_function(hir, *function)
                .is_some_and(|(candidate, _)| candidate == trait_id) =>
        {
            vec![hir.item_tree.functions[*function].name_range]
        }
        _ => trait_method_implementations(hir, trait_id, method_name),
    };
    let detail = types
        .method_signatures
        .get(&(body_id, expr_id))
        .and_then(|signature| format_instantiated_method(hir, method, signature))
        .unwrap_or_else(|| format_function(method));
    Some(Symbol {
        origin,
        detail,
        definition: Some(method.name_range),
        implementations: actual,
    })
}

/// Renders a method signature with the call-site-substituted parameter and
/// return types (`fun map(self: Iter<i32>, f: impl Fn(i32) -> u8) -> Map<..>`).
/// Returns `None` when the recorded signature shape no longer matches the
/// declared method, falling back to the declared signature.
fn format_instantiated_method(
    hir: &HirFile,
    function: &HirFunction,
    signature: &CallableSignature,
) -> Option<String> {
    if signature.params.len() != function.params.len() {
        return None;
    }
    let params = function
        .params
        .iter()
        .zip(&signature.params)
        .map(|(parameter, ty)| {
            if parameter.is_receiver {
                format!("self: {}", ty.display(hir))
            } else {
                format!("{}: {}", parameter.name.0, ty.display(hir))
            }
        })
        .collect::<Vec<_>>()
        .join(", ");
    let safety = if signature.is_unsafe { "unsafe " } else { "" };
    Some(format!(
        "{safety}fun {}({params}) -> {}",
        function.name.0,
        signature.ret.display(hir)
    ))
}

fn trait_method_declaration_symbol(
    hir: &HirFile,
    trait_id: TraitId,
    method: &HirFunction,
    origin: TextRange,
) -> Symbol {
    Symbol {
        origin,
        detail: format_function(method),
        definition: Some(method.name_range),
        implementations: trait_method_implementations(hir, trait_id, &method.name.0),
    }
}

fn trait_method<'a>(hir: &'a HirFile, trait_id: TraitId, name: &str) -> Option<&'a HirFunction> {
    hir.item_tree.traits[trait_id]
        .methods
        .iter()
        .find(|method| method.name.0 == name)
}

fn trait_method_implementations(hir: &HirFile, trait_id: TraitId, name: &str) -> Vec<TextRange> {
    hir.item_tree
        .impls
        .iter()
        .filter(|(_, implementation)| trait_id_for_impl(hir, implementation) == Some(trait_id))
        .flat_map(|(_, implementation)| implementation.methods.iter().copied())
        .filter_map(|function| {
            let function = &hir.item_tree.functions[function];
            (function.name.0 == name).then_some(function.name_range)
        })
        .collect()
}

fn trait_impl_for_function(hir: &HirFile, function: FunctionId) -> Option<(TraitId, &HirImpl)> {
    hir.item_tree.impls.iter().find_map(|(_, implementation)| {
        if !implementation.methods.contains(&function) {
            return None;
        }
        trait_id_for_impl(hir, implementation).map(|trait_id| (trait_id, implementation))
    })
}

fn trait_id_for_impl(hir: &HirFile, implementation: &HirImpl) -> Option<TraitId> {
    let HirTypeRef::Named(path) = implementation.trait_ty.as_ref()? else {
        return None;
    };
    match hir.type_resolutions.get(&path.range) {
        Some(ResolvedName::Trait(trait_id)) => Some(*trait_id),
        _ => None,
    }
}

fn reference_path_range(hir: &HirFile, origin: RefOrigin) -> Option<TextRange> {
    match origin {
        RefOrigin::Type { range } => Some(range),
        RefOrigin::Expr { body, expr } => match &hir.bodies[body].exprs[expr] {
            Expr::Path { path, .. } | Expr::Struct { path, .. } => Some(path.range),
            _ => None,
        },
    }
}

fn path_segment_ranges(source: &str, range: TextRange, segments: &[Name]) -> Vec<TextRange> {
    let start = usize::from(range.start());
    let end = usize::from(range.end());
    let Some(text) = source.get(start..end) else {
        return Vec::new();
    };
    let tokens = frontend::lexer::lex(text);
    let mut ranges = Vec::with_capacity(segments.len());
    let mut next = 0;
    for segment in segments {
        let Some((index, token)) = tokens.iter().enumerate().skip(next).find(|(_, token)| {
            token.kind == SyntaxKind::Ident && token.text(text) == segment.0.as_str()
        }) else {
            return Vec::new();
        };
        ranges.push(text_range(start + token.span.start, start + token.span.end));
        next = index + 1;
    }
    ranges
}

fn identifier_range_at(source: &str, offset: usize) -> Option<TextRange> {
    frontend::lexer::lex(source)
        .into_iter()
        .find(|token| {
            token.kind == SyntaxKind::Ident
                && token.span.start <= offset
                && offset <= token.span.end
        })
        .map(|token| text_range(token.span.start, token.span.end))
}

fn identifier_named_in_range(source: &str, range: TextRange, name: &str) -> Option<TextRange> {
    let start = usize::from(range.start());
    let end = usize::from(range.end());
    let text = source.get(start..end)?;
    frontend::lexer::lex(text)
        .into_iter()
        .find(|token| token.kind == SyntaxKind::Ident && token.text(text) == name)
        .map(|token| text_range(start + token.span.start, start + token.span.end))
}

fn last_identifier_named_in_range(source: &str, range: TextRange, name: &str) -> Option<TextRange> {
    let start = usize::from(range.start());
    let end = usize::from(range.end());
    let text = source.get(start..end)?;
    frontend::lexer::lex(text)
        .into_iter()
        .rev()
        .find(|token| token.kind == SyntaxKind::Ident && token.text(text) == name)
        .map(|token| text_range(start + token.span.start, start + token.span.end))
}

fn last_identifier_range(source: &str, range: TextRange) -> Option<TextRange> {
    let start = usize::from(range.start());
    let end = usize::from(range.end());
    let text = source.get(start..end)?;
    frontend::lexer::lex(text)
        .into_iter()
        .rev()
        .find(|token| token.kind == SyntaxKind::Ident)
        .map(|token| text_range(start + token.span.start, start + token.span.end))
}

fn receiver_struct_id(ty: &Type) -> Option<hir::item_tree::StructId> {
    match ty {
        Type::Struct(id, _) => Some(*id),
        Type::Ref(inner, _) | Type::Ptr { inner, .. } => receiver_struct_id(inner),
        _ => None,
    }
}

// Signature rendering moved to `hir::render` so `clue doc` shares it;
// hover keeps the compact 5-item truncation.
fn format_function(function: &HirFunction) -> String {
    hir::render::format_function(function)
}

fn format_nominal(kind: &str, name: &Name, generics: &[Name]) -> String {
    if generics.is_empty() {
        return format!("{kind} {}", name.0);
    }
    format!(
        "{kind} {}<{}>",
        name.0,
        generics
            .iter()
            .map(|generic| generic.0.as_str())
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn format_struct(strukt: &HirStruct) -> String {
    hir::render::format_struct_limited(strukt, HOVER_DECLARATION_ITEM_LIMIT)
}

fn format_enum(enumeration: &HirEnum) -> String {
    hir::render::format_enum_limited(enumeration, HOVER_DECLARATION_ITEM_LIMIT)
}

fn format_enum_variant(variant: &HirEnumVariant) -> String {
    hir::render::format_enum_variant(variant)
}
