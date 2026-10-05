use std::{collections::HashMap, hash::BuildHasher};

use hir::body::{Expr, Stmt};
use hir::item_tree::HirTypeRef;
use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, Range};
use riddlec::pipeline::CompileOptions;

use crate::{
    analysis::{AnalysisDepth, DocumentAnalysis, analyze_document_cancellable},
    navigation::call_parameter_names_at,
    server::Document,
    session::AnalysisSessions,
    text::{LineIndex, ranges_overlap},
};
use syntax::SyntaxKind;

#[cfg(feature = "test")]
#[must_use]
pub fn inlay_hints_for_source(source: &str, range: Range) -> Vec<InlayHint> {
    let mut session = riddlec::pipeline::CheckSession::new();
    let result = session
        .infer_with_options_cancellable(
            source,
            CompileOptions {
                use_std: false,
                ..Default::default()
            },
            || false,
        )
        .expect("inference should not be cancelled");
    inlay_hints_from_analysis(
        source,
        &DocumentAnalysis {
            result: std::sync::Arc::new(result),
            source: source.into(),
            source_map: None,
            macro_occurrences: Vec::new(),
            macro_source_map: None,
            path: None,
            project_root: None,
            project_revision: 0,
            files: Vec::new(),
        },
        range,
    )
}

/// Computes inlay hints for an open document.
///
/// # Errors
///
/// Returns an error when the document is unavailable or project analysis fails.
#[cfg(feature = "test")]
/// Computes inlay hints for an open test document.
///
/// # Errors
///
/// Returns an error when project analysis fails.
///
/// # Panics
///
/// Panics if non-cancellable test analysis is unexpectedly cancelled.
pub fn inlay_hints_for_document<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    range: Range,
    options: CompileOptions,
    sessions: &AnalysisSessions,
) -> std::result::Result<Vec<InlayHint>, String> {
    inlay_hints_for_document_cancellable(uri, docs, range, options, sessions, &|| false)
        .map(|hints| hints.expect("non-cancellable analysis cannot be cancelled"))
}

pub fn inlay_hints_for_document_cancellable<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    range: Range,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> std::result::Result<Option<Vec<InlayHint>>, String> {
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
    Ok(Some(inlay_hints_from_analysis(
        &document.text,
        &analysis,
        range,
    )))
}

pub fn inlay_hints_from_analysis(
    document_source: &str,
    analysis: &DocumentAnalysis,
    range: Range,
) -> Vec<InlayHint> {
    let Some(hir) = analysis.result.hir.as_ref() else {
        return Vec::new();
    };
    let mut hints = type_hints_from_analysis(document_source, analysis, hir);
    hints.extend(lambda_type_hints_from_analysis(
        document_source,
        analysis,
        hir,
    ));
    hints.extend(chain_type_hints_from_analysis(
        document_source,
        analysis,
        hir,
    ));
    hints.extend(parameter_hints_from_analysis(document_source, analysis));

    // LSP ranges have an exclusive end: a hint on range.end.line is only
    // inside the range when its character is strictly before range.end.character.
    hints.retain(|hint| {
        let line = hint.position.line;
        if line < range.start.line || line > range.end.line {
            return false;
        }
        if line == range.start.line && hint.position.character < range.start.character {
            return false;
        }
        if line == range.end.line && hint.position.character >= range.end.character {
            return false;
        }
        true
    });
    hints.sort_by_key(|hint| (hint.position.line, hint.position.character));
    hints
}

fn type_hints_from_analysis(
    document_source: &str,
    analysis: &DocumentAnalysis,
    hir: &hir::HirFile,
) -> Vec<InlayHint> {
    let type_result = &analysis.result.type_result;
    let mut hints = Vec::new();
    for (body_id, body) in hir.bodies.iter() {
        for (_, statement) in body.stmts.iter() {
            let Stmt::Let {
                pat,
                ty: HirTypeRef::Unknown,
                init: Some(init),
                ..
            } = statement
            else {
                continue;
            };
            let Some(name_range) = body.source_map.pat_ranges.get(pat) else {
                continue;
            };
            if matches!(body.exprs[*init], Expr::Struct { .. }) {
                continue;
            }
            let Some(init_range) = body.source_map.expr_ranges.get(init).copied() else {
                continue;
            };
            if error_overlaps(analysis, init_range) {
                continue;
            }
            let Some(ty) = type_result.expr_types.get(&(body_id, *init)) else {
                continue;
            };
            if !is_displayable_type(ty) {
                continue;
            }
            let Some(name_range) = analysis.local_authored_range(*name_range) else {
                continue;
            };
            hints.push(InlayHint {
                position: crate::diagnostics::position(
                    document_source,
                    usize::from(name_range.end()),
                ),
                label: InlayHintLabel::String(format!(": {}", ty.display(hir))),
                kind: Some(InlayHintKind::TYPE),
                text_edits: None,
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            });
        }
    }
    hints
}

/// Type hints for unannotated lambda parameters (`[v -> v * 2]` shows
/// `v: i32`), resolved from the call-site-substituted closure signature.
fn lambda_type_hints_from_analysis(
    document_source: &str,
    analysis: &DocumentAnalysis,
    hir: &hir::HirFile,
) -> Vec<InlayHint> {
    let types = &analysis.result.type_result;
    let mut hints = Vec::new();
    for (body_id, body) in hir.bodies.iter() {
        for (expr_id, expr) in body.exprs.iter() {
            let Expr::Lambda { params, .. } = expr else {
                continue;
            };
            let Some(inferred) = types
                .expr_types
                .get(&(body_id, expr_id))
                .and_then(type_checker::Type::callable_signature)
            else {
                continue;
            };
            for (parameter, ty) in params.iter().zip(&inferred.params) {
                if !matches!(parameter.ty, HirTypeRef::Unknown) {
                    continue;
                }
                let Some(name_range) = parameter.name_range else {
                    continue;
                };
                if !is_displayable_type(ty) {
                    continue;
                }
                let Some(name_range) = analysis.local_authored_range(name_range) else {
                    continue;
                };
                hints.push(type_hint(
                    crate::diagnostics::position(document_source, usize::from(name_range.end())),
                    format!(": {}", ty.display(hir)),
                ));
            }
        }
    }
    hints
}

/// Result-type hints for links of a method chain that is broken across lines
/// (`xs.iter()\n    .map(f)` shows `: Map<..>` after the call), in the style of
/// IntelliJ's chained-call type hints.
fn chain_type_hints_from_analysis(
    document_source: &str,
    analysis: &DocumentAnalysis,
    hir: &hir::HirFile,
) -> Vec<InlayHint> {
    let types = &analysis.result.type_result;
    let line_index = LineIndex::new(document_source);
    let mut hints = Vec::new();
    for (body_id, body) in hir.bodies.iter() {
        for (expr_id, expr) in body.exprs.iter() {
            let Expr::Call { callee, .. } = expr else {
                continue;
            };
            let Expr::FieldAccess { base, .. } = &body.exprs[*callee] else {
                continue;
            };
            let Expr::Call { .. } = body.exprs[*base] else {
                continue;
            };
            let (Some(call_range), Some(base_range)) = (
                body.source_map.expr_ranges.get(&expr_id).copied(),
                body.source_map.expr_ranges.get(base).copied(),
            ) else {
                continue;
            };
            let Some(call_end) =
                line_index.position(document_source, usize::from(call_range.end()))
            else {
                continue;
            };
            let Some(base_end) =
                line_index.position(document_source, usize::from(base_range.end()))
            else {
                continue;
            };
            // The field-access range includes its receiver, so compare line
            // endings instead: a formatted multiline chain puts the receiver's
            // closing paren on an earlier line than this call's own `)`.
            if base_end.line == call_end.line {
                continue;
            }
            let Some(ty) = types.expr_types.get(&(body_id, expr_id)) else {
                continue;
            };
            if !is_displayable_type(ty) {
                continue;
            }
            let Some(call_range) = analysis.local_authored_range(call_range) else {
                continue;
            };
            if error_overlaps(analysis, call_range) {
                continue;
            }
            hints.push(type_hint(
                crate::diagnostics::position(document_source, usize::from(call_range.end())),
                format!(": {}", ty.display(hir)),
            ));
        }
    }
    hints
}

#[must_use]
pub(crate) fn is_displayable_type(ty: &type_checker::Type) -> bool {
    !matches!(
        ty,
        type_checker::Type::Unknown
            | type_checker::Type::Error
            | type_checker::Type::InferVar(_)
            | type_checker::Type::Never
    )
}

fn error_overlaps(analysis: &DocumentAnalysis, range: rowan::TextRange) -> bool {
    analysis
        .result
        .hir_diagnostics
        .iter()
        .chain(analysis.result.type_result.diagnostics.iter())
        .filter(|diagnostic| diagnostic.severity == type_checker::Severity::Error)
        .flat_map(|diagnostic| &diagnostic.labels)
        .any(|label| ranges_overlap(label.range, range))
}

fn type_hint(position: lsp_types::Position, label: String) -> InlayHint {
    InlayHint {
        position,
        label: InlayHintLabel::String(label),
        kind: Some(InlayHintKind::TYPE),
        text_edits: None,
        tooltip: None,
        // The label starts with `: `, so it needs a space before it to read as
        // `value: i32` rather than `value: i32` glued to the identifier.
        padding_left: Some(true),
        padding_right: None,
        data: None,
    }
}

fn parameter_hint(position: lsp_types::Position, label: String) -> InlayHint {
    InlayHint {
        position,
        label: InlayHintLabel::String(label),
        kind: Some(InlayHintKind::PARAMETER),
        text_edits: None,
        tooltip: None,
        padding_left: None,
        // `name: value` needs the trailing space before the argument.
        padding_right: Some(true),
        data: None,
    }
}

fn parameter_hints_from_analysis(
    document_source: &str,
    analysis: &DocumentAnalysis,
) -> Vec<InlayHint> {
    let line_index = LineIndex::new(document_source);
    let tokens = frontend::lexer::lex(document_source)
        .into_iter()
        .filter(|token| {
            token.kind != SyntaxKind::Whitespace && token.kind != SyntaxKind::LineComment
        })
        .collect::<Vec<_>>();
    let mut hints = Vec::new();
    for index in 0..tokens.len().saturating_sub(1) {
        let token = &tokens[index];
        if token.kind != SyntaxKind::Ident
            || tokens[index + 1].kind != SyntaxKind::LParen
            || index
                .checked_sub(1)
                .and_then(|previous| tokens.get(previous))
                .is_some_and(|previous| previous.kind == SyntaxKind::Fun)
        {
            continue;
        }
        let Some(position) = line_index.position(document_source, token.span.start) else {
            continue;
        };
        let Some(parameters) = call_parameter_names_at(document_source, analysis, position) else {
            continue;
        };
        let arguments = call_argument_starts(&tokens, index);
        for (parameter, argument_start) in parameters.iter().zip(arguments) {
            let Some(argument) = tokens.get(argument_start) else {
                continue;
            };
            // An argument already written as `name` for parameter `name` says
            // nothing new.
            if argument.kind == SyntaxKind::Ident
                && &document_source[argument.span.clone()] == parameter
            {
                continue;
            }
            let Some(position) = line_index.position(document_source, argument.span.start) else {
                continue;
            };
            hints.push(parameter_hint(position, format!("{parameter}: ")));
        }
    }
    hints
}

/// Token indices where each argument of a call starts.
///
/// Only the first token of an argument is needed, but the *split* has to be
/// syntactic: tracking bracket depth means `f(a + b, c)` yields two arguments
/// rather than treating `a` as the whole first one.
fn call_argument_starts(tokens: &[frontend::lexer::Token], index: usize) -> Vec<usize> {
    let mut starts = Vec::new();
    let mut depth = 0usize;
    let mut expecting_argument = true;
    for (offset, argument) in tokens.iter().enumerate().skip(index + 2) {
        match argument.kind {
            SyntaxKind::RParen if depth == 0 => {
                if !expecting_argument {
                    starts.push(offset);
                }
                break;
            }
            SyntaxKind::Comma if depth == 0 => {
                expecting_argument = true;
                continue;
            }
            SyntaxKind::LParen | SyntaxKind::LBracket | SyntaxKind::LBrace => depth += 1,
            SyntaxKind::RParen | SyntaxKind::RBracket | SyntaxKind::RBrace => {
                depth = depth.saturating_sub(1);
            }
            _ => {}
        }
        if expecting_argument && depth == 0 {
            starts.push(offset);
            expecting_argument = false;
        }
    }
    starts
}
