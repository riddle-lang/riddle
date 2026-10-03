use std::{collections::HashMap, hash::BuildHasher};

use hir::body::{Expr, ResolvedName};
use hir::item_tree::{FunctionId, HirTrait};
use lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens, SemanticTokensDelta,
    SemanticTokensEdit, SemanticTokensLegend,
};
use riddlec::pipeline::CompileOptions;
use rowan::{TextRange, TextSize};
use syntax::SyntaxKind;

#[cfg(feature = "test")]
use crate::analysis::analyze_standalone_source;
use crate::{
    analysis::{AnalysisDepth, DocumentAnalysis, analyze_document_cancellable},
    completion::BUILTIN_TYPES,
    server::Document,
    session::AnalysisSessions,
    text::{LineIndex, is_identifier_continue, offset_for_position, ranges_overlap, text_range},
};

#[must_use]
/// Computes a semantic-token delta.
///
/// # Panics
///
/// Panics if token offsets do not fit in the LSP protocol's `u32` fields.
pub fn semantic_token_delta(
    previous: &[SemanticToken],
    current: &[SemanticToken],
    result_id: String,
) -> SemanticTokensDelta {
    let prefix = previous
        .iter()
        .zip(current)
        .take_while(|(left, right)| left == right)
        .count();
    // Nothing changed: return an empty edits list instead of a no-op edit.
    if prefix == previous.len() && prefix == current.len() {
        return SemanticTokensDelta {
            result_id: Some(result_id),
            edits: vec![],
        };
    }
    let suffix = previous[prefix..]
        .iter()
        .rev()
        .zip(current[prefix..].iter().rev())
        .take_while(|(left, right)| left == right)
        .count();
    let replacement_end = current.len() - suffix;
    SemanticTokensDelta {
        result_id: Some(result_id),
        edits: vec![SemanticTokensEdit {
            start: u32::try_from(prefix * 5).expect("semantic token offset should fit in u32"),
            delete_count: u32::try_from((previous.len() - prefix - suffix) * 5)
                .expect("semantic token edit length should fit in u32"),
            data: Some(current[prefix..replacement_end].to_vec()),
        }],
    }
}

pub const TOKEN_KEYWORD: u32 = 0;
pub const TOKEN_COMMENT: u32 = 1;
pub const TOKEN_STRING: u32 = 2;
pub const TOKEN_NUMBER: u32 = 3;
pub const TOKEN_OPERATOR: u32 = 4;
pub const TOKEN_FUNCTION: u32 = 5;
pub const TOKEN_METHOD: u32 = 6;
pub const TOKEN_VARIABLE: u32 = 7;
pub const TOKEN_TYPE: u32 = 8;
pub const TOKEN_STRUCT: u32 = 9;
pub const TOKEN_ENUM: u32 = 10;
pub const TOKEN_INTERFACE: u32 = 11;
pub const TOKEN_PROPERTY: u32 = 12;
pub const TOKEN_NAMESPACE: u32 = 13;
pub const TOKEN_PARAMETER: u32 = 14;
pub const TOKEN_MACRO: u32 = 15;
pub const MOD_DECLARATION: u32 = 1 << 0;
pub const MOD_MUTABLE: u32 = 1 << 1;
pub const MOD_STATIC: u32 = 1 << 2;
pub const MOD_DEFAULT_LIBRARY: u32 = 1 << 3;

pub fn semantic_tokens_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::KEYWORD,
            SemanticTokenType::COMMENT,
            SemanticTokenType::STRING,
            SemanticTokenType::NUMBER,
            SemanticTokenType::OPERATOR,
            SemanticTokenType::FUNCTION,
            SemanticTokenType::METHOD,
            SemanticTokenType::VARIABLE,
            SemanticTokenType::TYPE,
            SemanticTokenType::STRUCT,
            SemanticTokenType::ENUM,
            SemanticTokenType::INTERFACE,
            SemanticTokenType::PROPERTY,
            SemanticTokenType::NAMESPACE,
            SemanticTokenType::PARAMETER,
            SemanticTokenType::MACRO,
        ],
        token_modifiers: vec![
            SemanticTokenModifier::DECLARATION,
            SemanticTokenModifier::new("mutable"),
            SemanticTokenModifier::STATIC,
            SemanticTokenModifier::DEFAULT_LIBRARY,
        ],
    }
}

#[cfg(feature = "test")]
#[must_use]
pub fn semantic_tokens_for_source(source: &str) -> SemanticTokens {
    semantic_tokens_for_source_with_options(source, CompileOptions { use_std: false }, false)
}

#[cfg(feature = "test")]
#[must_use]
pub fn semantic_tokens_for_source_with_options(
    source: &str,
    compile_options: CompileOptions,
    default_library_source: bool,
) -> SemanticTokens {
    let analysis = analyze_standalone_source(
        source,
        compile_options,
        &mut riddlec::pipeline::CheckSession::new(),
        AnalysisDepth::Resolve,
        None,
    );
    semantic_tokens_from_analysis(source, &analysis, default_library_source)
}

/// Computes semantic tokens for an open document.
///
/// # Errors
///
/// Returns an error when the document is unavailable or project analysis fails.
#[cfg(feature = "test")]
/// Computes semantic tokens for an open test document.
///
/// # Errors
///
/// Returns an error when project analysis fails.
///
/// # Panics
///
/// Panics if non-cancellable test analysis is unexpectedly cancelled.
pub fn semantic_tokens_for_document<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    options: CompileOptions,
    sessions: &AnalysisSessions,
) -> std::result::Result<SemanticTokens, String> {
    semantic_tokens_for_document_cancellable(uri, docs, options, sessions, &|| false)
        .map(|tokens| tokens.expect("non-cancellable analysis cannot be cancelled"))
}

pub fn semantic_tokens_for_document_cancellable<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> std::result::Result<Option<SemanticTokens>, String> {
    let document = docs
        .get(uri)
        .ok_or_else(|| "document is not open".to_string())?;
    let Some(analysis) = analyze_document_cancellable(
        uri,
        docs,
        options,
        sessions,
        AnalysisDepth::Resolve,
        cancelled,
    )?
    else {
        return Ok(None);
    };
    Ok(Some(semantic_tokens_from_analysis(
        &document.text,
        &analysis,
        is_standard_library_uri(uri),
    )))
}

/// Semantic tokens restricted to `range`, for clients that only render a
/// visible window of a large file.
pub fn semantic_tokens_for_document_range_cancellable<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    range: lsp_types::Range,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    cancelled: &impl Fn() -> bool,
) -> std::result::Result<Option<SemanticTokens>, String> {
    let document = docs
        .get(uri)
        .ok_or_else(|| "document is not open".to_string())?;
    let Some(analysis) = analyze_document_cancellable(
        uri,
        docs,
        options,
        sessions,
        AnalysisDepth::Resolve,
        cancelled,
    )?
    else {
        return Ok(None);
    };
    Ok(Some(semantic_tokens_from_analysis_range(
        &document.text,
        &analysis,
        is_standard_library_uri(uri),
        range,
    )))
}

fn semantic_tokens_from_analysis(
    document_source: &str,
    analysis: &DocumentAnalysis,
    default_library_source: bool,
) -> SemanticTokens {
    semantic_tokens_from_analysis_range(
        document_source,
        analysis,
        default_library_source,
        lsp_types::Range::new(
            lsp_types::Position::new(0, 0),
            lsp_types::Position::new(u32::MAX, u32::MAX),
        ),
    )
}

fn semantic_tokens_from_analysis_range(
    document_source: &str,
    analysis: &DocumentAnalysis,
    default_library_source: bool,
    range: lsp_types::Range,
) -> SemanticTokens {
    let tokens = frontend::lexer::lex(document_source);
    let mut raw_tokens = Vec::new();

    for (index, token) in tokens.iter().enumerate() {
        let Some((token_type, token_modifiers_bitset)) =
            semantic_token(&tokens, index, document_source)
        else {
            continue;
        };
        // A token that spans several lines is emitted once per line rather than
        // dropped: multi-line block comments and doc comments are common in
        // this language's sources, and dropping them left whole regions of a
        // file unhighlighted.
        for range in split_token_lines(document_source, token) {
            raw_tokens.push(RawSemanticToken {
                range,
                token_type,
                token_modifiers_bitset,
                resolved: false,
            });
        }
    }

    for occurrence in &analysis.macro_occurrences {
        let Some(range) = analysis.local_macro_range(&occurrence.range) else {
            continue;
        };
        raw_tokens.push(RawSemanticToken {
            range,
            token_type: TOKEN_MACRO,
            token_modifiers_bitset: if occurrence.is_declaration {
                MOD_DECLARATION
            } else {
                0
            },
            resolved: true,
        });
    }

    if let Some(hir) = &analysis.result.hir {
        collect_hir_symbol_tokens(
            hir,
            analysis,
            document_source,
            &tokens,
            default_library_source,
            &mut raw_tokens,
        );
    }

    encode_semantic_tokens(
        document_source,
        remove_overlapping_tokens(raw_tokens),
        Some(range),
    )
}

/// Splits a lexer token into one range per line, trimming the trailing `\r`.
///
/// Single-line tokens come back unchanged except for a trailing `\r`, so this
/// costs one scan of the token's own text. A `\r` left inside the range would
/// count against the token's length while the encoder drops it from the text.
fn split_token_lines(source: &str, token: &frontend::lexer::Token) -> Vec<rowan::TextRange> {
    let text = token.text(source);
    let trim_carriage_return = |mut range: rowan::TextRange| {
        if source[usize::from(range.start())..usize::from(range.end())].ends_with('\r') {
            range = text_range(usize::from(range.start()), usize::from(range.end()) - 1);
        }
        range
    };
    if !text.contains('\n') {
        return vec![trim_carriage_return(text_range(
            token.span.start,
            token.span.end,
        ))];
    }
    let mut ranges = Vec::new();
    let mut line_start = token.span.start;
    for (offset, ch) in text.char_indices() {
        if ch != '\n' {
            continue;
        }
        let line_end = line_start + offset;
        let range = trim_carriage_return(text_range(line_start, line_end));
        if usize::from(range.end()) > usize::from(range.start()) {
            ranges.push(range);
        }
        line_start += offset + 1;
    }
    let range = trim_carriage_return(text_range(line_start, token.span.end));
    if usize::from(range.end()) > usize::from(range.start()) {
        ranges.push(range);
    }
    ranges
}

fn is_standard_library_uri(uri: &lsp_types::Url) -> bool {
    uri.path().contains("/std/std/") || uri.path().ends_with("/std/lib.rid")
}

#[derive(Debug, Clone, Copy)]
struct RawSemanticToken {
    range: TextRange,
    token_type: u32,
    token_modifiers_bitset: u32,
    resolved: bool,
}

fn collect_hir_symbol_tokens(
    hir: &hir::HirFile,
    analysis: &DocumentAnalysis,
    document_source: &str,
    tokens: &[frontend::lexer::Token],
    default_library_source: bool,
    out: &mut Vec<RawSemanticToken>,
) {
    let user_source_len = analysis.source.len();
    let mut symbol_types = HashMap::new();

    for (_, item) in hir.item_tree.structs.iter() {
        let in_source = analysis.local_range(item.name_range).is_some();
        let modifiers =
            if default_library_source || usize::from(item.name_range.start()) >= user_source_len {
                MOD_DEFAULT_LIBRARY
            } else {
                0
            };
        if in_source {
            symbol_types.insert(item.name.0.as_str(), (TOKEN_STRUCT, modifiers));
        } else {
            symbol_types
                .entry(item.name.0.as_str())
                .or_insert((TOKEN_STRUCT, modifiers));
        }
    }
    for (_, item) in hir.item_tree.enums.iter() {
        let in_source = analysis.local_range(item.name_range).is_some();
        let modifiers =
            if default_library_source || usize::from(item.name_range.start()) >= user_source_len {
                MOD_DEFAULT_LIBRARY
            } else {
                0
            };
        if in_source {
            symbol_types.insert(item.name.0.as_str(), (TOKEN_ENUM, modifiers));
        } else {
            symbol_types
                .entry(item.name.0.as_str())
                .or_insert((TOKEN_ENUM, modifiers));
        }
        for variant in &item.variants {
            let in_source = analysis.local_range(variant.name_range).is_some();
            let modifiers = if default_library_source
                || usize::from(variant.name_range.start()) >= user_source_len
            {
                MOD_DEFAULT_LIBRARY
            } else {
                0
            };
            if in_source {
                symbol_types.insert(variant.name.0.as_str(), (TOKEN_ENUM, modifiers));
            } else {
                symbol_types
                    .entry(variant.name.0.as_str())
                    .or_insert((TOKEN_ENUM, modifiers));
            }
        }
    }
    for (_, item) in hir.item_tree.traits.iter() {
        let in_source = analysis.local_range(item.name_range).is_some();
        let modifiers =
            if default_library_source || usize::from(item.name_range.start()) >= user_source_len {
                MOD_DEFAULT_LIBRARY
            } else {
                0
            };
        if in_source {
            symbol_types.insert(item.name.0.as_str(), (TOKEN_INTERFACE, modifiers));
        } else {
            symbol_types
                .entry(item.name.0.as_str())
                .or_insert((TOKEN_INTERFACE, modifiers));
        }
        collect_trait_method_tokens(item, analysis, default_library_source, out);
    }
    let method_modifiers =
        collect_method_symbol_tokens(hir, analysis, default_library_source, user_source_len, out);
    collect_named_symbol_tokens(tokens, document_source, &symbol_types, out);

    let function_modifiers = collect_function_symbol_tokens(
        hir,
        analysis,
        default_library_source,
        user_source_len,
        &method_modifiers,
        out,
    );
    collect_body_symbol_tokens(hir, analysis, &method_modifiers, &function_modifiers, out);
}

fn collect_trait_method_tokens(
    item: &HirTrait,
    analysis: &DocumentAnalysis,
    default_library_source: bool,
    out: &mut Vec<RawSemanticToken>,
) {
    for method in &item.methods {
        let Some(range) = analysis.local_range(method.name_range) else {
            continue;
        };
        let mut modifiers = MOD_DECLARATION;
        if method
            .params
            .first()
            .is_none_or(|param| param.name.0 != "self")
        {
            modifiers |= MOD_STATIC;
        }
        if default_library_source {
            modifiers |= MOD_DEFAULT_LIBRARY;
        }
        out.push(RawSemanticToken {
            range,
            token_type: TOKEN_METHOD,
            token_modifiers_bitset: modifiers,
            resolved: true,
        });
    }
}

fn collect_named_symbol_tokens(
    tokens: &[frontend::lexer::Token],
    document_source: &str,
    symbol_types: &HashMap<&str, (u32, u32)>,
    out: &mut Vec<RawSemanticToken>,
) {
    for token in tokens {
        let Some(&(token_type, token_modifiers_bitset)) =
            symbol_types.get(token.text(document_source))
        else {
            continue;
        };
        out.push(RawSemanticToken {
            range: text_range(token.span.start, token.span.end),
            token_type,
            token_modifiers_bitset,
            resolved: false,
        });
    }
}

fn collect_method_symbol_tokens(
    hir: &hir::HirFile,
    analysis: &DocumentAnalysis,
    default_library_source: bool,
    user_source_len: usize,
    out: &mut Vec<RawSemanticToken>,
) -> HashMap<FunctionId, u32> {
    let mut method_modifiers = HashMap::new();
    for (_, item) in hir.item_tree.impls.iter() {
        for method_id in &item.methods {
            let method = &hir.item_tree.functions[*method_id];
            let mut modifiers = 0;
            if method
                .params
                .first()
                .is_none_or(|param| param.name.0 != "self")
            {
                modifiers |= MOD_STATIC;
            }
            if default_library_source || usize::from(method.name_range.start()) >= user_source_len {
                modifiers |= MOD_DEFAULT_LIBRARY;
            }
            method_modifiers.insert(*method_id, modifiers);
            if let Some(range) = analysis.local_range(method.name_range) {
                out.push(RawSemanticToken {
                    range,
                    token_type: TOKEN_METHOD,
                    token_modifiers_bitset: MOD_DECLARATION | modifiers,
                    resolved: true,
                });
            }
        }
    }
    method_modifiers
}

fn collect_function_symbol_tokens(
    hir: &hir::HirFile,
    analysis: &DocumentAnalysis,
    default_library_source: bool,
    user_source_len: usize,
    method_modifiers: &HashMap<FunctionId, u32>,
    out: &mut Vec<RawSemanticToken>,
) -> HashMap<FunctionId, u32> {
    let mut function_modifiers = HashMap::new();
    for (function_id, function) in hir.item_tree.functions.iter() {
        let function_range = analysis.local_range(function.name_range);
        let in_source = function_range.is_some();
        let modifiers = if default_library_source
            || usize::from(function.name_range.start()) >= user_source_len
        {
            MOD_DEFAULT_LIBRARY
        } else {
            0
        };
        function_modifiers.insert(function_id, modifiers);
        if in_source && !method_modifiers.contains_key(&function_id) {
            out.push(RawSemanticToken {
                range: function_range.unwrap(),
                token_type: TOKEN_FUNCTION,
                token_modifiers_bitset: MOD_DECLARATION | modifiers,
                resolved: true,
            });
        }
        for param in &function.params {
            if param.name.0 != "self"
                && let Some(param_range) = analysis.local_range(param.name_range)
            {
                out.push(RawSemanticToken {
                    range: param_range,
                    token_type: TOKEN_PARAMETER,
                    token_modifiers_bitset: MOD_DECLARATION,
                    resolved: true,
                });
            }
        }
    }

    function_modifiers
}

fn collect_body_symbol_tokens(
    hir: &hir::HirFile,
    analysis: &DocumentAnalysis,
    method_modifiers: &HashMap<FunctionId, u32>,
    function_modifiers: &HashMap<FunctionId, u32>,
    out: &mut Vec<RawSemanticToken>,
) {
    for (_, body) in hir.bodies.iter() {
        for (pat_id, pat) in body.pats.iter() {
            let hir::body::Pattern::Binding { name, is_mut: true } = pat else {
                continue;
            };
            let Some(range) = body.source_map.pat_ranges.get(&pat_id).copied() else {
                continue;
            };
            // The pattern range covers `mut x`; the token is just the name,
            // which the parser always puts last.
            let name_len = TextSize::of(name.0.as_str());
            let range = TextRange::new(range.end() - name_len, range.end());
            let Some(range) = analysis.local_range(range) else {
                continue;
            };
            out.push(RawSemanticToken {
                range,
                token_type: TOKEN_VARIABLE,
                token_modifiers_bitset: MOD_DECLARATION | MOD_MUTABLE,
                resolved: true,
            });
        }

        for (expr_id, expr) in body.exprs.iter() {
            if let Expr::Lambda { params, .. } = expr {
                for param in params {
                    let Some(range) = param.name_range else {
                        continue;
                    };
                    if let Some(range) = analysis.local_range(range) {
                        out.push(RawSemanticToken {
                            range,
                            token_type: TOKEN_PARAMETER,
                            token_modifiers_bitset: MOD_DECLARATION,
                            resolved: true,
                        });
                    }
                }
            }

            let Expr::Path { path, resolved } = expr else {
                continue;
            };
            let Some(name) = path.segments.last() else {
                continue;
            };
            if path.as_single_name().is_some() && name.0 == "self" {
                continue;
            }

            let Some(range) = body
                .source_map
                .expr_ranges
                .get(&expr_id)
                .and_then(|range| last_identifier_range(&analysis.source, *range))
                .and_then(|range| analysis.local_range(range))
            else {
                continue;
            };

            let Some((token_type, token_modifiers_bitset)) = semantic_token_for_resolution(
                body,
                resolved.as_ref(),
                method_modifiers,
                function_modifiers,
            ) else {
                continue;
            };
            out.push(RawSemanticToken {
                range,
                token_type,
                token_modifiers_bitset,
                resolved: true,
            });
        }
    }
}

fn semantic_token_for_resolution(
    body: &hir::body::Body,
    resolved: Option<&ResolvedName>,
    method_modifiers: &HashMap<FunctionId, u32>,
    function_modifiers: &HashMap<FunctionId, u32>,
) -> Option<(u32, u32)> {
    match resolved {
        Some(ResolvedName::PatternBinding(id)) => {
            let is_mut = matches!(
                body.pats[id.pattern],
                hir::body::Pattern::Binding { is_mut: true, .. }
            );
            Some((TOKEN_VARIABLE, if is_mut { MOD_MUTABLE } else { 0 }))
        }
        Some(ResolvedName::Param(_) | ResolvedName::LambdaParam { .. }) => {
            Some((TOKEN_PARAMETER, 0))
        }
        Some(ResolvedName::Function(function_id)) => method_modifiers.get(function_id).map_or_else(
            || {
                Some((
                    TOKEN_FUNCTION,
                    function_modifiers.get(function_id).copied().unwrap_or(0),
                ))
            },
            |modifiers| Some((TOKEN_METHOD, *modifiers)),
        ),
        Some(ResolvedName::Struct(_)) => Some((TOKEN_STRUCT, 0)),
        Some(ResolvedName::Enum(_) | ResolvedName::EnumVariant(_, _)) => Some((TOKEN_ENUM, 0)),
        Some(ResolvedName::Trait(_)) => Some((TOKEN_INTERFACE, 0)),
        Some(ResolvedName::TypeAlias(_)) => Some((TOKEN_TYPE, 0)),
        Some(ResolvedName::Module(_)) => Some((TOKEN_NAMESPACE, 0)),
        Some(ResolvedName::Const(_)) => Some((TOKEN_VARIABLE, 0)),
        _ => None,
    }
}

fn trim_source_range(source: &str, range: TextRange) -> Option<TextRange> {
    let start = usize::from(range.start());
    let end = usize::from(range.end());
    let text = source.get(start..end)?;
    let start = start + text.len() - text.trim_start().len();
    let end = end - (text.len() - text.trim_end().len());

    (start < end).then(|| text_range(start, end))
}

fn last_identifier_range(source: &str, range: TextRange) -> Option<TextRange> {
    let range = trim_source_range(source, range)?;
    let end = usize::from(range.end());
    let text = source.get(usize::from(range.start())..end)?;
    let length = text
        .chars()
        .rev()
        .take_while(|ch| is_identifier_continue(*ch))
        .map(char::len_utf8)
        .sum::<usize>();
    (length > 0).then(|| text_range(end - length, end))
}

fn remove_overlapping_tokens(raw_tokens: Vec<RawSemanticToken>) -> Vec<RawSemanticToken> {
    let (mut preferred, mut fallback): (Vec<_>, Vec<_>) =
        raw_tokens.into_iter().partition(|token| {
            matches!(
                token.token_type,
                TOKEN_FUNCTION
                    | TOKEN_METHOD
                    | TOKEN_VARIABLE
                    | TOKEN_STRUCT
                    | TOKEN_ENUM
                    | TOKEN_INTERFACE
                    | TOKEN_PARAMETER
                    | TOKEN_MACRO
            )
        });
    preferred.sort_by_key(|token| {
        (
            token.range.start(),
            token.range.end(),
            std::cmp::Reverse(token.resolved),
            std::cmp::Reverse(token.token_modifiers_bitset.count_ones()),
        )
    });
    fallback.sort_by_key(|token| (token.range.start(), token.range.end()));

    let mut kept_preferred: Vec<RawSemanticToken> = Vec::new();
    for token in preferred {
        if let Some(kept) = kept_preferred
            .last_mut()
            .filter(|kept| ranges_overlap(kept.range, token.range))
        {
            if preferred_token_priority(&token) > preferred_token_priority(kept) {
                *kept = token;
            }
            continue;
        }
        kept_preferred.push(token);
    }

    let mut preferred_index = 0;
    let mut kept_fallback: Vec<RawSemanticToken> = Vec::new();
    for token in fallback {
        while kept_preferred
            .get(preferred_index)
            .is_some_and(|preferred| preferred.range.end() <= token.range.start())
        {
            preferred_index += 1;
        }
        if kept_preferred
            .get(preferred_index)
            .is_some_and(|preferred| ranges_overlap(preferred.range, token.range))
            || kept_fallback
                .last()
                .is_some_and(|kept| ranges_overlap(kept.range, token.range))
        {
            continue;
        }
        kept_fallback.push(token);
    }

    kept_preferred.extend(kept_fallback);
    kept_preferred.sort_by_key(|token| (token.range.start(), token.range.end()));
    kept_preferred
}

const fn preferred_token_priority(
    token: &RawSemanticToken,
) -> (bool, bool, std::cmp::Reverse<TextSize>, u32) {
    (
        token.token_type == TOKEN_MACRO,
        token.resolved,
        std::cmp::Reverse(token.range.len()),
        token.token_modifiers_bitset.count_ones(),
    )
}

fn encode_semantic_tokens(
    source: &str,
    raw_tokens: Vec<RawSemanticToken>,
    range: Option<lsp_types::Range>,
) -> SemanticTokens {
    let mut data = Vec::new();
    let mut prev_line = 0;
    let mut prev_start = 0;
    let line_index = LineIndex::new(source);
    // A token counts as inside the window when it overlaps it at all, which is
    // what the specification asks for and what clients expect when a token
    // straddles a fold boundary.
    let filter = range.and_then(|range| {
        let start = offset_for_position(source, range.start)?;
        let end = offset_for_position(source, range.end)?;
        Some((start, end))
    });

    for token in raw_tokens {
        let start_offset = usize::from(token.range.start());
        let end_offset = usize::from(token.range.end());
        if let Some((window_start, window_end)) = filter
            && (end_offset <= window_start || start_offset >= window_end)
        {
            continue;
        }
        let Some(text) = source.get(start_offset..end_offset) else {
            continue;
        };
        let text = text.strip_suffix('\r').unwrap_or(text);
        if text.is_empty() || text.contains('\n') {
            continue;
        }

        // Positions come from the negotiated encoding rather than a hardcoded
        // UTF-16 count, so a UTF-8 or UTF-32 client gets the columns it asked
        // for.
        let Some(position) = line_index.position(source, start_offset) else {
            continue;
        };
        let Some(end_position) = line_index.position(source, end_offset) else {
            continue;
        };
        let length = end_position.character - position.character;
        let delta_line = position.line - prev_line;
        let delta_start = if delta_line == 0 {
            position.character - prev_start
        } else {
            position.character
        };

        data.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: token.token_type,
            token_modifiers_bitset: token.token_modifiers_bitset,
        });
        prev_line = position.line;
        prev_start = position.character;
    }

    SemanticTokens {
        result_id: None,
        data,
    }
}

fn semantic_token(
    tokens: &[frontend::lexer::Token],
    index: usize,
    source: &str,
) -> Option<(u32, u32)> {
    let token = &tokens[index];
    let token_type = match token.kind {
        SyntaxKind::Whitespace | SyntaxKind::ErrorNode | SyntaxKind::Eof => None,
        SyntaxKind::LineComment => Some(TOKEN_COMMENT),
        SyntaxKind::String | SyntaxKind::Char => Some(TOKEN_STRING),
        SyntaxKind::Number | SyntaxKind::Float => Some(TOKEN_NUMBER),
        SyntaxKind::Ident => ident_token_type(tokens, index, source),
        kind if is_keyword(kind) => Some(TOKEN_KEYWORD),
        kind if is_operator(kind) => Some(TOKEN_OPERATOR),
        _ => None,
    }?;

    Some((token_type, 0))
}

fn ident_token_type(tokens: &[frontend::lexer::Token], index: usize, source: &str) -> Option<u32> {
    let text = tokens[index].text(source);
    if BUILTIN_TYPES.contains(&text) {
        return Some(TOKEN_KEYWORD);
    }
    let previous = previous_significant(tokens, index).map(|token| token.kind);
    let next = next_significant(tokens, index).map(|token| token.kind);
    match previous {
        Some(SyntaxKind::Fun) => Some(TOKEN_FUNCTION),
        Some(SyntaxKind::Struct) => Some(TOKEN_STRUCT),
        Some(SyntaxKind::Enum) => Some(TOKEN_ENUM),
        Some(SyntaxKind::Trait) => Some(TOKEN_INTERFACE),
        Some(SyntaxKind::Mod | SyntaxKind::Use) => Some(TOKEN_NAMESPACE),
        Some(SyntaxKind::TypeKw | SyntaxKind::Impl) => Some(TOKEN_TYPE),
        Some(SyntaxKind::Dot) => {
            if next == Some(SyntaxKind::LParen) {
                Some(TOKEN_METHOD)
            } else {
                Some(TOKEN_PROPERTY)
            }
        }
        _ if next == Some(SyntaxKind::LParen) => Some(TOKEN_FUNCTION),
        _ if token_starts_uppercase(text) => Some(TOKEN_TYPE),
        _ => None,
    }
}

fn previous_significant(
    tokens: &[frontend::lexer::Token],
    index: usize,
) -> Option<&frontend::lexer::Token> {
    tokens[..index]
        .iter()
        .rev()
        .find(|token| !token.kind.is_trivia())
}

fn next_significant(
    tokens: &[frontend::lexer::Token],
    index: usize,
) -> Option<&frontend::lexer::Token> {
    tokens[index + 1..]
        .iter()
        .find(|token| !token.kind.is_trivia())
}

fn token_starts_uppercase(text: &str) -> bool {
    text.chars().next().is_some_and(char::is_uppercase)
}

const fn is_keyword(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Let
            | SyntaxKind::Fun
            | SyntaxKind::Struct
            | SyntaxKind::If
            | SyntaxKind::Else
            | SyntaxKind::While
            | SyntaxKind::Loop
            | SyntaxKind::Break
            | SyntaxKind::Continue
            | SyntaxKind::Return
            | SyntaxKind::As
            | SyntaxKind::SelfKw
            | SyntaxKind::Mod
            | SyntaxKind::Use
            | SyntaxKind::Mut
            | SyntaxKind::Pub
            | SyntaxKind::SuperKw
            | SyntaxKind::CrateKw
            | SyntaxKind::Enum
            | SyntaxKind::Trait
            | SyntaxKind::Impl
            | SyntaxKind::Match
            | SyntaxKind::Const
            | SyntaxKind::TypeKw
            | SyntaxKind::Extern
            | SyntaxKind::Unsafe
            | SyntaxKind::Safe
            | SyntaxKind::For
            | SyntaxKind::In
            | SyntaxKind::Where
            | SyntaxKind::Move
            | SyntaxKind::True
            | SyntaxKind::False
    )
}

const fn is_operator(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Arrow
            | SyntaxKind::EqEq
            | SyntaxKind::BangEq
            | SyntaxKind::LessEq
            | SyntaxKind::GreaterEq
            | SyntaxKind::AmpAmp
            | SyntaxKind::PipePipe
            | SyntaxKind::FatArrow
            | SyntaxKind::PlusEq
            | SyntaxKind::MinusEq
            | SyntaxKind::StarEq
            | SyntaxKind::SlashEq
            | SyntaxKind::PercentEq
            | SyntaxKind::AmpEq
            | SyntaxKind::PipeEq
            | SyntaxKind::CaretEq
            | SyntaxKind::ShlEq
            | SyntaxKind::ShrEq
            | SyntaxKind::Shl
            | SyntaxKind::Shr
            | SyntaxKind::Plus
            | SyntaxKind::Minus
            | SyntaxKind::Star
            | SyntaxKind::Slash
            | SyntaxKind::Percent
            | SyntaxKind::Amp
            | SyntaxKind::Pipe
            | SyntaxKind::Caret
            | SyntaxKind::Less
            | SyntaxKind::Greater
            | SyntaxKind::Bang
            | SyntaxKind::Eq
    )
}
