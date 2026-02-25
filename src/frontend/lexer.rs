use logos::Logos;

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use miette::Result;

#[derive(Debug, Error, Diagnostic)]
#[error("{message}")]
#[diagnostic(code(riddle::lex), help("Check for invalid characters or whether token rules have missed certain cases."))]
pub struct LexDiag {
    message: String,

    #[source_code]
    src: NamedSource<String>,

    #[label("Here")]
    span: SourceSpan,
}

impl LexDiag {
    pub fn new(filename: impl Into<String>, src: &str, span: std::ops::Range<usize>, message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            src: NamedSource::new(filename.into(), src.to_string()),
            span: span.into(), // Range<usize> 可以直接转 SourceSpan
        }
    }
}


#[derive(Debug, Clone, PartialEq, Default)]
pub struct LexError {
    span: std::ops::Range<usize>,
}
impl LexError {
    fn from_lexer(lex: &mut logos::Lexer<'_, Tok>) -> Self {
        Self { span: lex.span() }
    }
}

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(error(LexError, LexError::from_lexer))]
pub enum Tok {
    #[token("+")]
    Plus,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,

    #[regex(r"[0-9]+")]
    Int,
}

pub fn lex(filename: &str, src: &str) -> Result<Vec<(Tok, std::ops::Range<usize>)>> {
    let mut lex = Tok::lexer(src);
    let mut out = vec![];

    while let Some(item) = lex.next() {
        match item {
            Ok(tok) => out.push((tok, lex.span())),
            Err(e) => {
                let bad = lex.slice();
                return Err(LexDiag::new(
                    filename,
                    src,
                    e.span,
                    format!("Unrecognized input: {bad:?}"),
                ))?;
            }
        }
    }
    Ok(out)
}