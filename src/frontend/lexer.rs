use logos::Logos;

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum Token {
    #[token("let")]
    Let,

    #[token("=")]
    Eq,
    #[token(":")]
    Colon,
    #[token(";")]
    Semi,
    #[token("+")]
    Plus,
    #[token("*")]
    Star,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    #[token("true")]
    True,
    #[token("false")]
    False,

    #[regex(r"[A-Za-z_][A-Za-z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),

    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    Number(i64),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        Some(s[1..s.len() - 1].to_string())
    })]
    Str(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    pub start: usize,
    pub end: usize,
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "lex error at {}..{}", self.start, self.end)
    }
}

impl std::error::Error for LexError {}

pub struct Lexer<'input> {
    inner: logos::Lexer<'input, Token>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            inner: Token::lexer(input),
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<(usize, Token, usize), LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.inner.next()?;
        let span = self.inner.span();

        Some(match tok {
            Ok(tok) => Ok((span.start, tok, span.end)),
            Err(_) => Err(LexError {
                start: span.start,
                end: span.end,
            }),
        })
    }
}
