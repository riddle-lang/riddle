#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Pos {
    pub byte: usize,
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Span {
    pub start: Pos,
    pub end: Pos,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    Number,

    Plus,      // +
    Minus,     // -
    Star,      // *
    Slash,     // /

    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }

    Semi,      // ;
    Comma,     // ,

    Assign,    // =
    EqEq,      // ==
    Bang,      // !
    BangEq,    // !=
    Lt,        // <
    Le,        // <=
    Gt,        // >
    Ge,        // >=

    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Token {
    pub kind: TokenKind,
    pub span: Span,
}