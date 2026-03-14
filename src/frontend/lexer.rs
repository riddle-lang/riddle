use crate::frontend::token::{Pos, Span, Token, TokenKind};

pub(crate) struct Lexer<'a> {
    src: &'a str,
    chars: std::str::CharIndices<'a>,
    filename: &'a str,
    pos: usize, // byte offset
    line: usize,
    col: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexErrorKind {
    UnexpectedChar(char),
    InvalidNumberLiteral,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub pos: Pos,
    pub filename: String,
}

impl std::fmt::Display for LexErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedChar(c) => write!(f, "unexpected character {:?}", c),
            Self::InvalidNumberLiteral => write!(f, "invalid number literal"),
        }
    }
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}: error: {}",
            self.filename, self.pos.line, self.pos.col, self.kind
        )
    }
}

impl std::error::Error for LexError {}

impl<'a> Lexer<'a> {
    pub(crate) fn new(src: &'a str, filename: &'a str) -> Self {
        Self {
            src,
            chars: src.char_indices(),
            filename,
            pos: 0,
            line: 1,
            col: 1,
        }
    }

    fn error(&self, kind: LexErrorKind) -> LexError {
        LexError {
            kind,
            pos: self.cur_pos(),
            filename: self.filename.to_string(),
        }
    }

    fn cur_pos(&self) -> Pos {
        Pos {
            byte: self.pos,
            line: self.line,
            col: self.col,
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next().map(|(_, c)| c)
    }

    fn peek2(&self) -> Option<char> {
        let mut it = self.chars.clone();
        it.next();
        it.next().map(|(_, c)| c)
    }

    fn bump(&mut self) -> Option<char> {
        let (byte_idx, ch) = self.chars.next()?;

        self.pos = byte_idx + ch.len_utf8();

        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }

        Some(ch)
    }

    fn eat(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn make_span(&self, start: Pos) -> Span {
        Span {
            start,
            end: self.cur_pos(),
        }
    }

    fn lex_ident(&mut self) -> Token {
        let start = self.cur_pos();

        self.bump();

        while let Some(c) = self.peek() {
            if c == '_' || c.is_alphanumeric() {
                self.bump();
            } else {
                break;
            }
        }

        Token {
            kind: TokenKind::Ident,
            span: self.make_span(start),
        }
    }

    fn lex_number(&mut self) -> Result<Token, LexError> {
        let start = self.cur_pos();

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.bump();
            } else {
                break;
            }
        }

        if let Some(c) = self.peek() {
            if c == '_' || c.is_alphabetic() {
                return Err(self.error(LexErrorKind::InvalidNumberLiteral));
            }
        }

        Ok(Token {
            kind: TokenKind::Number,
            span: self.make_span(start),
        })
    }


    fn single_char(&mut self, kind: TokenKind) -> Token {
        let start = self.cur_pos();
        self.bump();
        Token {
            kind,
            span: self.make_span(start),
        }
    }

    fn one_or_two(
        &mut self,
        first: char,
        second: char,
        one_kind: TokenKind,
        two_kind: TokenKind,
    ) -> Token {
        let start = self.cur_pos();
        self.eat(first);

        let kind = if self.eat(second) { two_kind } else { one_kind };

        Token {
            kind,
            span: self.make_span(start),
        }
    }

    pub(crate) fn lex(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();

        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.bump();
                continue;
            }

            let tok = match c {
                'a'..='z' | 'A'..='Z' | '_' => self.lex_ident(),
                '0'..='9' => self.lex_number()?,

                '+' => self.single_char(TokenKind::Plus),
                '-' => self.single_char(TokenKind::Minus),
                '*' => self.single_char(TokenKind::Star),
                '/' => self.single_char(TokenKind::Slash),

                '(' => self.single_char(TokenKind::LParen),
                ')' => self.single_char(TokenKind::RParen),
                '{' => self.single_char(TokenKind::LBrace),
                '}' => self.single_char(TokenKind::RBrace),

                ';' => self.single_char(TokenKind::Semi),
                ',' => self.single_char(TokenKind::Comma),

                '=' => self.one_or_two('=', '=', TokenKind::Assign, TokenKind::EqEq),
                '!' => self.one_or_two('!', '=', TokenKind::Bang, TokenKind::BangEq),
                '<' => self.one_or_two('<', '=', TokenKind::Lt, TokenKind::Le),
                '>' => self.one_or_two('>', '=', TokenKind::Gt, TokenKind::Ge),

                _ => return Err(self.error(LexErrorKind::UnexpectedChar(c))),
            };

            tokens.push(tok);
        }

        let pos = self.cur_pos();
        tokens.push(Token {
            kind: TokenKind::Eof,
            span: Span {
                start: pos,
                end: pos,
            },
        });

        Ok(tokens)
    }

    fn slice_span(&self, span: Span) -> &'a str {
        &self.src[span.start.byte..span.end.byte]
    }
}
