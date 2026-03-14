use crate::frontend::token::{Pos, Span, Token, TokenKind};

pub(crate) struct Lexer<'a> {
    src: &'a str,
    chars: std::str::CharIndices<'a>,
    filename: &'a str,
    pos: usize,   // byte offset
    line: usize,
    col: usize,
}

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

    fn lex_number(&mut self) -> Token {
        let start = self.cur_pos();

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.bump();
            } else {
                break;
            }
        }

        Token {
            kind: TokenKind::Number,
            span: self.make_span(start),
        }
    }

    pub(crate) fn lex(&mut self) -> Result<Vec<Token>, ()> {
        let mut tokens = Vec::new();

        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.bump();
                continue;
            }

            let tok = if c == '_' || c.is_alphabetic() {
                self.lex_ident()
            } else if c.is_ascii_digit() {
                self.lex_number()
            } else {
                let start = self.cur_pos();
                match c {
                    '+' => {
                        self.bump();
                        Token {
                            kind: TokenKind::Plus,
                            span: self.make_span(start),
                        }
                    }
                    '-' => {
                        self.bump();
                        Token {
                            kind: TokenKind::Minus,
                            span: self.make_span(start),
                        }
                    }
                    _ => return Err(()),
                }
            };

            tokens.push(tok);
        }

        let eof = Token {
            kind: TokenKind::Eof,
            span: Span {
                start: self.cur_pos(),
                end: self.cur_pos(),
            },
        };

        tokens.push(eof);
        Ok(tokens)
    }

    fn slice_span(&self, span: Span) -> &'a str {
        &self.src[span.start.byte..span.end.byte]
    }
}
