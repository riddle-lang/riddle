use std::str::Chars;
use crate::frontend::token::{Span, Token};

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    chars: Chars<'a>,
    pos: usize, // byte offset
    line: usize,
    column: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum LexError{
    Unexpected(char),
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LexError::Unexpected(c) => write!(f, "Unexpected character '{}'", c),
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input,
            chars: input.chars(),
            pos: 0,
            line: 1,
            column: 1,
        }
    }

    // get now char
    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn peek2(&self) -> Option<char> {
        let mut t = self.chars.clone();
        t.next();
        t.next()
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.pos += c.len_utf8();
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        Some(c)
    }

    /// If it matches, consume this character and return true; otherwise, return false
    fn eat(&mut self, expected: char) -> bool {
        let c = self.peek();
        if c == Some(expected) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn eat_str(&mut self, expected: &str) -> bool {
        debug_assert!(!expected.is_empty(), "expected empty string");

        if let Some(rest) = self.chars.as_str().strip_prefix(expected) {
            self.chars = rest.chars();
            true
        } else {
            false
        }
    }

    fn make_span(&self, start: usize) -> Span{
        Span{
            start,
            end: self.column
        }
    }

    fn parse_token(&mut self) -> Result<Token, ()> {
        let c = self.peek().ok_or(())?;
        let r = match c {
            'a'..='z' | 'A'..='Z' => self.parse_ident(),
            _ => panic!()
        };
        Ok(r)
    }

    fn parse_ident(&mut self) -> Token {
        todo!()
    }
}
