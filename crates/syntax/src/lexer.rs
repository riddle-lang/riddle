use crate::SyntaxKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: SyntaxKind,
    pub range: std::ops::Range<usize>,
}

pub fn lex(text: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let bytes = text.as_bytes();
    let mut pos = 0;

    while pos < bytes.len() {
        let start = pos;
        let c = bytes[pos];
        let kind = match c {
            b' ' | b'\t' | b'\r' | b'\n' => {
                while pos < bytes.len() && matches!(bytes[pos], b' ' | b'\t' | b'\r' | b'\n') {
                    pos += 1;
                }
                SyntaxKind::Whitespace
            }
            b'/' if bytes.get(pos + 1) == Some(&b'/') => {
                while pos < bytes.len() && bytes[pos] != b'\n' {
                    pos += 1;
                }
                SyntaxKind::Comment
            }
            b'0'..=b'9' => {
                while pos < bytes.len() && bytes[pos].is_ascii_digit() {
                    pos += 1;
                }
                SyntaxKind::IntNumber
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                while pos < bytes.len()
                    && (bytes[pos].is_ascii_alphanumeric() || bytes[pos] == b'_')
                {
                    pos += 1;
                }
                match &text[start..pos] {
                    "fun" => SyntaxKind::KwFun,
                    "let" => SyntaxKind::KwLet,
                    _ => SyntaxKind::Ident,
                }
            }
            b'"' => {
                pos += 1;
                while pos < bytes.len() && bytes[pos] != b'"' {
                    pos += 1;
                }
                if pos < bytes.len() {
                    pos += 1; // 收尾引号
                }
                SyntaxKind::String
            }
            b'+' => {
                pos += 1;
                SyntaxKind::Plus
            }
            b'-' => {
                pos += 1;
                SyntaxKind::Minus
            }
            b'*' => {
                pos += 1;
                SyntaxKind::Star
            }
            b'/' => {
                pos += 1;
                SyntaxKind::Slash
            }
            b'=' => {
                pos += 1;
                SyntaxKind::Eq
            }
            b'(' => {
                pos += 1;
                SyntaxKind::LParen
            }
            b')' => {
                pos += 1;
                SyntaxKind::RParen
            }
            b'{' => {
                pos += 1;
                SyntaxKind::LBrace
            }
            b'}' => {
                pos += 1;
                SyntaxKind::RBrace
            }
            b';' => {
                pos += 1;
                SyntaxKind::Semicolon
            }
            _ => {
                pos += 1;
                SyntaxKind::ErrorToken
            }
        };
        tokens.push(Token {
            kind,
            range: start..pos,
        });
    }
    tokens
}
