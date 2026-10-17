mod lexer;
mod parser;
mod syntax_kind;

pub use syntax_kind::SyntaxKind;

use rowan::Language;

/// 语言标识
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RiddleLang {}

impl Language for RiddleLang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        SyntaxKind::from_raw(raw)
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.to_raw()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<RiddleLang>;
pub type SyntaxToken = rowan::SyntaxToken<RiddleLang>;
pub type SyntaxElement = rowan::SyntaxElement<RiddleLang>;

/// 一次解析的结果
#[derive(Debug, Clone)]
pub struct Parse {
    green: rowan::GreenNode,
    errors: Vec<ParseError>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub message: String,
    /// 字节偏移范围
    pub range: std::ops::Range<usize>,
}

impl Parse {
    pub fn syntax_node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }

    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }

    pub fn ok(&self) -> bool {
        self.errors.is_empty()
    }
}

/// 解析入口
pub fn parse(text: &str) -> Parse {
    let tokens = lexer::lex(text);
    parser::parse(text, &tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_is_lossless() {
        let text = "fun main() {\n  let x = 1 + 2; // hi\n}\n";
        let parse = parse(text);
        assert!(parse.ok(), "{:?}", parse.errors());
        let node = parse.syntax_node();
        assert_eq!(node.text().to_string(), text);
    }

    #[test]
    fn broken_input_still_parses() {
        let parse = parse("fun ( } let =");
        assert!(!parse.ok());
        // 不应 panic，且树完整覆盖输入
        assert_eq!(parse.syntax_node().text().to_string(), "fun ( } let =");
    }
}
