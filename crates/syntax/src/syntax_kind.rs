use std::fmt::Formatter;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    // --- tokens ---
    Ident,
    IntNumber,
    String,

    // keywords
    KwFun,
    KwLet,

    // operator
    Plus,      // +
    Minus,     // -
    Star,      // *
    Slash,     // /
    Eq,        // =
    LParen,    // (
    RParen,    // )
    LBrace,    // [
    RBrace,    // ]
    Semicolon, // ;

    // special
    Whitespace,
    Comment,
    ErrorToken,
    Eof,

    // --- nodes ---
    SourceFile,
    Function,
    ParamList,
    Block,
    LetStmt,
    ExprStmt,
    BinaryExpr,
    ParenExpr,
    Literal,
    NameRef,    // ident in value
    Name,       // ident in define
    ErrorNode,

    /// Placeholder for a node whose kind is backpatched on completion.
    Tombstone,
}

impl std::fmt::Display for SyntaxKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxKind::Ident => write!(f, "<identifier>"),
            SyntaxKind::IntNumber => write!(f, "<number>"),
            SyntaxKind::String => write!(f, "<string>"),
            SyntaxKind::KwFun => write!(f, "`fun`"),
            SyntaxKind::KwLet => write!(f, "`let`"),
            SyntaxKind::Plus => write!(f, "`+`"),
            SyntaxKind::Minus => write!(f, "`-`"),
            SyntaxKind::Star => write!(f, "`*`"),
            SyntaxKind::Slash => write!(f, "`/`"),
            SyntaxKind::Eq => write!(f, "`=`"),
            SyntaxKind::LParen => write!(f, "`(`"),
            SyntaxKind::RParen => write!(f, "`)`"),
            SyntaxKind::LBrace => write!(f, "`{{`"),
            SyntaxKind::RBrace => write!(f, "`}}`"),
            SyntaxKind::Semicolon => write!(f, "`;`"),
            SyntaxKind::Whitespace => write!(f, "<whitespace>"),
            SyntaxKind::Comment => write!(f, "<comment>"),
            SyntaxKind::ErrorToken => write!(f, "<error token>"),
            SyntaxKind::Eof => write!(f, "<eof>"),
            SyntaxKind::SourceFile => write!(f, "<source file>"),
            SyntaxKind::Function => write!(f, "<function>"),
            SyntaxKind::ParamList => write!(f, "<parameter list>"),
            SyntaxKind::Block => write!(f, "<block>"),
            SyntaxKind::LetStmt => write!(f, "<let statement>"),
            SyntaxKind::ExprStmt => write!(f, "<expression statement>"),
            SyntaxKind::BinaryExpr => write!(f, "<binary expression>"),
            SyntaxKind::ParenExpr => write!(f, "<parenthesized expression>"),
            SyntaxKind::Literal => write!(f, "<literal>"),
            SyntaxKind::NameRef => write!(f, "<name reference>"),
            SyntaxKind::Name => write!(f, "<name>"),
            SyntaxKind::ErrorNode => write!(f, "<error node>"),
            SyntaxKind::Tombstone => write!(f, "<tombstone>"),
        }
    }
}

impl SyntaxKind {
    pub fn is_trivia(self) -> bool {
        matches!(self, SyntaxKind::Whitespace | SyntaxKind::Comment)
    }

    pub fn from_raw(raw: rowan::SyntaxKind) -> SyntaxKind {
        assert!(raw.0 <= SyntaxKind::Tombstone as u16);
        // SAFETY: The enumeration is #[repr(u16)], and the range has been checked above
        unsafe { std::mem::transmute(raw.0) }
    }

    pub fn to_raw(self) -> rowan::SyntaxKind {
        rowan::SyntaxKind(self as u16)
    }
}
