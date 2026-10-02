use logos::Logos;

fn raw_string(lex: &mut logos::Lexer<'_, SyntaxKind>) -> Option<()> {
    let hashes = lex.slice().len().checked_sub(2)?;
    let terminator = format!("\"{}", "#".repeat(hashes));
    let end = lex.remainder().find(&terminator)?;
    lex.bump(end + terminator.len());
    Some(())
}

fn block_comment(lex: &mut logos::Lexer<'_, SyntaxKind>) -> Option<()> {
    let mut depth = 1usize;
    let bytes = lex.remainder().as_bytes();
    let mut index = 0usize;
    while index + 1 < bytes.len() {
        match &bytes[index..index + 2] {
            b"/*" => {
                depth += 1;
                index += 2;
            }
            b"*/" => {
                depth -= 1;
                index += 2;
                if depth == 0 {
                    lex.bump(index);
                    return Some(());
                }
            }
            // A `//` that starts its own line comments out the rest of that
            // line, so a terminator inside it is text: commented-out code such
            // as `// let a = b; */` stays inside the block. A `//` in the
            // middle of a line is prose, and a terminator after it still closes
            // the comment. Either way scanning resumes on the next line.
            b"//" if comment_body_line_starts_at(bytes, index) => {
                match bytes[index + 2..].iter().position(|byte| *byte == b'\n') {
                    Some(offset) => index += 2 + offset + 1,
                    // The line comment runs to the end of the input, so no
                    // terminator is left and the block stays unterminated.
                    None => break,
                }
            }
            _ => index += 1,
        }
    }
    // Unterminated: the whole remainder becomes the comment token, so the
    // parser reports one error instead of re-lexing the body as code.
    lex.bump(bytes.len());
    Some(())
}

/// Whether the `//` at `index` is the first non-whitespace text on its line
/// inside a comment body. The body starts right after the opener, so reaching
/// position 0 without crossing a newline means the opener shares that line.
fn comment_body_line_starts_at(bytes: &[u8], index: usize) -> bool {
    let mut cursor = index;
    while cursor > 0 {
        match bytes[cursor - 1] {
            b'\n' => return true,
            byte if byte.is_ascii_whitespace() => cursor -= 1,
            _ => return false,
        }
    }
    false
}

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    #[regex(r"[ \t\r\n]+")]
    Whitespace = 0,

    #[regex(r"///[^\n]*", allow_greedy = true)]
    #[regex(r"//![^\n]*", allow_greedy = true)]
    DocComment,

    #[token("/**", block_comment)]
    #[token("/*!", block_comment)]
    DocBlockComment,

    #[token("/*", block_comment)]
    BlockComment,

    #[regex(r"//[^\n]*", allow_greedy = true)]
    LineComment,

    // keywords
    #[token("let")]
    Let,
    #[token("fun")]
    Fun,
    #[token("struct")]
    Struct,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("loop")]
    Loop,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("return")]
    Return,
    #[token("as")]
    As,
    #[token("self")]
    SelfKw,
    #[token("mod")]
    Mod,
    #[token("use")]
    Use,
    #[token("mut")]
    Mut,
    #[token("move")]
    Move,
    #[token("pub")]
    Pub,
    #[token("super")]
    SuperKw,
    #[token("crate")]
    CrateKw,
    #[token("enum")]
    Enum,
    #[token("trait")]
    Trait,
    #[token("impl")]
    Impl,
    #[token("dyn")]
    Dyn,
    #[token("match")]
    Match,
    #[token("const")]
    Const,
    #[token("type")]
    TypeKw,
    #[token("extern")]
    Extern,
    #[token("unsafe")]
    Unsafe,
    #[token("safe")]
    Safe,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("where")]
    Where,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex(r##"r#*""##, raw_string)]
    #[regex(r#""([^"\\]|\\.)*""#)]
    String,
    #[regex(r#"'([^'\\]|\\.)'"#)]
    Char,
    #[token("_")]
    Underscore,

    #[regex(r"[0-9]+\.[0-9]+(?:[eE][+-]?[0-9]+)?(?:f16|f32|f64|f128)?")]
    #[regex(r"[0-9]+(?:[eE][+-]?[0-9]+)(?:f16|f32|f64|f128)?")]
    #[regex(r"[0-9]+(?:f16|f32|f64|f128)")]
    Float,

    #[regex(r"[a-zA-Z][a-zA-Z0-9_]*")]
    #[regex(r"_[a-zA-Z0-9_]+")]
    Ident,

    #[regex(
        r"0x_*[0-9a-fA-F][0-9a-fA-F_]*(?:i8|i16|i32|i64|i128|isize|u8|u16|u32|u64|u128|usize)?"
    )]
    #[regex(r"0o_*[0-7][0-7_]*(?:i8|i16|i32|i64|i128|isize|u8|u16|u32|u64|u128|usize)?")]
    #[regex(r"0b_*[01][01_]*(?:i8|i16|i32|i64|i128|isize|u8|u16|u32|u64|u128|usize)?")]
    #[regex(r"0[xob][a-zA-Z0-9_]*", priority = 1)]
    #[regex(r"[0-9][0-9_]*(?:i8|i16|i32|i64|i128|isize|u8|u16|u32|u64|u128|usize)?")]
    Number,

    #[token("->")]
    Arrow,
    #[token("==")]
    EqEq,
    #[token("!=")]
    BangEq,
    #[token("<=")]
    LessEq,
    #[token(">=")]
    GreaterEq,
    #[token("&&")]
    AmpAmp,
    #[token("||")]
    PipePipe,
    #[token("=>")]
    FatArrow,
    #[token("+=")]
    PlusEq,
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    StarEq,
    #[token("/=")]
    SlashEq,
    #[token("%=")]
    PercentEq,
    #[token("&=")]
    AmpEq,
    #[token("|=")]
    PipeEq,
    #[token("^=")]
    CaretEq,
    #[token("<<=")]
    ShlEq,
    #[token(">>=")]
    ShrEq,
    #[token("<<")]
    Shl,
    #[token(">>")]
    Shr,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("&")]
    Amp,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("!")]
    Bang,
    #[token("?")]
    Question,
    #[token("#")]
    Hash,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,

    #[token(".")]
    Dot,
    #[token("..")]
    DotDot,
    #[token("..=")]
    DotDotEq,
    #[token(":")]
    Colon,
    #[token("::")]
    ColonColon,
    #[token(";")]
    Semi,
    #[token(",")]
    Comma,
    #[token("=")]
    Eq,

    // nodes
    // see docs/grammar.md
    Root,
    VarDecl,
    FuncDecl,
    Param,
    ParamList,
    StructDecl,
    StructField,
    StructFieldList,
    ModDecl,
    UseDecl,
    UseTree,
    UseTreeList,
    Path,
    PathSegment,
    IfStmt,
    WhileStmt,
    LetCondition,
    ForExpr,
    LoopExpr,
    BreakStmt,
    ContinueStmt,
    ReturnStmt,
    Block,
    ExprStmt,
    NameRef,
    NumberLit,
    BinaryExpr,
    UnaryExpr,
    ParenExpr,
    CallExpr,
    BracketLambdaExpr,
    ArgList,
    FieldExpr,
    IndexExpr,
    RangeExpr,
    StructExpr,
    StructExprField,
    NamedType,
    NeverType,
    TypeArgList,
    RefType,
    TupleType,
    ArrayType,
    ConstType,
    ImplTraitType,
    DynTraitType,
    CallableTraitArgs,
    ArrayExpr,
    MatchExpr,
    MatchArm,
    EnumDecl,
    EnumVariant,
    TraitDecl,
    ImplDecl,
    GenericParams,
    WhereClause,
    TypeAliasDecl,
    ConstDecl,
    TuplePattern,
    StructPattern,
    EnumPattern,
    ReferencePattern,
    /// `A | B | C` — top-level or-pattern in a match arm.
    OrPattern,
    /// `mut name` — a binding that is explicitly mutable.
    BindingPattern,
    WildcardPattern,
    LiteralPattern,
    FloatLit,
    StringLit,
    CharLit,
    BoolLit,
    ExternBlock,
    ExternFnDecl,
    Attribute,
    UnsafeExpr,
    CastExpr,
    TryExpr,
    MacroCall,
    PtrType,
    ErrorNode,

    // special
    Tombstone,
    Eof,
}

impl SyntaxKind {
    #[must_use]
    pub const fn is_trivia(self) -> bool {
        matches!(
            self,
            Self::Whitespace
                | Self::LineComment
                | Self::DocComment
                | Self::BlockComment
                | Self::DocBlockComment
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RiddleLang;

impl rowan::Language for RiddleLang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        // Safe: #[repr(u16)] with sequential discriminants 0..=Eof are all valid
        if raw.0 <= SyntaxKind::Eof as u16 {
            // Safety: raw.0 is within the valid discriminant range
            unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
        } else {
            SyntaxKind::ErrorNode
        }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}

pub type SyntaxNode = rowan::SyntaxNode<RiddleLang>;
pub type SyntaxToken = rowan::SyntaxToken<RiddleLang>;
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;
