use crate::error::Span;

#[derive(Debug, Clone)]
pub struct AstNode {
    pub kind: AstNodeKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum AstNodeKind {
    Program(Vec<AstNode>),

    // Statements
    VarDecl {
        name: String,
        type_expr: Option<Box<AstNode>>,
        value: Option<Box<AstNode>>,
    },
    FuncDecl {
        name: String,
        generic_params: Vec<String>,
        params: Vec<FuncParam>,
        return_type: Option<Box<AstNode>>,
        body: Box<AstNode>, // Block
    },
    StructDecl {
        name: String,
        generic_params: Vec<String>,
        fields: Vec<StructField>,
    },
    EnumDecl {
        name: String,
        generic_params: Vec<String>,
        variants: Vec<EnumVariant>,
    },
    TraitDecl {
        name: String,
        generic_params: Vec<String>,
        items: Vec<TraitItem>,
    },
    ImplDecl {
        generic_params: Vec<String>,
        trait_type: Option<Box<AstNode>>,
        target_type: Box<AstNode>,
        items: Vec<AstNode>,
    },
    ExternBlock {
        abi: Option<String>,
        functions: Vec<ExternFunc>,
    },
    ExternDecl {
        abi: Option<String>,
        function: ExternFunc,
    },
    Return(Box<AstNode>),
    Block {
        statements: Vec<AstNode>,
        final_expr: Option<Box<AstNode>>,
    },

    // Expressions
    BinaryExpr {
        lhs: Box<AstNode>,
        op: String,
        rhs: Box<AstNode>,
    },
    UnaryExpr {
        op: String,
        expr: Box<AstNode>,
    },
    Call {
        func: Box<AstNode>,
        args: Vec<AstNode>,
    },
    StructInst {
        name: String,
        fields: Vec<(String, AstNode)>,
    },
    MemberAccess {
        object: Box<AstNode>,
        member: String,
    },
    IndexAccess {
        object: Box<AstNode>,
        index: Box<AstNode>,
    },
    CastExpr {
        expr: Box<AstNode>,
        target_type: Box<AstNode>,
    },
    ListLiteral(Vec<AstNode>),

    If {
        cond: Box<AstNode>,
        then_block: Box<AstNode>,
        else_block: Option<Box<AstNode>>,
    },
    // Primary Expressions
    Identifier(String),
    Path(Vec<String>),
    TypeExpr {
        path: Vec<String>,
        args: Vec<AstNode>,
    },
    ArrayType(Box<AstNode>, usize),
    PointerType(Box<AstNode>),
    Literal(Literal),
}

#[derive(Debug, Clone)]
pub struct FuncParam {
    pub name: String,
    pub type_expr: Box<AstNode>,
}

#[derive(Debug, Clone)]
pub struct ExternFunc {
    pub name: String,
    pub params: Vec<FuncParam>,
    pub return_type: Option<Box<AstNode>>,
    pub is_variadic: bool,
}

#[derive(Debug, Clone)]
pub struct TraitItem {
    pub name: String,
    pub generic_params: Vec<String>,
    pub params: Vec<FuncParam>,
    pub return_type: Option<Box<AstNode>>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub type_expr: Box<AstNode>,
}

#[derive(Debug, Clone)]
pub enum EnumVariant {
    Unit(String),
    Tuple(String, Vec<AstNode>),
    Struct(String, Vec<StructField>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    CStr(String),
    CInt(i64),
}
