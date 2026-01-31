#[derive(Debug, Clone)]
pub enum AstNode {
    Program(Vec<AstNode>),
    
    // Statements
    VarDecl {
        name: String,
        type_expr: Option<Box<AstNode>>,
        value: Option<Box<AstNode>>,
    },
    FuncDecl {
        name: String,
        params: Vec<FuncParam>,
        return_type: Option<Box<AstNode>>,
        body: Box<AstNode>, // Block
    },
    StructDecl {
        name: String,
        fields: Vec<StructField>,
    },
    EnumDecl {
        name: String,
        variants: Vec<EnumVariant>,
    },
    ExprStmt(Box<AstNode>),
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
    PostfixExpr {
        base: Box<AstNode>,
        suffixes: Vec<AstNode>, // CallSuffix
    },
    CallSuffix(Vec<AstNode>), // Arguments
    
    // Primary Expressions
    Identifier(String),
    Literal(Literal),
}

#[derive(Debug, Clone)]
pub struct FuncParam {
    pub name: String,
    pub type_expr: Box<AstNode>,
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
    Str(String),
    Bool(bool),
}