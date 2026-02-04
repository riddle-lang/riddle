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
    TraitDecl {
        name: String,
        items: Vec<TraitItem>,
    },
    ImplDecl {
        trait_name: Option<String>,
        target_name: String,
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
pub struct ExternFunc {
    pub name: String,
    pub params: Vec<FuncParam>,
    pub return_type: Option<Box<AstNode>>,
}

#[derive(Debug, Clone)]
pub struct TraitItem {
    pub name: String,
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
}