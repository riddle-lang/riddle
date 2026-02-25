#[derive(Debug, Clone)]
pub struct Module {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        name: String,
        ty: Option<Expr>,
        init: Option<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(i64),
    Bool(bool),
    String(String),
    Var(String),
    Binary {
        left: Box<Expr>,
        op: String,
        right: Box<Expr>,
    },
}
