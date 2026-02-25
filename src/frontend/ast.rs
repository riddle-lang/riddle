#[derive(Debug, Clone)]
pub struct Module {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    ExprStmt(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(i64),
    Bool(bool),
    String(String),
}
