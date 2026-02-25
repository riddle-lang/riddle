#[derive(Debug)]
pub struct Module {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    Let { name: String, ty: Option<Expr>, init: Option<Expr>  },
}

#[derive(Debug)]
pub enum Expr{
    Number(i64),
    Bool(bool),
    String(String),
}