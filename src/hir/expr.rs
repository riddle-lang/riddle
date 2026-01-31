use crate::hir::id::StmtId;

#[derive(Debug)]
pub struct HirExpr {
    pub kind: HirExprKind,
}

#[derive(Debug)]
pub enum HirExprKind {
    Literal(Literal),
    Block{
        stmts: Vec<StmtId>,
    }
}

#[derive(Debug)]
pub enum Literal {
    Int(i64),
    Str(String),
}
