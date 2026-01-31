use crate::hir::id::StmtId;

#[derive(Debug)]
pub struct HirExpr {
    pub kind: HirExprKind,
}

#[derive(Debug)]
pub enum HirExprKind {
    Literal(HirLiteral),
    Block{
        stmts: Vec<StmtId>,
    }
}

#[derive(Debug)]
pub enum HirLiteral {
    Int(i64),
    Bool(bool),
    Str(String),
}
