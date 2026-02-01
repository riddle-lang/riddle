use crate::hir::id::{ExprId, LocalId, StmtId};

#[derive(Debug, Clone)]
pub struct HirExpr {
    pub kind: HirExprKind,
}

#[derive(Debug, Clone)]
pub enum HirExprKind {
    Literal(HirLiteral),
    BinaryOp {
        lhs: ExprId,
        op: String,
        rhs: ExprId,
    },
    Symbol {
        name: String,
        id: Option<LocalId>,
    },
    Call {
        callee: ExprId,
        args: Vec<ExprId>,
    },
    Block {
        stmts: Vec<StmtId>,
    },
}

#[derive(Debug, Clone)]
pub enum HirLiteral {
    Int(i64),
    Bool(bool),
    Str(String),
}
