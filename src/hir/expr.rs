use crate::hir::id::{ExprId, LocalId, StmtId, TyId};

#[derive(Debug, Clone)]
pub struct HirExpr {
    pub kind: HirExprKind,
    pub ty: Option<TyId>,
}

impl HirExpr {
    pub fn new(kind: HirExprKind) -> Self {
        Self { kind, ty: None }
    }
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
    Float(f64),
    Bool(bool),
    Str(String),
}
