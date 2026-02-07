use crate::error::Span;
use crate::hir::id::{ExprId, LocalId, StmtId, TyExprId};

#[derive(Debug, Clone)]
pub struct HirStmt {
    pub kind: HirStmtKind,
    pub span: Span,
}

impl HirStmt {
    pub fn new(kind: HirStmtKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// Here saving the return value is a meaningless operation like ` return ` or ` break `
#[derive(Debug, Clone)]
pub enum HirStmtKind {
    Expr {
        expr: ExprId,
        has_semi: bool,
    },
    // variable define and init
    Let {
        name: String,
        ty_annot: Option<TyExprId>,
        init: Option<ExprId>,
        id: LocalId,
    },
    Return {
        value: Option<ExprId>,
    },
    If {
        cond: ExprId,
        then_block: StmtId,
        else_block: Option<StmtId>,
    },
}
