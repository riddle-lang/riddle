use crate::hir::id::{ExprId, LocalId, TyExprId};

#[derive(Debug, Clone)]
pub struct HirStmt {
    pub kind: HirStmtKind,
}

impl HirStmt {
    pub fn new(kind: HirStmtKind) -> Self {
        Self { kind }
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
    Return{
        value: Option<ExprId>,
    }
}
