use crate::hir::id::{TyExprId, TyId};

#[derive(Debug, Clone)]
pub struct HirTypeExpr {
    pub kind: HirTypeExprKind,
    pub curr_ty: Option<TyId>,
}

impl HirTypeExpr {
    pub fn new(kind: HirTypeExprKind) -> Self {
        Self { kind, curr_ty: None }
    }
}

#[derive(Debug, Clone)]
pub enum HirTypeExprKind {
    /// named type like `int`
    Unit,
    Path(String), 
    Func(Vec<TyExprId>, TyExprId),
}