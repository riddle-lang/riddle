use crate::hir::id::TyExprId;

#[derive(Debug, Clone)]
pub struct HirTypeExpr {
    pub kind: HirTypeExprKind,
}

#[derive(Debug, Clone)]
pub enum HirTypeExprKind {
    /// named type like `int`
    Path(String), 
    Func(Vec<TyExprId>, TyExprId),
}