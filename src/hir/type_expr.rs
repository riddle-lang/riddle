use crate::hir::id::{TyExprId, TyId};
use crate::error::Span;

#[derive(Debug, Clone)]
pub struct HirTypeExpr {
    pub kind: HirTypeExprKind,
    pub curr_ty: Option<TyId>,
    pub span: Span,
}

impl HirTypeExpr {
    pub fn new(kind: HirTypeExprKind, span: Span) -> Self {
        Self {
            kind,
            curr_ty: None,
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum HirTypeExprKind {
    /// named type like `int`
    Unit,
    Path(String),
    Generic(String, Vec<TyExprId>),
    Func(Vec<TyExprId>, TyExprId),
    Ref(TyExprId),
}
