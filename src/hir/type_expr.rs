use crate::error::Span;
use crate::hir::id::{TyExprId, TyId};

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
    Pointer(TyExprId),
    Array(TyExprId, usize),
    Generic(String, Vec<TyExprId>),
    Func(Vec<TyExprId>, TyExprId),
}
