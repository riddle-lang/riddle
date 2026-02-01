use crate::hir::expr::HirExpr;
use crate::hir::items::HirItem;
use crate::hir::stmt::HirStmt;
use crate::hir::types::HirType;
use crate::hir::type_expr::HirTypeExpr;

#[derive(Debug)]
pub struct HirModule {
    pub items: Vec<HirItem>,
    pub exprs: Vec<HirExpr>,
    pub stmts: Vec<HirStmt>,
    pub types: Vec<HirType>,
    pub type_exprs: Vec<HirTypeExpr>,
    pub next_local_id: usize,
}

impl HirModule {
    pub fn new() -> Self {
        Self {
            items: vec![],
            exprs: vec![],
            stmts: vec![],
            types: vec![],
            type_exprs: vec![],
            next_local_id: 0,
        }
    }
}