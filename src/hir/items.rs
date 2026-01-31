use pest::pratt_parser::Op;
use crate::hir::id::{DefId, ExprId, StmtId, TyId, TyExprId};
use crate::hir::type_expr::HirTypeExpr;

#[derive(Debug)]
pub enum HirItem {
    Func(HirFunc),
    Enum(HirEnum),
    GlobalVariable(HirGlobalVariable),
}

#[derive(Debug)]
pub struct HirFuncParam {
    pub name: String,
    pub type_expr: TyExprId,
}

#[derive(Debug)]
pub struct HirFunc {
    pub name: String,
    pub param: Vec<HirFuncParam>,
    pub ret: TyExprId,
    pub id: DefId,
    pub body: Vec<StmtId>,
}

#[derive(Debug)]
pub struct HirEnum {
    pub name: String,
    pub id: DefId,
    // TODO: variants
}

#[derive(Debug)]
pub struct HirGlobalVariable {
    pub name: String,
    pub ty: Option<TyExprId>,
    pub value: ExprId,
}