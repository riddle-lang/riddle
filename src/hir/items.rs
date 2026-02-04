use crate::hir::id::{DefId, ExprId, LocalId, StmtId, TyExprId};

#[derive(Debug, Clone)]
pub enum HirItem {
    Func(HirFunc),
    Enum(HirEnum),
    Struct(HirStruct),
    GlobalVariable(HirGlobalVariable),
    ExternFunc(HirExternFunc),
}

#[derive(Debug, Clone)]
pub struct HirStructField {
    pub name: String,
    pub type_expr: TyExprId,
}

#[derive(Debug, Clone)]
pub struct HirStruct {
    pub name: String,
    pub fields: Vec<HirStructField>,
    pub id: DefId,
}

#[derive(Debug, Clone)]
pub struct HirFuncParam {
    pub name: String,
    pub type_expr: TyExprId,
    pub id: LocalId,
}

#[derive(Debug, Clone)]
pub struct HirFunc {
    pub name: String,
    pub param: Vec<HirFuncParam>,
    pub ret: TyExprId,
    pub id: DefId,
    pub body: Vec<StmtId>,
}

#[derive(Debug, Clone)]
pub struct HirExternFunc {
    pub abi: Option<String>,
    pub name: String,
    pub param: Vec<HirFuncParam>,
    pub ret: TyExprId,
    pub id: DefId,
}

#[derive(Debug, Clone)]
pub struct HirEnum {
    pub name: String,
    pub id: DefId,
    // TODO: variants
}

#[derive(Debug, Clone)]
pub struct HirGlobalVariable {
    pub name: String,
    pub ty: Option<TyExprId>,
    pub value: ExprId,
}