use crate::hir::id::{DefId, ExprId, LocalId, StmtId, TyExprId};

#[derive(Debug, Clone)]
pub enum HirItem {
    Func(HirFunc),
    Enum(HirEnum),
    Struct(HirStruct),
    GlobalVariable(HirGlobalVariable),
    ExternFunc(HirExternFunc),
    Trait(HirTrait),
    Impl(HirImpl),
}

#[derive(Debug, Clone)]
pub struct HirTrait {
    pub name: String,
    pub generic_params: Vec<String>,
    pub id: DefId,
    pub items: Vec<HirTraitItem>,
}

#[derive(Debug, Clone)]
pub struct HirTraitItem {
    pub name: String,
    pub generic_params: Vec<String>,
    pub params: Vec<HirFuncParam>,
    pub ret: TyExprId,
}

#[derive(Debug, Clone)]
pub struct HirImpl {
    pub generic_params: Vec<String>,
    pub trait_type: Option<TyExprId>,
    pub target_type: TyExprId,
    pub items: Vec<DefId>,
    pub id: DefId,
}

#[derive(Debug, Clone)]
pub struct HirStructField {
    pub name: String,
    pub type_expr: TyExprId,
}

#[derive(Debug, Clone)]
pub struct HirStruct {
    pub name: String,
    pub generic_params: Vec<String>,
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
    pub generic_params: Vec<String>,
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
    pub is_variadic: bool,
}

#[derive(Debug, Clone)]
pub struct HirEnum {
    pub name: String,
    pub generic_params: Vec<String>,
    pub id: DefId,
    pub variants: Vec<HirEnumVariant>,
}

#[derive(Debug, Clone)]
pub enum HirEnumVariant {
    Unit(String),
    Tuple(String, Vec<TyExprId>),
    Struct(String, Vec<HirStructField>),
}

#[derive(Debug, Clone)]
pub struct HirGlobalVariable {
    pub name: String,
    pub ty: Option<TyExprId>,
    pub value: ExprId,
}
