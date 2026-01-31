use crate::hir::id::{DefId, TyId, StmtId};

#[derive(Debug)]
pub enum HirItem {
    Func(HirFunc),
    Enum(HirEnum),
}

#[derive(Debug)]
pub struct HirFunc {
    pub name: String,
    pub ty: TyId,
    pub id: DefId,
    pub body: Vec<StmtId>,
}

#[derive(Debug)]
pub struct HirEnum {
    pub name: String,
    pub id: DefId,
    // TODO: variants
}