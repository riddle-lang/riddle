use crate::hir::id::TyId;

// use for type infer and type checker
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirType {
    /// width 1~128
    Int(u8),
    Float,
    Double,
    Bool,
    Unit,
    /// ret, param*
    Func(Vec<TyId>),
    /// DefId of the struct
    Struct(crate::hir::id::DefId),
    /// DefId of the enum
    Enum(crate::hir::id::DefId),
    /// For bidirectional type checking / inference
    Infer(InferTy),
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InferTy {
    /// Type variable for inference
    Id(usize),
}
