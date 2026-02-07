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
    Str,
    CInt,
    CStr,
    /// ret, param*, is_variadic
    Func(Vec<TyId>, bool),
    /// DefId of the struct, Generic arguments
    Struct(crate::hir::id::DefId, Vec<TyId>),
    /// DefId of the enum, Generic arguments
    Enum(crate::hir::id::DefId, Vec<TyId>),
    Pointer(TyId),
    Array(TyId, usize),
    GenericParam(String),
    /// For bidirectional type checking / inference
    Infer(InferTy),
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InferTy {
    /// Type variable for inference
    Id(usize),
}
