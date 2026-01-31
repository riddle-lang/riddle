// use for type infer and type checker
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirType {
    /// width 1~128
    Int(u8),
    Bool,
    Unit,
    /// ret, param*
    Func(Vec<HirType>),
    /// For bidirectional type checking / inference
    Infer(InferTy),
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InferTy {
    /// Type variable for inference
    Id(usize),
}