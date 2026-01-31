// In the normal case id would not be 0

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct ExprId(pub usize);
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct StmtId(pub usize);
// for global
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct DefId(pub usize);
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct TyId(pub usize);
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct TyExprId(pub usize);
// for local variable
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct LocalId(pub usize);
