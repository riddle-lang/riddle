use std::sync::Arc;

use la_arena::Arena;

use crate::func::Function;
use crate::types::{StructDef, Type};

/// Top-level compilation unit containing all functions and external declarations.
#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,

    pub functions: Arena<Function>,

    /// Order in which functions appear in the source (deterministic iteration).
    pub function_order: Vec<la_arena::Idx<Function>>,

    /// Externally-linked function signatures.
    pub externs: Vec<ExternFunc>,

    /// Every nominal struct definition the module's types mention.
    ///
    /// A recursive struct refers to its own definition through a `Weak`, so the
    /// module owns the strong handle for as long as any of its types can be
    /// resolved. Definitions are shared: one entry per monomorphized struct.
    pub struct_defs: Vec<Arc<StructDef>>,

    /// Errors discovered while lowering type-checked HIR.
    pub diagnostics: Vec<type_checker::Diagnostic>,
}

#[derive(Debug, Clone)]
pub struct ExternFunc {
    pub name: String,
    pub params: Vec<Type>,
    pub ret_type: Type,
}

impl Module {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            functions: Arena::new(),
            function_order: Vec::new(),
            externs: Vec::new(),
            struct_defs: Vec::new(),
            diagnostics: Vec::new(),
        }
    }

    pub fn add_function(&mut self, func: Function) -> la_arena::Idx<Function> {
        let id = self.functions.alloc(func);
        self.function_order.push(id);
        id
    }

    pub fn add_extern(&mut self, name: String, params: Vec<Type>, ret_type: Type) {
        self.externs.push(ExternFunc {
            name,
            params,
            ret_type,
        });
    }
}
