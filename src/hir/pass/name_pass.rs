use std::collections::HashMap;
use crate::hir::id::LocalId;
use crate::hir::module::HirModule;
use crate::hir::items::HirItem;
use crate::hir::stmt::HirStmtKind;
use crate::hir::expr::HirExprKind;
use crate::hir::id::{ExprId, StmtId};
use crate::error::{RiddleError, Result};

pub struct NamePass<'a> {
    module: &'a mut HirModule,
    scopes: Vec<HashMap<String, LocalId>>,
}

impl<'a> NamePass<'a> {
    pub fn new(module: &'a mut HirModule) -> Self {
        Self {
            module,
            scopes: vec![HashMap::new()],
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: String, id: LocalId) -> Result<()> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name) {
                return Err(RiddleError::Name(format!("Variable '{}' already defined in this scope", name), None));
            }
            scope.insert(name, id);
        }
        Ok(())
    }

    fn resolve(&self, name: &str) -> Option<LocalId> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.get(name) {
                return Some(*id);
            }
        }
        None
    }

    pub fn run(&mut self) -> Result<()> {
        self.process_items()
    }

    fn process_items(&mut self) -> Result<()> {
        let items_count = self.module.items.len();
        for i in 0..items_count {
            match &self.module.items[i] {
                HirItem::Func(_) => {
                    self.process_func(i)?;
                }
                HirItem::ExternFunc(_) => {}
                HirItem::GlobalVariable(gv) => {
                    self.process_expr(gv.value)?;
                }
                HirItem::Enum(_) | HirItem::Struct(_) => {}
                HirItem::Trait(_) | HirItem::Impl(_) => {}
            }
        }
        Ok(())
    }

    fn process_func(&mut self, func_idx: usize) -> Result<()> {
        self.push_scope();
        
        let (params, body_stmts) = if let HirItem::Func(func) = &self.module.items[func_idx] {
            (func.param.clone(), func.body.clone())
        } else {
            return Ok(());
        };

        for param in params {
            self.define(param.name, param.id)?;
        }

        for stmt_id in body_stmts {
            self.process_stmt(stmt_id)?;
        }

        self.pop_scope();
        Ok(())
    }

    fn process_stmt(&mut self, stmt_id: StmtId) -> Result<()> {
        let kind = self.module.stmts[stmt_id.0].kind.clone();
        match kind {
            HirStmtKind::Let { name, init, id, .. } => {
                if let Some(init_expr) = init {
                    self.process_expr(init_expr)?;
                }
                self.define(name, id)?;
            }
            HirStmtKind::Expr { expr, .. } => {
                self.process_expr(expr)?;
            }
            HirStmtKind::Return { value } => {
                if let Some(expr) = value {
                    self.process_expr(expr)?;
                }
            }
        }
        Ok(())
    }

    fn process_expr(&mut self, expr_id: ExprId) -> Result<()> {
        let kind = self.module.exprs[expr_id.0].kind.clone();
        match kind {
            HirExprKind::BinaryOp { lhs, rhs, .. } => {
                self.process_expr(lhs)?;
                self.process_expr(rhs)?;
            }
            HirExprKind::Symbol { ref name, .. } => {
                if let Some(id) = self.resolve(name) {
                    if let HirExprKind::Symbol { id: sym_id, .. } = &mut self.module.exprs[expr_id.0].kind {
                        *sym_id = Some(id);
                    }
                }
                // Global symbols are resolved in TypeInfer
            }
            HirExprKind::Call { callee, ref args } => {
                self.process_expr(callee)?;
                let args_clone = args.clone();
                for arg in args_clone {
                    self.process_expr(arg)?;
                }
            }
            HirExprKind::Block { ref stmts } => {
                self.push_scope();
                let stmts_clone = stmts.clone();
                for stmt_id in stmts_clone {
                    self.process_stmt(stmt_id)?;
                }
                self.pop_scope();
            }
            HirExprKind::StructInst { ref fields, .. } => {
                let fields_clone = fields.clone();
                for (_, expr_id) in fields_clone {
                    self.process_expr(expr_id)?;
                }
            }
            HirExprKind::MemberAccess { object, .. } => {
                self.process_expr(object)?;
            }
            _ => {}
        }
        Ok(())
    }
}