use crate::frontend::ast::{AstNode, Literal};
use crate::hir::builder::HirBuilder;
use crate::hir::expr::{HirExpr, HirExprKind, HirLiteral};
use crate::hir::id::TyExprId;
use crate::hir::items::{HirFuncParam, HirStructField};
use crate::hir::module::HirModule;
use crate::hir::type_expr::{HirTypeExpr, HirTypeExprKind};

pub struct AstLower<'a> {
    builder: HirBuilder<'a>,
    ast: &'a Vec<AstNode>,
    deep: usize,
}

impl<'a> AstLower<'a> {
    pub fn new(module: &'a mut HirModule, ast: &'a Vec<AstNode>) -> Self {
        Self {
            builder: HirBuilder::new(module),
            ast,
            deep: 0,
        }
    }

    pub fn lower(&mut self) -> Result<(), String> {
        for stmt in self.ast {
            match stmt {
                AstNode::VarDecl {
                    name,
                    type_expr,
                    value,
                } => {
                    if self.deep == 0 {
                        // global variable
                        let ty = self.lower_optional_type_expr(type_expr)?;
                        let value_node = value
                            .as_deref()
                            .ok_or_else(|| "Global variable initializer required".to_string())?;
                        let value = self.lower_expr(value_node)?;
                        let value = self.builder.create_expr(value);
                        self.builder
                            .create_global_var(name, ty, value)
                            .map_err(|_| "Failed to create global variable".to_string())?;
                    } else {
                        // local variable
                        let ty = self.lower_optional_type_expr(type_expr)?;
                        let value = match value {
                            Some(v) => {
                                let v = self.lower_expr(v)?;
                                Some(self.builder.create_expr(v))
                            }
                            None => None,
                        };
                        self.builder
                            .local(name, ty, value)
                            .map_err(|_| "Failed to create local variable".to_string())?;
                    }
                }
                AstNode::FuncDecl {
                    name,
                    params,
                    return_type,
                    body,
                } => {
                    let mut p = Vec::new();
                    for param in params {
                        let type_expr = self.lower_type_expr(&param.type_expr)?;
                        let type_expr = self.builder.create_type_expr(type_expr);
                        let id = self.builder.next_local_id();
                        p.push(HirFuncParam {
                            name: param.name.clone(),
                            type_expr,
                            id,
                        })
                    }
                    let ret = match return_type {
                        Some(ret_type) => self.lower_type_expr(ret_type)?,
                        None => HirTypeExpr::new(HirTypeExprKind::Unit),
                    };
                    let ret = self.builder.create_type_expr(ret);

                    let func_id = self
                        .builder
                        .create_func(name, ret, p)
                        .map_err(|_| "Failed to create function".to_string())?;

                    self.builder.set_func(func_id);
                    self.deep += 1;
                    self.lower_block(body)?;
                    self.deep -= 1;
                }
                AstNode::StructDecl { name, fields } => {
                    let mut hir_fields = Vec::new();
                    for field in fields {
                        let type_expr = self.lower_type_expr(&field.type_expr)?;
                        let type_expr_id = self.builder.create_type_expr(type_expr);
                        hir_fields.push(HirStructField {
                            name: field.name.clone(),
                            type_expr: type_expr_id,
                        });
                    }
                    self.builder
                        .create_struct(name, hir_fields)
                        .map_err(|_| "Failed to create struct".to_string())?;
                }
                AstNode::EnumDecl { name, .. } => {
                    self.builder
                        .create_enum(name)
                        .map_err(|_| "Failed to create enum".to_string())?;
                }
                AstNode::ExternBlock { abi, functions } => {
                    for func in functions {
                        self.lower_extern_func(abi.clone(), func)?;
                    }
                }
                AstNode::ExternDecl { abi, function } => {
                    self.lower_extern_func(abi.clone(), function)?;
                }
                _ => {
                    return Err("Expected statement".to_string());
                }
            }
        }
        Ok(())
    }

    fn lower_extern_func(
        &mut self,
        abi: Option<String>,
        func: &crate::frontend::ast::ExternFunc,
    ) -> Result<(), String> {
        let mut p = Vec::new();
        for param in &func.params {
            let type_expr = self.lower_type_expr(&param.type_expr)?;
            let type_expr = self.builder.create_type_expr(type_expr);
            let id = self.builder.next_local_id();
            p.push(HirFuncParam {
                name: param.name.clone(),
                type_expr,
                id,
            })
        }
        let ret = match &func.return_type {
            Some(ret_type) => self.lower_type_expr(ret_type)?,
            None => HirTypeExpr::new(HirTypeExprKind::Unit),
        };
        let ret = self.builder.create_type_expr(ret);

        self.builder
            .create_extern_func(abi, &func.name, ret, p)
            .map_err(|_| "Failed to create extern function".to_string())?;
        Ok(())
    }

    fn lower_type_expr(&mut self, type_expr: &Box<AstNode>) -> Result<HirTypeExpr, String> {
        match type_expr.as_ref() {
            AstNode::Identifier(name) => Ok(HirTypeExpr::new(HirTypeExprKind::Path(name.clone()))),
            _ => Err(format!("Unexpected node in type expr: {:?}", type_expr)),
        }
    }

    fn lower_optional_type_expr(
        &mut self,
        type_expr: &Option<Box<AstNode>>,
    ) -> Result<Option<TyExprId>, String> {
        match type_expr {
            Some(ty) => {
                let ty = self.lower_type_expr(ty)?;
                let ty = self.builder.create_type_expr(ty);
                Ok(Some(ty))
            }
            None => Ok(None),
        }
    }

    fn lower_block(&mut self, block: &AstNode) -> Result<(), String> {
        match block {
            AstNode::Block {
                statements,
                final_expr,
            } => {
                for stmt in statements {
                    self.lower_stmt(stmt)?;
                }
                if let Some(expr) = final_expr {
                    let expr = self.lower_expr(expr)?;
                    let expr_id = self.builder.create_expr(expr);
                    self.builder
                        .expr_stmt(expr_id, false)
                        .map_err(|_| "Failed to create expr stmt".to_string())?;
                }
                Ok(())
            }
            _ => Err("Expected block".to_string()),
        }
    }

    fn lower_stmt(&mut self, stmt: &AstNode) -> Result<(), String> {
        match stmt {
            AstNode::VarDecl {
                name,
                type_expr,
                value,
            } => {
                let ty = match type_expr {
                    Some(ty) => {
                        let ty = self.lower_type_expr(ty)?;
                        let ty = self.builder.create_type_expr(ty);
                        Some(ty)
                    }
                    None => None,
                };
                let value = match value {
                    Some(v) => {
                        let v = self.lower_expr(v)?;
                        Some(self.builder.create_expr(v))
                    }
                    None => None,
                };
                self.builder
                    .local(name, ty, value)
                    .map_err(|_| "Failed to create local variable".to_string())?;
            }
            AstNode::Return(val) => {
                let expr = self.lower_expr(val)?;
                let expr_id = self.builder.create_expr(expr);
                self.builder
                    .ret(expr_id)
                    .map_err(|_| "Failed to create return stmt".to_string())?;
            }
            AstNode::Block { .. } => {
                self.lower_block(stmt)?;
            }
            _ => {
                let expr = self.lower_expr(stmt)?;
                let expr_id = self.builder.create_expr(expr);
                self.builder
                    .expr_stmt(expr_id, true)
                    .map_err(|_| "Failed to create expr stmt".to_string())?;
            }
        }
        Ok(())
    }

    fn lower_expr(&mut self, expr: &AstNode) -> Result<HirExpr, String> {
        match expr {
            AstNode::Literal(lit) => match lit {
                Literal::Int(val) => Ok(HirExpr::new(HirExprKind::Literal(HirLiteral::Int(*val)))),
                Literal::Bool(val) => Ok(HirExpr::new(HirExprKind::Literal(HirLiteral::Bool(*val)))),
                Literal::Float(val) => Ok(HirExpr::new(HirExprKind::Literal(HirLiteral::Float(*val)))),
                Literal::Str(val) => Ok(HirExpr::new(HirExprKind::Literal(HirLiteral::Str(val.clone())))),
            },
            AstNode::BinaryExpr { lhs, op, rhs } => {
                let lhs = self.lower_expr(lhs)?;
                let lhs = self.builder.create_expr(lhs);
                let rhs = self.lower_expr(rhs)?;
                let rhs = self.builder.create_expr(rhs);
                Ok(HirExpr::new(HirExprKind::BinaryOp {
                    lhs,
                    op: op.clone(),
                    rhs,
                }))
            }
            AstNode::Identifier(name) => Ok(HirExpr::new(HirExprKind::Symbol {
                name: name.clone(),
                id: None,
            })),
            AstNode::Call { func, args } => {
                let callee = self.lower_expr(func)?;
                let callee = self.builder.create_expr(callee);
                let mut arg_ids = Vec::new();
                for arg in args {
                    let arg_expr = self.lower_expr(arg)?;
                    let arg_id = self.builder.create_expr(arg_expr);
                    arg_ids.push(arg_id);
                }
                Ok(HirExpr::new(HirExprKind::Call {
                    callee,
                    args: arg_ids,
                }))
            }
            AstNode::StructInst { name, fields } => {
                let mut hir_fields = Vec::new();
                for (f_name, f_expr) in fields {
                    let f_expr = self.lower_expr(f_expr)?;
                    let f_expr_id = self.builder.create_expr(f_expr);
                    hir_fields.push((f_name.clone(), f_expr_id));
                }
                Ok(HirExpr::new(HirExprKind::StructInst {
                    struct_name: name.clone(),
                    fields: hir_fields,
                }))
            }
            _ => todo!(),
        }
    }
}
