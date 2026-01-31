use crate::frontend::ast::AstNode::Identifier;
use crate::frontend::ast::{AstNode, Literal};
use crate::hir::builder::HirBuilder;
use crate::hir::expr::{HirExpr, HirExprKind, HirLiteral};
use crate::hir::items::HirFuncParam;
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
                        let ty = match type_expr {
                            Some(ty) => {
                                let ty = self.lower_type_expr(ty)?;
                                let ty = self.builder.create_type_expr(ty);
                                Some(ty)
                            }
                            None => None,
                        };
                        let value_node = value
                            .as_deref()
                            .ok_or_else(|| "Global variable initializer required".to_string())?;
                        let value = self.lower_expr(value_node)?;
                        let value = self.builder.create_expr(value);
                        self.builder
                            .create_global_var(name, ty, value)
                            .map_err(|_| "Failed to create global variable".to_string())?;
                    }
                }
                AstNode::FuncDecl {
                    name: _,
                    params,
                    return_type,
                    body: _,
                } => {
                    let mut p = Vec::new();
                    for param in params {
                        let type_expr = self.lower_type_expr(&param.type_expr)?;
                        let type_expr = self.builder.create_type_expr(type_expr);
                        p.push(HirFuncParam {
                            name: param.name.clone(),
                            type_expr,
                        })
                    }
                    let ret = match return_type {
                        Some(ret_type) => self.lower_type_expr(ret_type)?,
                        None => HirTypeExpr {
                            kind: HirTypeExprKind::Unit,
                        },
                    };

                }
                _ => {}
            }
        }
        Ok(())
    }

    fn lower_type_expr(&mut self, type_expr: &Box<AstNode>) -> Result<HirTypeExpr, String> {
        let ty = match type_expr.as_ref() {
            Identifier(name) => HirTypeExpr {
                kind: HirTypeExprKind::Path(name.clone()),
            },
            _ => todo!(),
        };
        Ok(ty)
    }

    fn lower_expr(&self, expr: &AstNode) -> Result<HirExpr, String> {
        match expr {
            AstNode::Literal(lit) => match lit {
                Literal::Int(val) => Ok(HirExpr {
                    kind: HirExprKind::Literal(HirLiteral::Int(*val)),
                }),
                Literal::Bool(val) => Ok(HirExpr {
                    kind: HirExprKind::Literal(HirLiteral::Bool(*val)),
                }),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}