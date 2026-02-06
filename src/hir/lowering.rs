use crate::frontend::ast::{AstNode, AstNodeKind, EnumVariant, Literal};
use crate::hir::builder::HirBuilder;
use crate::hir::expr::{HirExpr, HirExprKind, HirLiteral};
use crate::hir::id::TyExprId;
use crate::hir::items::{HirEnumVariant, HirFuncParam, HirStructField, HirTraitItem};
use crate::hir::module::HirModule;
use crate::hir::type_expr::{HirTypeExpr, HirTypeExprKind};
use crate::error::{RiddleError, Result, Span};

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

    pub fn lower(&mut self) -> Result<()> {
        for node in self.ast {
            let span = node.span;
            match &node.kind {
                AstNodeKind::VarDecl {
                    name,
                    type_expr,
                    value,
                } => {
                    if self.deep == 0 {
                        // global variable
                        let ty = self.lower_optional_type_expr(type_expr)?;
                        let value_node = value
                            .as_deref()
                            .ok_or_else(|| RiddleError::Lowering("Global variable initializer required".to_string(), Some(span)))?;
                        let value = self.lower_expr(value_node)?;
                        let value = self.builder.create_expr(value);
                        self.builder
                            .create_global_var(name, ty, value, span)
                            .map_err(|_| RiddleError::Lowering("Failed to create global variable".to_string(), Some(span)))?;
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
                            .local(name, ty, value, span)
                            .map_err(|_| RiddleError::Lowering("Failed to create local variable".to_string(), Some(span)))?;
                    }
                }
                AstNodeKind::FuncDecl {
                    name,
                    generic_params,
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
                        None => HirTypeExpr::new(HirTypeExprKind::Unit, span),
                    };
                    let ret = self.builder.create_type_expr(ret);

                    let func_id = self
                        .builder
                        .create_func(name, generic_params.clone(), ret, p, span)
                        .map_err(|_| RiddleError::Lowering("Failed to create function".to_string(), Some(span)))?;

                    self.builder.set_func(func_id);
                    self.deep += 1;
                    self.lower_block(body)?;
                    self.deep -= 1;
                }
                AstNodeKind::StructDecl {
                    name,
                    generic_params,
                    fields,
                } => {
                    let mut hir_fields = Vec::new();
                    for field in fields {
                        let type_expr = self.lower_type_expr(&field.type_expr)?;
                        let span_f = field.type_expr.span; // Use field type span
                        let type_expr_id = self.builder.create_type_expr(type_expr);
                        hir_fields.push(HirStructField {
                            name: field.name.clone(),
                            type_expr: type_expr_id,
                            span: span_f,
                        });
                    }
                    self.builder
                        .create_struct(name, generic_params.clone(), hir_fields, span)
                        .map_err(|_| RiddleError::Lowering("Failed to create struct".to_string(), Some(span)))?;
                }
                AstNodeKind::EnumDecl {
                    name,
                    generic_params,
                    variants,
                } => {
                    let mut hir_variants = Vec::new();
                    for variant in variants {
                        match variant {
                            EnumVariant::Unit(name) => {
                                hir_variants.push(HirEnumVariant::Unit(name.clone()));
                            }
                            EnumVariant::Tuple(name, types) => {
                                let mut hir_types = Vec::new();
                                for ty in types {
                                    let type_expr = self.lower_type_expr(&Box::new(ty.clone()))?;
                                    let type_expr_id = self.builder.create_type_expr(type_expr);
                                    hir_types.push(type_expr_id);
                                }
                                hir_variants.push(HirEnumVariant::Tuple(name.clone(), hir_types));
                            }
                            EnumVariant::Struct(name, fields) => {
                                let mut hir_fields = Vec::new();
                                for field in fields {
                                    let type_expr = self.lower_type_expr(&field.type_expr)?;
                                    let span_f = field.type_expr.span;
                                    let type_expr_id = self.builder.create_type_expr(type_expr);
                                    hir_fields.push(HirStructField {
                                        name: field.name.clone(),
                                        type_expr: type_expr_id,
                                        span: span_f,
                                    });
                                }
                                hir_variants
                                    .push(HirEnumVariant::Struct(name.clone(), hir_fields));
                            }
                        }
                    }
                    self.builder
                        .create_enum(name, generic_params.clone(), hir_variants, span)
                        .map_err(|_| RiddleError::Lowering("Failed to create enum".to_string(), Some(span)))?;
                }
                AstNodeKind::TraitDecl {
                    name,
                    generic_params,
                    items,
                } => {
                    let mut hir_items = Vec::new();
                    for item in items {
                        // Lower params
                        let mut params = Vec::new();
                        for p in &item.params {
                            let type_expr = self.lower_type_expr(&p.type_expr)?;
                            let type_expr = self.builder.create_type_expr(type_expr);
                            let id = self.builder.next_local_id();
                            params.push(HirFuncParam {
                                name: p.name.clone(),
                                type_expr,
                                id,
                            });
                        }
                        // Lower return type
                        let ret_te = if let Some(ret) = &item.return_type {
                            self.lower_type_expr(ret)?
                        } else {
                            HirTypeExpr::new(HirTypeExprKind::Unit, span) // TODO: Better span
                        };
                        let ret = self.builder.create_type_expr(ret_te);
                        hir_items.push(HirTraitItem {
                            name: item.name.clone(),
                            generic_params: item.generic_params.clone(),
                            params,
                            ret,
                            span, // TODO: Better span
                        });
                    }
                    self.builder
                        .create_trait(name, generic_params.clone(), hir_items, span)
                        .map_err(|_| RiddleError::Lowering("Failed to create trait".to_string(), Some(span)))?;
                }
                AstNodeKind::ImplDecl {
                    generic_params,
                    trait_type,
                    target_type,
                    items,
                } => {
                    let trait_te = if let Some(tr) = trait_type {
                        let tr_le = self.lower_type_expr(tr)?;
                        Some(self.builder.create_type_expr(tr_le))
                    } else {
                        None
                    };
                    let target_le = self.lower_type_expr(target_type)?;
                    let target_te = self.builder.create_type_expr(target_le);

                    let mut methods = Vec::new();
                    for it in items {
                        let i_span = it.span;
                        if let AstNodeKind::FuncDecl {
                            name,
                            generic_params: method_generic_params,
                            params,
                            return_type,
                            body,
                        } = &it.kind
                        {
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
                                None => HirTypeExpr::new(HirTypeExprKind::Unit, i_span),
                            };
                            let ret = self.builder.create_type_expr(ret);

                            // TODO: mangled name for generic target type
                            let mangled_name = format!("impl::{}", name);
                            let func_id = self
                                .builder
                                .create_func(&mangled_name, method_generic_params.clone(), ret, p, i_span)
                                .map_err(|_| RiddleError::Lowering("Failed to create function".to_string(), Some(i_span)))?;

                            self.builder.set_func(func_id);
                            self.deep += 1;
                            self.lower_block(&body)?;
                            self.deep -= 1;

                            methods.push(func_id);
                        }
                    }
                    self.builder
                        .create_impl(generic_params.clone(), trait_te, target_te, methods, span)
                        .map_err(|_| RiddleError::Lowering("Failed to create impl".to_string(), Some(span)))?;
                }
                AstNodeKind::ExternBlock { abi, functions } => {
                    for func in functions {
                        self.lower_extern_func(abi.clone(), func, span)?;
                    }
                }
                AstNodeKind::ExternDecl { abi, function } => {
                    self.lower_extern_func(abi.clone(), function, span)?;
                }
                _ => {
                    return Err(RiddleError::Lowering("Expected statement".to_string(), Some(span)));
                }
            }
        }
        Ok(())
    }

    fn lower_extern_func(
        &mut self,
        abi: Option<String>,
        func: &crate::frontend::ast::ExternFunc,
        span: Span,
    ) -> Result<()> {
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
            None => HirTypeExpr::new(HirTypeExprKind::Unit, span),
        };
        let ret = self.builder.create_type_expr(ret);

        self.builder
            .create_extern_func(abi, &func.name, ret, p, func.is_variadic, span)
            .map_err(|_| RiddleError::Lowering("Failed to create extern function".to_string(), Some(span)))?;
        Ok(())
    }

    fn lower_type_expr(&mut self, node: &Box<AstNode>) -> Result<HirTypeExpr> {
        let span = node.span;
        match &node.kind {
            AstNodeKind::Identifier(name) => Ok(HirTypeExpr::new(HirTypeExprKind::Path(name.clone()), span)),
            AstNodeKind::Path(parts) => Ok(HirTypeExpr::new(HirTypeExprKind::Path(parts.join("::")), span)),
            AstNodeKind::TypeExpr { path, args } => {
                let name = path.join("::");
                if args.is_empty() {
                    Ok(HirTypeExpr::new(HirTypeExprKind::Path(name), span))
                } else {
                    let mut hir_args = Vec::new();
                    for arg in args {
                        let arg_te = self.lower_type_expr(&Box::new(arg.clone()))?;
                        hir_args.push(self.builder.create_type_expr(arg_te));
                    }
                    Ok(HirTypeExpr::new(HirTypeExprKind::Generic(name, hir_args), span))
                }
            }
            AstNodeKind::RefType(inner) => {
                let inner_te = self.lower_type_expr(inner)?;
                let inner_te_id = self.builder.create_type_expr(inner_te);
                Ok(HirTypeExpr::new(HirTypeExprKind::Ref(inner_te_id), span))
            }
            _ => Err(RiddleError::Lowering(format!("Unexpected node in type expr: {:?}", node), Some(span))),
        }
    }

    fn lower_optional_type_expr(
        &mut self,
        type_expr: &Option<Box<AstNode>>,
    ) -> Result<Option<TyExprId>> {
        match type_expr {
            Some(ty) => {
                let ty_hir = self.lower_type_expr(ty)?;
                let ty_id = self.builder.create_type_expr(ty_hir);
                Ok(Some(ty_id))
            }
            None => Ok(None),
        }
    }

    fn lower_block(&mut self, node: &AstNode) -> Result<()> {
        let span = node.span;
        match &node.kind {
            AstNodeKind::Block {
                statements,
                final_expr,
            } => {
                for stmt in statements {
                    self.lower_stmt(stmt)?;
                }
                if let Some(expr) = final_expr {
                    let expr_span = expr.span;
                    let expr_hir = self.lower_expr(expr)?;
                    let expr_id = self.builder.create_expr(expr_hir);
                    self.builder
                        .expr_stmt(expr_id, false, expr_span)
                        .map_err(|_| RiddleError::Lowering("Failed to create expr stmt".to_string(), Some(expr_span)))?;
                }
                Ok(())
            }
            _ => Err(RiddleError::Lowering("Expected block".to_string(), Some(span))),
        }
    }

    fn lower_stmt(&mut self, node: &AstNode) -> Result<()> {
        let span = node.span;
        match &node.kind {
            AstNodeKind::VarDecl {
                name,
                type_expr,
                value,
            } => {
                let ty = match type_expr {
                    Some(ty) => {
                        let ty_hir = self.lower_type_expr(ty)?;
                        let ty_id = self.builder.create_type_expr(ty_hir);
                        Some(ty_id)
                    }
                    None => None,
                };
                let value = match value {
                    Some(v) => {
                        let v_hir = self.lower_expr(v)?;
                        Some(self.builder.create_expr(v_hir))
                    }
                    None => None,
                };
                self.builder
                    .local(name, ty, value, span)
                    .map_err(|_| RiddleError::Lowering("Failed to create local variable".to_string(), Some(span)))?;
            }
            AstNodeKind::Return(val) => {
                let expr = self.lower_expr(val)?;
                let expr_id = self.builder.create_expr(expr);
                self.builder
                    .ret(expr_id, span)
                    .map_err(|_| RiddleError::Lowering("Failed to create return stmt".to_string(), Some(span)))?;
            }
            AstNodeKind::Block { .. } => {
                self.lower_block(node)?;
            }
            _ => {
                let expr = self.lower_expr(node)?;
                let expr_id = self.builder.create_expr(expr);
                self.builder
                    .expr_stmt(expr_id, true, span)
                    .map_err(|_| RiddleError::Lowering("Failed to create expr stmt".to_string(), Some(span)))?;
            }
        }
        Ok(())
    }

    fn lower_expr(&mut self, node: &AstNode) -> Result<HirExpr> {
        let span = node.span;
        match &node.kind {
            AstNodeKind::Literal(lit) => match lit {
                Literal::Int(val) => Ok(HirExpr::new(HirExprKind::Literal(HirLiteral::Int(*val)), span)),
                Literal::Bool(val) => {
                    Ok(HirExpr::new(HirExprKind::Literal(HirLiteral::Bool(*val)), span))
                }
                Literal::Float(val) => {
                    Ok(HirExpr::new(HirExprKind::Literal(HirLiteral::Float(*val)), span))
                }
                Literal::Str(val) => Ok(HirExpr::new(HirExprKind::Literal(HirLiteral::Str(
                    val.clone(),
                )), span)),
            },
            AstNodeKind::BinaryExpr { lhs, op, rhs } => {
                let lhs_hir = self.lower_expr(lhs)?;
                let lhs_id = self.builder.create_expr(lhs_hir);
                let rhs_hir = self.lower_expr(rhs)?;
                let rhs_id = self.builder.create_expr(rhs_hir);
                Ok(HirExpr::new(HirExprKind::BinaryOp {
                    lhs: lhs_id,
                    op: op.clone(),
                    rhs: rhs_id,
                }, span))
            }
            AstNodeKind::Identifier(name) => Ok(HirExpr::new(HirExprKind::Symbol {
                name: name.clone(),
                id: None,
                def_id: None,
            }, span)),
            AstNodeKind::Path(parts) => {
                let name = parts.join("::");
                Ok(HirExpr::new(HirExprKind::Symbol {
                    name,
                    id: None,
                    def_id: None,
                }, span))
            }
            AstNodeKind::Call { func, args } => {
                let callee_hir = self.lower_expr(func)?;
                let callee_id = self.builder.create_expr(callee_hir);
                let mut arg_ids = Vec::new();
                for arg in args {
                    let arg_expr = self.lower_expr(arg)?;
                    let arg_id = self.builder.create_expr(arg_expr);
                    arg_ids.push(arg_id);
                }
                Ok(HirExpr::new(HirExprKind::Call {
                    callee: callee_id,
                    args: arg_ids,
                }, span))
            }
            AstNodeKind::StructInst { name, fields } => {
                let mut hir_fields = Vec::new();
                for (f_name, f_expr) in fields {
                    let f_expr_hir = self.lower_expr(f_expr)?;
                    let f_expr_id = self.builder.create_expr(f_expr_hir);
                    hir_fields.push((f_name.clone(), f_expr_id));
                }
                Ok(HirExpr::new(HirExprKind::StructInst {
                    struct_name: name.clone(),
                    fields: hir_fields,
                }, span))
            }
            AstNodeKind::MemberAccess { object, member } => {
                let object_hir = self.lower_expr(object)?;
                let object_id = self.builder.create_expr(object_hir);
                Ok(HirExpr::new(HirExprKind::MemberAccess {
                    object: object_id,
                    member: member.clone(),
                    id: None,
                }, span))
            }
            _ => Err(RiddleError::Lowering(format!("Unsupported expression: {:?}", node), Some(span))),
        }
    }
}
