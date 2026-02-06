use crate::frontend::ast::{AstNode, EnumVariant, Literal};
use crate::hir::builder::HirBuilder;
use crate::hir::expr::{HirExpr, HirExprKind, HirLiteral};
use crate::hir::id::TyExprId;
use crate::hir::items::{HirEnumVariant, HirFuncParam, HirStructField, HirTraitItem};
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
                        None => HirTypeExpr::new(HirTypeExprKind::Unit),
                    };
                    let ret = self.builder.create_type_expr(ret);

                    let func_id = self
                        .builder
                        .create_func(name, generic_params.clone(), ret, p)
                        .map_err(|_| "Failed to create function".to_string())?;

                    self.builder.set_func(func_id);
                    self.deep += 1;
                    self.lower_block(body)?;
                    self.deep -= 1;
                }
                AstNode::StructDecl {
                    name,
                    generic_params,
                    fields,
                } => {
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
                        .create_struct(name, generic_params.clone(), hir_fields)
                        .map_err(|_| "Failed to create struct".to_string())?;
                }
                AstNode::EnumDecl {
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
                                    let type_expr_id = self.builder.create_type_expr(type_expr);
                                    hir_fields.push(HirStructField {
                                        name: field.name.clone(),
                                        type_expr: type_expr_id,
                                    });
                                }
                                hir_variants
                                    .push(HirEnumVariant::Struct(name.clone(), hir_fields));
                            }
                        }
                    }
                    self.builder
                        .create_enum(name, generic_params.clone(), hir_variants)
                        .map_err(|_| "Failed to create enum".to_string())?;
                }
                AstNode::TraitDecl {
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
                            HirTypeExpr::new(HirTypeExprKind::Unit)
                        };
                        let ret = self.builder.create_type_expr(ret_te);
                        hir_items.push(HirTraitItem {
                            name: item.name.clone(),
                            generic_params: item.generic_params.clone(),
                            params,
                            ret,
                        });
                    }
                    self.builder
                        .create_trait(name, generic_params.clone(), hir_items)
                        .map_err(|_| "Failed to create trait".to_string())?;
                }
                AstNode::ImplDecl {
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
                        if let AstNode::FuncDecl {
                            name,
                            generic_params: method_generic_params,
                            params,
                            return_type,
                            body,
                        } = it
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
                                None => HirTypeExpr::new(HirTypeExprKind::Unit),
                            };
                            let ret = self.builder.create_type_expr(ret);

                            // TODO: mangled name for generic target type
                            let mangled_name = format!("impl::{}", name);
                            let func_id = self
                                .builder
                                .create_func(&mangled_name, method_generic_params.clone(), ret, p)
                                .map_err(|_| "Failed to create function".to_string())?;

                            self.builder.set_func(func_id);
                            self.deep += 1;
                            self.lower_block(&body)?;
                            self.deep -= 1;

                            methods.push(func_id);
                        }
                    }
                    self.builder
                        .create_impl(generic_params.clone(), trait_te, target_te, methods)
                        .map_err(|_| "Failed to create impl".to_string())?;
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
            .create_extern_func(abi, &func.name, ret, p, func.is_variadic)
            .map_err(|_| "Failed to create extern function".to_string())?;
        Ok(())
    }

    fn lower_type_expr(&mut self, type_expr: &Box<AstNode>) -> Result<HirTypeExpr, String> {
        match type_expr.as_ref() {
            AstNode::Identifier(name) => Ok(HirTypeExpr::new(HirTypeExprKind::Path(name.clone()))),
            AstNode::Path(parts) => Ok(HirTypeExpr::new(HirTypeExprKind::Path(parts.join("::")))),
            AstNode::TypeExpr { path, args } => {
                let name = path.join("::");
                if args.is_empty() {
                    Ok(HirTypeExpr::new(HirTypeExprKind::Path(name)))
                } else {
                    let mut hir_args = Vec::new();
                    for arg in args {
                        let arg_te = self.lower_type_expr(&Box::new(arg.clone()))?;
                        hir_args.push(self.builder.create_type_expr(arg_te));
                    }
                    Ok(HirTypeExpr::new(HirTypeExprKind::Generic(name, hir_args)))
                }
            }
            AstNode::RefType(inner) => {
                let inner_te = self.lower_type_expr(inner)?;
                let inner_te_id = self.builder.create_type_expr(inner_te);
                Ok(HirTypeExpr::new(HirTypeExprKind::Ref(inner_te_id)))
            }
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
                Literal::Bool(val) => {
                    Ok(HirExpr::new(HirExprKind::Literal(HirLiteral::Bool(*val))))
                }
                Literal::Float(val) => {
                    Ok(HirExpr::new(HirExprKind::Literal(HirLiteral::Float(*val))))
                }
                Literal::Str(val) => Ok(HirExpr::new(HirExprKind::Literal(HirLiteral::Str(
                    val.clone(),
                )))),
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
                def_id: None,
            })),
            AstNode::Path(parts) => {
                let name = parts.join("::");
                Ok(HirExpr::new(HirExprKind::Symbol {
                    name,
                    id: None,
                    def_id: None,
                }))
            }
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
            AstNode::MemberAccess { object, member } => {
                let object = self.lower_expr(object)?;
                let object = self.builder.create_expr(object);
                Ok(HirExpr::new(HirExprKind::MemberAccess {
                    object,
                    member: member.clone(),
                    id: None,
                }))
            }
            _ => todo!(),
        }
    }
}
