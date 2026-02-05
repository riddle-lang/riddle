use crate::hir::module::HirModule;
use crate::hir::types::{HirType, InferTy};
use crate::hir::id::{TyId, ExprId, StmtId, LocalId, TyExprId};
use crate::hir::expr::{HirExprKind, HirLiteral};
use crate::hir::stmt::HirStmtKind;
use crate::hir::items::{HirItem, HirEnumVariant};
use crate::hir::type_expr::HirTypeExprKind;
use std::collections::HashMap;

pub struct TypeInfer<'a> {
    module: &'a mut HirModule,
    /// Mapping from InferTy::Id to the actual type
    constraints: HashMap<usize, TyId>,
    /// Mapping from LocalId to its TyId
    locals: HashMap<LocalId, TyId>,
    /// Mapping from Item index to its TyId
    item_types: HashMap<usize, TyId>,
    current_ret_ty: Option<TyId>,
    next_infer_id: usize,
}

impl<'a> TypeInfer<'a> {
    pub fn new(module: &'a mut HirModule) -> Self {
        Self {
            module,
            constraints: HashMap::new(),
            locals: HashMap::new(),
            item_types: HashMap::new(),
            current_ret_ty: None,
            next_infer_id: 0,
        }
    }

    pub fn infer(&mut self) -> Result<(), String> {
        let item_count = self.module.items.len();
        
        // 1. Collect signatures
        for i in 0..item_count {
            let item = self.module.items[i].clone();
            match item {
                HirItem::Func(func) => {
                    let ret_ty = self.resolve_type_expr(func.ret)?;
                    let mut sig = vec![ret_ty];
                    for param in &func.param {
                        sig.push(self.resolve_type_expr(param.type_expr)?);
                    }
                    let ty = self.get_or_create_type(HirType::Func(sig));
                    self.item_types.insert(i, ty);
                }
                HirItem::ExternFunc(func) => {
                    let ret_ty = self.resolve_type_expr(func.ret)?;
                    let mut sig = vec![ret_ty];
                    for param in &func.param {
                        sig.push(self.resolve_type_expr(param.type_expr)?);
                    }
                    let ty = self.get_or_create_type(HirType::Func(sig));
                    self.item_types.insert(i, ty);
                }
                HirItem::GlobalVariable(gv) => {
                    let ty = if let Some(ty_expr) = gv.ty {
                        self.resolve_type_expr(ty_expr)?
                    } else {
                        self.new_infer_ty()
                    };
                    self.item_types.insert(i, ty);
                }
                _ => {}
            }
        }

        // 2. Infer bodies
        for i in 0..item_count {
            let item = self.module.items[i].clone();
            match item {
                HirItem::Func(func) => {
                    self.infer_function(func)?;
                }
                HirItem::GlobalVariable(gv) => {
                    let ty = self.item_types[&i];
                    let value_ty = self.infer_expr(gv.value)?;
                    self.unify(ty, value_ty)?;
                }
                _ => {}
            }
        }

        // 3. Finalize types
        self.finalize_types();
        Ok(())
    }

    fn finalize_types(&mut self) {
        let expr_count = self.module.exprs.len();
        for i in 0..expr_count {
            if let Some(ty_id) = self.module.exprs[i].ty {
                self.module.exprs[i].ty = Some(self.follow_id(ty_id));
            }
        }
        let te_count = self.module.type_exprs.len();
        for i in 0..te_count {
            if let Some(ty_id) = self.module.type_exprs[i].curr_ty {
                self.module.type_exprs[i].curr_ty = Some(self.follow_id(ty_id));
            }
        }
    }

    fn infer_function(&mut self, func: crate::hir::items::HirFunc) -> Result<(), String> {
        self.locals.clear();
        
        // Resolve parameter types
        for param in &func.param {
            let ty = self.resolve_type_expr(param.type_expr)?;
            self.locals.insert(param.id, ty);
        }

        // Resolve return type
        let ret_ty = self.resolve_type_expr(func.ret)?;
        self.current_ret_ty = Some(ret_ty);

        for stmt_id in &func.body {
            self.infer_stmt(*stmt_id)?;
        }

        self.current_ret_ty = None;
        Ok(())
    }

    fn infer_stmt(&mut self, stmt_id: StmtId) -> Result<(), String> {
        let stmt = self.module.stmts[stmt_id.0].clone();
        match stmt.kind {
            HirStmtKind::Let { id, ty_annot, init, .. } => {
                let annot_ty = if let Some(annot) = ty_annot {
                    Some(self.resolve_type_expr(annot)?)
                } else {
                    None
                };

                let init_ty = if let Some(init_expr_id) = init {
                    let ty = self.infer_expr(init_expr_id)?;
                    if let Some(aty) = annot_ty {
                        self.unify(aty, ty)?;
                    }
                    ty
                } else {
                    annot_ty.unwrap_or_else(|| self.new_infer_ty())
                };

                self.locals.insert(id, init_ty);
            }
            HirStmtKind::Expr { expr, .. } => {
                self.infer_expr(expr)?;
            }
            HirStmtKind::Return { value } => {
                let ret_ty = if let Some(expr_id) = value {
                    self.infer_expr(expr_id)?
                } else {
                    self.get_or_create_type(HirType::Unit)
                };
                
                if let Some(expected) = self.current_ret_ty {
                    self.unify(expected, ret_ty)?;
                }
            }
        }
        Ok(())
    }

    fn infer_expr(&mut self, expr_id: ExprId) -> Result<TyId, String> {
        let expr = self.module.exprs[expr_id.0].clone();
        let ty = match expr.kind {
            HirExprKind::Literal(lit) => match lit {
                HirLiteral::Int(_) => self.get_or_create_type(HirType::Int(32)),
                HirLiteral::Float(_) => self.get_or_create_type(HirType::Float), 
                HirLiteral::Bool(_) => self.get_or_create_type(HirType::Bool),
                HirLiteral::Str(_) => self.get_or_create_type(HirType::Unknown),
            },
            HirExprKind::BinaryOp { lhs, op, rhs } => {
                let lhs_ty = self.infer_expr(lhs)?;
                let rhs_ty = self.infer_expr(rhs)?;
                self.unify(lhs_ty, rhs_ty)?;
                if op == "==" || op == "!=" || op == "<" || op == ">" || op == "<=" || op == ">=" {
                    self.get_or_create_type(HirType::Bool)
                } else {
                    lhs_ty
                }
            }
            HirExprKind::Symbol { id, name } => {
                if let Some(local_id) = id {
                    if let Some(ty) = self.locals.get(&local_id) {
                        *ty
                    } else {
                        self.new_infer_ty()
                    }
                } else {
                    // Try to find in global items
                    let mut found_ty = None;
                    for i in 0..self.module.items.len() {
                        let item_name = match &self.module.items[i] {
                            HirItem::Func(f) => &f.name,
                            HirItem::ExternFunc(f) => &f.name,
                            HirItem::GlobalVariable(gv) => &gv.name,
                            _ => continue,
                        };
                        if item_name == &name {
                            found_ty = self.item_types.get(&i).copied();
                            break;
                        }
                    }

                    if found_ty.is_none() && name.contains("::") {
                        let parts: Vec<&str> = name.split("::").collect();
                        if parts.len() == 2 {
                            let enum_name = parts[0];
                            let variant_name = parts[1];
                            let mut found_variant = None;
                            for (i, item) in self.module.items.iter().enumerate() {
                                if let HirItem::Enum(e) = item {
                                    if e.name == enum_name {
                                        for variant in &e.variants {
                                            match variant {
                                                HirEnumVariant::Unit(v_name) if v_name == variant_name => {
                                                    found_variant = Some((i, variant.clone()));
                                                    break;
                                                }
                                                HirEnumVariant::Tuple(v_name, _) if v_name == variant_name => {
                                                    found_variant = Some((i, variant.clone()));
                                                    break;
                                                }
                                                HirEnumVariant::Struct(v_name, _) if v_name == variant_name => {
                                                    found_variant = Some((i, variant.clone()));
                                                    break;
                                                }
                                                _ => {}
                                            }
                                        }
                                    }
                                }
                                if found_variant.is_some() {
                                    break;
                                }
                            }

                            if let Some((enum_idx, variant)) = found_variant {
                                match variant {
                                    HirEnumVariant::Unit(_) => {
                                        found_ty = Some(self.get_or_create_type(HirType::Enum(crate::hir::id::DefId(enum_idx))));
                                    }
                                    HirEnumVariant::Tuple(_, params) => {
                                        let mut sig = Vec::new();
                                        let ret_ty = self.get_or_create_type(HirType::Enum(crate::hir::id::DefId(enum_idx)));
                                        sig.push(ret_ty);
                                        for p in params {
                                            sig.push(self.resolve_type_expr(p)?);
                                        }
                                        found_ty = Some(self.get_or_create_type(HirType::Func(sig)));
                                    }
                                    HirEnumVariant::Struct(_, fields) => {
                                        let mut sig = Vec::new();
                                        let ret_ty = self.get_or_create_type(HirType::Enum(crate::hir::id::DefId(enum_idx)));
                                        sig.push(ret_ty);
                                        for f in fields {
                                            sig.push(self.resolve_type_expr(f.type_expr)?);
                                        }
                                        found_ty = Some(self.get_or_create_type(HirType::Func(sig)));
                                    }
                                }
                            }
                        }
                    }

                    found_ty.unwrap_or_else(|| self.new_infer_ty())
                }
            }
            HirExprKind::Call { callee, args } => {
                let callee_ty = self.infer_expr(callee)?;
                let mut arg_tys = Vec::new();
                for arg in args {
                    arg_tys.push(self.infer_expr(arg)?);
                }
                
                let ret_ty = self.new_infer_ty();
                let mut func_sig = vec![ret_ty];
                func_sig.extend(arg_tys);
                let func_ty = self.get_or_create_type(HirType::Func(func_sig));
                
                self.unify(callee_ty, func_ty)?;
                ret_ty
            }
            HirExprKind::Block { stmts } => {
                let last_ty = self.get_or_create_type(HirType::Unit);
                for stmt_id in stmts {
                    self.infer_stmt(stmt_id)?;
                }
                last_ty
            }
            HirExprKind::StructInst { ref struct_name, ref fields } => {
                let mut struct_def = None;
                let mut def_id = None;
                for (i, item) in self.module.items.iter().enumerate() {
                    if let HirItem::Struct(s) = item {
                        if &s.name == struct_name {
                            struct_def = Some(s.clone());
                            def_id = Some(crate::hir::id::DefId(i));
                            break;
                        }
                    }
                }

                if let (Some(s), Some(did)) = (struct_def, def_id) {
                    for (f_name, f_expr_id) in fields {
                        let f_ty = self.infer_expr(*f_expr_id)?;
                        let mut found = false;
                        for s_field in &s.fields {
                            if &s_field.name == f_name {
                                let expected_f_ty = self.resolve_type_expr(s_field.type_expr)?;
                                self.unify(f_ty, expected_f_ty)?;
                                found = true;
                                break;
                            }
                        }
                        if !found {
                            return Err(format!("Field {} not found in struct {}", f_name, struct_name));
                        }
                    }
                    self.get_or_create_type(HirType::Struct(did))
                } else {
                    return Err(format!("Struct {} not found", struct_name));
                }
            }
            HirExprKind::MemberAccess { object, ref member, .. } => {
                let obj_ty = self.infer_expr(object)?;
                let obj_ty_followed = self.follow_id(obj_ty);
                let obj_hir_ty = self.module.types[obj_ty_followed.0].clone();
                let mut resolved_id = None;
                let ty = match obj_hir_ty {
                    HirType::Struct(def_id) => {
                        let mut member_ty = None;
                        let (s_name, s_fields) = if let HirItem::Struct(s) = &self.module.items[def_id.0] {
                            (s.name.clone(), s.fields.clone())
                        } else {
                            return Err("Expected struct item".to_string());
                        };

                        // 1. Check fields
                        for field in &s_fields {
                            if &field.name == member {
                                member_ty = Some(self.resolve_type_expr(field.type_expr)?);
                                break;
                            }
                        }

                        if member_ty.is_none() {
                            // 2. Check impls
                            for i in 0..self.module.items.len() {
                                if let HirItem::Impl(im) = &self.module.items[i] {
                                    if im.target_name == s_name {
                                        for method_def_id in &im.items {
                                            if let HirItem::Func(f) = &self.module.items[method_def_id.0] {
                                                if f.name == format!("{}::{}", s_name, member) {
                                                    member_ty = self.item_types.get(&method_def_id.0).copied();
                                                    resolved_id = Some(*method_def_id);
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                }
                                if member_ty.is_some() {
                                    break;
                                }
                            }
                        }
                        member_ty.ok_or_else(|| format!("Member {} not found on struct {}", member, s_name))?
                    }
                    _ => self.new_infer_ty(),
                };
                if let HirExprKind::MemberAccess { id, .. } = &mut self.module.exprs[expr_id.0].kind {
                    *id = resolved_id;
                }
                ty
            }
        };
        
        self.module.exprs[expr_id.0].ty = Some(ty);
        Ok(ty)
    }

    fn resolve_type_expr(&mut self, ty_expr_id: TyExprId) -> Result<TyId, String> {
        let ty_expr = self.module.type_exprs[ty_expr_id.0].clone();
        let ty_id = match ty_expr.kind {
            HirTypeExprKind::Unit => self.get_or_create_type(HirType::Unit),
            HirTypeExprKind::Path(name) => {
                if name == "int" {
                    self.get_or_create_type(HirType::Int(32))
                } else if name == "bool" {
                    self.get_or_create_type(HirType::Bool)
                } else {
                    // 查找结构体或枚举
                    let mut def_id = None;
                    let mut is_enum = false;
                    for (i, item) in self.module.items.iter().enumerate() {
                        match item {
                            HirItem::Struct(s) if s.name == name => {
                                def_id = Some(crate::hir::id::DefId(i));
                                break;
                            }
                            HirItem::Enum(e) if e.name == name => {
                                def_id = Some(crate::hir::id::DefId(i));
                                is_enum = true;
                                break;
                            }
                            _ => {}
                        }
                    }

                    if let Some(id) = def_id {
                        if is_enum {
                            self.get_or_create_type(HirType::Enum(id))
                        } else {
                            self.get_or_create_type(HirType::Struct(id))
                        }
                    } else {
                        self.get_or_create_type(HirType::Unknown)
                    }
                }
            }
            HirTypeExprKind::Func(params, ret) => {
                let ret_ty = self.resolve_type_expr(ret)?;
                let mut sig = vec![ret_ty];
                for p in params {
                    sig.push(self.resolve_type_expr(p)?);
                }
                self.get_or_create_type(HirType::Func(sig))
            }
        };
        self.module.type_exprs[ty_expr_id.0].curr_ty = Some(ty_id);
        Ok(ty_id)
    }

    fn new_infer_ty(&mut self) -> TyId {
        let id = self.next_infer_id;
        self.next_infer_id += 1;
        self.get_or_create_type(HirType::Infer(InferTy::Id(id)))
    }

    fn get_or_create_type(&mut self, ty: HirType) -> TyId {
        for (i, existing) in self.module.types.iter().enumerate() {
            if *existing == ty {
                return TyId(i);
            }
        }
        let id = TyId(self.module.types.len());
        self.module.types.push(ty);
        id
    }

    fn unify(&mut self, t1: TyId, t2: TyId) -> Result<(), String> {
        let t1 = self.follow_id(t1);
        let t2 = self.follow_id(t2);

        if t1 == t2 {
            return Ok(());
        }

        let ty1 = self.module.types[t1.0].clone();
        let ty2 = self.module.types[t2.0].clone();

        match (ty1, ty2) {
            (HirType::Infer(InferTy::Id(id)), _) => {
                self.constraints.insert(id, t2);
                Ok(())
            }
            (_, HirType::Infer(InferTy::Id(id))) => {
                self.constraints.insert(id, t1);
                Ok(())
            }
            (HirType::Func(f1), HirType::Func(f2)) => {
                if f1.len() != f2.len() {
                    return Err(format!("Arity mismatch: {} vs {}", f1.len(), f2.len()));
                }
                for (a, b) in f1.into_iter().zip(f2.into_iter()) {
                    self.unify(a, b)?;
                }
                Ok(())
            }
            (a, b) if a == b => Ok(()),
            (a, b) => Err(format!("Cannot unify {:?} and {:?}", a, b)),
        }
    }

    fn follow_id(&self, mut id: TyId) -> TyId {
        while let HirType::Infer(InferTy::Id(infer_id)) = &self.module.types[id.0] {
            if let Some(target) = self.constraints.get(infer_id) {
                id = *target;
            } else {
                break;
            }
        }
        id
    }
}
