use crate::error::{Result, RiddleError, Span};
use crate::hir::expr::{HirExprKind, HirLiteral};
use crate::hir::id::{ExprId, LocalId, StmtId, TyExprId, TyId};
use crate::hir::items::{HirEnumVariant, HirItem};
use crate::hir::module::HirModule;
use crate::hir::stmt::HirStmtKind;
use crate::hir::type_expr::HirTypeExprKind;
use crate::hir::types::{HirType, InferTy};
use std::collections::HashMap;

pub struct TypeInfer<'a> {
    module: &'a mut HirModule,
    /// Mapping from InferTy::ID to the actual type
    constraints: HashMap<usize, TyId>,
    /// Mapping from LocalId to its TyId
    locals: HashMap<LocalId, TyId>,
    /// Mapping from type parameter name to its TyId
    type_params: HashMap<String, TyId>,
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
            type_params: HashMap::new(),
            item_types: HashMap::new(),
            current_ret_ty: None,
            next_infer_id: 0,
        }
    }

    pub fn infer(&mut self) -> Result<()> {
        let item_count = self.module.items.len();

        // 1. Collect signatures
        for i in 0..item_count {
            let item = self.module.items[i].clone();
            match item {
                HirItem::Func(func) => {
                    self.type_params.clear();
                    for tp in &func.generic_params {
                        let ty = self.get_or_create_type(HirType::GenericParam(tp.clone()));
                        self.type_params.insert(tp.clone(), ty);
                    }
                    let ret_ty = self.resolve_type_expr(func.ret)?;
                    let mut sig = vec![ret_ty];
                    for param in &func.param {
                        sig.push(self.resolve_type_expr(param.type_expr)?);
                    }
                    let ty = self.get_or_create_type(HirType::Func(sig, false));
                    self.item_types.insert(i, ty);
                }
                HirItem::ExternFunc(func) => {
                    let ret_ty = self.resolve_type_expr(func.ret)?;
                    let mut sig = vec![ret_ty];
                    for param in &func.param {
                        sig.push(self.resolve_type_expr(param.type_expr)?);
                    }
                    let ty = self.get_or_create_type(HirType::Func(sig, func.is_variadic));
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
                    self.unify(ty, value_ty, Some(gv.span))?;
                }
                _ => {}
            }
        }

        // 3. Finalize types
        self.finalize_types();

        for (idx, ty) in self.item_types.iter() {
            let ty = self.follow_id(*ty);
            self.module
                .item_types
                .insert(crate::hir::id::DefId(*idx), ty);
        }
        Ok(())
    }

    pub fn finalize_types(&mut self) {
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

    fn infer_function(&mut self, func: crate::hir::items::HirFunc) -> Result<()> {
        self.locals.clear();
        self.type_params.clear();

        for tp in &func.generic_params {
            let ty = self.get_or_create_type(HirType::GenericParam(tp.clone()));
            self.type_params.insert(tp.clone(), ty);
        }

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

    fn infer_stmt(&mut self, stmt_id: StmtId) -> Result<()> {
        let stmt = self.module.stmts[stmt_id.0].clone();
        match stmt.kind {
            HirStmtKind::Let {
                id, ty_annot, init, ..
            } => {
                let annot_ty = if let Some(annot) = ty_annot {
                    Some(self.resolve_type_expr(annot)?)
                } else {
                    None
                };

                let init_ty = if let Some(init_expr_id) = init {
                    let ty = self.infer_expr(init_expr_id)?;
                    if let Some(aty) = annot_ty {
                        let s = self.module.stmts[stmt_id.0].span;
                        self.unify(aty, ty, Some(s))?;
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
            HirStmtKind::If {
                cond,
                then_block,
                else_block,
            } => {
                let cond_ty = self.infer_expr(cond)?;
                let bool_ty = self.get_or_create_type(HirType::Bool);
                let span = self.module.exprs[cond.0].span;
                self.unify(cond_ty, bool_ty, Some(span))?;
                self.infer_stmt(then_block)?;
                if let Some(eb) = else_block {
                    self.infer_stmt(eb)?;
                }
            }
            HirStmtKind::Return { value } => {
                let ret_ty = if let Some(expr_id) = value {
                    self.infer_expr(expr_id)?
                } else {
                    self.get_or_create_type(HirType::Unit)
                };

                if let Some(expected) = self.current_ret_ty {
                    let s = self.module.stmts[stmt_id.0].span;
                    self.unify(expected, ret_ty, Some(s))?;
                }
            }
        }
        Ok(())
    }

    fn infer_expr(&mut self, expr_id: ExprId) -> Result<TyId> {
        let expr = self.module.exprs[expr_id.0].clone();
        let ty = match expr.kind {
            HirExprKind::Literal(lit) => match lit {
                HirLiteral::Int(_) => self.get_or_create_type(HirType::Int(64)),
                HirLiteral::Float(_) => self.get_or_create_type(HirType::Float),
                HirLiteral::Bool(_) => self.get_or_create_type(HirType::Bool),
                HirLiteral::Str(_) => self.get_or_create_type(HirType::Str),
                HirLiteral::CStr(_) => self.get_or_create_type(HirType::CStr),
                HirLiteral::CInt(_) => self.get_or_create_type(HirType::CInt),
            },
            HirExprKind::Cast { expr, target_ty_expr } => {
                let _expr_ty = self.infer_expr(expr)?;
                self.resolve_type_expr(target_ty_expr)?
            }
            HirExprKind::BinaryOp { lhs, op, rhs } => {
                let lhs_ty = self.infer_expr(lhs)?;
                let rhs_ty = self.infer_expr(rhs)?;
                let s = self.module.exprs[expr_id.0].span;
                self.unify(lhs_ty, rhs_ty, Some(s))?;
                if op == "==" || op == "!=" || op == "<" || op == ">" || op == "<=" || op == ">=" {
                    self.get_or_create_type(HirType::Bool)
                } else {
                    lhs_ty
                }
            }
            HirExprKind::UnaryOp { op, expr } => {
                let e_ty = self.infer_expr(expr)?;
                let s = self.module.exprs[expr_id.0].span;
                match op.as_str() {
                    "&" => self.get_or_create_type(HirType::Pointer(e_ty)),
                    "*" => {
                        let inner_ty = self.new_infer_ty();
                        let ptr_ty = self.get_or_create_type(HirType::Pointer(inner_ty));
                        self.unify(e_ty, ptr_ty, Some(s))?;
                        inner_ty
                    }
                    "-" => e_ty,
                    "!" => {
                        let bool_ty = self.get_or_create_type(HirType::Bool);
                        self.unify(e_ty, bool_ty, Some(s))?;
                        bool_ty
                    }
                    _ => self.new_infer_ty(),
                }
            }
            HirExprKind::ListLiteral(elements) => {
                let elem_ty = self.new_infer_ty();
                let len = elements.len();
                for elem_id in elements {
                    let ty = self.infer_expr(elem_id)?;
                    let s = self.module.exprs[elem_id.0].span;
                    self.unify(elem_ty, ty, Some(s))?;
                }
                self.get_or_create_type(HirType::Array(elem_ty, len))
            }
            HirExprKind::Symbol { id, ref name, .. } => {
                if let Some(local_id) = id {
                    if let Some(ty) = self.locals.get(&local_id) {
                        *ty
                    } else {
                        self.new_infer_ty()
                    }
                } else {
                    // Try to find in global items
                    let mut found_ty = None;
                    if found_ty.is_none() {
                        for i in 0..self.module.items.len() {
                            let (item_name, item_generic_params) = match &self.module.items[i] {
                                HirItem::Func(f) => (f.name.clone(), f.generic_params.clone()),
                                HirItem::ExternFunc(f) => (f.name.clone(), vec![]),
                                HirItem::GlobalVariable(gv) => (gv.name.clone(), vec![]),
                                _ => continue,
                            };
                            if item_name == *name {
                                let base_ty = self.item_types.get(&i).copied().unwrap();
                                found_ty = Some(self.instantiate(base_ty, &item_generic_params));
                                if let HirExprKind::Symbol { def_id, .. } =
                                    &mut self.module.exprs[expr_id.0].kind
                                {
                                    *def_id = Some(crate::hir::id::DefId(i));
                                }
                                break;
                            }
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
                                                HirEnumVariant::Unit(v_name)
                                                    if v_name == variant_name =>
                                                {
                                                    found_variant = Some((i, variant.clone()));
                                                    break;
                                                }
                                                HirEnumVariant::Tuple(v_name, _)
                                                    if v_name == variant_name =>
                                                {
                                                    found_variant = Some((i, variant.clone()));
                                                    break;
                                                }
                                                HirEnumVariant::Struct(v_name, _)
                                                    if v_name == variant_name =>
                                                {
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
                                if let HirExprKind::Symbol { def_id, .. } =
                                    &mut self.module.exprs[expr_id.0].kind
                                {
                                    *def_id = Some(crate::hir::id::DefId(enum_idx));
                                }
                                let mut enum_type_params = HashMap::new();
                                let generic_params =
                                    if let HirItem::Enum(e) = &self.module.items[enum_idx] {
                                        e.generic_params.clone()
                                    } else {
                                        vec![]
                                    };
                                let generic_params_clone_2 = generic_params.clone();
                                for tp in generic_params {
                                    let ty =
                                        self.get_or_create_type(HirType::GenericParam(tp.clone()));
                                    enum_type_params.insert(tp, ty);
                                }
                                let old_params =
                                    std::mem::replace(&mut self.type_params, enum_type_params);

                                let mut sig_to_inst = None;
                                match variant {
                                    HirEnumVariant::Unit(_) => {
                                        found_ty = Some(self.get_or_create_type(HirType::Enum(
                                            crate::hir::id::DefId(enum_idx),
                                            vec![],
                                        )));
                                    }
                                    HirEnumVariant::Tuple(_, params) => {
                                        let mut sig = Vec::new();
                                        let ret_ty = self.get_or_create_type(HirType::Enum(
                                            crate::hir::id::DefId(enum_idx),
                                            vec![],
                                        ));
                                        sig.push(ret_ty);
                                        for p in params {
                                            sig.push(self.resolve_type_expr(p)?);
                                        }
                                        sig_to_inst = Some(
                                            self.get_or_create_type(HirType::Func(sig, false)),
                                        );
                                    }
                                    HirEnumVariant::Struct(_, fields) => {
                                        let mut sig = Vec::new();
                                        let ret_ty = self.get_or_create_type(HirType::Enum(
                                            crate::hir::id::DefId(enum_idx),
                                            vec![],
                                        ));
                                        sig.push(ret_ty);
                                        for f in fields {
                                            sig.push(self.resolve_type_expr(f.type_expr)?);
                                        }
                                        sig_to_inst = Some(
                                            self.get_or_create_type(HirType::Func(sig, false)),
                                        );
                                    }
                                }
                                self.type_params = old_params;

                                if let Some(s) = sig_to_inst {
                                    found_ty = Some(self.instantiate(s, &generic_params_clone_2));
                                } else if let Some(ty) = found_ty {
                                    found_ty = Some(self.instantiate(ty, &generic_params_clone_2));
                                }
                            }
                        }
                    }

                    found_ty.ok_or_else(|| {
                        RiddleError::Name(format!("Undefined symbol: {}", name), None)
                    })?
                }
            }
            HirExprKind::Call { callee, args } => {
                let callee_ty = self.infer_expr(callee)?;
                let mut arg_tys = Vec::new();

                if let HirExprKind::MemberAccess { object, .. } = &self.module.exprs[callee.0].kind {
                    if let Some(obj_ty) = self.module.exprs[object.0].ty {
                        arg_tys.push(obj_ty);
                    }
                }

                for arg in args {
                    arg_tys.push(self.infer_expr(arg)?);
                }

                let ret_ty = self.new_infer_ty();
                let mut func_sig = vec![ret_ty];
                func_sig.extend(arg_tys);
                let func_ty = self.get_or_create_type(HirType::Func(func_sig, false));

                let s = self.module.exprs[expr_id.0].span;
                self.unify(callee_ty, func_ty, Some(s))?;
                ret_ty
            }
            HirExprKind::IndexAccess { object, index } => {
                let obj_ty = self.infer_expr(object)?;
                let index_ty = self.infer_expr(index)?;

                let int64_ty = self.get_or_create_type(HirType::Int(64));
                let s = self.module.exprs[expr_id.0].span;
                self.unify(index_ty, int64_ty, Some(s))?;

                let obj_ty_followed = self.follow_id(obj_ty);
                match &self.module.types[obj_ty_followed.0] {
                    HirType::Pointer(inner) => *inner,
                    HirType::Array(inner, _) => *inner,
                    HirType::Struct(did, args) => {
                        let struct_item = &self.module.items[did.0];
                        if let HirItem::Struct(s_def) = struct_item {
                            if s_def.name == "Vec" {
                                if !args.is_empty() {
                                    args[0]
                                } else {
                                    self.get_or_create_type(HirType::Unknown)
                                }
                            } else {
                                return Err(RiddleError::Type(
                                    format!(
                                        "Index access not supported for type {}",
                                        self.type_to_string(obj_ty_followed)
                                    ),
                                    Some(s),
                                ));
                            }
                        } else {
                            return Err(RiddleError::Type(
                                format!(
                                    "Index access not supported for type {}",
                                    self.type_to_string(obj_ty_followed)
                                ),
                                Some(s),
                            ));
                        }
                    }
                    _ => {
                        return Err(RiddleError::Type(
                            format!(
                                "Index access not supported for type {}",
                                self.type_to_string(obj_ty_followed)
                            ),
                            Some(s),
                        ));
                    }
                }
            }
            HirExprKind::Block { stmts } => {
                let last_ty = self.get_or_create_type(HirType::Unit);
                for stmt_id in stmts {
                    self.infer_stmt(stmt_id)?;
                }
                last_ty
            }
            HirExprKind::StructInst {
                ref struct_name,
                ref fields,
            } => {
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
                    let mut mapping = HashMap::new();
                    for param in &s.generic_params {
                        mapping.insert(param.clone(), self.new_infer_ty());
                    }
                    let mut arg_tys = Vec::new();
                    for param in &s.generic_params {
                        arg_tys.push(mapping[param]);
                    }
                    let instance_ty = self.get_or_create_type(HirType::Struct(did, arg_tys));

                    for (f_name, f_expr_id) in fields {
                        let f_ty = self.infer_expr(*f_expr_id)?;
                        let mut found = false;
                        for s_field in &s.fields {
                            if &s_field.name == f_name {
                                // Merge struct generic params into current environment
                                let mut new_params = self.type_params.clone();
                                for tp in &s.generic_params {
                                    let ty =
                                        self.get_or_create_type(HirType::GenericParam(tp.clone()));
                                    new_params.insert(tp.clone(), ty);
                                }
                                let old_params =
                                    std::mem::replace(&mut self.type_params, new_params);
                                let expected_f_ty_base =
                                    self.resolve_type_expr(s_field.type_expr)?;
                                self.type_params = old_params;

                                let expected_f_ty = self.substitute(expected_f_ty_base, &mapping);
                                let s = self.module.exprs[expr_id.0].span;
                                self.unify(f_ty, expected_f_ty, Some(s))?;
                                found = true;
                                break;
                            }
                        }
                        if !found {
                            let s = self.module.exprs[expr_id.0].span;
                            return Err(RiddleError::Type(
                                format!("Field {} not found in struct {}", f_name, struct_name),
                                Some(s),
                            ));
                        }
                    }
                    instance_ty
                } else {
                    let s = self.module.exprs[expr_id.0].span;
                    return Err(RiddleError::Type(
                        format!("Struct {} not found", struct_name),
                        Some(s),
                    ));
                }
            }
            HirExprKind::MemberAccess {
                object, ref member, ..
            } => {
                let obj_ty = self.infer_expr(object)?;
                let mut obj_ty_followed = self.follow_id(obj_ty);
                let mut obj_hir_ty = self.module.types[obj_ty_followed.0].clone();

                let mut resolved_id = None;
                let ty = match obj_hir_ty {
                    HirType::Struct(def_id, args) => {
                        let mut member_ty = None;
                        let (_s_name, s_fields, s_generic_params) =
                            if let HirItem::Struct(s) = &self.module.items[def_id.0] {
                                (s.name.clone(), s.fields.clone(), s.generic_params.clone())
                            } else {
                                let s = self.module.exprs[expr_id.0].span;
                                return Err(RiddleError::Type(
                                    "Expected struct item".to_string(),
                                    Some(s),
                                ));
                            };

                        let mut mapping = HashMap::new();
                        for (i, param) in s_generic_params.iter().enumerate() {
                            if i < args.len() {
                                mapping.insert(param.clone(), args[i]);
                            }
                        }

                        // 1. Check fields
                        for field in &s_fields {
                            if &field.name == member {
                                // Temp set the struct's generic environment
                                let mut struct_type_params = HashMap::new();
                                for tp in &s_generic_params {
                                    let ty =
                                        self.get_or_create_type(HirType::GenericParam(tp.clone()));
                                    struct_type_params.insert(tp.clone(), ty);
                                }
                                let old_params =
                                    std::mem::replace(&mut self.type_params, struct_type_params);
                                let field_ty_base = self.resolve_type_expr(field.type_expr)?;
                                self.type_params = old_params;

                                member_ty = Some(self.substitute(field_ty_base, &mapping));
                                break;
                            }
                        }

                        if member_ty.is_none() {
                            // 2. Check impls
                            for i in 0..self.module.items.len() {
                                if let HirItem::Impl(im) = &self.module.items[i] {
                                    let im = im.clone();

                                    // Create fresh inference variables for impl generic params
                                    let mut im_mapping = HashMap::new();
                                    for param in &im.generic_params {
                                        im_mapping.insert(param.clone(), self.new_infer_ty());
                                    }

                                    let old_params =
                                        std::mem::replace(&mut self.type_params, im_mapping.clone());
                                    let target_ty = self.resolve_type_expr(im.target_type)?;
                                    self.type_params = old_params;

                                    if self.types_compatible(target_ty, obj_ty_followed) {
                                        if self.unify(target_ty, obj_ty_followed, None).is_ok() {
                                            for method_def_id in &im.items {
                                                let method_item =
                                                    self.module.items[method_def_id.0].clone();
                                                if let HirItem::Func(f) = method_item {
                                                    if f.name.ends_with(&format!("::{}", member))
                                                        || f.name == *member
                                                    {
                                                        let mut full_mapping = im_mapping.clone();
                                                        // Handle method's own generic parameters if any
                                                        if f.generic_params.len()
                                                            > im.generic_params.len()
                                                        {
                                                            for m_param in &f.generic_params
                                                                [im.generic_params.len()..]
                                                            {
                                                                full_mapping.insert(
                                                                    m_param.clone(),
                                                                    self.new_infer_ty(),
                                                                );
                                                            }
                                                        }

                                                        let base_sig = self
                                                            .item_types
                                                            .get(&method_def_id.0)
                                                            .unwrap();
                                                        member_ty = Some(
                                                            self.substitute(*base_sig, &full_mapping),
                                                        );
                                                        resolved_id = Some(*method_def_id);
                                                        break;
                                                    }
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
                        member_ty.ok_or_else(|| {
                            let s = self.module.exprs[expr_id.0].span;
                            RiddleError::Type(
                                format!(
                                    "Member {} not found on type {}",
                                    member,
                                    self.type_to_string(obj_ty_followed)
                                ),
                                Some(s),
                            )
                        })?
                    }
                    _ => {
                        let s = self.module.exprs[expr_id.0].span;
                        return Err(RiddleError::Type(
                            format!(
                                "Cannot access member {} on non-struct type {}",
                                member,
                                self.type_to_string(obj_ty_followed)
                            ),
                            Some(s),
                        ));
                    }
                };
                if let HirExprKind::MemberAccess { id, .. } = &mut self.module.exprs[expr_id.0].kind
                {
                    *id = resolved_id;
                }
                ty
            }
        };

        self.module.exprs[expr_id.0].ty = Some(ty);
        Ok(ty)
    }

    fn resolve_type_expr(&mut self, ty_expr_id: TyExprId) -> Result<TyId> {
        let ty_expr = self.module.type_exprs[ty_expr_id.0].clone();
        let ty_id = match ty_expr.kind {
            HirTypeExprKind::Unit => self.get_or_create_type(HirType::Unit),
            HirTypeExprKind::Path(name) => {
                if let Some(ty) = self.type_params.get(&name) {
                    return Ok(*ty);
                }
                if name == "int" {
                    self.get_or_create_type(HirType::Int(64))
                } else if name == "bool" {
                    self.get_or_create_type(HirType::Bool)
                } else if name == "str" {
                    self.get_or_create_type(HirType::Str)
                } else if name == "cint" {
                    self.get_or_create_type(HirType::CInt)
                } else if name == "cstr" {
                    self.get_or_create_type(HirType::CStr)
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
                            self.get_or_create_type(HirType::Enum(id, vec![]))
                        } else {
                            self.get_or_create_type(HirType::Struct(id, vec![]))
                        }
                    } else {
                        return Err(RiddleError::Type(
                            format!(
                                "Type not found: {} (params: {:?})",
                                name,
                                self.type_params.keys().collect::<Vec<_>>()
                            ),
                            None,
                        ));
                    }
                }
            }
            HirTypeExprKind::Generic(name, args) => {
                let mut arg_tys = Vec::new();
                for arg in args {
                    arg_tys.push(self.resolve_type_expr(arg)?);
                }

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
                        self.get_or_create_type(HirType::Enum(id, arg_tys))
                    } else {
                        self.get_or_create_type(HirType::Struct(id, arg_tys))
                    }
                } else {
                    return Err(RiddleError::Type(
                        format!("Generic type not found: {}", name),
                        None,
                    ));
                }
            }
            HirTypeExprKind::Func(params, ret) => {
                let ret_ty = self.resolve_type_expr(ret)?;
                let mut sig = vec![ret_ty];
                for p in params {
                    sig.push(self.resolve_type_expr(p)?);
                }
                self.get_or_create_type(HirType::Func(sig, false))
            }
            HirTypeExprKind::Pointer(inner) => {
                let inner_ty = self.resolve_type_expr(inner)?;
                self.get_or_create_type(HirType::Pointer(inner_ty))
            }
            HirTypeExprKind::Array(inner, size) => {
                let inner_ty = self.resolve_type_expr(inner)?;
                self.get_or_create_type(HirType::Array(inner_ty, size))
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

    fn unify(&mut self, t1: TyId, t2: TyId, span: Option<Span>) -> Result<()> {
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
            (HirType::Func(f1, v1), HirType::Func(f2, v2)) => {
                if v1 || v2 {
                    let min_len = if f1.len() < f2.len() {
                        f1.len()
                    } else {
                        f2.len()
                    };
                    for i in 0..min_len {
                        self.unify(f1[i], f2[i], span)?;
                    }
                } else {
                    if f1.len() != f2.len() {
                        return Err(RiddleError::Type(
                            format!(
                                "Arity mismatch: expected {} parameters, found {}",
                                f1.len() - 1,
                                f2.len() - 1
                            ),
                            span,
                        ));
                    }
                    for (a, b) in f1.into_iter().zip(f2.into_iter()) {
                        self.unify(a, b, span)?;
                    }
                }
                Ok(())
            }
            (HirType::Struct(id1, args1), HirType::Struct(id2, args2)) => {
                if id1 != id2 || args1.len() != args2.len() {
                    return Err(RiddleError::Type(
                        format!(
                            "Type mismatch: expected {}, found {}",
                            self.type_to_string(t1),
                            self.type_to_string(t2)
                        ),
                        span,
                    ));
                }
                for (a, b) in args1.into_iter().zip(args2.into_iter()) {
                    self.unify(a, b, span)?;
                }
                Ok(())
            }
            (HirType::Enum(id1, args1), HirType::Enum(id2, args2)) => {
                if id1 != id2 || args1.len() != args2.len() {
                    return Err(RiddleError::Type(
                        format!(
                            "Type mismatch: expected {}, found {}",
                            self.type_to_string(t1),
                            self.type_to_string(t2)
                        ),
                        span,
                    ));
                }
                for (a, b) in args1.into_iter().zip(args2.into_iter()) {
                    self.unify(a, b, span)?;
                }
                Ok(())
            }
            (HirType::Array(inner1, size1), HirType::Array(inner2, size2)) => {
                if size1 != size2 {
                    return Err(RiddleError::Type(
                        format!(
                            "Type mismatch: expected array of size {}, found {}",
                            size1, size2
                        ),
                        span,
                    ));
                }
                self.unify(inner1, inner2, span)
            }
            (HirType::Pointer(p1), HirType::Pointer(p2)) => {
                self.unify(p1, p2, span)?;
                Ok(())
            }
            (a, b) if a == b => Ok(()),
            (_, _) => Err(RiddleError::Type(
                format!(
                    "Cannot unify types: expected {}, found {}",
                    self.type_to_string(t1),
                    self.type_to_string(t2)
                ),
                span,
            )),
        }
    }

    pub fn type_to_string(&self, ty_id: TyId) -> String {
        let ty_id = self.follow_id(ty_id);
        let ty = &self.module.types[ty_id.0];
        match ty {
            HirType::Int(w) => format!("int{}", w),
            HirType::Float => "float".to_string(),
            HirType::Double => "double".to_string(),
            HirType::Bool => "bool".to_string(),
            HirType::Unit => "()".to_string(),
            HirType::Str => "str".to_string(),
            HirType::CInt => "cint".to_string(),
            HirType::CStr => "cstr".to_string(),
            HirType::Func(sig, var) => {
                let ret = self.type_to_string(sig[0]);
                let params: Vec<String> =
                    sig[1..].iter().map(|t| self.type_to_string(*t)).collect();
                format!(
                    "fun({}) -> {}{}",
                    params.join(", "),
                    ret,
                    if *var { "..." } else { "" }
                )
            }
            HirType::Struct(def_id, args) => {
                if let HirItem::Struct(s) = &self.module.items[def_id.0] {
                    if args.is_empty() {
                        s.name.clone()
                    } else {
                        let args_str: Vec<String> =
                            args.iter().map(|t| self.type_to_string(*t)).collect();
                        format!("{}<{}>", s.name, args_str.join(", "))
                    }
                } else {
                    "struct".to_string()
                }
            }
            HirType::Enum(def_id, args) => {
                if let HirItem::Enum(e) = &self.module.items[def_id.0] {
                    if args.is_empty() {
                        e.name.clone()
                    } else {
                        let args_str: Vec<String> =
                            args.iter().map(|t| self.type_to_string(*t)).collect();
                        format!("{}<{}>", e.name, args_str.join(", "))
                    }
                } else {
                    "enum".to_string()
                }
            }
            HirType::GenericParam(name) => name.clone(),
            HirType::Pointer(inner) => format!("*{}", self.type_to_string(*inner)),
            HirType::Array(inner, size) => format!("[{}; {}]", self.type_to_string(*inner), size),
            HirType::Infer(InferTy::Id(id)) => format!("?{}", id),
            HirType::Unknown => "unknown".to_string(),
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

    fn types_compatible(&self, t1: TyId, t2: TyId) -> bool {
        let t1 = self.follow_id(t1);
        let t2 = self.follow_id(t2);
        if t1 == t2 {
            return true;
        }
        let ty1 = &self.module.types[t1.0];
        let ty2 = &self.module.types[t2.0];
        match (ty1, ty2) {
            (HirType::Infer(_), _) | (_, HirType::Infer(_)) => true,
            (HirType::Struct(id1, _), HirType::Struct(id2, _)) => id1 == id2,
            (HirType::Enum(id1, _), HirType::Enum(id2, _)) => id1 == id2,
            (HirType::Pointer(_), HirType::Pointer(_)) => true,
            (HirType::Array(_, _), HirType::Array(_, _)) => true,
            (a, b) => a == b,
        }
    }

    fn instantiate(&mut self, ty_id: TyId, generic_params: &[String]) -> TyId {
        if generic_params.is_empty() {
            return ty_id;
        }
        let ty = self.module.types[ty_id.0].clone();
        let mut mapping = HashMap::new();
        for param in generic_params {
            mapping.insert(param.clone(), self.new_infer_ty());
        }

        match ty {
            HirType::Struct(id, args) if args.is_empty() => {
                let mut new_args = Vec::new();
                for param in generic_params {
                    new_args.push(mapping[param]);
                }
                self.get_or_create_type(HirType::Struct(id, new_args))
            }
            HirType::Enum(id, args) if args.is_empty() => {
                let mut new_args = Vec::new();
                for param in generic_params {
                    new_args.push(mapping[param]);
                }
                self.get_or_create_type(HirType::Enum(id, new_args))
            }
            _ => self.substitute(ty_id, &mapping),
        }
    }

    fn substitute(&mut self, ty_id: TyId, mapping: &HashMap<String, TyId>) -> TyId {
        let ty = self.module.types[ty_id.0].clone();
        match ty {
            HirType::GenericParam(name) => {
                if let Some(new_ty) = mapping.get(&name) {
                    *new_ty
                } else {
                    ty_id
                }
            }
            HirType::Func(sig, is_variadic) => {
                let mut new_sig = Vec::new();
                for t in sig {
                    new_sig.push(self.substitute(t, mapping));
                }
                self.get_or_create_type(HirType::Func(new_sig, is_variadic))
            }
            HirType::Struct(id, args) => {
                let mut new_args = Vec::new();
                for t in args {
                    new_args.push(self.substitute(t, mapping));
                }
                self.get_or_create_type(HirType::Struct(id, new_args))
            }
            HirType::Enum(id, args) => {
                let mut new_args = Vec::new();
                for t in args {
                    new_args.push(self.substitute(t, mapping));
                }
                self.get_or_create_type(HirType::Enum(id, new_args))
            }
            HirType::Pointer(inner) => {
                let new_inner = self.substitute(inner, mapping);
                self.get_or_create_type(HirType::Pointer(new_inner))
            }
            _ => ty_id,
        }
    }
}
