use crate::codegen::mangle;
use crate::hir::expr::{HirExprKind, HirLiteral};
use crate::hir::id::{DefId, ExprId, LocalId, StmtId, TyId};
use crate::hir::items::HirItem;
use crate::hir::module::HirModule;
use crate::hir::stmt::HirStmtKind;
use crate::hir::types::HirType;
use cranelift::prelude::*;
use cranelift::prelude::codegen::ir::Opcode;
use cranelift_module::{DataDescription, FuncId, Linkage, Module};
use cranelift_object::ObjectModule;
use std::collections::HashMap;

pub struct FunctionTranslator<'a> {
    pub(crate) builder: FunctionBuilder<'a>,
    pub(crate) module: &'a mut ObjectModule,
    pub(crate) fn_ids: &'a mut HashMap<(DefId, Vec<TyId>), FuncId>,
    pub(crate) specialization_queue: &'a mut Vec<(DefId, Vec<TyId>)>,
    pub(crate) variables: HashMap<LocalId, Variable>,
    pub(crate) hir: &'a HirModule,
    pub(crate) gc_alloc_id: Option<FuncId>,
}

impl<'a> FunctionTranslator<'a> {
    pub fn translate_stmt(&mut self, stmt_id: StmtId) {
        let stmt = &self.hir.stmts[stmt_id.0];
        match &stmt.kind {
            HirStmtKind::Expr { expr, .. } => {
                self.translate_expr(*expr);
            }
            HirStmtKind::Let { init, id, .. } => {
                let var = self.builder.declare_var(types::I64);
                if let Some(init_expr) = init {
                    let val = self.translate_expr(*init_expr);
                    self.builder.def_var(var, val);
                }
                self.variables.insert(*id, var);
            }
            HirStmtKind::If {
                cond,
                then_block,
                else_block,
            } => {
                let cond_val = self.translate_expr(*cond);
                // Ensure condition is b1 for branching
                let zero = self.builder.ins().iconst(types::I64, 0);
                let cond_b1 = self.builder.ins().icmp(IntCC::NotEqual, cond_val, zero);

                let then_block_ir = self.builder.create_block();
                let else_block_ir = self.builder.create_block();
                let merge_block_ir = self.builder.create_block();

                self.builder
                    .ins()
                    .brif(cond_b1, then_block_ir, &[], else_block_ir, &[]);

                self.builder.switch_to_block(then_block_ir);
                self.translate_stmt(*then_block);
                let then_filled = self.is_block_filled(then_block_ir);
                if !then_filled {
                    self.builder.ins().jump(merge_block_ir, &[]);
                }

                self.builder.switch_to_block(else_block_ir);
                if let Some(eb) = else_block {
                    self.translate_stmt(*eb);
                }
                let else_filled = self.is_block_filled(else_block_ir);
                if !else_filled {
                    self.builder.ins().jump(merge_block_ir, &[]);
                }

                self.builder.seal_block(then_block_ir);
                self.builder.seal_block(else_block_ir);
                self.builder.seal_block(merge_block_ir);

                self.builder.switch_to_block(merge_block_ir);
            }
            HirStmtKind::Return { value } => {
                let mut vals = Vec::new();
                if let Some(expr_id) = value {
                    vals.push(self.translate_expr(*expr_id));
                } else {
                    // Default return 0 if no value
                    vals.push(self.builder.ins().iconst(types::I64, 0));
                }
                self.builder.ins().return_(&vals);
            }
        }
    }

    pub fn translate_expr(&mut self, expr_id: ExprId) -> Value {
        let expr = &self.hir.exprs[expr_id.0];
        match &expr.kind {
            HirExprKind::Literal(lit) => match lit {
                HirLiteral::Int(i) => self.builder.ins().iconst(types::I64, *i),
                HirLiteral::Bool(b) => self
                    .builder
                    .ins()
                    .iconst(types::I64, if *b { 1 } else { 0 }),
                HirLiteral::Str(s) => {
                    let mut data_ctx = DataDescription::new();
                    let mut s_with_null = s.clone();
                    s_with_null.push('\0');
                    data_ctx.define(s_with_null.into_bytes().into_boxed_slice());
                    let data_id = self
                        .module
                        .declare_data(&format!("str_{}", expr_id.0), Linkage::Local, false, false)
                        .unwrap();
                    self.module.define_data(data_id, &data_ctx).unwrap();
                    let data_ref = self
                        .module
                        .declare_data_in_func(data_id, &mut self.builder.func);
                    self.builder.ins().symbol_value(types::I64, data_ref)
                }
                HirLiteral::CStr(s) => {
                    let mut data_ctx = DataDescription::new();
                    let mut s_with_null = s.clone();
                    s_with_null.push('\0');
                    data_ctx.define(s_with_null.into_bytes().into_boxed_slice());
                    let data_id = self
                        .module
                        .declare_data(&format!("cstr_{}", expr_id.0), Linkage::Local, false, false)
                        .unwrap();
                    self.module.define_data(data_id, &data_ctx).unwrap();
                    let data_ref = self
                        .module
                        .declare_data_in_func(data_id, &mut self.builder.func);
                    self.builder.ins().symbol_value(types::I64, data_ref)
                }
                HirLiteral::CInt(i) => self.builder.ins().iconst(types::I64, *i),
                HirLiteral::Float(f) => {
                    self.builder.ins().f64const(*f)
                }
            },
            HirExprKind::BinaryOp { lhs, op, rhs } => {
                let r = self.translate_expr(*rhs);
                if op == "=" {
                    let lhs_expr = &self.hir.exprs[lhs.0];
                    match &lhs_expr.kind {
                        HirExprKind::Symbol { id, .. } => {
                            if let Some(local_id) = id {
                                let var = self.variables[local_id];
                                self.builder.def_var(var, r);
                            } else {
                                unimplemented!("Assigning to non-local symbols not supported");
                            }
                        }
                        HirExprKind::UnaryOp { op, expr } if op == "*" => {
                            let addr = self.translate_expr(*expr);
                            self.builder.ins().store(MemFlags::new(), r, addr, 0);
                        }
                        HirExprKind::MemberAccess { object, member, .. } => {
                            let obj_ptr = self.translate_expr(*object);
                            let obj_expr = &self.hir.exprs[object.0];
                            let ty_id = obj_expr.ty.expect("Member access object must have a type");
                            let ty = &self.hir.types[ty_id.0];
                            if let HirType::Struct(struct_did, _) = ty {
                                let offset = self.get_field_offset(*struct_did, member);
                                self.builder.ins().store(MemFlags::new(), r, obj_ptr, offset as i32);
                            }
                        }
                        HirExprKind::IndexAccess { object, index } => {
                            let mut obj_ptr = self.translate_expr(*object);
                            let index_val = self.translate_expr(*index);

                            let obj_expr = &self.hir.exprs[object.0];
                            let ty_id = obj_expr.ty.expect("Index access object must have a type");
                            let ty = self.follow_id(ty_id);
                            let ty = &self.hir.types[ty.0];

                            let mut scale = self.builder.ins().iconst(types::I64, 8);
                            if let HirType::Struct(did, _) = ty {
                                let item = &self.hir.items[did.0];
                                if let HirItem::Struct(s) = item {
                                    if s.name == "Vec" {
                                        let offset_data = self.get_field_offset(*did, "data");
                                        let offset_size = self.get_field_offset(*did, "elem_size");
                                        let data_ptr = self.builder.ins().load(
                                            types::I64,
                                            MemFlags::new(),
                                            obj_ptr,
                                            offset_data as i32,
                                        );
                                        scale = self.builder.ins().load(
                                            types::I64,
                                            MemFlags::new(),
                                            obj_ptr,
                                            offset_size as i32,
                                        );
                                        obj_ptr = data_ptr;
                                    }
                                }
                            }

                            let offset = self.builder.ins().imul(index_val, scale);
                            let addr = self.builder.ins().iadd(obj_ptr, offset);
                            self.builder.ins().store(MemFlags::new(), r, addr, 0);
                        }
                        _ => unimplemented!("Left-hand side of assignment not supported"),
                    }
                    r
                } else {
                    let l = self.translate_expr(*lhs);
                    match op.as_str() {
                        "+" => self.builder.ins().iadd(l, r),
                        "-" => self.builder.ins().isub(l, r),
                        "*" => self.builder.ins().imul(l, r),
                        "/" => self.builder.ins().sdiv(l, r),
                        "==" => {
                            let val = self.builder.ins().icmp(IntCC::Equal, l, r);
                            self.builder.ins().uextend(types::I64, val)
                        }
                        "!=" => {
                            let val = self.builder.ins().icmp(IntCC::NotEqual, l, r);
                            self.builder.ins().uextend(types::I64, val)
                        }
                        "<" => {
                            let val = self.builder.ins().icmp(IntCC::SignedLessThan, l, r);
                            self.builder.ins().uextend(types::I64, val)
                        }
                        ">" => {
                            let val = self.builder.ins().icmp(IntCC::SignedGreaterThan, l, r);
                            self.builder.ins().uextend(types::I64, val)
                        }
                        _ => unimplemented!("Operator {} not supported", op),
                    }
                }
            }
            HirExprKind::UnaryOp { op, expr } => {
                let e = self.translate_expr(*expr);
                match op.as_str() {
                    "&" => {
                        let inner_expr = &self.hir.exprs[expr.0];
                        match &inner_expr.kind {
                            HirExprKind::Symbol { id, .. } => {
                                if let Some(local_id) = id {
                                    let var = self.variables[local_id];
                                    // This is tricky in Cranelift as it doesn't normally allow taking address of a Variable
                                    // But since our variables are mostly in memory or on stack, 
                                    // we might need them to be stack slots.
                                    // However, Riddle seems to treat variables as I64.
                                    // For simplicity, let's assume we can't take address of local yet or it's a stack slot.
                                    unimplemented!("Taking address of local variable not supported yet")
                                } else {
                                    unimplemented!("Taking address of non-local symbol not supported yet")
                                }
                            }
                            HirExprKind::MemberAccess { object, member, .. } => {
                                let obj_ptr = self.translate_expr(*object);
                                let obj_expr = &self.hir.exprs[object.0];
                                let ty_id = obj_expr.ty.expect("Member access object must have a type");
                                let ty = &self.hir.types[ty_id.0];
                                if let HirType::Struct(struct_did, _) = ty {
                                    let offset = self.get_field_offset(*struct_did, member);
                                    self.builder.ins().iadd_imm(obj_ptr, offset as i64)
                                } else {
                                    unimplemented!("Taking address of non-struct member")
                                }
                            }
                            _ => unimplemented!("Taking address of this expression not supported"),
                        }
                    }
                    "*" => {
                        self.builder.ins().load(types::I64, MemFlags::new(), e, 0)
                    }
                    "-" => {
                        self.builder.ins().ineg(e)
                    }
                    "!" => {
                        // Invert boolean (assuming 0 or 1)
                        let zero = self.builder.ins().iconst(types::I64, 0);
                        let is_zero = self.builder.ins().icmp(IntCC::Equal, e, zero);
                        self.builder.ins().uextend(types::I64, is_zero)
                    }
                    _ => unimplemented!("Unary operator {} not supported", op),
                }
            }
            HirExprKind::Symbol { name, id, .. } => {
                if let Some(local_id) = id {
                    if let Some(var) = self.variables.get(local_id) {
                        self.builder.use_var(*var)
                    } else {
                        // 可能是函数名，如果 NamePass 没把函数名当作 LocalId
                        self.resolve_fn_call(name, &[])
                    }
                } else {
                    self.resolve_fn_call(name, &[])
                }
            }
            HirExprKind::Call { callee, args } => {
                let callee_expr = &self.hir.exprs[callee.0];
                let mut arg_vals = Vec::new();
                
                let mut self_val = None;
                if let HirExprKind::MemberAccess { object, .. } = &callee_expr.kind {
                    self_val = Some(self.translate_expr(*object));
                }

                if let Some(v) = self_val {
                    arg_vals.push(v);
                }

                for arg in args {
                    arg_vals.push(self.translate_expr(*arg));
                }

                match &callee_expr.kind {
                    HirExprKind::Symbol { name, def_id, .. } => {
                        if let Some(did) = def_id {
                            self.call_by_id(*did, &arg_vals, callee_expr.ty)
                        } else {
                            self.resolve_fn_call(name, &arg_vals)
                        }
                    }
                    HirExprKind::MemberAccess { member, id, .. } => {
                        if let Some(def_id) = id {
                            self.call_by_id(*def_id, &arg_vals, callee_expr.ty)
                        } else {
                            self.resolve_fn_call(member, &arg_vals)
                        }
                    }
                    _ => unimplemented!("Indirect calls not supported yet"),
                }
            }
            HirExprKind::MemberAccess { object, member, .. } => {
                let obj_ptr = self.translate_expr(*object);
                let obj_expr = &self.hir.exprs[object.0];
                let ty_id = obj_expr.ty.expect("Member access object must have a type");
                let ty = self.follow_id(ty_id);
                let ty = &self.hir.types[ty.0];

                if let HirType::Struct(struct_did, _) = ty {
                    let offset = self.get_field_offset(*struct_did, member);
                    self.builder
                        .ins()
                        .load(types::I64, MemFlags::new(), obj_ptr, offset as i32)
                } else {
                    self.builder.ins().iconst(types::I64, 0)
                }
            }
            HirExprKind::IndexAccess { object, index } => {
                let mut obj_ptr = self.translate_expr(*object);
                let index_val = self.translate_expr(*index);

                let obj_expr = &self.hir.exprs[object.0];
                let ty_id = obj_expr.ty.expect("Index access object must have a type");
                let ty = self.follow_id(ty_id);
                let ty = &self.hir.types[ty.0];

                let mut scale = self.builder.ins().iconst(types::I64, 8);
                if let HirType::Struct(did, _) = ty {
                    let item = &self.hir.items[did.0];
                    if let HirItem::Struct(s) = item {
                        if s.name == "Vec" {
                            let offset_data = self.get_field_offset(*did, "data");
                            let offset_size = self.get_field_offset(*did, "elem_size");
                            let data_ptr =
                                self.builder
                                    .ins()
                                    .load(types::I64, MemFlags::new(), obj_ptr, offset_data as i32);
                            scale =
                                self.builder
                                    .ins()
                                    .load(types::I64, MemFlags::new(), obj_ptr, offset_size as i32);
                            obj_ptr = data_ptr;
                        }
                    }
                }

                let offset = self.builder.ins().imul(index_val, scale);
                let addr = self.builder.ins().iadd(obj_ptr, offset);
                self.builder.ins().load(types::I64, MemFlags::new(), addr, 0)
            }
            HirExprKind::Cast { expr, target_ty_expr } => {
                let val = self.translate_expr(*expr);
                let target_ty_id = self.hir.type_exprs[target_ty_expr.0]
                    .curr_ty
                    .expect("Target type of cast must be resolved");
                let target_ty = &self.hir.types[target_ty_id.0];
                match target_ty {
                    HirType::CInt => {
                        // For now we use I64 for everything
                        val
                    }
                    HirType::CStr => val,
                    _ => val,
                }
            }
            HirExprKind::Block { stmts } => {
                let mut last_val = self.builder.ins().iconst(types::I64, 0);
                for &stmt_id in stmts {
                    let stmt = &self.hir.stmts[stmt_id.0];
                    if let HirStmtKind::Expr { expr, .. } = &stmt.kind {
                        last_val = self.translate_expr(*expr);
                    } else {
                        self.translate_stmt(stmt_id);
                    }
                }
                last_val
            }
            HirExprKind::StructInst {
                struct_name,
                fields,
            } => {
                let mut struct_def = None;
                let mut did = None;
                for (i, item) in self.hir.items.iter().enumerate() {
                    if let HirItem::Struct(s) = item {
                        if s.name == *struct_name {
                            struct_def = Some(s);
                            did = Some(DefId(i));
                            break;
                        }
                    }
                }

                let s = struct_def.expect("Struct not found");
                let size = (s.fields.len() * 8) as i64;
                let size_val = self.builder.ins().iconst(types::I64, size);

                let obj_ptr = if let Some(gc_alloc_id) = self.gc_alloc_id {
                    let local_func = self
                        .module
                        .declare_func_in_func(gc_alloc_id, &mut self.builder.func);
                    let call = self.builder.ins().call(local_func, &[size_val]);
                    self.builder.inst_results(call)[0]
                } else {
                    // Fallback to stack allocation if GC is not available (though we initialized it)
                    let ss = self.builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        size as u32,
                        3,
                    ));
                    self.builder.ins().stack_addr(types::I64, ss, 0)
                };

                for (f_name, f_expr_id) in fields {
                    let val = self.translate_expr(*f_expr_id);
                    let offset = self.get_field_offset(did.unwrap(), f_name);
                    self.builder
                        .ins()
                        .store(MemFlags::new(), val, obj_ptr, offset as i32);
                }

                obj_ptr
            }
            &HirExprKind::ListLiteral(ref elements) => {
                let size = (elements.len() * 8) as i64;
                let size_val = self.builder.ins().iconst(types::I64, size);

                let obj_ptr = if let Some(gc_alloc_id) = self.gc_alloc_id {
                    let local_func = self
                        .module
                        .declare_func_in_func(gc_alloc_id, &mut self.builder.func);
                    let call = self.builder.ins().call(local_func, &[size_val]);
                    self.builder.inst_results(call)[0]
                } else {
                    unimplemented!("GC not available for ListLiteral");
                };

                for (i, &elem_id) in elements.iter().enumerate() {
                    let val = self.translate_expr(elem_id);
                    self.builder
                        .ins()
                        .store(MemFlags::new(), val, obj_ptr, (i * 8) as i32);
                }
                obj_ptr
            }
        }
    }

    fn get_field_offset(&self, struct_did: DefId, member_name: &str) -> usize {
        let item = &self.hir.items[struct_did.0];
        if let HirItem::Struct(s) = item {
            for (i, field) in s.fields.iter().enumerate() {
                if field.name == member_name {
                    return i * 8;
                }
            }
        }
        0
    }

    fn call_by_id(
        &mut self,
        def_id: DefId,
        args: &[Value],
        instantiated_ty: Option<TyId>,
    ) -> Value {
        let item = &self.hir.items[def_id.0];
        if let HirItem::ExternFunc(f) = item {
            if f.is_variadic {
                return self.emit_variadic_call(def_id, args);
            }
        }

        let generic_args = if let Some(ty) = instantiated_ty {
            self.extract_generic_args(def_id, ty)
        } else {
            vec![]
        };

        let key = (def_id, generic_args.clone());
        if !self.fn_ids.contains_key(&key) {
            let name = match &self.hir.items[def_id.0] {
                HirItem::Func(f) => mangle(&f.name, &generic_args),
                HirItem::Enum(e) => mangle(&e.name, &generic_args),
                HirItem::Struct(s) => mangle(&s.name, &generic_args),
                _ => unreachable!(),
            };

            let mut sig = self.module.make_signature();
            let ty_id = self.hir.item_types[&def_id];
            let param_count = match &self.hir.types[ty_id.0] {
                crate::hir::types::HirType::Func(sig_types, _) => sig_types.len() - 1,
                _ => 0,
            };

            for _ in 0..param_count {
                sig.params.push(AbiParam::new(types::I64));
            }
            sig.returns.push(AbiParam::new(types::I64));

            let fn_id = self
                .module
                .declare_function(&name, Linkage::Export, &sig)
                .unwrap();
            self.fn_ids.insert(key.clone(), fn_id);
            if matches!(&self.hir.items[def_id.0], HirItem::Func(_)) {
                self.specialization_queue.push(key);
            }
        }

        let fn_id = self.fn_ids[&(def_id, generic_args)];
        let local_func = self
            .module
            .declare_func_in_func(fn_id, &mut self.builder.func);
        let call = self.builder.ins().call(local_func, args);
        self.builder.inst_results(call)[0]
    }

    fn extract_generic_args(&self, def_id: DefId, instantiated_ty: TyId) -> Vec<TyId> {
        let ty = &self.hir.types[instantiated_ty.0];
        match ty {
            crate::hir::types::HirType::Struct(_, args) => return args.clone(),
            crate::hir::types::HirType::Enum(_, args) => return args.clone(),
            _ => {}
        }

        let orig_ty_id = match self.hir.item_types.get(&def_id) {
            Some(id) => *id,
            None => return vec![],
        };
        let mut args = Vec::new();

        let item = &self.hir.items[def_id.0];
        let generic_params = match item {
            HirItem::Func(f) => &f.generic_params,
            _ => return vec![],
        };

        if generic_params.is_empty() {
            return vec![];
        }

        if let (
            crate::hir::types::HirType::Func(orig_sig, _),
            crate::hir::types::HirType::Func(inst_sig, _),
        ) = (&self.hir.types[orig_ty_id.0], ty)
        {
            for gp_name in generic_params {
                let mut found = false;
                for (i, &orig_p_id) in orig_sig.iter().enumerate() {
                    if let crate::hir::types::HirType::GenericParam(name) =
                        &self.hir.types[orig_p_id.0]
                    {
                        if name == gp_name {
                            if i < inst_sig.len() {
                                args.push(inst_sig[i]);
                                found = true;
                                break;
                            }
                        }
                    }
                }
                if !found {
                    // Fallback or error
                }
            }
        }

        args
    }

    // Handling variable length parameter functions
    fn emit_variadic_call(&mut self, def_id: DefId, args: &[Value]) -> Value {
        let fn_id = self.fn_ids[&(def_id, vec![])];
        let mut sig = self.module.make_signature();

        let default_cc = self.module.isa().default_call_conv();
        sig.call_conv = default_cc;

        for _ in 0..args.len() {
            sig.params.push(AbiParam::new(types::I64));
        }
        sig.returns.push(AbiParam::new(types::I64));

        let sig_ref = self.builder.import_signature(sig);
        let local_func = self
            .module
            .declare_func_in_func(fn_id, &mut self.builder.func);
        let func_ptr = self.builder.ins().func_addr(types::I64, local_func);
        let call = self.builder.ins().call_indirect(sig_ref, func_ptr, args);
        self.builder.inst_results(call)[0]
    }

    fn resolve_fn_call(&mut self, name: &str, args: &[Value]) -> Value {
        let mut target_def_id = None;
        for (i, item) in self.hir.items.iter().enumerate() {
            match item {
                HirItem::Func(f) if f.name == name => {
                    target_def_id = Some(DefId(i));
                    break;
                }
                HirItem::ExternFunc(f) if f.name == name => {
                    target_def_id = Some(DefId(i));
                    break;
                }
                _ => {}
            }
        }

        if let Some(def_id) = target_def_id {
            self.call_by_id(def_id, args, None)
        } else {
            // Check if it is an enum variant
            if name.contains("::") {
                let parts: Vec<&str> = name.split("::").collect();
                if parts.len() == 2 {
                    let enum_name = parts[0];
                    let variant_name = parts[1];
                    for item in &self.hir.items {
                        if let HirItem::Enum(e) = item {
                            if e.name == enum_name {
                                for variant in &e.variants {
                                    let v_name = match variant {
                                        crate::hir::items::HirEnumVariant::Unit(n) => n,
                                        crate::hir::items::HirEnumVariant::Tuple(n, _) => n,
                                        crate::hir::items::HirEnumVariant::Struct(n, _) => n,
                                    };
                                    if v_name == variant_name {
                                        // For now, return a dummy value 0
                                        return self.builder.ins().iconst(types::I64, 0);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            panic!("Function {} not found", name);
        }
    }

    fn follow_id(&self, mut id: TyId) -> TyId {
        while let HirType::Infer(crate::hir::types::InferTy::Id(infer_id)) = &self.hir.types[id.0] {
            // How to get constraints? We might need to store them in HirModule or somewhere.
            // But actually TypeInfer already finalized types, so they shouldn't be InferTy.
            break;
        }
        id
    }

    fn is_block_filled(&self, block: cranelift::prelude::Block) -> bool {
        if let Some(inst) = self.builder.func.layout.block_insts(block).last() {
            let opcode = self.builder.func.dfg.insts[inst].opcode();
            opcode.is_terminator()
        } else {
            false
        }
    }
}
