use crate::hir::expr::{HirExprKind, HirLiteral};
use crate::hir::id::{DefId, ExprId, LocalId, StmtId};
use crate::hir::items::HirItem;
use crate::hir::module::HirModule;
use crate::hir::stmt::HirStmtKind;
use cranelift::prelude::*;
use cranelift_module::{DataDescription, FuncId, Linkage, Module};
use cranelift_object::ObjectModule;
use std::collections::HashMap;

pub struct FunctionTranslator<'a> {
    pub(crate) builder: FunctionBuilder<'a>,
    pub(crate) module: &'a mut ObjectModule,
    pub(crate) fn_ids: &'a HashMap<DefId, FuncId>,
    pub(crate) variables: HashMap<LocalId, Variable>,
    pub(crate) hir: &'a HirModule,
}

impl<'a> FunctionTranslator<'a> {
    pub fn translate_stmt(&mut self, stmt_id: StmtId) {
        let stmt = &self.hir.stmts[stmt_id.0];
        match &stmt.kind {
            HirStmtKind::Expr { expr, .. } => {
                self.translate_expr(*expr);
            }
            HirStmtKind::Let { init, id, .. } => {
                let var = Variable::new(id.0);
                self.builder.declare_var(var, types::I64);
                if let Some(init_expr) = init {
                    let val = self.translate_expr(*init_expr);
                    self.builder.def_var(var, val);
                }
                self.variables.insert(*id, var);
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
                HirLiteral::Bool(b) => self.builder.ins().iconst(types::I64, if *b { 1 } else { 0 }),
                HirLiteral::Str(s) => {
                    let mut data_ctx = DataDescription::new();
                    let mut s_with_null = s.clone();
                    s_with_null.push('\0');
                    data_ctx.define(s_with_null.into_bytes().into_boxed_slice());
                    let data_id = self.module.declare_data(
                        &format!("str_{}", expr_id.0),
                        Linkage::Local,
                        false,
                        false
                    ).unwrap();
                    self.module.define_data(data_id, &data_ctx).unwrap();
                    let data_ref = self.module.declare_data_in_func(data_id, &mut self.builder.func);
                    self.builder.ins().symbol_value(types::I64, data_ref)
                }
                _ => unimplemented!("Literal type not supported yet"),
            },
            HirExprKind::BinaryOp { lhs, op, rhs } => {
                let l = self.translate_expr(*lhs);
                let r = self.translate_expr(*rhs);
                match op.as_str() {
                    "+" => self.builder.ins().iadd(l, r),
                    "-" => self.builder.ins().isub(l, r),
                    "*" => self.builder.ins().imul(l, r),
                    "/" => self.builder.ins().sdiv(l, r),
                    _ => unimplemented!("Operator {} not supported", op),
                }
            }
            HirExprKind::Symbol { name, id } => {
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
                for arg in args {
                    arg_vals.push(self.translate_expr(*arg));
                }

                match &callee_expr.kind {
                    HirExprKind::Symbol { name, .. } => self.resolve_fn_call(name, &arg_vals),
                    HirExprKind::MemberAccess { member, id, .. } => {
                        if let Some(def_id) = id {
                            self.call_by_id(*def_id, &arg_vals)
                        } else {
                            self.resolve_fn_call(member, &arg_vals)
                        }
                    }
                    _ => unimplemented!("Indirect calls not supported yet"),
                }
            }
            HirExprKind::MemberAccess { object, .. } => {
                self.translate_expr(*object);
                self.builder.ins().iconst(types::I64, 0)
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
            HirExprKind::StructInst { fields, .. } => {
                for (_, expr_id) in fields {
                    self.translate_expr(*expr_id);
                }
                self.builder.ins().iconst(types::I64, 0)
            }
        }
    }

    fn call_by_id(&mut self, def_id: DefId, args: &[Value]) -> Value {
        let item = &self.hir.items[def_id.0];
        if let HirItem::ExternFunc(f) = item {
            if f.is_variadic {
                return self.emit_variadic_call(def_id, args);
            }
        }

        let fn_id = self.fn_ids[&def_id];
        let local_func = self
            .module
            .declare_func_in_func(fn_id, &mut self.builder.func);
        let call = self.builder.ins().call(local_func, args);
        self.builder.inst_results(call)[0]
    }

    // Handling variable length parameter functions
    fn emit_variadic_call(&mut self, def_id: DefId, args: &[Value]) -> Value {
        let fn_id = self.fn_ids[&def_id];
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
                HirItem::Func(f) => {
                    if f.name == name {
                        target_def_id = Some(DefId(i));
                        break;
                    }
                }
                HirItem::ExternFunc(f) => {
                    if f.name == name {
                        target_def_id = Some(DefId(i));
                        break;
                    }
                }
                _ => {}
            }
        }

        if let Some(def_id) = target_def_id {
            self.call_by_id(def_id, args)
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
}
