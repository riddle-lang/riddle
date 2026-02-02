use std::collections::HashMap;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module, FuncId};
use crate::hir::module::HirModule;
use crate::hir::id::{DefId, LocalId, ExprId, StmtId};
use crate::hir::items::{HirItem, HirFunc};
use crate::hir::expr::{HirExprKind, HirLiteral};
use crate::hir::stmt::HirStmtKind;

pub struct Codegen {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    module: JITModule,
    fn_ids: HashMap<DefId, FuncId>,
}

impl Codegen {
    pub fn new() -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names()).unwrap();
        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
            fn_ids: HashMap::new(),
        }
    }

    pub fn compile(&mut self, hir: &HirModule) {
        // 1. 声明所有函数
        for item in &hir.items {
            if let HirItem::Func(func) = item {
                let mut sig = self.module.make_signature();
                for _ in &func.param {
                    sig.params.push(AbiParam::new(types::I64));
                }
                sig.returns.push(AbiParam::new(types::I64));

                let fn_id = self.module
                    .declare_function(&func.name, Linkage::Export, &sig)
                    .unwrap();
                self.fn_ids.insert(func.id, fn_id);
            }
        }

        // 2. 定义函数体
        let items = hir.items.clone();
        for item in &items {
            if let HirItem::Func(func) = item {
                self.compile_func(func, hir);
            }
        }

        self.module.finalize_definitions().unwrap();
    }

    fn compile_func(&mut self, func: &HirFunc, hir: &HirModule) {
        let fn_id = self.fn_ids[&func.id];
        
        let mut sig = self.module.make_signature();
        for _ in &func.param {
            sig.params.push(AbiParam::new(types::I64));
        }
        sig.returns.push(AbiParam::new(types::I64));
        self.ctx.func.signature = sig;

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let mut translator = FunctionTranslator {
            builder,
            module: &mut self.module,
            fn_ids: &self.fn_ids,
            variables: HashMap::new(),
            hir,
        };

        for (i, param) in func.param.iter().enumerate() {
            let val = translator.builder.block_params(entry_block)[i];
            let var = Variable::new(param.id.0);
            translator.builder.declare_var(var, types::I64);
            translator.builder.def_var(var, val);
            translator.variables.insert(param.id, var);
        }

        for stmt_id in &func.body {
            translator.translate_stmt(*stmt_id);
        }

        translator.builder.finalize();
        self.module.define_function(fn_id, &mut self.ctx).expect("Failed to define function");
        self.module.clear_context(&mut self.ctx);
    }

    pub fn get_fn_ptr(&mut self, name: &str) -> *const u8 {
        let fn_id = self.module.get_name(name).expect("Function not found");
        if let cranelift_module::FuncOrDataId::Func(id) = fn_id {
            self.module.get_finalized_function(id)
        } else {
            panic!("Not a function")
        }
    }
}

struct FunctionTranslator<'a> {
    builder: FunctionBuilder<'a>,
    module: &'a mut JITModule,
    fn_ids: &'a HashMap<DefId, FuncId>,
    variables: HashMap<LocalId, Variable>,
    hir: &'a HirModule,
}

impl<'a> FunctionTranslator<'a> {
    fn translate_stmt(&mut self, stmt_id: StmtId) {
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

    fn translate_expr(&mut self, expr_id: ExprId) -> Value {
        let expr = &self.hir.exprs[expr_id.0];
        match &expr.kind {
            HirExprKind::Literal(lit) => {
                match lit {
                    HirLiteral::Int(i) => self.builder.ins().iconst(types::I64, *i),
                    _ => unimplemented!("Literal type not supported yet"),
                }
            }
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
                if let HirExprKind::Symbol { name, .. } = &callee_expr.kind {
                    let mut arg_vals = Vec::new();
                    for arg in args {
                        arg_vals.push(self.translate_expr(*arg));
                    }
                    self.resolve_fn_call(name, &arg_vals)
                } else {
                    unimplemented!("Indirect calls not supported yet")
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
        }
    }

    fn resolve_fn_call(&mut self, name: &str, args: &[Value]) -> Value {
        let mut target_fn_id = None;
        for item in &self.hir.items {
            if let HirItem::Func(f) = item {
                if f.name == name {
                    target_fn_id = Some(self.fn_ids[&f.id]);
                    break;
                }
            }
        }

        if let Some(fn_id) = target_fn_id {
            let local_func = self.module.declare_func_in_func(fn_id, &mut self.builder.func);
            let call = self.builder.ins().call(local_func, args);
            self.builder.inst_results(call)[0]
        } else {
            panic!("Function {} not found", name);
        }
    }
}
