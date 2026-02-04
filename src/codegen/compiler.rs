use std::collections::HashMap;
use cranelift::prelude::*;
use cranelift::prelude::isa::CallConv;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module, FuncId};
use crate::hir::module::HirModule;
use crate::hir::id::DefId;
use crate::hir::items::{HirItem, HirFunc};
use crate::codegen::translator::FunctionTranslator;

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
            match item {
                HirItem::Func(func) => {
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
                HirItem::ExternFunc(func) => {
                    let mut sig = self.module.make_signature();
                    // 处理 ABI
                    if let Some(abi) = &func.abi {
                        match abi.as_str() {
                            "C" => sig.call_conv = CallConv::SystemV, // 简化处理
                            "C++" => sig.call_conv = CallConv::SystemV, // 实际上在 Cranelift 中通常也是这个
                            _ => {}
                        }
                    }

                    for _ in &func.param {
                        sig.params.push(AbiParam::new(types::I64));
                    }
                    sig.returns.push(AbiParam::new(types::I64));

                    let fn_id = self.module
                        .declare_function(&func.name, Linkage::Import, &sig)
                        .unwrap();
                    self.fn_ids.insert(func.id, fn_id);
                }
                _ => {}
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
        println!("{}", self.ctx.func.display());
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
