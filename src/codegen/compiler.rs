use crate::codegen::translator::FunctionTranslator;
use crate::hir::id::DefId;
use crate::hir::items::{HirExternFunc, HirFunc, HirItem};
use crate::hir::module::HirModule;
use cranelift::codegen::ir::UserFuncName;
use cranelift::prelude::isa::CallConv;
use cranelift::prelude::*;
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::collections::HashMap;

pub struct Codegen {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    module: ObjectModule,
    fn_ids: HashMap<DefId, FuncId>,
}

impl Codegen {
    pub fn new() -> Self {
        let flag_builder = settings::builder();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let builder = ObjectBuilder::new(
            isa,
            "riddle".to_string(),
            cranelift_module::default_libcall_names(),
        )
        .unwrap();
        let module = ObjectModule::new(builder);
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

                    let fn_id = self
                        .module
                        .declare_function(&func.name, Linkage::Export, &sig)
                        .unwrap();
                    self.fn_ids.insert(func.id, fn_id);
                }
                HirItem::ExternFunc(func) => {
                    let mut sig = self.module.make_signature();
                    // 处理 ABI
                    if let Some(abi) = &func.abi {
                        match abi.as_str() {
                            "C" => sig.call_conv = CallConv::SystemV,
                            "C++" => sig.call_conv = CallConv::SystemV,
                            _ => {}
                        }
                    }

                    for _ in &func.param {
                        sig.params.push(AbiParam::new(types::I64));
                    }
                    sig.returns.push(AbiParam::new(types::I64));

                    let fn_id = self
                        .module
                        .declare_function(&func.name, Linkage::Import, &sig)
                        .unwrap();
                    self.fn_ids.insert(func.id, fn_id);
                    self.print_extern_decl(func, &sig);
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
    }

    fn print_extern_decl(&self, func: &HirExternFunc, sig: &Signature) {
        let params = sig
            .params
            .iter()
            .map(|p| format!("{}", p.value_type))
            .collect::<Vec<_>>()
            .join(", ");
        let returns = if sig.returns.is_empty() {
            "void".to_string()
        } else {
            sig.returns
                .iter()
                .map(|r| format!("{}", r.value_type))
                .collect::<Vec<_>>()
                .join(", ")
        };
        let abi = func.abi.as_deref().unwrap_or("default");
        println!(
            "declare extern {:?} {}({}) -> {}",
            abi, func.name, params, returns
        );
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
        let pretty = format!("{}#{}", func.name, fn_id.as_u32());
        builder.func.name = UserFuncName::testcase(pretty);

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
        self.module
            .define_function(fn_id, &mut self.ctx)
            .expect("Failed to define function");
        self.module.clear_context(&mut self.ctx);
    }

    pub fn finish(self) -> Vec<u8> {
        let product = self.module.finish();
        product.emit().unwrap()
    }
}
