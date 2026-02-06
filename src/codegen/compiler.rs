use crate::codegen::mangle;
use crate::codegen::translator::FunctionTranslator;
use crate::hir::id::{DefId, TyId};
use crate::hir::items::{HirExternFunc, HirFunc, HirItem};
use crate::hir::module::HirModule;
use cranelift::codegen::ir::UserFuncName;
use cranelift::prelude::*;
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::collections::HashMap;

pub struct Codegen {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    module: ObjectModule,
    fn_ids: HashMap<(DefId, Vec<TyId>), FuncId>,
    queue: Vec<(DefId, Vec<TyId>)>,
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
            queue: vec![],
        }
    }

    pub fn compile(&mut self, hir: &HirModule) {
        for (i, item) in hir.items.iter().enumerate() {
            match item {
                HirItem::Func(f) if f.generic_params.is_empty() => {
                    self.get_or_declare_fn(DefId(i), vec![], hir);
                }
                HirItem::ExternFunc(_) => {
                    self.get_or_declare_fn(DefId(i), vec![], hir);
                }
                _ => {}
            }
        }

        let mut compiled = std::collections::HashSet::new();
        while let Some(key) = self.queue.pop() {
            if compiled.contains(&key) {
                continue;
            }
            let (def_id, args) = key.clone();
            let func = match &hir.items[def_id.0] {
                HirItem::Func(f) => f.clone(),
                _ => continue,
            };
            self.compile_func(&func, &args, hir);
            compiled.insert(key);
        }
    }

    fn get_or_declare_fn(
        &mut self,
        def_id: DefId,
        args: Vec<TyId>,
        hir: &HirModule,
    ) -> FuncId {
        if let Some(&fn_id) = self.fn_ids.get(&(def_id, args.clone())) {
            return fn_id;
        }

        let item = &hir.items[def_id.0];
        let (name, is_extern) = match item {
            HirItem::Func(f) => (mangle(&f.name, &args), false),
            HirItem::ExternFunc(f) => (f.name.clone(), true),
            _ => panic!("Not a function"),
        };

        let mut sig = self.module.make_signature();
        if is_extern {
            sig.call_conv = self.module.isa().default_call_conv();
        }

        let ty_id = hir.item_types[&def_id];
        let param_count = match &hir.types[ty_id.0] {
            crate::hir::types::HirType::Func(sig_types, _) => sig_types.len() - 1,
            _ => 0,
        };

        for _ in 0..param_count {
            sig.params.push(AbiParam::new(types::I64));
        }
        sig.returns.push(AbiParam::new(types::I64));

        let linkage = if is_extern {
            Linkage::Import
        } else {
            Linkage::Export
        };
        let fn_id = self
            .module
            .declare_function(&name, linkage, &sig)
            .unwrap();

        self.fn_ids.insert((def_id, args.clone()), fn_id);
        if !is_extern {
            self.queue.push((def_id, args));
        }
        fn_id
    }

    fn print_extern_decl(&self, func: &HirExternFunc, sig: &Signature) {
        let mut params = sig
            .params
            .iter()
            .map(|p| format!("{}", p.value_type))
            .collect::<Vec<_>>()
            .join(", ");
        if func.is_variadic {
            if params.is_empty() {
                params = "...".to_string();
            } else {
                params.push_str(", ...");
            }
        }
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

    fn compile_func(
        &mut self,
        func: &HirFunc,
        generic_args: &[TyId],
        hir: &HirModule,
    ) {
        let fn_id = self.fn_ids[&(func.id, generic_args.to_vec())];

        let mut sig = self.module.make_signature();
        let ty_id = hir.item_types[&func.id];
        let param_count = match &hir.types[ty_id.0] {
            crate::hir::types::HirType::Func(sig_types, _) => sig_types.len() - 1,
            _ => 0,
        };
        for _ in 0..param_count {
            sig.params.push(AbiParam::new(types::I64));
        }
        sig.returns.push(AbiParam::new(types::I64));
        self.ctx.func.signature = sig;

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        let pretty = mangle(&func.name, generic_args);
        builder.func.name = UserFuncName::testcase(pretty);

        let mut translator = FunctionTranslator {
            builder,
            module: &mut self.module,
            fn_ids: &mut self.fn_ids,
            specialization_queue: &mut self.queue,
            variables: HashMap::new(),
            hir,
        };

        for (i, param) in func.param.iter().enumerate() {
            let val = translator.builder.block_params(entry_block)[i];
            let var = translator.builder.declare_var(types::I64);
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

