extern crate core;

use crate::frontend::ast::AstNode::Program;
use crate::hir::lowering::AstLower;
use hir::module::HirModule;
use hir::pass::type_infer::TypeInfer;
use crate::hir::pass::name_pass::NamePass;

mod hir;
mod frontend;
mod codegen;

use crate::codegen::Codegen;

fn main() {
    let code = r#"
        fun test(x: int, y: int) -> int {
            return (x * y) - (x / y);
        }
        
        fun main() -> int {
            return test(10, 2);
        }
    "#;

    let ast = frontend::parser::parse(code).unwrap();

    let mut module = HirModule::new();

    let mut lower = AstLower::new(&mut module, match &ast {
        Program(stmts) => stmts,
        _ => unreachable!(),
    });

    lower.lower().unwrap();
    
    let mut name_pass = NamePass::new(&mut module);
    name_pass.run();
    
    let mut type_infer = TypeInfer::new(&mut module);
    type_infer.infer().unwrap();
    
    // println!("{:#?}", module);

    let mut codegen = Codegen::new();
    codegen.compile(&module);

    let main_ptr = codegen.get_fn_ptr("main");
    let main_fn: fn() -> i64 = unsafe { std::mem::transmute(main_ptr) };
    let result = main_fn();
    println!("Result: {}", result);
}
