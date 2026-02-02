extern crate core;

use crate::frontend::ast::AstNode::Program;
use crate::hir::lowering::AstLower;
use hir::module::HirModule;
use hir::pass::type_infer::TypeInfer;
use crate::hir::pass::name_pass::NamePass;

mod hir;
mod frontend;

fn main() {
    let code = r#"
        fun add(x: int, y: int) -> int {
            return x + y;
        }
        
        fun main() {
            var a = 1;
            var b = 2;
            var c = add(a, b);
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
    
    println!("{:#?}", module);
}
