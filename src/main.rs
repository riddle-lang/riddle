extern crate core;

use crate::frontend::ast::AstNode::Program;
use crate::hir::lowering::AstLower;
use crate::hir::name_pass::NamePass;
use hir::module::HirModule;

mod hir;
mod frontend;

fn main() {
    let code = r#"
        fun add(x: int, y: int) -> int {
            return x + y;
        }
    "#;

    let ast = frontend::parser::parse(code).unwrap();

    println!("{:#?}", ast);

    let mut module = HirModule::new();

    let mut lower = AstLower::new(&mut module, match &ast {
        Program(stmts) => stmts,
        _ => unreachable!(),
    });

    lower.lower().unwrap();
    
    let mut name_pass = NamePass::new(&mut module);
    name_pass.run();
    
    println!("{:#?}", module);
}
