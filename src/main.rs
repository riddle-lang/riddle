extern crate core;

use crate::frontend::ast::AstNode::Program;
use crate::hir::lowering::AstLower;
use hir::module::HirModule;

mod hir;
mod frontend;

fn main() {
    let code = r#"
        fun main() { x = 10; x(1) }
        var a = 1;
    "#;

    let ast = frontend::parser::parse(code).unwrap();

    println!("{:#?}", ast);

    let mut module = HirModule::new();

    let mut lower = AstLower::new(&mut module, match &ast {
        Program(stmts) => stmts,
        _ => unreachable!(),
    });

    lower.lower().unwrap();
    
    println!("{:#?}", module);
}
