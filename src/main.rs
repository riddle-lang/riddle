use codegen::Codegen;
use frontend::ast::AstNode::Program;
use hir::lowering::AstLower;
use hir::module::HirModule;
use hir::pass::name_pass::NamePass;
use hir::pass::type_infer::TypeInfer;

mod codegen;
mod frontend;
mod hir;

fn main() {
    let code = r#"
        extern "C" {
            fun printf(fmt: &str, ...) -> int;
        }

        fun main() -> int {
            printf("hello %d", 42);
            return printf("hello world");
        }
    "#;

    let ast = frontend::parser::parse(code).unwrap();

    let mut module = HirModule::new();

    let mut lower = AstLower::new(
        &mut module,
        match &ast {
            Program(stmts) => stmts,
            _ => unreachable!(),
        },
    );

    lower.lower().unwrap();

    let mut name_pass = NamePass::new(&mut module);
    name_pass.run();

    let mut type_infer = TypeInfer::new(&mut module);
    type_infer.infer().unwrap();

    // println!("{:#?}", module);

    let mut codegen = Codegen::new();
    codegen.compile(&module);
    let bytes = codegen.finish();
    std::fs::write("out.o", bytes).unwrap();
}
