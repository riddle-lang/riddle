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
        fun ex<T>(x: T)->T{
            return x;
        }
        extern "C" {
            fun printf(fmt: &str, ...) -> int;
        }

        fun main() -> int {
            printf("hello %d", 42);
            let t = ex(1);
            let t2 = ex("str");
            return printf("hello world %d", t);
        }
    "#;

    let ast = match frontend::parser::parse(code) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Parse error:\n{}", e);
            return;
        }
    };

    let mut module = HirModule::new();

    let mut lower = AstLower::new(
        &mut module,
        match &ast {
            Program(stmts) => stmts,
            _ => unreachable!(),
        },
    );

    if let Err(e) = lower.lower() {
        eprintln!("Lowering error: {}", e);
        return;
    }

    let mut name_pass = NamePass::new(&mut module);
    name_pass.run();

    let mut type_infer = TypeInfer::new(&mut module);
    if let Err(e) = type_infer.infer() {
        eprintln!("Type inference error: {}", e);
        return;
    }

    // println!("{:#?}", module);

    let mut codegen = Codegen::new();
    codegen.compile(&module);
    let bytes = codegen.finish();
    std::fs::write("out.o", bytes).unwrap();
}
