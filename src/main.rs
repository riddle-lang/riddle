use codegen::Codegen;
use hir::lowering::AstLower;
use hir::module::HirModule;
use hir::pass::name_pass::NamePass;
use hir::pass::type_infer::TypeInfer;
use crate::frontend::ast::AstNodeKind::Program;

mod codegen;
mod frontend;
mod hir;
mod error;

fn main() {
    let code = r#"
        struct A{
            x: int
        }
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
            let a = A{x: 1};
            let t3 = ex(a).x;
            return printf("hello world %d", t3.x);
        }
    "#;

    let ast = match frontend::parser::parse(code) {
        Ok(ast) => ast,
        Err(e) => {
            e.report(code);
            return;
        }
    };

    let mut module = HirModule::new();

    let mut lower = AstLower::new(
        &mut module,
        match &ast.kind {
            Program(stmts) => stmts,
            _ => unreachable!(),
        },
    );

    if let Err(e) = lower.lower() {
        e.report(code);
        return;
    }

    let mut name_pass = NamePass::new(&mut module);
    if let Err(e) = name_pass.run() {
        e.report(code);
        return;
    }

    let mut type_infer = TypeInfer::new(&mut module);
    if let Err(e) = type_infer.infer() {
        e.report(code);
        return;
    }

    // println!("{:#?}", module);

    let mut codegen = Codegen::new();
    codegen.compile(&module);
    let bytes = codegen.finish();
    std::fs::write("out.o", bytes).unwrap();
}
