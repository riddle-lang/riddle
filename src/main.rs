use codegen::Codegen;
use frontend::ast::AstNode::Program;
use hir::lowering::AstLower;
use hir::pass::name_pass::NamePass;
use hir::module::HirModule;
use hir::pass::type_infer::TypeInfer;

mod codegen;
mod frontend;
mod hir;

fn main() {
    let code = r#"
        struct Box<T> {
            inner: T
        }

        fun get_inner<T>(b: Box<T>) -> T {
            return b.inner;
        }

        fun main() -> int {
            let b = Box { inner: 1 };
            return get_inner(b);
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
