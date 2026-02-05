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
        enum Color {
            Red,
            Green,
            Blue,
            Rgb(int, int, int),
            Custom { r: int, g: int, b: int }
        }

        fun test_enum(c: Color) -> int {
            return 0;
        }

        struct Foo{
            x: int
        }

        extern "C" fun test1(x: int, y: int) -> int;

        trait TestTrait {
            fun test(x: int, y: int) -> int;
        }

        impl TestTrait for Foo {
            fun test(x: int, y: int) -> int {
                return (x * y) - (x / y);
            }
        }

        fun test(x: int, y: int) -> int {
            return (x * y) - (x / y);
        }
        
        fun main() -> int {
            let x: Foo = Foo { x: 1 };
            let t = Color::Rgb(1, 2, 3);
            return x.test(1, 2);
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
