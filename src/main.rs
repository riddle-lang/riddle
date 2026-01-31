extern crate core;

use hir::type_expr::HirTypeExprKind;
use hir::builder::HirBuilder;
use hir::module::HirModule;
use hir::type_expr::HirTypeExpr;
use hir::types::HirType;

mod hir;
mod frontend;

fn main() {
    let code = r#"fun main() { x = 10; 10 }"#;

    let ast = frontend::parser::parse(code).unwrap();

    println!("{:#?}", ast);

    let mut module = HirModule::new();

    let mut builder = HirBuilder::new(&mut module);
    let int_ty = builder.create_type(HirType::Int(32));
    let main_id = builder.create_func("main", int_ty).unwrap();
    builder.set_func(main_id);

    let int_ty_expr = builder.create_type_expr(HirTypeExpr {
        kind: HirTypeExprKind::Path("int".to_string())
    });
    builder.local("x", Some(int_ty_expr), None).unwrap();
    
    println!("{:#?}", module);
}
