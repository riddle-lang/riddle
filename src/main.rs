use std::os::raw::c_int;
use crate::frontend::ast::AstNodeKind::Program;
use codegen::Codegen;
use hir::lowering::AstLower;
use hir::module::HirModule;
use hir::pass::name_pass::NamePass;
use hir::pass::type_infer::TypeInfer;
use std::process::Command;

mod codegen;
mod error;
mod frontend;
mod hir;

fn main() {
    let code = r#"
        extern "C" {
            fun printf(fmt: cstr, ...);
            fun riddle_gc_alloc(size: int) -> *int;
        }
        extern "C" fun riddle_vec_reserve(data: *int, len: int, cap: int, elem_size: int) -> *int;

        struct Vec<T> {
            data: *T,
            len: int,
            cap: int,
            elem_size: int
        }

        trait VecTrait<T> {
            fun push(self: Vec<T>, value: T) -> Vec<T>;
            fun get(self: Vec<T>, idx: int) -> T;
        }

        impl<T> VecTrait<T> for Vec<T> {
            fun push(self: Vec<T>, value: T) -> Vec<T> {
                let p_raw = riddle_vec_reserve(self.data as *int, self.len, self.cap, self.elem_size);
                let out = p_raw as Vec<T>;
                out.data[out.len] = value;
                out.len = out.len + 1;
                return out;
            }

            fun get(self: Vec<T>, idx: int) -> T {
                return self.data[idx];
            }
        }

        fun vec_new<T>(cap: int, elem_size: int) -> Vec<T> {
            let raw = riddle_gc_alloc(cap * elem_size);
            return Vec {
                data: raw as *T,
                len: 0,
                cap: cap,
                elem_size: elem_size
            };
        }

        fun main() -> int {
            var v: Vec<int> = vec_new(2, 8);
            v = v.push(100);
            v = v.push(200);
            v = v.push(300);
            v = v.push(400);
            v = v.push(500);

            let x = v.get(1);
            printf("v[1]: %d\n" as cstr, x as cint);

            let y = v.get(4);
            printf("v[4]: %d\n" as cstr, y as cint);

            printf("Success\n" as cstr);

            return x + y;
        }
    "#;

    let ast = match frontend::parser::parse(&code) {
        Ok(ast) => ast,
        Err(e) => {
            e.report(&code);
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
        e.report(&code);
        return;
    }

    let mut name_pass = NamePass::new(&mut module);
    if let Err(e) = name_pass.run() {
        e.report(&code);
        return;
    }

    let mut type_infer = TypeInfer::new(&mut module);
    if let Err(e) = type_infer.infer() {
        e.report(&code);
        return;
    }

    type_infer.finalize_types();

    // println!("{:#?}", module);

    let mut codegen = Codegen::new();
    codegen.compile(&module);
    let bytes = codegen.finish();
    std::fs::write("out.o", bytes).unwrap();
    Command::new("gcc")
        .arg("out.o")
        .arg("resources/runtime/runtime.c")
        .arg("-o")
        .arg("out.exe")
        .status()
        .unwrap();
}
