pub mod compiler;
pub mod translator;

pub use compiler::Codegen;

pub fn mangle(name: &str, args: &[crate::hir::id::TyId]) -> String {
    if args.is_empty() {
        return name.to_string();
    }
    let mut res = name.to_string();
    res.push_str("__");
    for arg in args {
        res.push_str(&format!("_{}", arg.0));
    }
    res
}
