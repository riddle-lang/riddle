fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/frontend/parser.lalrpop");

    lalrpop::process_root().unwrap();
}
