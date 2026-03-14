use crate::frontend::lexer::Lexer;

mod frontend;

fn main(){
    let src = r#"
        123
    "#;
    let mut lexer = Lexer::new(src, "2123");
    let t = lexer.lex();
    println!("{:?}", t);
}