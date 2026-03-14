use crate::frontend::lexer::Lexer;

mod frontend;

fn main(){
    let src = r#"
        123id
    "#;
    let mut lexer = Lexer::new(src, "main.rid");
    let t = lexer.lex();
    match t {
        Ok(o) => {
            println!("{:?}", o);
        }
        Err(e) => {
            println!("{}", e);
        }
    }
}