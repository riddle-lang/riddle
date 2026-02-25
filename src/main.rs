use crate::frontend::lexer::lex;

mod frontend;

fn main(){
    let src = "let x = 123@abc;";
    let _tokens = match lex("input.lang", src) {
        Ok(tokens) => println!("ok: {:?}", tokens),
        Err(e) => eprintln!("{:?}", e),
    };
}