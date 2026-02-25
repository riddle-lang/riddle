mod frontend;

use lalrpop_util::lalrpop_mod;
use rayon::prelude::*;

lalrpop_mod!(parser, "/frontend/parser.rs");

use lalrpop_util::ParseError;
use crate::frontend::ast::Module;
use crate::frontend::lexer::{LexError, Lexer, Token};

type PErr = ParseError<usize, Token, LexError>;

fn parse_one(src: &str) -> Result<Module, PErr> {
    let lexer = Lexer::new(src);
    parser::ModuleParser::new().parse(lexer)
}

fn main() {
    let sources = vec![
        ("a.lang", "let x = 1 + 2 * 3; let y = x + 4;"),
        ("b.lang", "let n = (2 + 3) * 10;"),
        ("bad.lang", "let = 1;"),
    ];

    let results: Vec<_> = sources
        .par_iter() // Rayon 并行
        .map(|(name, src)| (*name, parse_one(src)))
        .collect();

    for (name, result) in results {
        match result {
            Ok(module) => println!("[OK] {name}\n{module:#?}\n"),
            Err(e) => println!("[ERR] {name}\n{e:?}\n"),
        }
    }
}
