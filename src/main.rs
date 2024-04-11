pub mod lexer;
pub mod parser;
use std::{fs, process::exit};
use crate::parser::Parser;
use crate::lexer::Lexer;

fn main() {
    let text = fs::read_to_string("expr.txt").expect("Could not read file");
    let mut lexer = Lexer::new();
    let tokens = lexer.tokenize(&text);

    println!("*** LEXER ***");
    if lexer.has_errors() {
        println!("Number of errors: {}", lexer.n_errors());
        exit(1);
    }

    tokens.iter().for_each(|t| t.print());

    let mut parser = Parser::new(tokens);
    println!("\n*** PARSER ***");
    if parser.has_errors() {
        println!("Number of errors: {}", parser.n_errors());
        exit(1);
    }
    while !parser.is_finished() {
        parser.parse().print();
        print!("\n");
    }
}
