pub mod lexer;
use std::{fs, process::exit};

use lexer::{token::Token, Lexer};

fn main() {
    let text = fs::read_to_string("input.txt").expect("Could not read file");
    let mut lexer = Lexer::new();
    let tokens = lexer.tokenize(&text);

    if lexer.has_errors() {
        println!("Number of errors: {}", lexer.n_errors());
        exit(1);
    }

    tokens.iter().for_each(Token::print);
}
