use std::process::exit;
use std::{env, fs, process};

use ast::Program;
use ast_passes::{perform_identification, perform_typechecking};
use lexer::token::Token;
use lexer::Lexer;
use parser::Parser;

pub fn tokenize(text: &str) -> Box<[Token]> {
    Lexer::new(text).tokenize().unwrap_or_else(|nerr| {
        println!("Compilation failed with {nerr} error{}",
            if nerr > 1 { "s" } else { "" }
        );
        exit(1);
    })
}

pub fn parse(tokens: Box<[Token]>, src: &str) -> Program {
    Parser::new(&tokens, src).parse().unwrap_or_else(|nerr| {
        println!("Compilation failed with {nerr} error{}",
            if nerr > 1 { "s" } else { "" }
        );
        exit(1);
    })
}

fn process(text: &str) {
    /* println!("*** LEXER ***"); */
    let tokens = tokenize(text);
    /* tokens.iter().for_each(|t| println!("{t:#?}")); */
    /* println!("\n*** PARSER ***"); */
    let program = parse(tokens, text);

    if perform_identification(&program).is_err() {
        return
    }

    if perform_typechecking(&program).is_err() {
        return
    }

    println!("{program:#?}")
}

fn main() {
    let f = env::args().nth(1).unwrap_or_else(|| "/dev/stdin".to_string());
    let text = fs::read_to_string(&f).unwrap_or_else(|err| {
        eprintln!("Error reading \"{f}\": {err}");
        process::exit(1);
    });
    process(&text);
}


