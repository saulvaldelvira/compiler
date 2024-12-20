use std::process::exit;
use std::{env, fs, io::{stdin, Read}, process};

pub mod config;
use ast::Program;
use config::Config;
use interpreter::Interpreter;
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
    /* tokens.iter().for_each(|t| t.print()); */
    /* println!("\n*** PARSER ***"); */
    let program = parse(tokens, text);
    let interpreter = Interpreter::new();
    let mut interpreter = interpreter;
    println!("\
================================= Executing  ===================================
{program:#?}
================================================================================");
    interpreter.interpret(&program);
}

fn main() {
    let conf = Config::parse(env::args());
    for file in conf.files() {
        let text = fs::read_to_string(file).unwrap_or_else(|err| {
            eprintln!("Could not read file \"{file}\": {err}");
            process::exit(1);
        });
        process(&text);
    }
    if conf.files().is_empty() {
        let mut text = String::new();
        stdin().read_to_string(&mut text).expect("Error reading from stdin");
        process(&text);
    }
}
