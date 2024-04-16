use std::{env, fs, io::{stdin, Read}};

use compiler::{parse, tokenize};

pub mod config;
use config::Config;

fn process(text: &str) {
    println!("*** LEXER ***");
    let tokens = tokenize(text);
    tokens.iter().for_each(|t| t.print());
    println!("\n*** PARSER ***");
    let program = parse(tokens);
    for expr in program.get_expressions() {
        expr.print();
        println!(" = {}", expr.eval());
    }
}

fn main() {
    let conf = Config::parse(env::args());
    for file in conf.files() {
        let text = fs::read_to_string(file).expect("Could not read file");
        process(&text);
    }
    if conf.files().is_empty() {
        let mut text = String::new();
        stdin().read_to_string(&mut text).expect("Error reading from stdin");
        process(&text);
    }
}
