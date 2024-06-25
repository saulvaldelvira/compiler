use std::{env, fs, io::{stdin, Read}, process};

use compiler::{parse, tokenize};

pub mod config;
use config::Config;

fn process(text: &str) {
    println!("*** LEXER ***");
    let tokens = tokenize(text);
    tokens.iter().for_each(|t| t.print());
    println!("\n*** PARSER ***");
    let program = parse(tokens);
    for stmt in program.get_stmts() {
        stmt.execute();
    }
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
