use std::io::stderr;
use std::process::exit;
use std::{env, fs, io::{stdin, Read}, process};

pub mod config;
use ast::Program;
use ast_passes::{perform_identification, perform_typechecking};
use codegen::target::MaplTarget;
use config::{Config, Target};
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

fn fail(n: usize) -> ! {
    println!("Compilation failed with {n} error{}",
        if n > 1 { "s" } else { "" }
    );
    exit(1);
}

pub fn parse(tokens: Box<[Token]>, src: &str) -> Program {
    Parser::new(&tokens, src).parse().unwrap_or_else(|nerr| {
        fail(nerr as usize)
    })
}

fn process(text: &str, target: Target) -> String {
    /* println!("*** LEXER ***"); */
    let tokens = tokenize(text);
    /* tokens.iter().for_each(|t| println!("{t:#?}")); */
    /* println!("\n*** PARSER ***"); */
    let program = parse(tokens, text);

    if let Err(em) = perform_identification(&program) {
        em.print_errors(&mut stderr().lock()).unwrap();
        fail(em.n_errors())
    }

    if let Err(em) = perform_typechecking(&program) {
        em.print_errors(&mut stderr().lock()).unwrap();
        fail(em.n_errors())
    }

    #[cfg(debug_assertions)]
    eprintln!("\
================================================================================
{program:#?}
================================================================================");

    match target {
        Target::Mapl => codegen::process::<MaplTarget>(&program)
    }

}

fn main() {
    let conf = Config::parse(env::args());
    for file in conf.files() {
        let text = fs::read_to_string(file).unwrap_or_else(|err| {
            eprintln!("Error reading \"{file}\": {err}");
            process::exit(1);
        });
        let out = process(&text, conf.target());
        let ext = file.char_indices().rev().find(|&(_,c)| c == '.').map(|(i,_)| i).unwrap_or(0);
        let start = &file[..ext];
        let fname = format!("{start}.out");
        fs::write(&fname, out).unwrap();
        println!("Program written to {fname}");
    }
    if conf.files().is_empty() {
        let mut text = String::new();
        stdin().read_to_string(&mut text).unwrap_or_else(|err| {
            eprintln!("Error reading stdin: {err}");
            exit(1)
        });
        let out = process(&text, conf.target());
        println!("{out}");
    }
}

