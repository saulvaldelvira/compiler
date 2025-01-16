use std::io::{stdout, Write};
use std::process::exit;
use std::{env, fs, io::{stdin, Read}, process};

pub mod config;
use ast::{Program, AST};
use ast_passes::{perform_identification, print_ast};
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

fn make_ast(text: &str) -> Program {
    let tokens = tokenize(text);
    parse(tokens, text)
}

fn compile(text: &str, _conf: &Config) -> utils::Result<()> {
    let program = make_ast(text);

    perform_identification(&program).map_err(|err| {
        format!("Identification: {err} errors")
    })?;

    let interpreter = Interpreter::new();
    let mut interpreter = interpreter;
    #[cfg(debug_assertions)]
    println!("\
================================= Executing  ===================================
{program:#?}
================================================================================");
    interpreter.interpret(&program);
    Ok(())
}

fn print(text: &str, _conf: &Config) -> utils::Result<()> {
    let program = make_ast(text);
    let mut buf = String::new();
    print_ast(&AST::Program(program), &mut buf)?;
    stdout().write_all(buf.as_bytes())?;
    Ok(())
}

fn process(text: &str, conf: &Config) {
    match conf.op() {
        config::Op::Compile => compile(text, conf),
        config::Op::Print => print(text, conf),
    }.unwrap_or_else(|err| {
        eprint!("ERROR: {err}");
        process::exit(1)
    })
}

fn main() {
    let conf = Config::parse(env::args());
    if conf.files().is_empty() {
        let mut text = String::new();
        stdin().read_to_string(&mut text).unwrap_or_else(|err| {
            eprintln!("Error reading stdin: {err}");
            exit(1)
        });
        process(&text, &conf);
    } else {
        for file in conf.files() {
            let text = fs::read_to_string(file).unwrap_or_else(|err| {
                eprintln!("Error reading \"{file}\": {err}");
                process::exit(1);
            });
            process(&text, &conf);
        }
    }
}

