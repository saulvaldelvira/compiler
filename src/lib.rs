use std::process::exit;
use lexer::token::Token;
use lexer::Lexer;
use ast::Program;
use parser::Parser;

pub fn tokenize(text: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(text);
    let tokens = lexer.tokenize();
    if lexer.has_errors() {
        println!("Compilation failed with {} error{}",
                  lexer.n_errors(),
                  if lexer.n_errors() > 1 { "s" } else { "" }
        );
        exit(1);
    }
    tokens
}

pub fn parse(tokens: Vec<Token>) -> Program {
    let mut parser = Parser::new(tokens);
    let program = parser.parse();
    if parser.has_errors() {
        println!("Compilation failed with {} error{}",
                  parser.n_errors(),
                  if parser.n_errors() > 1 { "s" } else { "" }
        );
        exit(1);
    }
    program
}
