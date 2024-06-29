use std::process::exit;
use lexer::token::Token;
use lexer::Lexer;
use ast::Program;
use parser::Parser;

pub fn tokenize(text: &str) -> Vec<Token> {
    let mut lexer = Lexer::new();
    let tokens = lexer.tokenize(text);
    if lexer.has_errors() {
        println!("Number of errors: {}", lexer.n_errors());
        exit(1);
    }
    tokens
}

pub fn parse(tokens: Vec<Token>) -> Program {
    let mut parser = Parser::new(tokens);
    let program = parser.parse();
    if parser.has_errors() {
        println!("Number of errors: {}", parser.n_errors());
        exit(1);
    }
    program
}
