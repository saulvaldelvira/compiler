pub mod ast;
pub mod error;

use crate::lexer::token::{Token, TokenType};

use self::{
    ast::expr::{self, Binary, Literal, Unary},
    error::ParseError
};

type Expr = Box<dyn expr::Expr>;

type Result<T> = std::result::Result<T,ParseError>;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    n_errors: u32,
}

impl Parser {
    /* PUBLIC */
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {tokens, current:0, n_errors:0}
    }
    pub fn parse(&mut self) -> Expr {
        self.expression().unwrap()
    }
    pub fn has_errors(&self) -> bool { self.n_errors > 0 }
    pub fn n_errors(&self) -> u32 { self.n_errors }
    /* PRIVATE */
    fn expression(&mut self) -> Result<Expr> {
        let mut expr = self.equality()?;
        if self.match_type(&[TokenType::Comma]) {
            let op = self.previous().take();
            let right = self.expression()?;
            expr = Binary::new(expr, op, right).as_box();
        }
        Ok(expr)
    }
    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;
        while self.match_type(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let op = self.previous().take();
            let right = self.comparison()?;
            expr = Binary::new(expr, op, right).as_box();
        }
        Ok(expr)
    }
    fn comparison(&mut self) -> Result<Expr> {
        let mut expr = self.term()?;

        while self.match_type(&[TokenType::Greater,
                                TokenType::GreaterEqual,
                                TokenType::Less,
                                TokenType::LessEqual]
                             ){
            let op = self.previous().take();
            let right = self.term()?;
            expr = Binary::new(expr,op,right).as_box();
        }
        Ok(expr)
    }
    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;

        while self.match_type(&[TokenType::Minus,TokenType::Plus]){
            let op = self.previous().take();
            let right = self.factor()?;
            expr = Binary::new(expr,op,right).as_box();
        }
        Ok(expr)
    }
    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;

        while self.match_type(&[TokenType::Slash,TokenType::Star]){
            let op = self.previous().take();
            let right = self.unary()?;
            expr = Binary::new(expr,op,right).as_box();
        }
        Ok(expr)
    }
    fn unary(&mut self) -> Result<Expr> {
        if self.match_type(&[TokenType::Bang,TokenType::Minus]) {
            let op = self.previous().take();
            let expr = self.unary()?;
            return Ok(Unary::new(op, expr).as_box());
        }
        self.primary()
    }
    fn primary(&mut self) -> Result<Expr> {
        if self.match_type(&[TokenType::False]) {
            return Ok(Literal::new(false).as_box());
        }
        if self.match_type(&[TokenType::True]) {
            return Ok(Literal::new(true).as_box());
        }
        if self.match_type(&[TokenType::Nil]) {
            return Ok(Literal::nil().as_box());
        }
        if self.match_type(&[TokenType::Number,TokenType::String]) {
            return Ok(Literal::new(self.previous().take_lexem()).as_box());
        }
        if self.match_type(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen, "Expected ')' after expression.")?;
            return Ok(expr);
        }
        ParseError::from_string(
             format!("Expected literal, found: {}", self.peek().get_lexem()))
    }
    fn consume(&mut self, t: TokenType, msg: &'static str) -> Result<&mut Token> {
        if self.check(t) { return Ok(self.advance()); }
        ParseError::from_str(msg)
    }
    fn match_type(&mut self, types: &[TokenType]) -> bool {
        for t in types {
            if self.check(*t) {
                self.advance();
                return true;
            }
        }
        false
    }
    fn check(&mut self, t: TokenType) -> bool {
        if self.is_finished() { return false; }
        self.peek().get_type() == t
    }
    fn advance(&mut self) -> &mut Token {
        if !self.is_finished() {
            self.current += 1;
        }
        self.previous()
    }
    pub fn is_finished(&self) -> bool {
        self.current >= self.tokens.len()
    }
    fn peek(&self) -> &Token {
        self.tokens.get(self.current)
                   .expect("Index should be valid when calling peek")
    }
    fn previous(&mut self) -> &mut Token {
        self.tokens.get_mut(self.current - 1)
                   .expect("Invalid index when calling previous")
    }
    fn synchronize(&mut self) {
        while !self.is_finished() {
            self.advance();
            if self.previous().get_type() == TokenType::Semicolon {
                return;
            }

            match self.peek().get_type() {
                TokenType::Class | TokenType::Fun | TokenType::Var   |
                TokenType::For   | TokenType::If  | TokenType::While |
                TokenType::Print | TokenType::Return => return,
                _ => {},
            }
        }
    }
    fn error(&mut self, err: &str) {
        eprintln!("Parser: [{},{}] {err}", self.peek().get_start(), self.peek().get_end());
        self.n_errors += 1;
    }
}
