pub mod ast;
pub mod error;

use crate::lexer::token::{Token, TokenType};

use self::{
    ast::{expr::{Binary, Expr, Literal, Ternary, Unary}, Program},
    error::ParseError
};

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
    pub fn parse(&mut self) -> Program {
        let mut exprs = Vec::new();
        while !self.is_finished() {
            if let Some(expr) = self.statement() {
                exprs.push(expr);
            }
        }
        Program::new(exprs)
    }
    pub fn has_errors(&self) -> bool { self.n_errors > 0 }
    pub fn n_errors(&self) -> u32 { self.n_errors }
    /* PRIVATE */
    fn statement(&mut self) -> Option<Expr> {
        match self.expression() {
                Err(e) => {
                    self.error(e.get_message());
                    if self.synchronize() {
                        self.statement()
                    }else {
                        None
                    }
                },
                Ok(expr) => Some(expr),
            }
    }
    fn expression(&mut self) -> Result<Expr> {
        let mut expr = self.ternary()?;
        if self.match_type(&[TokenType::Comma]) {
            let op = self.previous()?.take();
            let right = self.expression()?;
            expr = Binary::new(expr, op, right).as_box();
        }
        Ok(expr)
    }
    fn ternary(&mut self) -> Result<Expr> {
        let mut expr = self.equality()?;
        if self.match_type(&[TokenType::Question]) {
            let if_true = self.equality()?;
            self.consume(TokenType::Colon, "Expected ':'")?;
            let if_false = self.equality()?;
            expr = Ternary::new(expr, if_true, if_false).as_box();
        }
        Ok(expr)
    }
    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;
        while self.match_type(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let op = self.previous()?.take();
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
            let op = self.previous()?.take();
            let right = self.term()?;
            expr = Binary::new(expr,op,right).as_box();
        }
        Ok(expr)
    }
    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;

        while self.match_type(&[TokenType::Minus,TokenType::Plus]){
            let op = self.previous()?.take();
            let right = self.factor()?;
            expr = Binary::new(expr,op,right).as_box();
        }
        Ok(expr)
    }
    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;

        while self.match_type(&[TokenType::Slash,TokenType::Star]){
            let op = self.previous()?.take();
            let right = self.unary()?;
            expr = Binary::new(expr,op,right).as_box();
        }
        Ok(expr)
    }
    fn unary(&mut self) -> Result<Expr> {
        if self.match_type(&[TokenType::Bang,TokenType::Minus]) {
            let op = self.previous()?.take();
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
            return Ok(Literal::new(self.previous()?.take_lexem()).as_box());
        }
        if self.match_type(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen, "Expected ')' after expression.")?;
            return Ok(expr);
        }
        ParseError::from_string(
             format!("Expected literal, found: {}", self.peek()?.get_lexem())).err()
    }
    fn consume(&mut self, t: TokenType, msg: &'static str) -> Result<&mut Token> {
        if self.check(t) { return Ok(self.advance()?); }
        ParseError::from_str(msg).err()
    }
    fn match_type(&mut self, types: &[TokenType]) -> bool {
        for t in types {
            if self.check(*t) {
                self.advance().unwrap();
                return true;
            }
        }
        false
    }
    fn check(&mut self, t: TokenType) -> bool {
        if self.is_finished() { return false; }
        self.peek().unwrap().get_type() == t
    }
    fn advance(&mut self) -> Result<&mut Token> {
        if !self.is_finished() {
            self.current += 1;
        }
        self.previous()
    }
    pub fn is_finished(&self) -> bool {
        self.current >= self.tokens.len()
    }
    fn peek(&mut self) -> Result<&mut Token> {
        self.tokens.get_mut(self.current)
                   .ok_or_else(|| ParseError::from_str("Index should be valid when calling peek"))
    }
    fn previous(&mut self) -> Result<&mut Token> {
        self.tokens.get_mut(self.current - 1)
                   .ok_or_else(|| ParseError::from_str("Index should be valid when calling peek"))
    }
    fn synchronize(&mut self) -> bool {
        if self.is_finished() { return false; }
        loop {
            if self.advance().unwrap().get_type() == TokenType::Semicolon {
                return true;
            }
            if self.is_finished() { return false; }
            match self.peek().unwrap().get_type() {
                TokenType::Class | TokenType::Fun | TokenType::Var   |
                TokenType::For   | TokenType::If  | TokenType::While |
                TokenType::Print | TokenType::Return => return true,
                _ => {},
            }
        }
    }
    fn error(&mut self, err: &str) {
        let tok = if self.is_finished() {
            self.previous()
        } else {
            self.peek()
        }.unwrap();
        eprintln!("Parser: [{},{}] {err}", tok.get_start(), tok.get_end());
        self.n_errors += 1;
    }
}
