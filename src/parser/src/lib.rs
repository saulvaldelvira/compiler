pub mod error;

use ast::{expr::LitValue, Stmt, Program};
use lexer::token::{Token, TokenType};

use ast::expr::{Expr,Expression};
use ast::stmt::Statement;
use ast::declaration::Declaration;
use self::error::ParseError;

use Expression::*;
use Statement::*;
use Declaration::*;

type Result<T> = std::result::Result<T,ParseError>;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    n_errors: u32,
}

impl Parser {
    /* PUBLIC */
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {tokens, current:0, n_errors:0 }
    }
    pub fn parse(&mut self) -> Program {
        let mut stmts = Vec::new();
        while !self.is_finished() {
            match self.declaration() {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => {
                    self.error(e.get_message());
                    self.synchronize();
                }
            }
        }
        Program::new(stmts)
    }
    pub fn has_errors(&self) -> bool { self.n_errors > 0 }
    pub fn n_errors(&self) -> u32 { self.n_errors }
    /* PRIVATE */
    fn declaration(&mut self) -> Result<Stmt> {
        if self.match_type(TokenType::Var) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }
    fn var_declaration(&mut self) -> Result<Stmt> {
        let name = self.consume(TokenType::Identifier, "Expected variable name.")?.take_lexem();
        let mut init = None;
        if self.match_type(TokenType::Equal) {
            init = Some(self.expression()?);
        }
        self.consume(TokenType::Semicolon, "Expected ';' after variable declaration.")?;
        Ok(Declaration(VariableDecl { name, init }).as_box())
    }
    fn statement(&mut self) -> Result<Stmt> {
        if self.match_type(TokenType::Print) {
            self.print_stmt()
        } else {
            self.expression_as_stmt()
        }
    }
    fn print_stmt(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expected ';'")?;
        Ok(Print(expr).as_box())
    }
    fn expression_as_stmt(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expected ';'")?;
        Ok(ExprAsStmt(expr).as_box())
    }
    fn expression(&mut self) -> Result<Expr> {
        let mut left = self.ternary()?;
        if self.match_type(TokenType::Comma) {
            let op = self.previous()?.take();
            let right = self.expression()?;
            left = Binary {left, op, right}.as_box();
        }
        Ok(left)
    }
    fn ternary(&mut self) -> Result<Expr> {
        let mut cond = self.equality()?;
        if self.match_type(TokenType::Question) {
            let if_true = self.equality()?;
            self.consume(TokenType::Colon, "Expected ':'")?;
            let if_false = self.equality()?;
            cond = Ternary {cond, if_true, if_false}.as_box();
        }
        Ok(cond)
    }
    fn equality(&mut self) -> Result<Expr> {
        let mut left = self.comparison()?;
        while self.match_types(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let op = self.previous()?.take();
            let right = self.comparison()?;
            left = Binary { left, op, right }.as_box();
        }
        Ok(left)
    }
    fn comparison(&mut self) -> Result<Expr> {
        let mut left = self.term()?;

        while self.match_types(&[TokenType::Greater,
                                TokenType::GreaterEqual,
                                TokenType::Less,
                                TokenType::LessEqual]
                             ){
            let op = self.previous()?.take();
            let right = self.term()?;
            left = Binary { left, op, right }.as_box();
        }
        Ok(left)
    }
    fn term(&mut self) -> Result<Expr> {
        let mut left = self.factor()?;

        while self.match_types(&[TokenType::Minus,TokenType::Plus]){
            let op = self.previous()?.take();
            let right = self.factor()?;
            left = Binary { left, op, right }.as_box();
        }
        Ok(left)
    }
    fn factor(&mut self) -> Result<Expr> {
        let mut left = self.unary()?;

        while self.match_types(&[TokenType::Slash,TokenType::Star]){
            let op = self.previous()?.take();
            let right = self.unary()?;
            left = Binary { left, op, right }.as_box();
        }
        Ok(left)
    }
    fn unary(&mut self) -> Result<Expr> {
        if self.match_types(&[TokenType::Bang,TokenType::Minus]) {
            let op = self.previous()?.take();
            let expr = self.unary()?;
            return Ok(Unary {op, expr}.as_box());
        }
        self.primary()
    }
    fn literal(&mut self) -> Result<LitValue> {
        Ok(
            if self.match_type(TokenType::False) {
                LitValue::Bool(false)
            }
            else if self.match_type(TokenType::True) {
                LitValue::Bool(true)
            }
            else if self.match_type(TokenType::Nil) {
                LitValue::Nil
            }
            else if self.match_type(TokenType::String) {
                LitValue::Str(self.previous()?.take_lexem())
            }
            else if self.match_type(TokenType::Number) {
                let f: f64 = self.previous()?.take_lexem().parse().unwrap();
                LitValue::Number(f)
            } else {
                return Err("".into());
            }
        )
    }
    fn primary(&mut self) -> Result<Expr> {
        if let Ok(lit_value) = self.literal() {
            return Ok(Literal(lit_value).as_box());
        }
        if self.match_type(TokenType::LeftParen) {
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
    fn match_type(&mut self, t: TokenType) -> bool {
        if self.check(t) {
            self.advance().unwrap();
            return true;
        }
        false
    }
    fn match_types(&mut self, types: &[TokenType]) -> bool {
        return types.iter().any(|t| self.match_type(*t))
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
