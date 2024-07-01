pub mod error;

use ast::{expr::LitValue, Stmt, Program};
use lexer::token::{Token, TokenType};

use ast::expr::{expr, AssignmentExpr, BinaryExpr, Expr, Expression, TernaryExpr, UnaryExpr, VariableExpr};
use ast::stmt::{stmt, DeclarationStmt, ExprAsStmt, IfStmt, PrintStmt, WhileStmt};
use ast::stmt::{BlockStmt, Statement};
use ast::declaration::{Declaration, VariableDecl};
use self::error::ParseError;

use Expression::*;

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
            match self.statement() {
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
    fn var_declaration(&mut self) -> Result<Stmt> {
        let name = self.consume(TokenType::Identifier, "Expected variable name.")?.take_lexem();
        let mut init = None;
        if self.match_type(TokenType::Equal) {
            init = Some(self.expression()?);
        }
        self.consume(TokenType::Semicolon, "Expected ';' after variable declaration.")?;
        let inner: Declaration = VariableDecl { name, init }.into();
        Ok(stmt!( DeclarationStmt { inner } ))
    }
    fn statement(&mut self) -> Result<Stmt> {
        if self.match_type(TokenType::Var) {
            self.var_declaration()
        } else {
            self.block()
        }
    }
    fn if_stmt(&mut self) -> Result<Stmt> {
        self.consume(TokenType::LeftParen, "Expected '('")?;
        let cond = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')'")?;
        let if_true = self.block()?;
        let mut if_false = None;
        if self.match_type(TokenType::Else) {
            if_false = Some(self.block()?);
        }
        Ok(stmt!( IfStmt { cond, if_true, if_false } ))
    }
    fn while_stmt(&mut self) -> Result<Stmt> {
        self.consume(TokenType::LeftParen, "Expected '('")?;
        let cond = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')'")?;
        let stmts = self.block()?;
        Ok(stmt!( WhileStmt { cond, stmts } ))
    }
    fn block(&mut self) -> Result<Stmt> {
        if self.match_type(TokenType::LeftBrace) {
            let mut stmts = Vec::new();
            while !self.check(TokenType::RightBrace) && !self.is_finished() {
                stmts.push(self.statement()?);
            }
            self.consume(TokenType::RightBrace, "Missing '{'")?;
            Ok(stmt!( BlockStmt{stmts} ))
        } else if self.match_type(TokenType::If) {
            self.if_stmt()
        } else if self.match_type(TokenType::While) {
            self.while_stmt()
        } else {
            self.single_line_stmt()
        }
    }
    fn single_line_stmt(&mut self) -> Result<Stmt> {
        if self.match_type(TokenType::Print) {
            self.print_stmt()
        } else {
            self.expression_as_stmt()
        }
    }
    fn print_stmt(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expected ';'")?;
        Ok(stmt!( PrintStmt { expr } ))
    }
    fn expression_as_stmt(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expected ';'")?;
        Ok(stmt!( ExprAsStmt { expr } ))
    }
    fn expression(&mut self) -> Result<Expr> {
        self.comma()
    }
    fn comma(&mut self) -> Result<Expr> {
        let mut left = self.ternary()?;
        if self.match_type(TokenType::Comma) {
            let op = self.previous()?.take_lexem();
            let right = self.comma()?;
            left = expr!( BinaryExpr {left, op, right} );
        }
        Ok(left)
    }
    fn ternary(&mut self) -> Result<Expr> {
        let mut cond = self.assignment()?;
        if self.match_type(TokenType::Question) {
            let if_true = self.assignment()?;
            self.consume(TokenType::Colon, "Expected ':'")?;
            let if_false = self.assignment()?;
            cond = expr!( TernaryExpr {cond, if_true, if_false} );
        }
        Ok(cond)
    }
    fn assignment(&mut self) -> Result<Expr> {
        let left = self.equality()?;
        Ok(if self.match_type(TokenType::Equal) {
            let right = self.assignment()?;
            expr!( AssignmentExpr { left, right } )
        } else {
            left
        })
    }
    fn equality(&mut self) -> Result<Expr> {
        let mut left = self.comparison()?;
        while self.match_types(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let op = self.previous()?.take_lexem();
            let right = self.comparison()?;
            left = expr!( BinaryExpr { left, op, right } );
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
            let op = self.previous()?.take_lexem();
            let right = self.term()?;
            left = expr!( BinaryExpr { left, op, right } );
        }
        Ok(left)
    }
    fn term(&mut self) -> Result<Expr> {
        let mut left = self.factor()?;

        while self.match_types(&[TokenType::Minus,TokenType::Plus]){
            let op = self.previous()?.take_lexem();
            let right = self.factor()?;
            left = expr!( BinaryExpr { left, op, right } );
        }
        Ok(left)
    }
    fn factor(&mut self) -> Result<Expr> {
        let mut left = self.unary()?;

        while self.match_types(&[TokenType::Slash,TokenType::Star]){
            let op = self.previous()?.take_lexem();
            let right = self.unary()?;
            left = expr!( BinaryExpr { left, op, right } );
        }
        Ok(left)
    }
    fn unary(&mut self) -> Result<Expr> {
        if self.match_types(&[TokenType::Bang,TokenType::Minus]) {
            let op = self.previous()?.take_lexem();
            let expr = self.unary()?;
            return Ok(expr!( UnaryExpr {op, expr} ));
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
        if self.match_type(TokenType::Identifier) {
            let name = self.previous()?.take_lexem().into();
            return Ok(expr!( VariableExpr { name } ));
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
