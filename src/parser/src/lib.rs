pub mod error;
mod cursor;

use ast::{expr::LitValue, Stmt, Program};
use cursor::Cursor;
use lexer::token::{Token, TokenType};

use ast::expr::{AssignmentExpr, BinaryExpr, Expr, Expression, LitExpr, TernaryExpr, UnaryExpr, VariableExpr};
use ast::stmt::{DeclarationStmt, ExprAsStmt, IfStmt, PrintStmt, WhileStmt};
use ast::stmt::{BlockStmt, Statement};
use ast::stmt::stmt;
use ast::expr::expr;
use ast::declaration::{Declaration, VariableDecl};
use lexer::Spannable;
use self::error::ParseError;

type Result<T> = std::result::Result<T,ParseError>;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    n_errors: u32,
    cursor: Cursor
}

impl Parser {
    /* PUBLIC */
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current:0, n_errors:0, cursor: Cursor::new() }
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
    fn pop<T>(&mut self, mut ast: Box<T>) -> Result<Box<T>>
    where
        T: Spannable
    {
        let span = self.cursor.pop(self.tokens[self.current-1].span());
        ast.set_span(span);
        Ok(ast)
    }
    fn push(&mut self) {
        let span = self.tokens[self.current].span();
        self.cursor.push(span);
    }
    fn var_declaration(&mut self) -> Result<Stmt> {
        self.push();
        let name = self.consume(TokenType::Identifier, "Expected variable name.")?.take_lexem();
        let mut init = None;
        if self.match_type(TokenType::Equal) {
            init = Some(self.expression()?);
        }
        self.consume(TokenType::Semicolon, "Expected ';' after variable declaration.")?;
        let inner: Declaration = VariableDecl::new(name, init).into();
        self.pop(stmt!{ DeclarationStmt { inner } })
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
        self.pop(stmt!(IfStmt { cond, if_true, if_false }))
    }
    fn while_stmt(&mut self) -> Result<Stmt> {
        self.consume(TokenType::LeftParen, "Expected '('")?;
        let cond = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')'")?;
        let stmts = self.block()?;
        self.pop(stmt!( WhileStmt { cond, stmts } ))
    }
    fn block(&mut self) -> Result<Stmt> {
        self.push();
        if self.match_type(TokenType::LeftBrace) {
            let mut stmts = Vec::new();
            while !self.check(TokenType::RightBrace) && !self.is_finished() {
                stmts.push(self.statement()?);
            }
            self.consume(TokenType::RightBrace, "Missing '{'")?;
            self.pop(stmt!(BlockStmt { stmts }))
        } else if self.match_type(TokenType::If) {
            self.if_stmt()
        } else if self.match_type(TokenType::While) {
            self.while_stmt()
        } else {
            self.single_line_stmt()
        }
    }
    fn single_line_stmt(&mut self) -> Result<Stmt> {
        let ast =
        if self.match_type(TokenType::Print) {
            self.print_stmt()?
        } else {
            self.expression_as_stmt()?
        };
        self.pop(ast)
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
        self.push();
        let mut left = self.ternary()?;
        if self.match_type(TokenType::Comma) {
            let op = self.previous()?.take_lexem();
            let right = self.comma()?;
            left = expr!( BinaryExpr { left, op, right } );
        }
        self.pop(left)
    }
    fn ternary(&mut self) -> Result<Expr> {
        self.push();
        let mut cond = self.assignment()?;
        if self.match_type(TokenType::Question) {
            let if_true = self.assignment()?;
            self.consume(TokenType::Colon, "Expected ':'")?;
            let if_false = self.assignment()?;
            cond = expr!( TernaryExpr {cond, if_true, if_false } );
        }
        self.pop(cond)
    }
    fn assignment(&mut self) -> Result<Expr> {
        self.push();
        let left = self.equality()?;
        let ast = if self.match_type(TokenType::Equal) {
            let right = self.assignment()?;
            expr!( AssignmentExpr { left, right } )
        } else {
            left
        };
        self.pop(ast)
    }
    fn equality(&mut self) -> Result<Expr> {
        self.push();
        let mut left = self.comparison()?;
        while self.match_types(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            self.push();
            let op = self.previous()?.take_lexem();
            let right = self.comparison()?;
            left = self.pop(expr!( BinaryExpr { left, op, right } ))?;
        }
        self.pop(left)
    }
    fn comparison(&mut self) -> Result<Expr> {
        self.push();
        let mut left = self.term()?;
        while self.match_types(&[TokenType::Greater,
                                TokenType::GreaterEqual,
                                TokenType::Less,
                                TokenType::LessEqual]
                             ){
            self.push();
            let op = self.previous()?.take_lexem();
            let right = self.term()?;
            left = self.pop(expr!( BinaryExpr { left, op, right } ))?;
        }
        self.pop(left)
    }
    fn term(&mut self) -> Result<Expr> {
        self.push();
        let mut left = self.factor()?;
        while self.match_types(&[TokenType::Minus,TokenType::Plus]){
            self.push();
            let op = self.previous()?.take_lexem();
            let right = self.factor()?;
            left = self.pop(expr!( BinaryExpr { left, op, right } ))?;
        }
        self.pop(left)
    }
    fn factor(&mut self) -> Result<Expr> {
        self.push();
        let mut left = self.unary()?;
        while self.match_types(&[TokenType::Slash,TokenType::Star]){
            self.push();
            let op = self.previous()?.take_lexem();
            let right = self.unary()?;
            left = self.pop(expr!( BinaryExpr { left, op, right } ))?;
        }
        self.pop(left)
    }
    fn unary(&mut self) -> Result<Expr> {
        self.push();
        if self.match_types(&[TokenType::Bang,TokenType::Minus]) {
            let op = self.previous()?.take_lexem();
            let expr = self.unary()?;
            return self.pop(expr!( UnaryExpr {op, expr } ));
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
        if let Ok(value) = self.literal() {
            return self.pop(expr!(LitExpr { value }));
        }
        if self.match_type(TokenType::LeftParen) {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen, "Expected ')' after expression.")?;
            return self.pop(expr);
        }
        if self.match_type(TokenType::Identifier) {
            let name = self.previous()?.take_lexem().into();
            return self.pop(expr!( VariableExpr { name } ));
        }
        ParseError::new(
             format!("Expected literal, found: {}", self.peek()?.get_lexem())).err()
    }
    fn consume(&mut self, t: TokenType, msg: &'static str) -> Result<&mut Token> {
        if self.check(t) { return self.advance(); }
        ParseError::new(msg).err()
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
                   .ok_or_else(|| ParseError::new("Index should be valid when calling peek"))
    }
    fn previous(&mut self) -> Result<&mut Token> {
        self.tokens.get_mut(self.current - 1)
                   .ok_or_else(|| ParseError::new("Index should be valid when calling peek"))
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
        eprintln!("Parser: [{},{}] {err}", tok.span().start_line(), tok.span().start_col());
        self.n_errors += 1;
    }
}
