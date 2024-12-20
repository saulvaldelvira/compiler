pub mod error;

use ast::Expression;
use ast::{expr::LitValue, Statement, Program};
use lexer::token::{Token, TokenType};

use ast::expr::{AssignmentExpr, BinaryExpr, ExpressionKind, LitExpr, TernaryExpr, UnaryExpr, VariableExpr};
use ast::stmt::{BreakStmt, ContinueStmt, DeclarationStmt, EmptyStmt, ExprAsStmt, ForStmt, IfStmt, PrintStmt, StatementKind, WhileStmt};
use ast::stmt::BlockStmt;
use ast::declaration::{Declaration, DeclarationKind, VariableDecl};
use lexer::Span;
use self::error::ParseError;

type Result<T> = std::result::Result<T,ParseError>;

const VARIABLE_DECL: [TokenType; 2] = [TokenType::Var, TokenType::Const];

pub struct Parser {
    tokens: Box<[Token]>,
    current: usize,
    n_errors: u32,
}

impl Parser {
    /* PUBLIC */
    pub fn new(tokens: Box<[Token]>) -> Self {
        Self { tokens, current:0, n_errors:0 }
    }
    pub fn parse(mut self) -> std::result::Result<Program,u32> {
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
        if self.has_errors() {
            Err(self.n_errors())
        } else {
            Ok( Program { stmts } )
        }
    }
    pub fn has_errors(&self) -> bool { self.n_errors > 0 }
    pub fn n_errors(&self) -> u32 { self.n_errors }
    /* PRIVATE */
    fn var_decl(&mut self) -> Result<Declaration> {
        let prev = self.previous_mut().unwrap();
        let prev_span = prev.span();
        let is_const = prev.get_type() == TokenType::Const;

        let name_token = self.consume(TokenType::Identifier, "Expected variable name.")?;
        let name = name_token.take_lexem();

        let start_span = if is_const {
            prev_span
        } else {
            name_token.span()
        };

        let mut init = None;
        if self.match_type(TokenType::Equal) {
            init = Some(self.expression()?);
        }

        let span = match &init {
            Some(i) => start_span.join(&i.span),
            None => start_span
        };
        let decl = Declaration {
            kind: DeclarationKind::Variable(
                      VariableDecl { is_const, name, init }
                  ),
            span
        };
        Ok(decl)
    }
    fn var_decl_stmt(&mut self) -> Result<Statement> {
        let inner = self.var_decl()?;
        let comma = self.consume(TokenType::Semicolon, "Expected ';' after variable declaration.")?;
        let span = inner.span.join(&comma.span());

        let stmt = Statement {
            kind: StatementKind::Decl(
                      DeclarationStmt { inner }
                  ),
            span
        };
        Ok(stmt)
    }
    fn statement(&mut self) -> Result<Statement> {
        if self.match_types(&VARIABLE_DECL) {
            self.var_decl_stmt()
        } else {
            self.block()
        }
    }
    fn if_stmt(&mut self) -> Result<Statement> {
        let mut span = self.consume(TokenType::LeftParen, "Expected '('")?.span();
        let cond = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')'")?;

        let if_true = Box::new(self.block()?);
        span = span.join(&if_true.span);

        let mut if_false = None;
        if self.match_type(TokenType::Else) {
            if_false = Some(Box::new(self.block()?));
            span = span.join(&if_false.as_ref().unwrap().span);
        }
        Ok(Statement {
            kind: StatementKind::If(IfStmt { cond, if_true, if_false }),
            span
        })
    }
    fn while_stmt(&mut self) -> Result<Statement> {
        let start_span = self.consume(TokenType::LeftParen, "Expected '('")?.span();
        let cond = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')'")?;
        let stmts = Box::new(self.block()?);
        let span = start_span.join(&stmts.span);
        Ok(Statement {
            kind: StatementKind::While(WhileStmt { cond, stmts }),
            span
        })
    }
    fn block(&mut self) -> Result<Statement> {
        if self.match_type(TokenType::LeftBrace) {
            let start_span = self.previous_mut().unwrap().span();
            let mut stmts = Vec::new();
            while !self.check(TokenType::RightBrace) && !self.is_finished() {
                stmts.push(self.statement()?);
            }
            let end_span = self.consume(TokenType::RightBrace, "Missing '}'")?.span();
            Ok(Statement {
                kind: StatementKind::Block(BlockStmt { stmts }),
                span: start_span.join(&end_span)
            })
        } else if self.match_type(TokenType::If) {
            self.if_stmt()
        } else if self.match_type(TokenType::While) {
            self.while_stmt()
        } else if self.match_type(TokenType::For) {
            self.for_stmt()
        } else {
            self.single_line_stmt()
        }
    }
    fn for_stmt(&mut self) -> Result<Statement> {
        let span = self.previous_mut().unwrap().span();
        self.consume(TokenType::LeftParen, "Expected '(' after 'for'")?;
        let init = if self.match_types(&VARIABLE_DECL) {
            Some(self.var_decl()?)
        } else { None };
        self.consume(TokenType::Semicolon, "Expected ';'")?;
        let cond = if !self.check(TokenType::Semicolon) {
            Some(self.expression()?)
        } else { None };
        self.consume(TokenType::Semicolon, "Expected ';'")?;
        let inc = if !self.check(TokenType::RightParen) {
            Some(self.expression()?)
        } else { None };
        self.consume(TokenType::RightParen, "Expected ')' after for")?;
        let body = Box::new(self.statement()?);
        let span =  span.join(&body.span);
        Ok(Statement {
            kind: StatementKind::For(ForStmt { init, cond, inc, body }),
            span
        })
    }
    fn single_line_stmt(&mut self) -> Result<Statement> {
        let ast =
        if self.match_type(TokenType::Print) {
            self.print_stmt()?
        } else if self.match_type(TokenType::Semicolon) {
            Statement {
                kind: StatementKind::Empty(EmptyStmt),
                span: self.previous_span().unwrap()
            }
        } else if self.match_type(TokenType::Break) {
            Statement {
                kind: StatementKind::Break(BreakStmt),
                span: self.previous_span().unwrap()
            }
        } else if self.match_type(TokenType::Continue) {
            Statement {
                kind: StatementKind::Continue(ContinueStmt),
                span: self.previous_span().unwrap()
            }
        } else {
            self.expression_as_stmt()?
        };
        Ok(ast)
    }
    fn print_stmt(&mut self) -> Result<Statement> {
        let start_span = self.previous_span().unwrap();
        let expr = self.expression()?;
        let end_span = self.consume(TokenType::Semicolon, "Expected ';'")?.span();
        Ok(Statement {
            kind: StatementKind::Print(PrintStmt { expr }),
            span: start_span.join(&end_span)
        })
    }
    fn expression_as_stmt(&mut self) -> Result<Statement> {
        let expr = self.expression()?;
        let span = self.consume(TokenType::Semicolon, "Expected ';'")?.span();
        let span = expr.span.join(&span);
        Ok(Statement {
            kind: StatementKind::Expression(ExprAsStmt { expr }),
            span
        })
    }
    fn expression(&mut self) -> Result<Expression> {
        self.comma()
    }
    fn comma(&mut self) -> Result<Expression> {
        let mut left = self.ternary()?;
        if self.match_type(TokenType::Comma) {
            let op = self.previous_mut()?.take_lexem();
            let right = Box::new(self.comma()?);
            let span = left.span.join(&right.span);
            left = Expression {
                kind: ExpressionKind::Binary(
                    BinaryExpr { left: Box::new(left), op, right }
                ),
                span
            };
        }
        Ok(left)
    }
    fn ternary(&mut self) -> Result<Expression> {
        let mut cond = self.assignment()?;
        if self.match_type(TokenType::Question) {
            let if_true = Box::new(self.assignment()?);
            self.consume(TokenType::Colon, "Expected ':'")?;
            let if_false = Box::new(self.assignment()?);
            let span = cond.span.join(&if_false.span);
            cond = Expression {
                kind: ExpressionKind::Ternary(TernaryExpr { cond: Box::new(cond), if_true, if_false }),
                span
            }
        }
        Ok(cond)
    }
    fn assignment(&mut self) -> Result<Expression> {
        let left = self.equality()?;
        let ast = if self.match_type(TokenType::Equal) {
            let right = Box::new(self.assignment()?);
            let span = left.span.join(&right.span);
            Expression {
                kind: ExpressionKind::Assignment(AssignmentExpr { left: Box::new(left), right }),
                span
            }
        } else {
            left
        };
        Ok(ast)
    }
    fn equality(&mut self) -> Result<Expression> {
        let mut left = self.comparison()?;
        while self.match_types(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let op = self.previous_mut()?.take_lexem();
            let right = Box::new(self.comparison()?);
            let span = left.span.join(&right.span);
            left = Expression {
                kind: ExpressionKind::Binary(BinaryExpr { left: Box::new(left), op, right }),
                span
            }
        }
        Ok(left)
    }
    fn comparison(&mut self) -> Result<Expression> {
        let mut left = self.term()?;
        while self.match_types(&[TokenType::Greater,
                                TokenType::GreaterEqual,
                                TokenType::Less,
                                TokenType::LessEqual]
                             ){
            let op = self.previous_mut()?.take_lexem();
            let right = Box::new(self.term()?);
            let span = left.span.join(&right.span);
            left = Expression {
                kind: ExpressionKind::Binary(BinaryExpr { left: Box::new(left), op, right }),
                span
            }
        }
        Ok(left)
    }
    fn term(&mut self) -> Result<Expression> {
        let mut left = self.factor()?;
        while self.match_types(&[TokenType::Minus,TokenType::Plus]){
            let op = self.previous_mut()?.take_lexem();
            let right = Box::new(self.factor()?);
            let span = left.span.join(&right.span);
            left = Expression {
                kind: ExpressionKind::Binary(BinaryExpr { left: Box::new(left), op, right }),
                span
            }
        }
        Ok(left)
    }
    fn factor(&mut self) -> Result<Expression> {
        let mut left = self.unary()?;
        while self.match_types(&[TokenType::Slash,TokenType::Star]){
            let op = self.previous_mut()?.take_lexem();
            let right = Box::new(self.unary()?);
            let span = left.span.join(&right.span);
            left = Expression {
                kind: ExpressionKind::Binary(BinaryExpr { left: Box::new(left), op, right }),
                span
            }
        }
        Ok(left)
    }
    fn unary(&mut self) -> Result<Expression> {
        if self.match_types(&[TokenType::Bang,TokenType::Minus,TokenType::Plus]) {
            let start_span = self.previous_span().unwrap();
            let op = self.previous_mut()?.take_lexem();
            let expr = Box::new(self.unary()?);
            let span = start_span.join(&expr.span);
            Ok(Expression {
                kind: ExpressionKind::Unary(UnaryExpr { op, expr }),
                span
            })
        } else {
            self.primary()
        }
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
                LitValue::Str(self.previous_mut()?.take_lexem())
            }
            else if self.match_type(TokenType::Number) {
                let f: f64 = self.previous_mut()?.take_lexem().parse().unwrap();
                LitValue::Number(f)
            } else {
                return Err("".into());
            }
        )
    }
    fn primary(&mut self) -> Result<Expression> {
        if let Ok(value) = self.literal() {
            return Ok(Expression {
                kind: ExpressionKind::Literal(LitExpr { value }),
                span: self.previous_span().unwrap()
            })
        }
        if self.match_type(TokenType::LeftParen) {
            let start = self.previous_span().unwrap();
            let mut expr = self.expression()?;
            let end = self.consume(TokenType::RightParen, "Expected ')' after expression.")?.span();
            expr.span = start.join(&end);
            return Ok(expr)
        }
        if self.match_type(TokenType::Identifier) {
            let prev = self.previous_mut()?;
            let name = prev.take_lexem();
            return Ok(Expression {
                kind: ExpressionKind::Variable(VariableExpr { name }),
                span: prev.span()
            })
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
        self.previous_mut()
    }
    pub fn is_finished(&self) -> bool {
        self.current >= self.tokens.len()
    }
    fn peek(&mut self) -> Result<&mut Token> {
        self.tokens.get_mut(self.current)
                   .ok_or_else(|| ParseError::new("Index should be valid when calling peek"))
    }
    fn previous_mut(&mut self) -> Result<&mut Token> {
        self.tokens.get_mut(self.current - 1)
                   .ok_or_else(|| ParseError::new("Index should be valid when calling peek"))
    }
    fn previous(&self) -> Result<&Token> {
        self.tokens.get(self.current - 1)
                   .ok_or_else(|| ParseError::new("Index should be valid when calling peek"))
    }
    fn previous_span(&self) -> Result<Span> {
        self.previous().map(|s| s.span())
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
            self.previous_mut()
        } else {
            self.peek()
        }.unwrap();
        eprintln!("Parser: [{},{}] {err}", tok.span().start_line, tok.span().start_col);
        self.n_errors += 1;
    }
}
