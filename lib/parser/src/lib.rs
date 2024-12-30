pub mod error;

use ast::types::{CustomType, Type, TypeKind};
use ast::Expression;
use ast::{expr::LitValue, Statement, Program};
use lexer::token::{Token, TokenKind};

use ast::expr::{AssignmentExpr, BinaryExpr, CallExpr, ExpressionKind, LitExpr, TernaryExpr, UnaryExpr, VariableExpr};
use ast::stmt::{BreakStmt, ContinueStmt, DeclarationStmt, EmptyStmt, ExprAsStmt, ForStmt, IfStmt, PrintStmt, StatementKind, WhileStmt};
use ast::stmt::BlockStmt;
use ast::declaration::{Declaration, DeclarationKind, FunctionArgument, FunctionDecl, VariableDecl};
use lexer::{Span, unescaped::Unescaped};
use self::error::ParseError;

type Result<T> = std::result::Result<T,ParseError>;

pub struct Parser<'src> {
    tokens: &'src [Token],
    src: &'src str,
    current: usize,
    n_errors: u32,
}

impl<'src> Parser<'src> {
    /* PUBLIC */
    pub fn new(tokens: &'src [Token], src: &'src str) -> Self {
        Self { tokens, src, current:0, n_errors:0 }
    }
    pub fn parse(mut self) -> std::result::Result<Program,u32> {
        let mut decls = Vec::new();
        while !self.is_finished() {
            match self.declaration() {
                Ok(stmt) => decls.push(stmt),
                Err(e) => {
                    self.error(e.get_message());
                    self.synchronize();
                }
            }
        }
        if self.has_errors() {
            Err(self.n_errors())
        } else {
            Ok( Program { decls: decls.into_boxed_slice() } )
        }
    }
    pub fn has_errors(&self) -> bool { self.n_errors > 0 }
    pub fn n_errors(&self) -> u32 { self.n_errors }
    /* PRIVATE */
    fn declaration(&mut self) -> Result<Declaration> {
        if self.match_types(&[TokenKind::Let,TokenKind::Const]) {
            self.var_decl()
        }
        else if self.match_type(TokenKind::Fn) {
            self.function()
        } else {
            Err("Expected declaration".into())
        }
    }
    fn consume_ident(&mut self) -> Result<Box<str>> {
        let span = self.consume(TokenKind::Identifier)?.span;
        Ok(Box::from(span.slice(self.src)))
    }
    fn function(&mut self) -> Result<Declaration> {
        let start_span = self.previous_span()?;
        let name = self.consume_ident()?;
        self.consume(TokenKind::LeftParen)?;

        let mut args = Vec::new();

        while !self.match_type(TokenKind::RightParen) {
            let arg_name = self.consume_ident()?;
            self.consume(TokenKind::Colon)?;
            let ty = self.ty()?;
            if !self.match_type(TokenKind::Comma) {
                break;
            }

            args.push(FunctionArgument {
                ty,
                name: arg_name
            });
        }

        let return_type = if self.match_type(TokenKind::Arrow) {
            self.ty()?
        } else { Type { kind: TypeKind::Empty } };

        let Statement {
            kind: StatementKind::Block(block),
            span
        } = self.block()? else { unreachable!() };

        let fun = FunctionDecl {
            return_type,
            name,
            args: args.into_boxed_slice(),
            body: block
        };
        let span = start_span.join(&span);
        Ok(Declaration {
            kind: DeclarationKind::Function(fun),
            span
        })
    }
    fn ty(&mut self) -> Result<Type> {
        if self.match_type(TokenKind::Int) {
            Ok(Type{kind: TypeKind::Int})
        }
        else if self.match_type(TokenKind::Float) {
            Ok(Type{kind: TypeKind::Float})
        }
        else if self.match_type(TokenKind::Char) {
            Ok(Type{kind: TypeKind::Char})
        }
        else if self.match_type(TokenKind::Bool) {
            Ok(Type{kind: TypeKind::Bool})
        }
        else if self.match_type(TokenKind::Identifier) {
            let name = self.previous_lexem()?;
            Ok(Type{kind: TypeKind::Custom(CustomType { name })})
        } else {
            Err("Expected type".into())
        }
    }
    fn var_decl(&mut self) -> Result<Declaration> {
        let prev = self.previous()?;
        let prev_span = prev.span;
        let is_const = prev.kind == TokenKind::Const;

        let name_token = self.consume(TokenKind::Identifier).cloned()?;
        let name = self.get_lexem(&name_token).into();
        let name_token_span = name_token.span;

        let start_span = if is_const {
            prev_span
        } else {
            name_token_span
        };

        let mut init = None;
        if self.match_type(TokenKind::Equal) {
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
        let comma = self.consume(TokenKind::Semicolon)?;
        let span = inner.span.join(&comma.span);

        let stmt = Statement {
            kind: StatementKind::Decl(
                      DeclarationStmt { inner }
                  ),
            span
        };
        Ok(stmt)
    }
    fn statement(&mut self) -> Result<Statement> {
        if self.match_types(&[TokenKind::Let,TokenKind::Const]) {
            self.var_decl_stmt()
        } else {
            self.block()
        }
    }
    fn if_stmt(&mut self) -> Result<Statement> {
        let mut span = self.consume(TokenKind::LeftParen)?.span;
        let cond = self.expression()?;
        self.consume(TokenKind::RightParen)?;

        let if_true = Box::new(self.block()?);
        span = span.join(&if_true.span);

        let mut if_false = None;
        if self.match_type(TokenKind::Else) {
            if_false = Some(Box::new(self.block()?));
            span = span.join(&if_false.as_ref().unwrap().span);
        }
        Ok(Statement {
            kind: StatementKind::If(IfStmt { cond, if_true, if_false }),
            span
        })
    }
    fn while_stmt(&mut self) -> Result<Statement> {
        let start_span = self.consume(TokenKind::LeftParen)?.span;
        let cond = self.expression()?;
        self.consume(TokenKind::RightParen)?;
        let stmts = Box::new(self.block()?);
        let span = start_span.join(&stmts.span);
        Ok(Statement {
            kind: StatementKind::While(WhileStmt { cond, stmts }),
            span
        })
    }
    fn block(&mut self) -> Result<Statement> {
        if self.match_type(TokenKind::LeftBrace) {
            let start_span = self.previous_span()?;
            let mut stmts = Vec::new();
            while !self.check(TokenKind::RightBrace) && !self.is_finished() {
                stmts.push(self.statement()?);
            }
            let end_span = self.consume(TokenKind::RightBrace)?.span;
            Ok(Statement {
                kind: StatementKind::Block(BlockStmt { stmts }),
                span: start_span.join(&end_span)
            })
        } else if self.match_type(TokenKind::If) {
            self.if_stmt()
        } else if self.match_type(TokenKind::While) {
            self.while_stmt()
        } else if self.match_type(TokenKind::For) {
            self.for_stmt()
        } else {
            self.single_line_stmt()
        }
    }
    fn for_stmt(&mut self) -> Result<Statement> {
        let span = self.previous()?.span;
        self.consume(TokenKind::LeftParen)?;
        let init = if self.match_types(&[TokenKind::Let,TokenKind::Const]) {
            Some(self.var_decl()?)
        } else { None };
        self.consume(TokenKind::Semicolon)?;
        let cond = if !self.check(TokenKind::Semicolon) {
            Some(self.expression()?)
        } else { None };
        self.consume(TokenKind::Semicolon)?;
        let inc = if !self.check(TokenKind::RightParen) {
            Some(self.expression()?)
        } else { None };
        self.consume(TokenKind::RightParen)?;
        let body = Box::new(self.statement()?);
        let span =  span.join(&body.span);
        Ok(Statement {
            kind: StatementKind::For(ForStmt { init, cond, inc, body }),
            span
        })
    }
    fn single_line_stmt(&mut self) -> Result<Statement> {
        let ast =
        if self.match_type(TokenKind::Print) {
            self.print_stmt()?
        } else if self.match_type(TokenKind::Semicolon) {
            Statement {
                kind: StatementKind::Empty(EmptyStmt),
                span: self.previous_span().unwrap()
            }
        } else if self.match_type(TokenKind::Break) {
            Statement {
                kind: StatementKind::Break(BreakStmt),
                span: self.previous_span().unwrap()
            }
        } else if self.match_type(TokenKind::Continue) {
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
        let end_span = self.consume(TokenKind::Semicolon)?.span;
        Ok(Statement {
            kind: StatementKind::Print(PrintStmt { expr }),
            span: start_span.join(&end_span)
        })
    }
    fn expression_as_stmt(&mut self) -> Result<Statement> {
        let expr = self.expression()?;
        let span = self.consume(TokenKind::Semicolon)?.span;
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
        if self.match_type(TokenKind::Comma) {
            let op = self.previous_lexem()?;
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
        if self.match_type(TokenKind::Question) {
            let if_true = Box::new(self.assignment()?);
            self.consume(TokenKind::Colon)?;
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
        let ast = if self.match_type(TokenKind::Equal) {
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
        while self.match_types(&[TokenKind::BangEqual, TokenKind::EqualEqual]) {
            let op = self.previous_lexem()?;
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
        while self.match_types(&[TokenKind::Greater,
                                TokenKind::GreaterEqual,
                                TokenKind::Less,
                                TokenKind::LessEqual]
                             ){
            let op = self.previous_lexem()?;
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
        while self.match_types(&[TokenKind::Minus,TokenKind::Plus]){
            let op = self.previous_lexem()?;
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
        while self.match_types(&[TokenKind::Slash,TokenKind::Star]){
            let op = self.previous_lexem()?;
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
        if self.match_types(&[TokenKind::Bang,TokenKind::Minus,TokenKind::Plus]) {
            let start_span = self.previous_span().unwrap();
            let op = self.previous_lexem()?;
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
    fn literal(&mut self) -> Result<Option<LitValue>> {
        Ok(Some(if self.match_type(TokenKind::False) {
            LitValue::Bool(false)
        }
        else if self.match_type(TokenKind::True) {
            LitValue::Bool(true)
        }
        else if self.match_type(TokenKind::String) {
            LitValue::Str(self.previous_lexem()?)
        }
        else if self.match_types(&[TokenKind::IntLiteral, TokenKind::FloatLiteral]) {
            let f: f64 = self.get_lexem(self.previous()?).parse()?;
            LitValue::Number(f)
        } else if self.match_type(TokenKind::CharLiteral) {
            let prev = self.previous().unwrap();
            let lit = self.get_lexem(prev)
                .strip_prefix('\'').unwrap()
                .strip_suffix('\'').unwrap();
            let lit = Unescaped::from(lit).next().ok_or_else(|| format!("Invalid escape '{lit}'"))?;
            LitValue::Char(lit)
        }
        else {
            return Ok(None)
        }))
    }
    fn primary(&mut self) -> Result<Expression> {
        if let Some(value) = self.literal()? {
            return Ok(Expression {
                kind: ExpressionKind::Literal(LitExpr { value }),
                span: self.previous_span().unwrap()
            });
        }
        if self.match_type(TokenKind::LeftParen) {
            let start = self.previous_span().unwrap();
            let mut expr = self.expression()?;
            let end = self.consume(TokenKind::RightParen)?.span;
            expr.span = start.join(&end);
            return Ok(expr)
        }
        if self.match_type(TokenKind::Identifier) {
            let prev = self.previous()?;
            let name = self.owned_lexem(prev);
            let prev_span = prev.span;
            if self.match_type(TokenKind::LeftParen) {
                let mut args = Vec::new();
                let mut first = true;
                while !self.match_type(TokenKind::RightParen) {
                    if !first {
                        self.consume(TokenKind::Comma)?;
                    }
                    first = false;
                    args.push(self.expression()?);

                }
                let span = prev_span.join(&self.previous_span()?);
                return Ok(Expression{
                    kind: ExpressionKind::Call(
                        CallExpr {
                            callee: name,
                            args: args.into_boxed_slice()
                        }),
                    span
                })
            } else {
                return Ok(Expression {
                    kind: ExpressionKind::Variable(VariableExpr { name }),
                    span: prev_span
                })
            }
        }
        ParseError::new(
             format!("Expected expression, found: {}", self.get_lexem(self.peek()?))).err()
    }
    fn owned_lexem(&self, tok: &Token) -> Box<str> {
        Box::from(self.get_lexem(tok))
    }
    fn get_lexem(&self, tok: &Token) -> &str {
        &self.src[tok.span.offset..tok.span.offset+tok.span.len]
    }
    fn consume(&mut self, t: TokenKind) -> Result<&Token> {
        if self.check(t) { return self.advance(); }
        ParseError::new(format!("Expected {t:?}")).err()
    }
    fn match_type(&mut self, t: TokenKind) -> bool {
        if self.check(t) {
            self.advance().unwrap();
            return true;
        }
        false
    }
    fn match_types(&mut self, types: &[TokenKind]) -> bool {
        types.iter().any(|t| self.match_type(*t))
    }
    fn check(&mut self, t: TokenKind) -> bool {
        if self.is_finished() { return false; }
        self.peek().unwrap().kind == t
    }
    fn bump(&mut self) {
        if !self.is_finished() {
            self.current += 1;
        }
    }
    fn advance(&mut self) -> Result<&Token> {
        self.bump();
        self.previous()
    }
    pub fn is_finished(&self) -> bool {
        self.current >= self.tokens.len()
    }
    fn peek(&self) -> Result<&Token> {
        self.tokens.get(self.current)
                   .ok_or_else(|| ParseError::new("Index should be valid when calling peek"))
    }
    fn previous(&self) -> Result<&Token> {
        self.tokens.get(self.current - 1)
                   .ok_or_else(|| ParseError::new("Index should be valid when calling peek"))
    }
    fn previous_span(&self) -> Result<Span> {
        self.previous().map(|s| s.span)
    }
    fn previous_lexem(&self) -> Result<Box<str>> {
        Ok(self.owned_lexem(self.previous()?))
    }
    fn synchronize(&mut self) -> bool {
        if self.is_finished() { return false; }
        self.bump();
        loop {
            if self.advance().unwrap().kind == TokenKind::Semicolon {
                return true;
            }
            if self.is_finished() { return false; }
            match self.peek().unwrap().kind {
                TokenKind::Class | TokenKind::Fn | TokenKind::Let |
                TokenKind::For   | TokenKind::If  | TokenKind::While |
                TokenKind::Print | TokenKind::Return => return true,
                _ => {},
            }
        }
    }
    fn error(&mut self, err: &str) {
        use lexer::span::FilePosition;

        let tok = if self.is_finished() {
            self.previous()
        } else {
            self.peek()
        }.unwrap();

        let FilePosition {
            start_line,
            start_col,
            ..
        } = tok.span.file_position(self.src);

        eprintln!("Parser: [{start_line},{start_col}] {err}");
        self.n_errors += 1;
    }
}
