pub mod error;

use core::str;
use std::rc::Rc;
use std::str::FromStr;

use ast::types::{ArrayType, RefType, StructType, Type, TypeKind};
use ast::{AstRef, Expression};
use ast::{expr::LitValue, Statement, Program};
use session::Symbol;
use lexer::token::{Token, TokenKind};

use ast::expr::{ArrayAccess, AssignmentExpr, BinaryExpr, BinaryExprKind, BinaryExprOp, CallExpr, Dereference, ExpressionKind, LitExpr, Reference, StructAccess, TernaryExpr, UnaryExpr, UnaryExprOp, VariableExpr};
use ast::stmt::{BreakStmt, ContinueStmt, DeclarationStmt, EmptyStmt, ExprAsStmt, ForStmt, IfStmt, PrintStmt, ReadStmt, ReturnStmt, StatementKind, WhileStmt};
use ast::stmt::BlockStmt;
use ast::declaration::{Declaration, DeclarationKind, FunctionDecl, StructDecl, StructField, VariableDecl};
use lexer::unescaped::Unescaped;
use span::Span;
use session::{with_session, with_session_interner};
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
                    self.synchronize_with(&[
                        TokenKind::Let, TokenKind::Const, TokenKind::Fn
                    ]);
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

    fn consume_ident(&mut self) -> Result<Symbol> {
        let span = self.consume(TokenKind::Identifier)?.span;
        Ok(with_session_interner(|i| {
            i.get_or_intern(span.slice(self.src))
        }))
    }

    /* ===== DECLARATION ===== */
    fn declaration(&mut self) -> Result<Declaration> {
        if let Some(vdecl) = self.try_var_decl() {
            vdecl
        }
        else if let Some(func) = self.try_function() {
            func
        }
        else if let Some(s) = self.try_struct() {
            s
        }
        else {
            Err("Expected declaration".into())
        }
    }
    fn try_function(&mut self) -> Option<Result<Declaration>> {
        if self.match_type(TokenKind::Fn) {
            Some(self.function())
        } else { None }
    }
    fn try_struct(&mut self) -> Option<Result<Declaration>> {
        if self.match_type(TokenKind::Struct) {
            Some(self.struct_decl())
        } else { None }
    }
    fn struct_decl(&mut self) -> Result<Declaration> {
        let start_span = self.previous_span()?;
        let name = self.consume_ident()?;
        self.consume(TokenKind::LeftBrace)?;

        let mut fields = Vec::new();
        loop {
            if self.check(TokenKind::RightBrace) { break };

            let field_name = self.consume_ident()?;
            self.consume(TokenKind::Colon)?;
            let field_ty = self.ty()?;

            let field = StructField::new(field_name, field_ty);
            fields.push(field);

            if self.match_type(TokenKind::Comma) {
                continue;
            } else {
                break
            }
        }

        let end_span = self.consume(TokenKind::RightBrace)?.span;

        let span = start_span.join(&end_span);

        let s = StructDecl {
            name,
            fields: fields.into_boxed_slice(),
        };

        Ok(
            Declaration {
                kind: DeclarationKind::Struct(Rc::new(s)),
                span
            }
        )
    }
    fn function(&mut self) -> Result<Declaration> {
        let start_span = self.previous_span()?;
        let name = self.consume_ident()?;
       self.consume(TokenKind::LeftParen)?;

        let mut args = Vec::new();

        let mut first = true;
        while !self.match_type(TokenKind::RightParen) {
            if !first {
                self.consume(TokenKind::Comma)?;
            }
            first = false;
            let arg_name = self.consume_ident()?;
            self.consume(TokenKind::Colon)?;
            let ty = self.ty()?;

            let mut vardecl = VariableDecl::new(
                arg_name,
                None,
                false,
            );
            vardecl.ty = Some(ty);

            args.push(Rc::new(vardecl));
        }

        let return_type = if self.match_type(TokenKind::Arrow) {
            self.ty()?
        } else { Type::empty() };

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
            kind: DeclarationKind::Function(fun.into()),
            span
        })
    }
    fn try_var_decl(&mut self) -> Option<Result<Declaration>> {
        if self.match_types(&[TokenKind::Let,TokenKind::Const]) {
            Some(self.var_decl())
        } else { None }
    }
    fn var_decl(&mut self) -> Result<Declaration> {
        let prev = self.previous()?;
        let prev_span = prev.span;
        let is_const = prev.kind == TokenKind::Const;

        let name_token = self.consume(TokenKind::Identifier).cloned()?;
        let name = self.owned_lexem(name_token.span);
        let name_token_span = name_token.span;

        let mut span = if is_const {
            prev_span
        } else {
            name_token_span
        };

        let ty =
        if self.match_type(TokenKind::Colon) {
            let ty = self.ty()?;
            span = span.join(&self.previous_span()?);
            Some(ty)
        } else { None };

        let mut init = None;
        if self.match_type(TokenKind::Equal) {
            init = Some(self.expression()?);
        }

        let semicolon = self.consume(TokenKind::Semicolon)?.span;
        let span = span.join(&semicolon);

        let mut vdecl = VariableDecl::new(name, init, is_const);
        vdecl.ty = ty;

        let decl = Declaration {
            kind: DeclarationKind::Variable(
                      vdecl.into()
                  ),
            span
        };
        Ok(decl)
    }
    fn array_type(&mut self) -> Result<Type> {
        let ty = self.ty()?;
        self.consume(TokenKind::Semicolon)?;
        self.consume(TokenKind::IntLiteral)?;
        let length = self.previous_parse::<usize>()?;
        self.consume(TokenKind::RightBracket)?;
        Ok(Type { kind: TypeKind::Array(
                ArrayType {
                    of: Box::new(ty),
                    length
                }
        ) })
    }
    fn ty(&mut self) -> Result<Type> {
        macro_rules! ty {
            ($tk:expr) => {
                Ok(Type{
                    kind: $tk,
                })
            };
        }
        if self.match_type(TokenKind::Int) {
            ty!(TypeKind::Int)
        }
        else if self.match_type(TokenKind::Float) {
            ty!(TypeKind::Float)
        }
        else if self.match_type(TokenKind::Char) {
            ty!(TypeKind::Char)
        }
        else if self.match_type(TokenKind::Bool) {
            ty!(TypeKind::Bool)
        }
        else if self.match_type(TokenKind::Identifier) {
            let name = self.previous_lexem()?;
            ty!(TypeKind::Struct(StructType::new(name)))
        }
        else if self.match_type(TokenKind::LeftBracket) {
            self.array_type()
        }
        else if self.match_type(TokenKind::Ampersand) {
            let inner = self.ty()?;
            ty!(TypeKind::Ref(RefType { of: Box::new(inner) }))
        }
        else if self.match_type(TokenKind::And) {
            let inner = self.ty()?;
            let t = Type { kind: TypeKind::Ref(RefType { of: Box::new(inner) }) };
            ty!(TypeKind::Ref(RefType { of: Box::new(t) }))
        }
        else {
            Err("Expected type".into())
        }
    }
    fn statement(&mut self, stmts: &mut Vec<Statement>) -> Result<()> {
        let stmt = if let Some(vdecl) = self.try_var_decl() {
            let vdecl = vdecl?;
            let span = vdecl.span;
            let stmt = Statement {
                kind: StatementKind::Decl(
                          DeclarationStmt { inner: vdecl }
                      ),
                span
            };
            Ok(stmt)
        }
        else if let Some(block) = self.try_block() {
            block
        }
        else if self.match_type(TokenKind::If) {
            self.if_stmt()
        }
        else if self.match_type(TokenKind::While) {
            self.while_stmt()
        }
        else if self.match_type(TokenKind::For) {
            self.for_stmt()
        }
        else if self.match_type(TokenKind::Return) {
            self.ret_stmt()
        }
        else {
            self.single_line_stmt(stmts)?;
            return Ok(())
        };
        stmts.push(stmt?);
        Ok(())
    }
    fn ret_stmt(&mut self) -> Result<Statement> {
        let span = self.previous_span()?;
        let expr = self.try_expression();
        let endspan = self.consume(TokenKind::Semicolon)?.span;
        Ok(Statement {
            kind: StatementKind::Return(ReturnStmt {
                expr
            }),
            span: span.join(&endspan)
        })
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
    fn block_inner(&mut self) -> Result<Vec<Statement>> {
        let mut stmts = Vec::new();
        while !self.check(TokenKind::RightBrace) && !self.is_finished() {
            self.statement(&mut stmts)?
        }
        Ok(stmts)
    }
    fn try_block(&mut self) -> Option<Result<Statement>> {
        if self.match_type(TokenKind::LeftBrace) {
           Some(self.block())
        } else { None }
    }
    fn block(&mut self) -> Result<Statement> {
        self.consume(TokenKind::LeftBrace)?;
        let start_span = self.previous_span()?;
        let stmts = self.block_inner()?;
        let end_span = self.consume(TokenKind::RightBrace).unwrap().span;
        Ok(Statement {
            kind: StatementKind::Block(BlockStmt { stmts: stmts.into_boxed_slice() }),
            span: start_span.join(&end_span)
        })
    }
    fn statement_flatten(&mut self) -> Result<Statement> {
        let mut stmts = Vec::new();
        self.statement(&mut stmts)?;
        Ok(if stmts.len() > 1 {
            let span = stmts.first().unwrap().span;
            let end_span = stmts.last().unwrap().span;
            Statement {
                kind: StatementKind::Block(BlockStmt { stmts: stmts.into_boxed_slice() }),
                span: span.join(&end_span),
            }
        } else {
            stmts.pop().unwrap()
        })
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
        let body = Box::new(self.statement_flatten()?);
        let span =  span.join(&body.span);
        Ok(Statement {
            kind: StatementKind::For(ForStmt { init, cond, inc, body }),
            span
        })
    }
    fn single_line_stmt(&mut self, stmts: &mut Vec<Statement>) -> Result<()> {
        let ast =
        if self.match_type(TokenKind::Print) {
            self.print_stmt(stmts)?;
            return Ok(())
        }
        else if self.match_type(TokenKind::Read) {
            self.read_stmt(stmts)?;
            return Ok(())
        }
        else if self.match_type(TokenKind::Semicolon) {
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
        stmts.push(ast);
        Ok(())
    }
    fn print_stmt(&mut self, stmts: &mut Vec<Statement>) -> Result<()> {
        let start_span = self.previous_span().unwrap();
        let expr = self.expression()?;
        let span = start_span.join(&expr.span);

        stmts.push(Statement {
            kind: StatementKind::Print(PrintStmt { expr }),
            span
        });

        while self.match_type(TokenKind::Comma) {
            let expr = self.expression()?;
            let span = expr.span;
            stmts.push(Statement {
                kind: StatementKind::Print(PrintStmt { expr }),
                span
            });
        }

        self.consume(TokenKind::Semicolon)?;

        Ok(())
    }

    fn read_stmt(&mut self, stmts: &mut Vec<Statement>) -> Result<()> {
        let start_span = self.previous_span().unwrap();
        let expr = self.expression()?;
        let span = start_span.join(&expr.span);

        stmts.push(Statement {
            kind: StatementKind::Read(ReadStmt { expr }),
            span
        });

        while self.match_type(TokenKind::Comma) {
            let expr = self.expression()?;
            let span = expr.span;
            stmts.push(Statement {
                kind: StatementKind::Read(ReadStmt { expr }),
                span
            });
        }

        self.consume(TokenKind::Semicolon)?;

        Ok(())
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
        self.assignment()
    }

    fn try_expression(&mut self) -> Option<Expression> {
        self.expression().ok()
    }

    fn assignment(&mut self) -> Result<Expression> {
        let left = self.ternary()?;
        let ast = if self.match_type(TokenKind::Equal) {
            let right = Box::new(self.assignment()?);
            let span = left.span.join(&right.span);
            Expression::new(
                ExpressionKind::Assignment(AssignmentExpr { left: Box::new(left), right }),
                span
            )
        } else {
            left
        };
        Ok(ast)
    }
    fn ternary(&mut self) -> Result<Expression> {
        let mut cond = self.logical()?;
        if self.match_type(TokenKind::Question) {
            let if_true = Box::new(self.logical()?);
            self.consume(TokenKind::Colon)?;
            let if_false = Box::new(self.logical()?);
            let span = cond.span.join(&if_false.span);
            cond = Expression::new(
                ExpressionKind::Ternary(TernaryExpr { cond: Box::new(cond), if_true, if_false }),
                span
            )
        }
        Ok(cond)
    }
    fn logical(&mut self) -> Result<Expression> {
        let mut left = self.equality()?;
        while self.match_types(&[TokenKind::Or, TokenKind::And]) {
            let op = self.previous()?.kind;
            let op = BinaryExprOp::try_from(op).map_err(|_| {
                ParseError::new(format!("Unknown operand {op:#?}"))
            })?;
            let right = Box::new(self.logical()?);
            let span = left.span.join(&right.span);
            left = Expression::new(
                ExpressionKind::Binary(BinaryExpr { left: Box::new(left), op, right, kind: BinaryExprKind::Logical }),
                span
            )
        }
        Ok(left)
    }
    fn equality(&mut self) -> Result<Expression> {
        let mut left = self.comparison()?;
        while self.match_types(&[TokenKind::BangEqual, TokenKind::EqualEqual]) {
            let op = self.previous()?.kind;
            let op = BinaryExprOp::try_from(op).map_err(|_| {
                ParseError::new(format!("Unknown operand {op:#?}"))
            })?;
            let right = Box::new(self.comparison()?);
            let span = left.span.join(&right.span);
            left = Expression::new(
                ExpressionKind::Binary(BinaryExpr { left: Box::new(left), op, right, kind: BinaryExprKind::Comparison }),
                span
            )
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
            let op = self.previous()?.kind;
            let op = BinaryExprOp::try_from(op).map_err(|_| {
                ParseError::new(format!("Unknown operand {op:#?}"))
            })?;
            let right = Box::new(self.term()?);
            let span = left.span.join(&right.span);
            left = Expression::new(
                ExpressionKind::Binary(BinaryExpr { left: Box::new(left), op, right, kind: BinaryExprKind::Comparison }),
                span
            )
        }
        Ok(left)
    }
    fn term(&mut self) -> Result<Expression> {
        let mut left = self.factor()?;
        while self.match_types(&[TokenKind::Minus,TokenKind::Plus]){
            let op = self.previous()?.kind;
            let op = BinaryExprOp::try_from(op).map_err(|_| {
                ParseError::new(format!("Unknown operand {op:#?}"))
            })?;
            let right = Box::new(self.factor()?);
            let span = left.span.join(&right.span);
            left = Expression::new(
                ExpressionKind::Binary(BinaryExpr { left: Box::new(left), op, right, kind: BinaryExprKind::Arithmetic }),
                span
            )
        }
        Ok(left)
    }
    fn factor(&mut self) -> Result<Expression> {
        let mut left = self.unary()?;
        while self.match_types(&[TokenKind::Slash,TokenKind::Star,TokenKind::Mod]){
            let op = self.previous()?.kind;
            let op = BinaryExprOp::try_from(op).map_err(|_| {
                ParseError::new(format!("Unknown operand {op:#?}"))
            })?;
            let right = Box::new(self.unary()?);
            let span = left.span.join(&right.span);
            left = Expression::new(
                ExpressionKind::Binary(BinaryExpr { left: Box::new(left), op, right, kind: BinaryExprKind::Arithmetic }),
                span
            )
        }
        Ok(left)
    }
    fn unary(&mut self) -> Result<Expression> {
        if self.match_types(&[TokenKind::Bang,TokenKind::Minus,TokenKind::Plus]) {
            let start_span = self.previous_span().unwrap();
            let op = self.previous()?.kind;
            let op = UnaryExprOp::try_from(op).map_err(|_| {
                ParseError::new(format!("Invalid unary operand {op:#?}"))
            })?;
            let expr = Box::new(self.unary()?);
            let span = start_span.join(&expr.span);
            Ok(Expression::new(
                ExpressionKind::Unary(UnaryExpr { op, expr }),
                span
            ))
        }
        else if self.match_type(TokenKind::Ampersand) {
            let start_span = self.previous_span().unwrap();
            let expr = Box::new(self.unary()?);
            let span = start_span.join(&expr.span);
            Ok(Expression::new(ExpressionKind::Ref(Reference {
                of: expr,
            }), span))
        }
        else if self.match_type(TokenKind::Star) {
            let start_span = self.previous_span().unwrap();
            let expr = Box::new(self.unary()?);
            let span = start_span.join(&expr.span);
            Ok(Expression::new(ExpressionKind::Deref(Dereference {
                of: expr,
            }), span))
        }
        else {
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
        else if self.match_type(TokenKind::IntLiteral) {
            LitValue::Int(self.previous_parse::<i32>()?)
        }
        else if self.match_type(TokenKind::FloatLiteral) {
            let f: f64 = self.previous()?.span.slice(self.src).parse()?;
            LitValue::Float(f)
        }
        else if self.match_type(TokenKind::CharLiteral) {
            let prev = self.previous().unwrap();
            let lit = prev.span.slice(self.src)
                .strip_prefix('\'').unwrap()
                .strip_suffix('\'').unwrap();
            let lit = Unescaped::from(lit).next().ok_or_else(|| format!("Invalid escape '{lit}'"))?;
            LitValue::Char(lit)
        }
        else {
            return Ok(None)
        }))
    }
    fn access(&mut self, mut expr: Expression) -> Result<Expression> {
        if self.match_type(TokenKind::LeftBracket) {
            let index = self.expression()?;
            let span = self.consume(TokenKind::RightBracket)?.span;
            let span = expr.span.join(&span);

            let acc = ArrayAccess {
                array: Box::new(expr),
                index: Box::new(index),
            };
            expr = Expression::new(ExpressionKind::ArrayAccess(acc), span);
            self.access(expr)
        }
        else if self.match_type(TokenKind::Dot) {
            let field = self.consume(TokenKind::Identifier)?;
            let field_span = field.span;
            let field = self.owned_lexem(field_span);
            let span = expr.span.join(&field_span);
            let saccess = StructAccess::new(expr, field);
            expr = Expression::new(ExpressionKind::StructAccess(saccess), span);
            self.access(expr)
        }
        else {
             Ok(expr)
        }
    }
    fn primary(&mut self) -> Result<Expression> {
        let expr = self.__primary()?;
        self.access(expr)
    }
    fn __primary(&mut self) -> Result<Expression> {
        if let Some(value) = self.literal()? {
            return Ok(Expression::new(
                ExpressionKind::Literal(LitExpr { value }),
                self.previous_span().unwrap()
            ));
        }
        if self.match_type(TokenKind::LeftParen) {
            let start = self.previous_span().unwrap();
            let mut expr = self.expression()?;
            let end = self.consume(TokenKind::RightParen)?.span;
            expr.span = start.join(&end);
            return Ok(expr)
        }
        if self.match_type(TokenKind::Identifier) {
            let prev_span = self.previous()?.span;
            let name = self.owned_lexem(prev_span);
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
                return Ok(Expression::new(
                    ExpressionKind::Call(
                        CallExpr {
                            callee: name,
                            args: args.into_boxed_slice(),
                            decl: AstRef::empty()
                        }),
                    span
                ))
            } else {
                return Ok(Expression::new(
                    ExpressionKind::Variable(
                              VariableExpr {
                                  name,
                                  decl: AstRef::empty()
                              }),
                    prev_span,
                ))
            }
        }
        ParseError::new(
             format!("Expected expression, found: {}", self.peek()?.span.slice(self.src))).err()
    }
    fn owned_lexem(&mut self, span: Span) -> Symbol {
        let slice = span.slice(self.src);
        with_session(|sess| {
            sess.string_interner.get_or_intern(slice)
        })
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
    fn previous_parse<T: FromStr>(&self) -> Result<T> {
        self.previous()?.span.slice(self.src).parse::<T>().map_err(|_| {
            ParseError::from("Error parsing lexem")
        })
    }
    fn previous_lexem(&mut self) -> Result<Symbol> {
        Ok(self.owned_lexem(self.previous()?.span))
    }
    fn synchronize_with(&mut self, safe: &[TokenKind]) -> bool {
        self.bump();
        while !self.is_finished() {
            if safe.contains(&self.peek().unwrap().kind) {
                return true;
            }
            self.bump();
        }
        false
    }
    fn error(&mut self, err: &str) {
        use span::FilePosition;

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

        eprintln!("\nParser: [{start_line},{start_col}] {err}");
        let line = self.src.lines().nth(start_line).unwrap_or("").trim();
        eprintln!("|-> {line}");

        self.n_errors += 1;
    }
}
