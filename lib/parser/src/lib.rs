pub mod error;

use core::str;
use std::borrow::Cow;
use std::str::FromStr;

use ast::declaration::{VariableConstness, DeclarationKind, Field, Param};
use ast::expr::{BinaryExprOp, ExpressionKind, UnaryExprOp};
use ast::stmt::StatementKind;
use ast::types::{Type, TypeKind};
use ast::{Block, Declaration, Expression, Parenthesized};
use ast::{expr::LitValue, Statement, Program};
use error::ParseErrorKind;
use error_manager::ErrorManager;
use lexer::TokenStream;
use session::Symbol;
use lexer::token::{Token, TokenKind};

use lexer::unescaped::Unescaped;
use span::{Span, Spanned};
use session::{with_session, with_session_interner};
use self::error::ParseError;

type Result<T> = std::result::Result<T,ParseErrorKind>;

pub fn parse<'src>(stream: TokenStream<'_,'src>, src: &'src str, em: &mut ErrorManager) -> Option<Program> {
    Parser {
        stream,
        src,
        em
    }.parse()
}

struct Parser<'sess, 'src> {
    stream: TokenStream<'sess, 'src>,
    src: &'src str,
    em: &'sess mut ErrorManager,
}

impl<'sess, 'src> Parser<'sess, 'src> {
    fn parse(mut self) -> Option<Program> {
        let mut decls = Vec::new();
        while !self.is_finished() {
            match self.declaration() {
                Ok(stmt) => decls.push(stmt),
                Err(e) => {
                    self.error(e);
                    self.synchronize_with(&[
                        TokenKind::Let, TokenKind::Const, TokenKind::Fn
                    ]);
                }
            }
        }
        if self.em.has_errors() {
            None
        } else {
            Some( Program { decls: decls.into_boxed_slice() } )
        }
    }

    fn consume_ident_spanned(&mut self) -> Result<Spanned<Symbol>> {
        let span = self.consume(TokenKind::Identifier)?.span;
        Ok(with_session_interner(|i| {
            let sym = i.get_or_intern(span.slice(self.src));
            Spanned {
                val: sym,
                span,
            }
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
            Err(ParseErrorKind::ExpectedNode("declaration"))
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
    fn struct_field(&mut self) -> Result<Field> {
        let name = self.consume_ident_spanned()?;
        self.consume(TokenKind::Colon)?;
        let ty = self.ty()?;
        let span = name.span.join(&ty.span);
        Ok(Field {
            ty,
            name,
            span,
        })
    }
    fn struct_decl(&mut self) -> Result<Declaration> {
        let kw_struct = self.previous_span()?;
        let name = self.consume_ident_spanned()?;
        let lb = self.consume(TokenKind::LeftBrace)?.span;

        let mut fields = Vec::new();
        loop {
            if self.check(TokenKind::RightBrace) { break };


            let field = self.struct_field()?;
            fields.push(field);

            if self.match_type(TokenKind::Comma) {
                continue;
            } else {
                break
            }
        }

        let end_span = self.consume(TokenKind::RightBrace)?.span;

        let fields = Block {
            val: fields.into_boxed_slice(),
            open_bracket: lb,
            close_bracket: end_span,
        };

        let span = kw_struct.join(&fields.close_bracket);

        Ok(
            Declaration {
                kind: DeclarationKind::Struct { kw_struct, name, fields },
                span,
            }
        )
    }
    fn param(&mut self) -> Result<Param> {
        let name = self.consume_ident_spanned()?;
        self.consume(TokenKind::Colon)?;
        let ty = self.ty()?;
        let span = name.span.join(&ty.span);
        Ok(Param {
            span,
            ty,
            name,
        })
    }
    fn function(&mut self) -> Result<Declaration> {
        let kw_fn = self.previous_span()?;

        let name = self.consume_ident_spanned()?;

        self.consume(TokenKind::LeftParen)?;

        let mut params = Vec::new();

        let mut first = true;
        while !self.match_type(TokenKind::RightParen) {
            if !first {
                self.consume(TokenKind::Comma)?;
            }
            first = false;

            let p = self.param()?;
            params.push(p);
        }

        let return_type = if self.match_type(TokenKind::Arrow) {
            Some(self.ty()?)
        } else { None };

        let body = self.block()?;

        let span = kw_fn.join(&body.close_bracket);

        Ok(Declaration {
            kind: DeclarationKind::Function {
                kw_fn,
                name,
                params: params.into_boxed_slice(),
                return_type,
                body,
            },
            span,
        })
    }
    fn try_var_decl(&mut self) -> Option<Result<Declaration>> {
        if self.check_types(&[TokenKind::Let,TokenKind::Const]) {
            Some(self.var_decl())
        } else { None }
    }
    fn expected_err<T>(&self, tokens: &'static [TokenKind]) -> Result<T> {
        let found = self.peek()?.kind;
        Err(ParseErrorKind::ExpectedToken { tokens: tokens.into(), found })
    }
    fn var_decl(&mut self) -> Result<Declaration> {
        let constness =
        if self.match_type(TokenKind::Let) {
            VariableConstness::Let(self.previous_span()?)
        } else if self.match_type(TokenKind::Const) {
            VariableConstness::Const(self.previous_span()?)
        } else {
            return self.expected_err(&[TokenKind::Let, TokenKind::Const])
        };
        let prev = self.previous()?;
        let prev_span = prev.span;
        let is_const = prev.kind == TokenKind::Const;

        let name = self.consume_ident_spanned()?;

        let mut span = if is_const {
            prev_span
        } else {
            name.span
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

        let decl = Declaration {
            kind: DeclarationKind::Variable {
                constness,
                name,
                ty,
                init,
                semicolon
            },
            span
        };
        Ok(decl)
    }
    fn array_type(&mut self) -> Result<Type> {
        let ob = self.previous_span()?;
        let ty = Box::new(self.ty()?);
        let sc = self.consume(TokenKind::Semicolon)?.span;
        self.consume(TokenKind::IntLiteral)?;
        let length = self.previous_parse::<usize>()?;
        let cb = self.consume(TokenKind::RightBracket)?.span;
        let span = ob.join(&cb);
        Ok(Type {
            kind: TypeKind::Array {
                open_brace: ob,
                ty,
                semicollon: sc,
                length,
                close_brace: cb
            },
            span
        })
    }
    fn ty(&mut self) -> Result<Type> {
        macro_rules! ty {
            ($tk:expr) => {
                {
                    let span = self.previous_span()?;
                    Ok(Type{
                        kind: $tk(span),
                        span,
                    })
                }
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
            let name = self.previous_lexem_spanned()?;
            let span = name.span;
            Ok(Type {
                kind: TypeKind::Struct(name),
                span
            })
        }
        else if self.match_type(TokenKind::LeftBracket) {
            self.array_type()
        }
        else if self.match_type(TokenKind::Ampersand) {
            let aspan = self.previous_span()?;
            let inner = self.ty()?;
            let span = aspan.join(&inner.span);
            Ok(Type {
                kind: TypeKind::Ref { ampersand: aspan , of: Box::new(inner) },
                span,
            })
        }
        else if self.match_type(TokenKind::And) {
            let aspan = self.previous_span()?;
            let inner = self.ty()?;
            let span = aspan.join(&inner.span);
            let t = Type {
                kind: TypeKind::Ref { ampersand: aspan, of: Box::new(Type {
                    kind: TypeKind::Ref { ampersand: aspan, of: Box::new(inner) },
                    span,
                })},
                span,
            };
            Ok(t)
        }
        else {
            Err(ParseErrorKind::ExpectedNode("type"))
        }
    }
    fn statement(&mut self) -> Result<Statement> {
        let stmt =
        if let Some(vdecl) = self.try_var_decl() {
            let vdecl = vdecl?;
            let span = vdecl.span;
            let stmt = Statement {
                kind: StatementKind::Decl(vdecl),
                span
            };
            Ok(stmt)
        }
        else if let Some(block) = self.try_block() {
            block.map(Statement::from)
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
            return self.single_line_stmt();
        };
        stmt
    }
    fn ret_stmt(&mut self) -> Result<Statement> {
        let kw_ret = self.previous_span()?;
        let expr = self.try_expression();
        let semmicollon = self.consume(TokenKind::Semicolon)?.span;
        Ok(Statement {
            kind: StatementKind::Return { kw_ret, expr, semmicollon },
            span: kw_ret.join(&semmicollon)
        })
    }

    fn parenthesized<T>(&mut self, f: impl for <'a> FnOnce(&'a mut Parser<'sess, 'src>) -> Result<T>) -> Result<Parenthesized<T>> {
        let op = self.consume(TokenKind::LeftParen)?.span;
        let val = f(self)?;
        let cp = self.consume(TokenKind::RightParen)?.span;
        Ok(Parenthesized {
            open_paren: op,
            val,
            close_paren: cp,
        })
    }

    fn if_stmt(&mut self) -> Result<Statement> {
        let kw_if = self.previous_span()?;

        let cond = self.parenthesized(Self::expression)?;

        let if_body = Box::new(self.statement()?);
        let mut span = kw_if.join(&if_body.span);

        let mut else_body = None;
        let mut kw_else = None;
        if self.match_type(TokenKind::Else) {
            kw_else = Some(self.previous_span()?);
            let blk = self.statement()?;
            span = span.join(&blk.span);
            else_body = Some(Box::new(blk));
        }

        Ok(Statement {
            kind: StatementKind::If { kw_if, cond, if_body, kw_else, else_body },
            span
        })
    }
    fn while_stmt(&mut self) -> Result<Statement> {
        let kw_while = self.consume(TokenKind::LeftParen)?.span;
        let cond = self.expression()?;
        self.consume(TokenKind::RightParen)?;
        let body = Box::new(Statement::from(self.block()?));
        let span = kw_while.join(&body.span);
        Ok(Statement {
            kind: StatementKind::While { kw_while, cond, body },
            span
        })
    }
    fn block_inner(&mut self) -> Result<Vec<Statement>> {
        let mut stmts = Vec::new();
        while !self.check(TokenKind::RightBrace) && !self.is_finished() {
            let stmt = self.statement()?;
            stmts.push(stmt);
        }
        Ok(stmts)
    }
    fn try_block(&mut self) -> Option<Result<Block<Statement>>> {
        if self.check(TokenKind::LeftBrace) {
           Some(self.block())
        } else { None }
    }
    fn block(&mut self) -> Result<Block<Statement>> {
        let open = self.consume(TokenKind::LeftBrace)?.span;
        let stmts = self.block_inner()?;
        let close = self.consume(TokenKind::RightBrace)?.span;
        Ok(Block {
            open_bracket: open,
            close_bracket: close,
            val: stmts.into_boxed_slice()
        })
    }
    fn for_stmt(&mut self) -> Result<Statement> {
        let kw_for = self.previous_span()?;
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
        let span =  kw_for.join(&body.span);
        Ok(Statement {
            kind: StatementKind::For { kw_for, init, cond, inc, body },
            span
        })
    }
    fn single_line_stmt(&mut self) -> Result<Statement> {
        let ast =
        if self.match_type(TokenKind::Print) {
            return self.print_stmt();
        }
        else if self.match_type(TokenKind::Read) {
            return self.read_stmt();
        }
        else if self.match_type(TokenKind::Semicolon) {
            Statement {
                kind: StatementKind::Empty(self.previous_span()?),
                span: self.previous_span()?,
            }
        } else if self.match_type(TokenKind::Break) {
            Statement {
                kind: StatementKind::Break(self.previous_span()?),
                span: self.previous_span()?,
            }
        } else if self.match_type(TokenKind::Continue) {
            Statement {
                kind: StatementKind::Continue(self.previous_span()?),
                span: self.previous_span()?,
            }
        } else {
            self.expression_as_stmt()?
        };
        Ok(ast)
    }
    fn args(&mut self) -> Result<Parenthesized<Box<[Expression]>>> {
        let op = self.previous_span()?;

        let mut exprs = Vec::new();

        let mut first = true;
        while !self.match_type(TokenKind::RightParen) {
            if !first {
                self.consume(TokenKind::Comma)?;
            }
            first = false;
            let expr = self.expression()?;
            exprs.push(expr);
        }

        let cp = self.previous_span()?;

        Ok(Parenthesized {
            val: exprs.into_boxed_slice(),
            open_paren: op,
            close_paren: cp,
        })

    }
    fn comma_sep_expr(&mut self) -> Result<Box<[Expression]>> {
        let mut exprs = Vec::new();
        let expr = self.expression()?;
        exprs.push(expr);

        while self.match_type(TokenKind::Comma) {
            let expr = self.expression()?;
            exprs.push(expr);
        }

        Ok(exprs.into_boxed_slice())
    }
    fn print_stmt(&mut self) -> Result<Statement> {
        let start_span = self.previous_span()?;
        let exprs = self.comma_sep_expr()?;
        let semmi = self.consume(TokenKind::Semicolon)?.span;
        let span = start_span.join(&semmi);

        Ok(Statement {
            kind: StatementKind::Print(start_span, exprs, semmi),
            span
        })
    }

    fn read_stmt(&mut self) -> Result<Statement> {
        let start_span = self.previous_span()?;
        let exprs = self.comma_sep_expr()?;
        let semmi = self.consume(TokenKind::Semicolon)?.span;
        let span = start_span.join(&semmi);

        Ok(Statement {
            kind: StatementKind::Read(start_span, exprs, semmi),
            span
        })
    }

    fn expression_as_stmt(&mut self) -> Result<Statement> {
        let expr = self.expression()?;
        let semmi = self.consume(TokenKind::Semicolon)?.span;
        let span = expr.span.join(&semmi);
        Ok(Statement {
            kind: StatementKind::Expression(expr, semmi),
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
            let eq = Spanned {
                val: BinaryExprOp::Assign,
                span: self.previous_span()?
            };
            let right = Box::new(self.assignment()?);
            let span = left.span.join(&right.span);
            Expression {
                kind: ExpressionKind::Binary { op: eq, left: Box::new(left), right },
                span
            }
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
            cond = Expression {
                kind: ExpressionKind::Ternary { cond: Box::new(cond), if_true, if_false },
                span,
            }
        }
        Ok(cond)
    }
    fn logical(&mut self) -> Result<Expression> {
        let mut left = self.equality()?;
        while self.match_types(&[TokenKind::Or, TokenKind::And]) {
            let op = self.previous()?;
            let ops = BinaryExprOp::try_from(op.kind).map_err(|_| {
                ParseErrorKind::InvalidBinaryOp(op.kind)
            })?;
            let op = Spanned {
                val: ops,
                span: op.span,
            };
            let right = Box::new(self.factor()?);
            let span = left.span.join(&right.span);
            left = Expression {
                kind: ExpressionKind::Binary { op, left: Box::new(left), right },
                span,
            };
        }
        Ok(left)
    }
    fn equality(&mut self) -> Result<Expression> {
        let mut left = self.comparison()?;
        while self.match_types(&[TokenKind::BangEqual, TokenKind::EqualEqual]) {
            let op = self.previous()?;
            let ops = BinaryExprOp::try_from(op.kind).map_err(|_| {
                ParseErrorKind::InvalidBinaryOp(op.kind)
            })?;
            let op = Spanned {
                val: ops,
                span: op.span,
            };
            let right = Box::new(self.factor()?);
            let span = left.span.join(&right.span);
            left = Expression {
                kind: ExpressionKind::Binary { op, left: Box::new(left), right },
                span,
            };
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
            let op = self.previous()?;
            let ops = BinaryExprOp::try_from(op.kind).map_err(|_| {
                ParseErrorKind::InvalidBinaryOp(op.kind)
            })?;
            let op = Spanned {
                val: ops,
                span: op.span,
            };
            let right = Box::new(self.factor()?);
            let span = left.span.join(&right.span);
            left = Expression {
                kind: ExpressionKind::Binary { op, left: Box::new(left), right },
                span,
            };
        }
        Ok(left)
    }
    fn term(&mut self) -> Result<Expression> {
        let mut left = self.factor()?;
        while self.match_types(&[TokenKind::Minus,TokenKind::Plus]){
            let op = self.previous()?;
            let ops = BinaryExprOp::try_from(op.kind).map_err(|_| {
                ParseErrorKind::InvalidBinaryOp(op.kind)
            })?;
            let op = Spanned {
                val: ops,
                span: op.span,
            };
            let right = Box::new(self.factor()?);
            let span = left.span.join(&right.span);
            left = Expression {
                kind: ExpressionKind::Binary { op, left: Box::new(left), right },
                span,
            };
        }
        Ok(left)
    }
    fn factor(&mut self) -> Result<Expression> {
        let mut left = self.unary()?;
        while self.match_types(&[TokenKind::Slash,TokenKind::Star,TokenKind::Mod]){
            let op = self.previous()?;
            let ops = BinaryExprOp::try_from(op.kind).map_err(|_| {
                ParseErrorKind::InvalidBinaryOp(op.kind)
            })?;
            let op = Spanned {
                val: ops,
                span: op.span,
            };
            let right = Box::new(self.factor()?);
            let span = left.span.join(&right.span);
            left = Expression {
                kind: ExpressionKind::Binary { op, left: Box::new(left), right },
                span,
            };
        }
        Ok(left)
    }
    fn unary(&mut self) -> Result<Expression> {
        if self.match_types(&[TokenKind::Bang,TokenKind::Minus,TokenKind::Plus,TokenKind::Ampersand,TokenKind::Star]) {
            let op = self.previous()?;
            let opk = UnaryExprOp::try_from(op.kind).map_err(|_| {
                ParseErrorKind::InvalidUnaryOp(op.kind)
            })?;
            let op = Spanned {
                val: opk,
                span: op.span,
            };
            let expr = Box::new(self.unary()?);
            let span = op.span.join(&expr.span);
            Ok(Expression {
                kind: ExpressionKind::Unary { op, expr },
                span,
            })
        }
        else {
            self.primary()
        }
    }
    fn literal(&mut self) -> Result<Option<Spanned<LitValue>>> {
        macro_rules! spanned_lit {
            ($v:ident, $e:expr) => {
               Spanned {
                   val: LitValue::  $v ($e),
                   span: self.previous_span()?
               }
            };
        }
        Ok(Some(if self.match_type(TokenKind::False) {
            spanned_lit!(Bool, false)
        }
        else if self.match_type(TokenKind::True) {
            spanned_lit!(Bool, true)
        }
        else if self.match_type(TokenKind::String) {
            spanned_lit!(Str, self.previous_lexem()?)
        }
        else if self.match_type(TokenKind::IntLiteral) {
            spanned_lit!(Int, self.previous_parse::<i32>()?)
        }
        else if self.match_type(TokenKind::FloatLiteral) {
            let f: f64 = self.previous()?.span.slice(self.src).parse().unwrap();
            spanned_lit!(Float, f)
        }
        else if self.match_type(TokenKind::CharLiteral) {
            let prev = self.previous()?;
            let lit = prev.span.slice(self.src)
                .strip_prefix('\'').unwrap()
                .strip_suffix('\'').unwrap();
            let lit = Unescaped::from(lit).next().ok_or_else(|| ParseErrorKind::InvalidEscape(lit.to_string()))?;
            spanned_lit!(Char, lit)
        }
        else {
            return Ok(None)
        }))
    }
    fn access(&mut self, mut expr: Expression) -> Result<Expression> {
        if self.match_type(TokenKind::LeftBracket) {
            let index = self.expression()?;
            let cb = self.consume(TokenKind::RightBracket)?.span;
            let span = expr.span.join(&cb);

            expr = Expression {
                kind: ExpressionKind::ArrayAccess { arr: Box::new(expr), index: Box::new(index), closing_bracket: cb },
                span
            };
            self.access(expr)
        }
        else if self.match_type(TokenKind::Dot) {
            let field = self.consume_ident_spanned()?;
            let span = expr.span.join(&field.span);

            expr = Expression {
                kind: ExpressionKind::StructAccess { st: Box::new(expr), field },
                span,
            };
            self.access(expr)
        }
        else if self.match_type(TokenKind::LeftParen) {
            let args = self.args()?;
            let span = expr.span.join(&args.close_paren);
            expr = Expression {
                kind: ExpressionKind::Call { callee: Box::new(expr), args },
                span
            };
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
            let span = value.span;
            return Ok(Expression {
                kind: ExpressionKind::Literal(value),
                span,
            })
        }
        if self.match_type(TokenKind::LeftParen) {
            let start = self.previous_span()?;
            let mut expr = self.expression()?;
            let end = self.consume(TokenKind::RightParen)?.span;
            expr.span = start.join(&end);
            return Ok(expr)
        }
        if self.match_type(TokenKind::Identifier) {
            let name = self.previous_lexem_spanned()?;
            let span = name.span;
            return Ok(Expression {
                kind: ExpressionKind::Path(name),
                span,
            });
        }
        Err(ParseErrorKind::ExpectedConstruct {
            expected: "expression",
            found: self.peek()?.span.slice(self.src).to_string()
        })
    }
    fn owned_lexem(&mut self, span: Span) -> Symbol {
        let slice = span.slice(self.src);
        with_session(|sess| {
            sess.string_interner.get_or_intern(slice)
        })
    }
    fn consume(&mut self, t: TokenKind) -> Result<&Token> {
        if self.check(t) { return self.advance(); }
        Err(ParseErrorKind::ExpectedToken {
            tokens: Cow::from(&[t]).into_owned().into(),
            found: self.peek()?.kind
        })
    }
    fn match_type(&mut self, t: TokenKind) -> bool {
        if self.check(t) {
            self.advance().unwrap_or_else(|_| unreachable!());
            return true;
        }
        false
    }
    fn match_types(&mut self, types: &[TokenKind]) -> bool {
        types.iter().any(|t| self.match_type(*t))
    }
    fn check_types(&mut self, types: &[TokenKind]) -> bool {
        types.iter().any(|t| self.check(*t))
    }
    fn check(&mut self, t: TokenKind) -> bool {
        if self.is_finished() { return false; }
        self.peek().unwrap_or_else(|_| unreachable!()).kind == t
    }
    fn bump(&mut self) {
        self.stream.next();
    }
    fn advance(&mut self) -> Result<&Token> {
        self.bump();
        self.previous()
    }
    pub fn is_finished(&self) -> bool {
        self.stream.is_finished()
    }
    fn peek(&self) -> Result<&Token> {
        self.stream.peek()
                   .ok_or_else(|| ParseErrorKind::CantPeek)
    }
    fn previous(&self) -> Result<&Token> {
        self.stream.previous()
                   .ok_or_else(|| ParseErrorKind::NoPreviousToken)
    }
    fn previous_span(&self) -> Result<Span> {
        self.previous().map(|s| s.span)
    }
    fn previous_parse<T: FromStr>(&self) -> Result<T> {
        self.previous()?.span.slice(self.src).parse::<T>().map_err(|_| {
            ParseErrorKind::LexemParseError
        })
    }
    fn previous_lexem(&mut self) -> Result<Symbol> {
        Ok(self.owned_lexem(self.previous()?.span))
    }

    fn previous_lexem_spanned(&mut self) -> Result<Spanned<Symbol>> {
        let span = self.previous()?.span;
        let lex = self.owned_lexem(span);
        Ok(Spanned {
            val: lex,
            span,
        })
    }
    fn synchronize_with(&mut self, safe: &[TokenKind]) -> bool {
        self.bump();
        while !self.is_finished() {
            if safe.contains(&self.peek().unwrap_or_else(|_| unreachable!()).kind) {
                return true;
            }
            self.bump();
        }
        false
    }
    fn error(&mut self, kind: ParseErrorKind) {
        let tok = if self.is_finished() {
            self.previous()
        } else {
            self.peek()
        }.unwrap_or_else(|_| unreachable!());

        self.em.emit_error(ParseError {
            kind,
            span: tok.span,
        });
    }
}
