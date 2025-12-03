use ast::expr::ExpressionKind;
use ast::{stmt::StatementKind, Statement};
use lexer::token::TokenKind;

use super::{Parser, Result};

impl Parser<'_, '_> {
    pub(super) fn statement(&mut self) -> Result<Statement> {
        if let Some(item) = self.try_item() {
            let item = item?;
            let span = item.span;
            let stmt = Statement {
                kind: StatementKind::Item(Box::new(item)),
                span,
            };
            Ok(stmt)
        }
        else if self.match_type(TokenKind::While) {
            self.while_stmt()
        } else if self.match_type(TokenKind::For) {
            self.for_stmt()
        } else if self.match_type(TokenKind::Return) {
            self.ret_stmt()
        } else {
            self.single_line_stmt()
        }
    }
    fn ret_stmt(&mut self) -> Result<Statement> {
        let kw_ret = self.previous_span()?;
        let expr = self.try_expression();
        let semmicollon = self.consume(TokenKind::Semicolon)?.span;
        Ok(Statement {
            kind: StatementKind::Return {
                kw_ret,
                expr,
                semmicollon,
            },
            span: kw_ret.join(&semmicollon),
        })
    }

    fn while_stmt(&mut self) -> Result<Statement> {
        let kw_while = self.previous_span()?;
        self.consume(TokenKind::LeftParen)?;
        let cond = self.expression()?;
        self.consume(TokenKind::RightParen)?;
        let body = Box::new(Statement::from(self.block(Self::statement)?));
        let span = kw_while.join(&body.span);
        Ok(Statement {
            kind: StatementKind::While {
                kw_while,
                cond,
                body,
            },
            span,
        })
    }
    fn for_stmt(&mut self) -> Result<Statement> {
        let kw_for = self.previous_span()?;
        self.consume(TokenKind::LeftParen)?;
        let init = if self.check_types(&[TokenKind::Let, TokenKind::Const]) {
            Some(Box::new(self.var_decl()?))
        } else {
            None
        };
        let cond = if self.check(TokenKind::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(TokenKind::Semicolon)?;
        let inc = if self.check(TokenKind::RightParen) {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(TokenKind::RightParen)?;
        let body = Box::new(self.statement()?);
        let span = kw_for.join(&body.span);
        Ok(Statement {
            kind: StatementKind::For {
                kw_for,
                init,
                cond,
                inc,
                body,
            },
            span,
        })
    }
    fn single_line_stmt(&mut self) -> Result<Statement> {
        let ast =  if self.match_type(TokenKind::Semicolon) {
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

    fn expression_as_stmt(&mut self) -> Result<Statement> {
        let expr = self.expression()?;
        let mut span = expr.span;
        let mut semmi = None;
        if !matches!(expr.kind, ExpressionKind::If { .. } |  ExpressionKind::Block(_)) {
            let s = self.consume(TokenKind::Semicolon)?.span;
            span = span.join(&s);
            semmi = Some(s);
        }
        Ok(Statement {
            kind: StatementKind::Expression(expr, semmi),
            span,
        })
    }
}
