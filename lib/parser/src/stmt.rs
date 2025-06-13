use ast::{stmt::StatementKind, Statement};
use lexer::token::TokenKind;

use super::{Parser, Result};

impl Parser<'_, '_> {
    pub(super) fn statement(&mut self) -> Result<Statement> {
        let mut item = self.try_var_decl();
        if item.is_none() {
            item = self.try_struct();
        }
        if let Some(item) = item {
            let item = item?;
            let span = item.span;
            let stmt = Statement {
                kind: StatementKind::Item(Box::new(item)),
                span,
            };
            Ok(stmt)
        }
        else if let Some(block) = self.try_block() {
            block.map(Statement::from)
        } else if self.match_type(TokenKind::If) {
            self.if_stmt()
        } else if self.match_type(TokenKind::While) {
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
            kind: StatementKind::If {
                kw_if,
                cond,
                if_body,
                kw_else,
                else_body,
            },
            span,
        })
    }
    fn while_stmt(&mut self) -> Result<Statement> {
        let kw_while = self.previous_span()?;
        self.consume(TokenKind::LeftParen)?;
        let cond = self.expression()?;
        self.consume(TokenKind::RightParen)?;
        let body = Box::new(Statement::from(self.block()?));
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
        let ast = if self.match_type(TokenKind::Print) {
            return self.print_stmt();
        } else if self.match_type(TokenKind::Read) {
            return self.read_stmt();
        } else if self.match_type(TokenKind::Semicolon) {
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
    fn print_stmt(&mut self) -> Result<Statement> {
        let start_span = self.previous_span()?;
        let exprs = self.comma_sep_expr()?;
        let semmi = self.consume(TokenKind::Semicolon)?.span;
        let span = start_span.join(&semmi);

        Ok(Statement {
            kind: StatementKind::Print(start_span, exprs, semmi),
            span,
        })
    }

    fn read_stmt(&mut self) -> Result<Statement> {
        let start_span = self.previous_span()?;
        let exprs = self.comma_sep_expr()?;
        let semmi = self.consume(TokenKind::Semicolon)?.span;
        let span = start_span.join(&semmi);

        Ok(Statement {
            kind: StatementKind::Read(start_span, exprs, semmi),
            span,
        })
    }

    fn expression_as_stmt(&mut self) -> Result<Statement> {
        let expr = self.expression()?;
        let semmi = self.consume(TokenKind::Semicolon)?.span;
        let span = expr.span.join(&semmi);
        Ok(Statement {
            kind: StatementKind::Expression(expr, semmi),
            span,
        })
    }
}
