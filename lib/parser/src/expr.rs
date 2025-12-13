use ast::{Block, Statement};
use ast::{
    expr::{BinaryExprOp, ExpressionKind, LitValue, UnaryExprOp},
    Expression, Parenthesized,
};
use lexer::{token::TokenKind, unescaped::Unescaped};
use span::{Span, Spanned};

use super::{Parser, Result};
use crate::error::ParseErrorKind;

impl Parser<'_, '_> {
    pub(super) fn expression(&mut self) -> Result<Expression> {
        if let Some(bexpr) = self.try_block_expr() {
            bexpr
        } else if let Some(ifexpr) = self.try_if_expr() {
            ifexpr
        } else {
            self.assignment()
        }
    }

    pub(super) fn try_expression(&mut self) -> Option<Expression> { self.expression().ok() }

    pub(super) fn try_block_expr(&mut self) -> Option<Result<Expression>> {
       self.check(TokenKind::LeftBrace).then(|| self.block_expr())
    }

    fn block_body(&mut self, stmts: &mut Vec<Statement>, expr: &mut Option<Expression>) -> Result<bool> {
        if let Some(e) = self.try_expression() {
            if self.check(TokenKind::RightBrace) {
                *expr = Some(e);
                return Ok(true)
            }
            stmts.push(self.expression_into_stmt(e)?);
        } else {
            stmts.push(self.statement()?);
        }
        Ok(false)
    }
    pub(super) fn block(&mut self) -> Result<Block<Statement, Expression>> {
        let left_brace = self.consume(TokenKind::LeftBrace)?.span;

        let mut stmts = Vec::new();
        let mut expr = None;
        while !self.is_finished() && !self.check(TokenKind::RightBrace) {
            let finish = self.block_body(&mut stmts, &mut expr)
                .unwrap_or_else(|err| {
                    self.error(err);
                    self.synchronize_with(&[TokenKind::Semicolon, TokenKind::RightBrace]);
                    false
                });
            if finish { break }
        }


        let right_brace = self.consume(TokenKind::RightBrace)?.span;

        Ok(Block {
            val: stmts.into(),
            tail: expr.map(Box::new),
            open_brace: left_brace,
            close_brace: right_brace,
        })
    }

    pub(super) fn block_expr(&mut self) -> Result<Expression> {
        let block = self.block()?;
        let span = block.open_brace.join(&block.close_brace);
        Ok(Expression {
            kind: ExpressionKind::Block(block),
            span
        })
    }

    fn try_if_expr(&mut self) -> Option<Result<Expression>> {
        if self.match_type(TokenKind::If) {
            Some(self.if_expr())
        } else {
            None
        }
    }

    fn if_expr(&mut self) -> Result<Expression> {
        let kw_if = self.previous_span()?;

        let cond = Box::new(self.expression()?);

        let if_body = self.block()?;
        let mut span = kw_if.join(&if_body.close_brace);

        let mut else_body = None;
        let mut kw_else = None;
        if self.match_type(TokenKind::Else) {
            kw_else = Some(self.previous_span()?);
            if self.match_type(TokenKind::If) {
                let other_if = self.if_expr()?;
                span = span.join(&other_if.span);
                else_body = Some(other_if);
            } else {
                let blk = self.block()?;
                span = span.join(&blk.close_brace);
                else_body = Some(Expression {
                    kind: ExpressionKind::Block(blk),
                    span,
                });
            }
        }

        Ok(Expression {
            kind: ExpressionKind::If {
                kw_if,
                cond,
                if_body,
                kw_else,
                else_body: else_body.map(Box::new),
            },
            span,
        })
    }

    fn assignment(&mut self) -> Result<Expression> {
        let left = self.logical()?;
        let ast = if self.match_type(TokenKind::Equal) {
            let eq = Spanned {
                val: BinaryExprOp::Assign,
                span: self.previous_span()?,
            };
            let right = Box::new(self.expression()?);
            let span = left.span.join(&right.span);
            Expression {
                kind: ExpressionKind::Binary {
                    op: eq,
                    left: Box::new(left),
                    right,
                },
                span,
            }
        } else {
            left
        };
        Ok(ast)
    }
    fn logical(&mut self) -> Result<Expression> {
        let mut left = self.equality()?;
        while self.match_types(&[TokenKind::Or, TokenKind::And]) {
            let op = self.previous()?;
            let ops = BinaryExprOp::try_from(op.kind)
                .map_err(|()| ParseErrorKind::InvalidBinaryOp(op.kind))?;
            let op = Spanned {
                val: ops,
                span: op.span,
            };
            let right = Box::new(self.logical()?);
            let span = left.span.join(&right.span);
            left = Expression {
                kind: ExpressionKind::Binary {
                    op,
                    left: Box::new(left),
                    right,
                },
                span,
            };
        }
        Ok(left)
    }
    fn equality(&mut self) -> Result<Expression> {
        let mut left = self.comparison()?;
        while self.match_types(&[TokenKind::BangEqual, TokenKind::EqualEqual]) {
            let op = self.previous()?;
            let ops = BinaryExprOp::try_from(op.kind)
                .map_err(|()| ParseErrorKind::InvalidBinaryOp(op.kind))?;
            let op = Spanned {
                val: ops,
                span: op.span,
            };
            let right = Box::new(self.equality()?);
            let span = left.span.join(&right.span);
            left = Expression {
                kind: ExpressionKind::Binary {
                    op,
                    left: Box::new(left),
                    right,
                },
                span,
            };
        }
        Ok(left)
    }

    fn comparison(&mut self) -> Result<Expression> {
        let mut left = self.term()?;
        while self.match_types(&[
            TokenKind::Greater,
            TokenKind::GreaterEqual,
            TokenKind::Less,
            TokenKind::LessEqual,
        ]) {
            let op = self.previous()?;
            let ops = BinaryExprOp::try_from(op.kind)
                .map_err(|()| ParseErrorKind::InvalidBinaryOp(op.kind))?;
            let op = Spanned {
                val: ops,
                span: op.span,
            };
            let right = Box::new(self.comparison()?);
            let span = left.span.join(&right.span);
            left = Expression {
                kind: ExpressionKind::Binary {
                    op,
                    left: Box::new(left),
                    right,
                },
                span,
            };
        }
        Ok(left)
    }
    fn term(&mut self) -> Result<Expression> {
        let mut left = self.factor()?;
        while self.match_types(&[TokenKind::Minus, TokenKind::Plus]) {
            let op = self.previous()?;
            let ops = BinaryExprOp::try_from(op.kind)
                .map_err(|()| ParseErrorKind::InvalidBinaryOp(op.kind))?;
            let op = Spanned {
                val: ops,
                span: op.span,
            };
            let right = Box::new(self.term()?);
            let span = left.span.join(&right.span);
            left = Expression {
                kind: ExpressionKind::Binary {
                    op,
                    left: Box::new(left),
                    right,
                },
                span,
            };
        }
        Ok(left)
    }
    fn factor(&mut self) -> Result<Expression> {
        let mut left = self.cast()?;
        while self.match_types(&[TokenKind::Slash, TokenKind::Star, TokenKind::Modulo]) {
            let op = self.previous()?;
            let ops = BinaryExprOp::try_from(op.kind)
                .map_err(|()| ParseErrorKind::InvalidBinaryOp(op.kind))?;
            let op = Spanned {
                val: ops,
                span: op.span,
            };
            let right = Box::new(self.factor()?);
            let span = left.span.join(&right.span);
            left = Expression {
                kind: ExpressionKind::Binary {
                    op,
                    left: Box::new(left),
                    right,
                },
                span,
            };
        }
        Ok(left)
    }
    fn cast(&mut self) -> Result<Expression> {
        let mut expr = self.unary()?;
        while self.match_type(TokenKind::As) {
            let kw_as = self.previous_span()?;
            let ty = self.ty()?;
            let span = expr.span.join(&ty.span);
            expr = Expression {
                kind: ExpressionKind::Cast {
                    expr: Box::new(expr),
                    kw_as,
                    ty,
                },
                span,
            };
        }
        Ok(expr)
    }
    fn unary(&mut self) -> Result<Expression> {
        if self.match_types(&[
            TokenKind::Bang,
            TokenKind::Minus,
            TokenKind::Plus,
            TokenKind::Ampersand,
            TokenKind::Star,
        ]) {
            let op = self.previous()?;
            let opk = UnaryExprOp::try_from(op.kind)
                .map_err(|()| ParseErrorKind::InvalidUnaryOp(op.kind))?;
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
        } else {
            self.primary()
        }
    }
    fn int_literal(&mut self) -> Result<Spanned<LitValue>> {
        let int = self.previous_parse::<i32>()?;
        Ok(Spanned {
            val: LitValue::Int(int),
            span: self.previous_span()?,
        })
    }
    fn literal(&mut self) -> Result<Option<Spanned<LitValue>>> {
        macro_rules! spanned_lit {
            ($v:ident, $e:expr) => {
                Spanned {
                    val: LitValue::$v($e),
                    span: self.previous_span()?,
                }
            };
        }
        Ok(Some(
            if self.match_type(TokenKind::False) {
                spanned_lit!(Bool, false)
            } else if self.match_type(TokenKind::True) {
                spanned_lit!(Bool, true)
            } else if self.match_type(TokenKind::String) {
                spanned_lit!(Str, self.previous_lexem()?)
            } else if self.match_type(TokenKind::IntLiteral) {
                self.int_literal()?
            } else if self.match_type(TokenKind::FloatLiteral) {
                let span = self.previous()?.span;
                let f: f64 = span.slice(self.base_offset, self.src).parse().unwrap();
                spanned_lit!(Float, f)
            } else if self.match_type(TokenKind::CharLiteral) {
                let prev = self.previous()?;
                let Some(lit) = prev.span
                    .slice(self.base_offset, self.src)
                    .strip_prefix('\'')
                    .unwrap()
                    .strip_suffix('\'')
                    else { return Ok(None) };
                let lit = Unescaped::from(lit)
                    .next()
                    .ok_or_else(|| ParseErrorKind::InvalidEscape(lit.to_string()))?;
                spanned_lit!(Char, lit)
            } else {
                return Ok(None);
            },
        ))
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

    fn access(&mut self, mut expr: Expression) -> Result<Expression> {
        if self.match_type(TokenKind::LeftBracket) {
            let index = self.expression()?;
            let cb = self.consume(TokenKind::RightBracket)?.span;
            let span = expr.span.join(&cb);

            expr = Expression {
                kind: ExpressionKind::ArrayAccess {
                    arr: Box::new(expr),
                    index: Box::new(index),
                    closing_bracket: cb,
                },
                span,
            };
            self.access(expr)
        } else if self.match_type(TokenKind::Dot) {
            if self.match_type(TokenKind::IntLiteral) {
                let index = self.int_literal()?;
                let span = expr.span.join(&index.span);
                expr = Expression {
                    kind: ExpressionKind::TupleAccess { tuple: Box::new(expr), index },
                    span
                }

            }
            else if self.match_type(TokenKind::FloatLiteral) {
                let prev_span = self.previous_span().unwrap();
                let (i, j) = self.previous_lexem().unwrap().borrow(|num| {
                    let (i, j) = num.split_once('.').unwrap();
                    let ispan = Span {
                        offset: prev_span.offset,
                        len: i.len(),
                    };
                    let jspan = Span {
                        offset: ispan.offset + ispan.len + 1,
                        len: j.len()
                    };
                    let i = Spanned {
                        span: ispan,
                        val: LitValue::Int(i.parse::<i32>().unwrap())
                    };
                    let j = Spanned {
                        span: jspan,
                        val: LitValue::Int(j.parse::<i32>().unwrap())
                    };
                    (i, j)
                });
                let span = expr.span.join(&i.span);
                expr = Expression {
                    kind: ExpressionKind::TupleAccess { tuple: Box::new(expr), index: i },
                    span
                };
                let span = expr.span.join(&j.span);
                expr = Expression {
                    kind: ExpressionKind::TupleAccess { tuple: Box::new(expr), index: j },
                    span
                };
            }
            else {
                let field = self.consume_ident_spanned()?;
                let span = expr.span.join(&field.span);

                expr = Expression {
                    kind: ExpressionKind::StructAccess {
                        st: Box::new(expr),
                        field,
                    },
                    span,
                };
            }
            self.access(expr)
        } else if self.match_type(TokenKind::LeftParen) {
            let args = self.args()?;
            let span = expr.span.join(&args.close_paren);
            expr = Expression {
                kind: ExpressionKind::Call {
                    callee: Box::new(expr),
                    args,
                },
                span,
            };
            self.access(expr)
        } else {
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
            });
        }
        if self.match_type(TokenKind::LeftParen) {
            let start = self.previous_span()?;
            let expr = self.expression()?;
            let end = self.consume(TokenKind::RightParen)?.span;
            let span = start.join(&end);
            let expr = Parenthesized {
                val: Box::new(expr),
                open_paren: start,
                close_paren: end,
            };
            return Ok(Expression {
                kind: ExpressionKind::Paren(expr),
                span,
            });
        }
        if self.match_type(TokenKind::LeftBracket) {
            let mut exprs = Vec::new();
            let open_bracket = self.previous_span().unwrap();
            while !self.match_type(TokenKind::RightBracket) {
                let expr = self.expression()?;
                exprs.push(expr);
                if self.check(TokenKind::RightBracket) { continue }
                self.consume(TokenKind::Comma)?;
            }

            let close_bracket = self.previous_span().unwrap();
            let span = open_bracket.join(&close_bracket);

            return Ok(Expression {
                kind: ExpressionKind::Array {
                    exprs: exprs.into_boxed_slice(),
                    open_bracket,
                    close_bracket,
                },
                span
            })
        }
        if let Some((path, _)) = self.try_path(false) {
            let span = path.span;
            return Ok(Expression {
                kind: ExpressionKind::Path(path),
                span,
            })
        }
        let span = self.peek()?.span;
        Err(ParseErrorKind::ExpectedConstruct {
            expected: "expression",
            found: span.slice(self.base_offset, self.src).to_string(),
        })
    }
}
