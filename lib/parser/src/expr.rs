use ast::{
    expr::{BinaryExprOp, ExpressionKind, LitValue, UnaryExprOp},
    Expression, Parenthesized,
};
use lexer::{token::TokenKind, unescaped::Unescaped};
use span::Spanned;

use super::{Parser, Result};
use crate::error::ParseErrorKind;

impl<'sess, 'src> Parser<'sess, 'src> {
    pub(super) fn expression(&mut self) -> Result<Expression> { self.assignment() }

    pub(super) fn try_expression(&mut self) -> Option<Expression> { self.expression().ok() }

    pub(super) fn comma_sep_expr(&mut self) -> Result<Box<[Expression]>> {
        let mut exprs = Vec::new();
        let expr = self.expression()?;
        exprs.push(expr);

        while self.match_type(TokenKind::Comma) {
            let expr = self.expression()?;
            exprs.push(expr);
        }

        Ok(exprs.into_boxed_slice())
    }

    fn assignment(&mut self) -> Result<Expression> {
        let left = self.ternary()?;
        let ast = if self.match_type(TokenKind::Equal) {
            let eq = Spanned {
                val: BinaryExprOp::Assign,
                span: self.previous_span()?,
            };
            let right = Box::new(self.assignment()?);
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
    fn ternary(&mut self) -> Result<Expression> {
        let mut cond = self.logical()?;
        if self.match_type(TokenKind::Question) {
            let if_true = Box::new(self.logical()?);
            self.consume(TokenKind::Colon)?;
            let if_false = Box::new(self.logical()?);
            let span = cond.span.join(&if_false.span);
            cond = Expression {
                kind: ExpressionKind::Ternary {
                    cond: Box::new(cond),
                    if_true,
                    if_false,
                },
                span,
            }
        }
        Ok(cond)
    }
    fn logical(&mut self) -> Result<Expression> {
        let mut left = self.equality()?;
        while self.match_types(&[TokenKind::Or, TokenKind::And]) {
            let op = self.previous()?;
            let ops = BinaryExprOp::try_from(op.kind)
                .map_err(|_| ParseErrorKind::InvalidBinaryOp(op.kind))?;
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
    fn equality(&mut self) -> Result<Expression> {
        let mut left = self.comparison()?;
        while self.match_types(&[TokenKind::BangEqual, TokenKind::EqualEqual]) {
            let op = self.previous()?;
            let ops = BinaryExprOp::try_from(op.kind)
                .map_err(|_| ParseErrorKind::InvalidBinaryOp(op.kind))?;
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
                .map_err(|_| ParseErrorKind::InvalidBinaryOp(op.kind))?;
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
    fn term(&mut self) -> Result<Expression> {
        let mut left = self.factor()?;
        while self.match_types(&[TokenKind::Minus, TokenKind::Plus]) {
            let op = self.previous()?;
            let ops = BinaryExprOp::try_from(op.kind)
                .map_err(|_| ParseErrorKind::InvalidBinaryOp(op.kind))?;
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
    fn factor(&mut self) -> Result<Expression> {
        let mut left = self.cast()?;
        while self.match_types(&[TokenKind::Slash, TokenKind::Star, TokenKind::Modulo]) {
            let op = self.previous()?;
            let ops = BinaryExprOp::try_from(op.kind)
                .map_err(|_| ParseErrorKind::InvalidBinaryOp(op.kind))?;
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
                .map_err(|_| ParseErrorKind::InvalidUnaryOp(op.kind))?;
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
                spanned_lit!(Int, self.previous_parse::<i32>()?)
            } else if self.match_type(TokenKind::FloatLiteral) {
                let f: f64 = self.previous()?.span.slice(self.src).parse().unwrap();
                spanned_lit!(Float, f)
            } else if self.match_type(TokenKind::CharLiteral) {
                let prev = self.previous()?;
                let lit = prev
                    .span
                    .slice(self.src)
                    .strip_prefix('\'')
                    .unwrap()
                    .strip_suffix('\'')
                    .unwrap();
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
            let field = self.consume_ident_spanned()?;
            let span = expr.span.join(&field.span);

            expr = Expression {
                kind: ExpressionKind::StructAccess {
                    st: Box::new(expr),
                    field,
                },
                span,
            };
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
        if self.check(TokenKind::Identifier) {
            return self.path();
        }
        Err(ParseErrorKind::ExpectedConstruct {
            expected: "expression",
            found: self.peek()?.span.slice(self.src).to_string(),
        })
    }
}
