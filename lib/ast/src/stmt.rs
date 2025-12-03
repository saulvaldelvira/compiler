//! Statements
//!
use core::fmt;

use span::Span;

use crate::expr::ExpressionKind;
use crate::item::Item;
use crate::{Block, Expression};

#[derive(Debug)]
pub enum StatementKind {
    Expression(Expression, Option<Span>),
    Item(Box<Item>),
    While {
        kw_while: Span,
        cond: Expression,
        body: Box<Statement>,
    },
    For {
        kw_for: Span,
        init: Option<Box<Item>>,
        cond: Option<Expression>,
        inc: Option<Expression>,
        body: Box<Statement>,
    },
    Empty(Span),
    Break(Span),
    Continue(Span),
    Return {
        kw_ret: Span,
        expr: Option<Expression>,
        semmicollon: Span,
    },
}

pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

impl From<Block<Statement>> for Statement {
    fn from(value: Block<Statement>) -> Self {
        let span = value.open_brace.join(&value.close_brace);
        let expr = Expression::new(ExpressionKind::Block(value.to()), span);
        Statement {
            kind: StatementKind::Expression(expr, None),
            span,
        }
    }
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{:#?}", self.kind) }
}
