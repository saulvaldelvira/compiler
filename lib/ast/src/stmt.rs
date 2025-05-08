//! Statements
//!
use core::fmt;

use crate::{Block, Expression, Parenthesized};
use span::Span;

use super::declaration::Declaration;

#[derive(Debug)]
pub enum StatementKind {
    Expression(Expression, Span),
    Print(Span, Box<[Expression]>, Span),
    Read(Span, Box<[Expression]>, Span),
    Decl(Box<Declaration>),
    Block(Block<Statement>),
    If { kw_if: Span, cond: Parenthesized<Expression>, if_body: Box<Statement>, kw_else: Option<Span>, else_body: Option<Box<Statement>> },
    While { kw_while: Span, cond: Expression, body: Box<Statement> },
    For {
        kw_for: Span,
        init: Option<Box<Declaration>>,
        cond: Option<Expression>,
        inc: Option<Expression>,
        body: Box<Statement>,
    },
    Empty(Span),
    Break(Span),
    Continue(Span),
    Return{ kw_ret: Span, expr: Option<Expression>, semmicollon: Span },
}

pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

impl From<Block<Statement>> for Statement {
    fn from(value: Block<Statement>) -> Self {
        let span = value.open_brace.join(&value.close_brace);
        Statement {
            kind: StatementKind::Block(value),
            span
        }
    }
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self.kind)
    }
}
