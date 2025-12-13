//! Expressions
//!

use lexer::token::TokenKind;
use interner::Symbol;
use span::{Span, Spanned};

use crate::{Block, Path, Statement};
use crate::{types::Type, Parenthesized};
type Expr = Box<Expression>;

#[derive(Debug)]
pub enum UnaryExprOp {
    Negation,
    Plus,
    Not,
    Deref,
    Ref,
}

impl TryFrom<TokenKind> for UnaryExprOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenKind::Minus => Self::Negation,
            TokenKind::Bang => Self::Not,
            TokenKind::Plus => Self::Plus,
            TokenKind::Ampersand => Self::Ref,
            TokenKind::Star => Self::Deref,
            _ => return Err(()),
        })
    }
}

#[derive(Debug)]
pub enum BinaryExprOp {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    Neq,
    And,
    Or,
    Mod,
    Assign,
}

impl TryFrom<TokenKind> for BinaryExprOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenKind::Minus => Self::Sub,
            TokenKind::Plus => Self::Add,
            TokenKind::Slash => Self::Div,
            TokenKind::Star => Self::Mul,
            TokenKind::BangEqual => Self::Neq,
            TokenKind::EqualEqual => Self::Eq,
            TokenKind::Greater => Self::Gt,
            TokenKind::GreaterEqual => Self::Ge,
            TokenKind::Less => Self::Lt,
            TokenKind::LessEqual => Self::Le,
            TokenKind::And => Self::And,
            TokenKind::Or => Self::Or,
            TokenKind::Modulo => Self::Mod,
            TokenKind::Equal => Self::Assign,
            _ => return Err(()),
        })
    }
}

#[derive(Debug)]
pub struct StructAccess {
    pub st: Box<Expression>,
    pub field: Spanned<Symbol>,
}

impl StructAccess {
    pub fn new(st: impl Into<Box<Expression>>, field: Spanned<Symbol>) -> Self {
        Self {
            st: st.into(),
            field,
        }
    }
}

#[derive(Debug)]
pub enum ExpressionKind {
    Unary {
        op: Spanned<UnaryExprOp>,
        expr: Expr,
    },
    Paren(Parenthesized<Expr>),
    Binary {
        op: Spanned<BinaryExprOp>,
        left: Expr,
        right: Expr,
    },
    Path(Path),
    Literal(Spanned<LitValue>),
    Cast {
        expr: Box<Expression>,
        kw_as: Span,
        ty: Type,
    },
    Call {
        callee: Expr,
        args: Parenthesized<Box<[Expression]>>,
    },
    ArrayAccess {
        arr: Expr,
        index: Expr,
        closing_bracket: Span,
    },
    TupleAccess {
        tuple: Expr,
        index: Spanned<LitValue>,
    },
    StructAccess {
        st: Expr,
        field: Spanned<Symbol>,
    },
    Block(Block<Statement, Expression>),
    Array {
        open_bracket: Span,
        exprs: Box<[Expression]>,
        close_bracket: Span,
    },
    If {
        kw_if: Span,
        cond: Box<Expression>,
        if_body: Block<Statement, Expression>,
        kw_else: Option<Span>,
        else_body: Option<Box<Expression>>,
    },
}

#[derive(Debug)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

impl Expression {
    pub fn new(kind: ExpressionKind, span: Span) -> Self { Expression { kind, span } }
}

#[derive(Clone, Debug)]
pub enum LitValue {
    Int(i32),
    Float(f64),
    Str(Symbol),
    Bool(bool),
    Char(char),
}
