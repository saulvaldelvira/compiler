//! Expressions
//!
type Expr = Box<Expression>;

#[derive(Debug)]
pub struct UnaryExpr {
    pub op: Box<str>,
    pub expr: Expr
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub left: Expr,
    pub op: Box<str>,
    pub right: Expr,
}

#[derive(Debug)]
pub struct TernaryExpr {
    pub cond: Expr,
    pub if_true: Expr,
    pub if_false: Expr,
}

#[derive(Debug)]
pub struct AssignmentExpr {
    pub left: Expr,
    pub right: Expr
}

#[derive(Debug)]
pub struct VariableExpr {
    pub name: Box<str>,
}

#[derive(Debug)]
pub struct LitExpr {
    pub value: LitValue,
}

#[derive(Debug)]
pub enum ExpressionKind {
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Ternary(TernaryExpr),
    Assignment(AssignmentExpr),
    Variable(VariableExpr),
    Literal(LitExpr),
}

#[derive(Debug)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

use lexer::Span;
use ExpressionKind::*;

impl Expression {
    pub fn has_side_effect(&self) -> bool {
        match &self.kind {
            Unary(UnaryExpr { expr, .. }) => expr.has_side_effect(),
            Binary(b) => b.left.has_side_effect() || b.right.has_side_effect(),
            Ternary(t) =>
                t.cond.has_side_effect() || t.if_true.has_side_effect() || t.if_false.has_side_effect(),
            Assignment(_) => true,
            Literal(_) | Variable(_) => false,
        }
    }
    pub fn lvalue(&self) -> bool {
        matches!(self.kind, Variable(_))
    }
}

#[derive(Clone,Debug)]
pub enum LitValue {
    Number(f64),
    Str(Box<str>),
    Bool(bool),
    Nil
}

impl LitValue {
    pub fn truthy(&self) -> bool {
        match self {
            LitValue::Number(n) => *n != 0.0,
            LitValue::Bool(b) => *b,
            LitValue::Nil | LitValue::Str(_) => false,
        }
    }
    pub fn print(&self) {
        match self {
            LitValue::Number(n) => print!("{n}"),
            LitValue::Str(s) => print!("{}", s.strip_prefix('"').unwrap()
                                              .strip_suffix('"').unwrap()
                                              .replace("\\n", "\n")),
            LitValue::Bool(b) => print!("{b}"),
            LitValue::Nil => print!("nil"),
        }
    }
}
