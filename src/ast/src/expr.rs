use std::borrow::Cow;

use builders::AsBox;
use lexer::token::Token;

pub type Expr = Box<Expression>;

#[derive(AsBox)]
pub enum Expression {
    Unary {
        op: Token,
        expr: Expr
    },
    Binary {
        left: Expr,
        op: Token,
        right: Expr,
    },
    Ternary {
        cond: Expr,
        if_true: Expr,
        if_false: Expr,
    },
    Variable{ name: Cow<'static,str> },
    Literal(LitValue),
}

impl Expression {
    pub fn has_side_effect(&self) -> bool {
        use Expression::*;
        match self {
            Expression::Unary { op: _, expr } => expr.has_side_effect(),
            Expression::Binary { left, op: _, right } => left.has_side_effect() || right.has_side_effect(),
            Expression::Ternary { cond, if_true, if_false } =>
                cond.has_side_effect() || if_true.has_side_effect() || if_false.has_side_effect(),
            Literal(_) | Variable{ .. } => false,
        }
    }
}

#[derive(Clone)]
pub enum LitValue {
    Number(f64),
    Str(String),
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
            LitValue::Str(s) => print!("{s}"),
            LitValue::Bool(b) => print!("{b}"),
            LitValue::Nil => print!("nil"),
        }
    }
}
