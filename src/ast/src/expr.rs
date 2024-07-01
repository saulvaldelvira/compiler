//! Expressions
//!
use std::borrow::Cow;

use builders::{AsBox, IntoEnum};

pub type Expr = Box<Expression>;

#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Expression, field = Unary)]
pub struct UnaryExpr {
    pub op: String,
    pub expr: Expr
}

#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Expression, field = Binary)]
pub struct BinaryExpr {
    pub left: Expr,
    pub op: String,
    pub right: Expr,
}

#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Expression, field = Ternary)]
pub struct TernaryExpr {
    pub cond: Expr,
    pub if_true: Expr,
    pub if_false: Expr,
}

#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Expression, field = Assignment)]
pub struct AssignmentExpr {
    pub left: Expr,
    pub right: Expr
}

#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Expression, field = Variable)]
pub struct VariableExpr {
    pub name: Cow<'static,str>,
}

#[derive(AsBox,Debug)]
pub enum Expression {
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Ternary(TernaryExpr),
    Assignment(AssignmentExpr),
    Variable(VariableExpr),
    Literal(LitValue),
}

use Expression::*;

impl Expression {
    pub fn has_side_effect(&self) -> bool {
        match self {
            Unary(UnaryExpr { expr, .. }) => expr.has_side_effect(),
            Binary(BinaryExpr { left, op: _, right }) => left.has_side_effect() || right.has_side_effect(),
            Ternary(TernaryExpr { cond, if_true, if_false }) =>
                cond.has_side_effect() || if_true.has_side_effect() || if_false.has_side_effect(),
            Assignment(_) => true,
            Literal(_) | Variable(_) => false,
        }
    }
    pub fn lvalue(&self) -> bool {
        match self {
            Variable(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone,Debug)]
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
            LitValue::Str(s) => print!("{}", s.strip_prefix("\"").unwrap()
                                              .strip_suffix("\"").unwrap()
                                              .replace("\\n", "\n")),
            LitValue::Bool(b) => print!("{b}"),
            LitValue::Nil => print!("nil"),
        }
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __expr {
    ($e:expr) => {
        {
            let expr: Expression = $e.into();
            expr.as_box()
        }
    };
}

pub use __expr as expr;
