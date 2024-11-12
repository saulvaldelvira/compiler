//! Expressions
//!
use builders::{AsBox, IntoEnum};

pub type Expr = Box<Expression>;

#[spanned]
#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Expression, field = Unary)]
pub struct UnaryExpr {
    pub op: Box<str>,
    pub expr: Expr
}

#[spanned]
#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Expression, field = Binary)]
pub struct BinaryExpr {
    pub left: Expr,
    pub op: Box<str>,
    pub right: Expr,
}

#[spanned]
#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Expression, field = Ternary)]
pub struct TernaryExpr {
    pub cond: Expr,
    pub if_true: Expr,
    pub if_false: Expr,
}

#[spanned]
#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Expression, field = Assignment)]
pub struct AssignmentExpr {
    pub left: Expr,
    pub right: Expr
}

#[spanned]
#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Expression, field = Variable)]
pub struct VariableExpr {
    pub name: Box<str>,
}

#[spanned]
#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Expression, field = Literal)]
pub struct LitExpr {
    pub value: LitValue,
}

#[derive(AsBox,Debug,IntoEnum,Spanned)]
#[into_enum(enum_name = AST)]
pub enum Expression {
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Ternary(TernaryExpr),
    Assignment(AssignmentExpr),
    Variable(VariableExpr),
    Literal(LitExpr),
}

use lexer::{spanned, Spanned};
use Expression::*;

impl Expression {
    pub fn has_side_effect(&self) -> bool {
        match self {
            Unary(UnaryExpr { expr, .. }) => expr.has_side_effect(),
            Binary(b) => b.left.has_side_effect() || b.right.has_side_effect(),
            Ternary(t) =>
                t.cond.has_side_effect() || t.if_true.has_side_effect() || t.if_false.has_side_effect(),
            Assignment(_) => true,
            Literal(_) | Variable(_) => false,
        }
    }
    pub fn lvalue(&self) -> bool {
        matches!(self, Variable(_))
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

#[doc(hidden)]
#[macro_export]
macro_rules! __expr {
    ($variant:ident { $( $i:ident $( : $val:expr )?  ),*  } ) => {
        $crate::ast!(Expression : $variant { $( $i $( : $val )? ),* , span: None })
    };
    ($variant:ident ( $( $e:expr ),* ) ) => {
        $crate::ast!(Expression : $variant ( $( $e ),* , None))
    };
}

pub use __expr as expr;

use crate::AST;

