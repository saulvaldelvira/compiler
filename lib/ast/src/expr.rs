//! Expressions
//!
type Expr = Box<Expression>;

#[derive(Debug)]
pub struct UnaryExpr {
    pub op: Symbol,
    pub expr: Expr
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub left: Expr,
    pub op: Symbol,
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
    pub name: Symbol,
    pub decl: AstRef<VariableDecl>,
}

#[derive(Debug)]
pub struct CallExpr {
    pub callee: Symbol,
    pub args: Box<[Expression]>,
    pub decl: AstRef<FunctionDecl>,
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
    Call(CallExpr),
}

pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self.kind)
    }
}

use std::fmt;

use session;
use lexer::unescaped::Unescaped;
use lexer::Span;
use session::Symbol;
use ExpressionKind::*;

use crate::declaration::{FunctionDecl, VariableDecl};
use crate::AstRef;

impl Expression {
    pub fn has_side_effect(&self) -> bool {
        match &self.kind {
            Unary(UnaryExpr { expr, .. }) => expr.has_side_effect(),
            Binary(b) => b.left.has_side_effect() || b.right.has_side_effect(),
            Ternary(t) =>
                t.cond.has_side_effect() || t.if_true.has_side_effect() || t.if_false.has_side_effect(),
            Assignment(_) | Call(_) => true,
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
    Str(Symbol),
    Bool(bool),
    Char(char),
    Nil
}

impl LitValue {
    pub fn truthy(&self) -> bool {
        match self {
            LitValue::Number(n) => *n != 0.0,
            LitValue::Bool(b) => *b,
            LitValue::Nil | LitValue::Char(_) | LitValue::Str(_) => false,
        }
    }
    pub fn print(&self) {
        match self {
            LitValue::Number(n) => print!("{n}"),
            LitValue::Str(s) => {
                let s = session::get_symbol_str(*s).unwrap();
                let s = s.strip_prefix('"').unwrap()
                    .strip_suffix('"').unwrap();
                Unescaped::from(s).for_each(|c| print!("{c}"));
            },
            LitValue::Bool(b) => print!("{b}"),
            LitValue::Nil => print!("nil"),
            LitValue::Char(c) => print!("{c}"),
        }
    }
}
