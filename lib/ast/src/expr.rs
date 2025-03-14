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
    pub op: BinaryExprOp,
    pub right: Expr,
    pub kind: BinaryExprKind,
}

#[derive(Debug)]
pub enum BinaryExprOp {
    Add,
    Sub,
    Mul,
    Div,
    Comma,
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    Neq,
    And,
    Or,
}

impl TryFrom<TokenKind> for BinaryExprOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenKind::Comma => Self::Comma,
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
            _ => return Err(())
        })
    }
}

#[derive(Debug)]
pub enum BinaryExprKind {
    Logical,
    Arithmetic,
    Comparison,
    Comma,
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

impl VariableExpr {
    pub fn new(name: Symbol) -> Self {
        Self { name, decl: AstDecorated::new() }
    }
}

impl From<VariableExpr> for ExpressionKind {
    fn from(value: VariableExpr) -> Self {
        ExpressionKind::Variable(value)
    }
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

#[derive(Debug)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
    pub ty: AstDecorated<Type>,
}

impl Expression {
    pub fn new(kind: ExpressionKind, span: Span) -> Self {
        Expression {
            kind,
            span,
            ty: Default::default()
        }
    }
}

/* impl fmt::Debug for Expression { */
/*     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { */
/*         write!(f, "{:#?}", self.kind) */
/*     } */
/* } */

use lexer::token::TokenKind;
use lexer::unescaped::Unescaped;
use lexer::Span;
use session::Symbol;
use ExpressionKind::*;

use crate::declaration::{FunctionDecl, VariableDecl};
use crate::types::Type;
use crate::{AstDecorated, AstRef};

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
        match &self.kind {
            Variable(_) => true,
            Assignment(a) => a.left.lvalue(),
            _ => false
        }
    }
}

#[derive(Clone,Debug)]
pub enum LitValue {
    Int(i32),
    Float(f64),
    Str(Symbol),
    Bool(bool),
    Char(char),
}

impl LitValue {
    pub fn truthy(&self) -> bool {
        match self {
            LitValue::Int(n) => *n != 0,
            LitValue::Float(n) => *n != 0.0,
            LitValue::Bool(b) => *b,
            LitValue::Char(_) | LitValue::Str(_) => false,
        }
    }
    pub fn print(&self) {
        match self {
            LitValue::Int(n) => print!("{n}"),
            LitValue::Float(n) => print!("{n}"),
            LitValue::Str(s) => {
                session::with_symbol(*s, |s| {
                    let s = s.strip_prefix('"').unwrap()
                        .strip_suffix('"').unwrap();
                    Unescaped::from(s).for_each(|c| print!("{c}"));
                });
            },
            LitValue::Bool(b) => print!("{b}"),
            LitValue::Char(c) => print!("{c}"),
        }
    }
}
