//! Expressions
//!
type Expr = Box<Expression>;

#[derive(Debug)]
pub enum UnaryExprOp {
    Negation,
    Plus,
    Not,
}

impl TryFrom<TokenKind> for UnaryExprOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenKind::Minus => Self::Negation,
            TokenKind::Bang => Self::Not,
            TokenKind::Plus => Self::Plus,
            _ => return Err(())
        })
    }
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub op: UnaryExprOp,
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
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    Neq,
    And,
    Or,
    Mod,
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
            TokenKind::Mod => Self::Mod,
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
        Self { name, decl: AstRef::empty() }
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
pub struct ArrayAccess {
    pub array: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Debug)]
pub struct StructAccess {
    pub st: Box<Expression>,
    pub field: Symbol,
}

impl StructAccess {
    pub fn new(st: impl Into<Box<Expression>>, field: Symbol) -> Self {
        Self {
            st: st.into(),
            field,
        }
    }
}

#[derive(Debug)]
pub struct Reference {
    pub of: Box<Expression>,
}

#[derive(Debug)]
pub struct Dereference {
    pub of: Box<Expression>,
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
    ArrayAccess(ArrayAccess),
    StructAccess(StructAccess),
    Ref(Reference),
    Deref(Dereference),
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

use std::cell::Ref;

use lexer::token::TokenKind;
use lexer::unescaped::Unescaped;
use lexer::Span;
use session::Symbol;
use ExpressionKind as EK;

use crate::declaration::{FunctionDecl, VariableDecl};
use crate::types::{ErrorType, RefType, Type, TypeKind};
use crate::{AstDecorated, AstRef};

impl Expression {

    pub fn get_type(&self) -> Ref<'_, Type> {
        self.ty.unwrap()
    }

    pub fn has_side_effect(&self) -> bool {
        match &self.kind {
            EK::Unary(UnaryExpr { expr, .. }) => expr.has_side_effect(),
            EK::Binary(b) => b.left.has_side_effect() || b.right.has_side_effect(),
            EK::ArrayAccess(arr) => arr.array.has_side_effect() || arr.index.has_side_effect(),
            EK::Ternary(t) =>
                t.cond.has_side_effect() || t.if_true.has_side_effect() || t.if_false.has_side_effect(),
            EK::Ref(r) => r.of.has_side_effect(),
            EK::Deref(dr) => dr.of.has_side_effect(),
            EK::Assignment(_) | EK::Call(_) => true,
            EK::Literal(_) | EK::Variable(_) | EK::StructAccess(_) => false,
        }
    }
    pub fn lvalue(&self) -> bool {
        match &self.kind {
            EK::Variable(_) => true,
            EK::Assignment(a) => a.left.lvalue(),
            EK::Deref(dr) => matches!(dr.of.dereference().kind, TypeKind::Ref(_)),
            EK::ArrayAccess(_) => true,
            EK::StructAccess(_) => true,
            _ => false,
        }
    }

    pub fn dereference(&self) -> Type {
        if let TypeKind::Ref(rt) = &self.get_type().kind {
            Type::clone(&rt.of)
        } else {
            Type { kind: TypeKind::Error(ErrorType::new("Can't dereference a non-reference expression")) }
        }
    }

    pub fn reference(&self) -> Type {
        let kind =
        if self.lvalue() {
            TypeKind::Ref(RefType { of: Box::new(self.get_type().clone()) })
        } else {
            TypeKind::Error(ErrorType::new("Can't reference non-directionable expression"))
        };
        Type { kind }
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
