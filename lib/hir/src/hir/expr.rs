use session::Symbol;
use span::Span;

use crate::hir_id::HirId;

use super::{Ident, Res};

#[derive(Debug)]
pub enum UnaryOp {
    Not,
    Neg,
    Deref,
}

#[derive(Debug)]
pub enum LogicalOp {
    And,
    Or,
}

#[derive(Debug)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug)]
pub enum CmpOp {
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    Neq,
}

#[derive(Debug)]
pub struct Path {
    pub ident: Ident,
    pub id: HirId,
    pub def: Res<HirId>,
}

#[derive(Clone,Debug)]
pub enum LitValue {
    Int(i32),
    Float(f64),
    Str(Symbol),
    Bool(bool),
    Char(char),
}

#[derive(Debug)]
pub enum ExpressionKind<'hir> {
    Array(&'hir [Expression<'hir>]),
    Unary { op: UnaryOp, expr: &'hir Expression<'hir> },
    Logical { left: &'hir Expression<'hir>, op: LogicalOp, right: &'hir Expression<'hir> },
    Comparison { left: &'hir Expression<'hir>, op: CmpOp, right: &'hir Expression<'hir> },
    Arithmetic { left: &'hir Expression<'hir>, op: ArithmeticOp, right: &'hir Expression<'hir> },
    Ternary { cond: &'hir Expression<'hir>, if_true: &'hir Expression<'hir>, if_false: &'hir Expression<'hir> },
    Assignment { left: &'hir Expression<'hir>, right: &'hir Expression<'hir> },
    Variable(Path),
    Literal(LitValue),
    Call { callee: &'hir Expression<'hir>, args: &'hir [Expression<'hir>] },
    ArrayAccess { arr: &'hir Expression<'hir>, index: &'hir Expression<'hir> },
    StructAccess { st: &'hir Expression<'hir>, field: Path },
}

#[derive(Debug)]
pub struct Expression<'hir> {
    pub id: HirId,
    pub kind: ExpressionKind<'hir>,
    pub span: Span,
}

