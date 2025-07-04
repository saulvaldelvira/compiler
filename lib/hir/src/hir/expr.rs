use interner::Symbol;
use span::Span;

use super::{Ident, Path, Type};
use crate::{
    hir_id::{HirId, HirNode},
    node_map::HirNodeKind,
};

#[derive(Debug)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, Copy)]
pub enum LogicalOp {
    And,
    Or,
}

#[derive(Debug, Clone, Copy)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy)]
pub enum CmpOp {
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    Neq,
}

#[derive(Clone, Debug, Copy)]
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
    Unary {
        op: UnaryOp,
        expr: &'hir Expression<'hir>,
    },
    Ref(&'hir Expression<'hir>),
    Deref(&'hir Expression<'hir>),
    Logical {
        left: &'hir Expression<'hir>,
        op: LogicalOp,
        right: &'hir Expression<'hir>,
    },
    Comparison {
        left: &'hir Expression<'hir>,
        op: CmpOp,
        right: &'hir Expression<'hir>,
    },
    Arithmetic {
        left: &'hir Expression<'hir>,
        op: ArithmeticOp,
        right: &'hir Expression<'hir>,
    },
    Ternary {
        cond: &'hir Expression<'hir>,
        if_true: &'hir Expression<'hir>,
        if_false: &'hir Expression<'hir>,
    },
    Assignment {
        left: &'hir Expression<'hir>,
        right: &'hir Expression<'hir>,
    },
    Variable(Path),
    Literal(LitValue),
    Call {
        callee: &'hir Expression<'hir>,
        args: &'hir [Expression<'hir>],
    },
    Cast {
        expr: &'hir Expression<'hir>,
        to: &'hir Type<'hir>,
    },
    ArrayAccess {
        arr: &'hir Expression<'hir>,
        index: &'hir Expression<'hir>,
    },
    StructAccess {
        st: &'hir Expression<'hir>,
        field: Ident,
    },
}

#[derive(Debug)]
pub struct Expression<'hir> {
    pub id: HirId,
    pub kind: ExpressionKind<'hir>,
    pub span: Span,
}

impl<'hir> Expression<'hir> {
    pub fn new(kind: ExpressionKind<'hir>, span: Span) -> Self {
        Self {
            kind,
            span,
            id: HirId::DUMMY,
        }
    }
}

impl<'hir> HirNode<'hir> for Expression<'hir> {
    fn get_hir_id(&self) -> HirId { self.id }

    fn get_hir_node_kind(&'hir self) -> HirNodeKind<'hir> { HirNodeKind::Expr(self) }

    fn set_hir_id(&mut self, id: HirId) { self.id = id; }
}
