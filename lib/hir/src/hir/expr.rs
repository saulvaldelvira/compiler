use interner::Symbol;
use span::Span;

use super::{Ident, Path, Type};
use crate::Statement;
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

#[derive(Debug, Clone, Copy)]
pub struct BlockExpr<'hir> {
    pub stmts: &'hir [Statement<'hir>],
    pub tail: Option<&'hir Expression<'hir>>,
}

#[derive(Debug, Clone, Copy)]
pub struct StructAccess<'hir> {
    pub st: &'hir Expression<'hir>,
    pub field: Ident,
}

#[derive(Debug)]
pub enum ExpressionKind<'hir> {
    Array(&'hir [Expression<'hir>]),
    Unary {
        op: UnaryOp,
        expr: &'hir Expression<'hir>,
    },
    Block(BlockExpr<'hir>),
    If {
        cond: &'hir Expression<'hir>,
        if_true: &'hir Expression<'hir>,
        if_false: Option<&'hir Expression<'hir>>,
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
    TupleAccess {
        tuple: &'hir Expression<'hir>,
        index: u16,
    },
    StructAccess(&'hir StructAccess<'hir>),
}

impl<'hir> ExpressionKind<'hir> {
    pub fn as_block(&self) -> Option<&BlockExpr<'hir>> {
        match self {
            ExpressionKind::Block(b) => Some(b),
            _ => None
        }
    }
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
