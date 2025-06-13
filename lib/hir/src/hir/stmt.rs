use span::Span;

use super::Expression;
use crate::item::Item;
use crate::{HirId, hir_id::HirNode, impl_hir_node, node_map::HirNodeKind};

#[derive(Debug, Clone, Copy)]
pub enum StatementKind<'hir> {
    Expr(&'hir Expression<'hir>),
    Block(&'hir [Statement<'hir>]),
    If {
        cond: &'hir Expression<'hir>,
        if_true: &'hir Statement<'hir>,
        if_false: Option<&'hir Statement<'hir>>,
    },
    While {
        cond: &'hir Expression<'hir>,
        body: &'hir Statement<'hir>,
    },
    For {
        init: Option<&'hir Item<'hir>>,
        cond: Option<&'hir Expression<'hir>>,
        inc: Option<&'hir Expression<'hir>>,
        body: &'hir Statement<'hir>,
    },
    Empty,
    Break,
    Continue,
    Return(Option<&'hir Expression<'hir>>),
    Print(&'hir Expression<'hir>),
    Read(&'hir Expression<'hir>),
    Item(&'hir Item<'hir>),
}

#[derive(Debug, Clone, Copy)]
pub struct Statement<'hir> {
    pub kind: StatementKind<'hir>,
    pub span: Span,
    pub id: HirId,
}

impl<'hir> Statement<'hir> {
    pub fn new(kind: StatementKind<'hir>, span: Span) -> Self {
        Self {
            kind,
            span,
            id: HirId::DUMMY,
        }
    }
}

impl_hir_node!(Statement<'hir>, Stmt);
