use span::Span;

use super::Expression;
use crate::item::Item;
use crate::{HirId, hir_id::HirNode, impl_hir_node, node_map::HirNodeKind};

#[derive(Debug, Clone, Copy)]
pub struct ForStmt<'hir> {
    pub init: Option<&'hir Item<'hir>>,
    pub cond: Option<&'hir Expression<'hir>>,
    pub inc: Option<&'hir Expression<'hir>>,
    pub body: &'hir Statement<'hir>,
}

#[derive(Debug, Clone, Copy)]
pub enum StatementKind<'hir> {
    Expr(&'hir Expression<'hir>),
    While {
        cond: &'hir Expression<'hir>,
        body: &'hir Statement<'hir>,
    },
    For(&'hir ForStmt<'hir>),
    Empty,
    Break,
    Continue,
    Return(Option<&'hir Expression<'hir>>),
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
