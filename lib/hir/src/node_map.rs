use std::collections::HashMap;

use span::Span;

use crate::item::Item;
use crate::UseItem;
use crate::{Expression, HirId, Module, PathDef, Statement, Type, item::Field};

#[derive(Clone, Copy, Debug)]
pub enum HirNodeKind<'hir> {
    Expr(&'hir Expression<'hir>),
    Item(&'hir Item<'hir>),
    Use(&'hir UseItem<'hir>),
    PathDef(&'hir PathDef),
    Stmt(&'hir Statement<'hir>),
    Field(&'hir Field<'hir>),
    Ty(&'hir Type<'hir>),
    Module(&'hir Module<'hir>),
}

impl<'hir> HirNodeKind<'hir> {
    /// Expects this node to be a [Item] variant.
    ///
    /// # Panics
    /// - If the node is NOT a [Item] variant
    ///
    /// [Item]: HirNodeKind::Item
    pub fn expect_item(self) -> &'hir Item<'hir> {
        self.as_item().unwrap_or_else(|| {
            unreachable!("Expected item")
        })
    }

    pub fn as_item(self) -> Option<&'hir Item<'hir>> {
        match self {
            Self::Item(item) => Some(item),
            _ => None
        }
    }

    pub fn get_span(&self) -> Option<Span> {
        Some(match self {
            HirNodeKind::Expr(expression) => expression.span,
            HirNodeKind::Item(item) => item.span,
            HirNodeKind::Use(u) => u.span,
            HirNodeKind::Stmt(statement) => statement.span,
            HirNodeKind::Field(field) => field.span,
            HirNodeKind::PathDef(_) |
            HirNodeKind::Ty(_) => return None,
            HirNodeKind::Module(module) => module.span,
        })
    }
}

impl<'hir> From<&'hir Expression<'hir>> for HirNodeKind<'hir> {
    fn from(value: &'hir Expression<'hir>) -> Self { HirNodeKind::Expr(value) }
}

#[derive(Default)]
pub struct NodeMap<'hir> {
    map: HashMap<HirId, HirNodeKind<'hir>>,
    id_count: usize,
}

impl<'hir> NodeMap<'hir> {
    pub fn define(&mut self, id: HirId, val: impl Into<HirNodeKind<'hir>>) {
        self.map.insert(id, val.into());
    }

    pub fn get_by_id(&self, id: &HirId) -> Option<HirNodeKind<'hir>> { self.map.get(id).copied() }

    pub fn get_next_id(&mut self) -> HirId {
        self.id_count += 1;
        HirId(self.id_count)
    }
}
