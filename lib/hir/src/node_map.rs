use std::collections::HashMap;

use crate::def::{Field, PathDef};
use crate::{Definition, Expression, HirId, Program, Statement, Type};

#[derive(Clone,Copy)]
pub enum HirNodeKind<'hir> {
    Expr(&'hir Expression<'hir>),
    Def(&'hir Definition<'hir>),
    PathDef(&'hir PathDef),
    Stmt(&'hir Statement<'hir>),
    Field(&'hir Field<'hir>),
    Ty(&'hir Type<'hir>),
    Prog(&'hir Program<'hir>),
}

impl<'hir> HirNodeKind<'hir> {
    /// Expects this node to be a [Def] variant.
    ///
    /// # Panics
    /// - If the node is NOT a [Def] variant
    ///
    /// [Def]: HirNodeKind::Def
    pub fn expect_definition(self) -> &'hir Definition<'hir> {
        match self {
            Self::Def(def) => def,
            _ => unreachable!("Expected definition")
        }
    }
}

impl<'hir> From<&'hir Expression<'hir>> for HirNodeKind<'hir> {
    fn from(value: &'hir Expression<'hir>) -> Self {
        HirNodeKind::Expr(value)
    }
}

#[derive(Default)]
pub struct NodeMap<'hir> {
    map: HashMap<HirId,HirNodeKind<'hir>>,
    id_count: usize,
}

impl<'hir> NodeMap<'hir> {
    pub fn define(&mut self, id: HirId, val: impl Into<HirNodeKind<'hir>>) {
        self.map.insert(id, val.into());
    }

    pub fn get_by_id(&self, id: &HirId) -> Option<HirNodeKind<'hir>> {
        self.map.get(id).copied()
    }

    pub fn get_next_id(&mut self) -> HirId {
        self.id_count += 1;
        HirId(self.id_count)
    }
}
