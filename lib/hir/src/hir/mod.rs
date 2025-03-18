use std::fmt::Debug;
use crate::hir_id::{HirId, HirNode};
use crate::node_map::HirNodeKind;

#[derive(Debug,Clone,Copy)]
pub struct Ident {
    pub sym: Symbol,
    pub span: Span,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.sym == other.sym
    }
}

#[derive(Debug,Clone,Copy)]
pub enum Constness {
    Const,
    Default,
}

pub mod types;
pub use types::Type;

pub mod stmt;
pub use stmt::Statement;

pub mod expr;
pub use expr::{Expression,Path};
use session::Symbol;
use span::Span;
pub use def::Definition;

pub mod def;
mod node_ref;
pub use node_ref::{NodeRef, NodeRefKind};

#[derive(Debug)]
pub struct Program<'hir> {
    pub id: HirId,
    pub defs: &'hir [Definition<'hir>],
}

impl<'hir> Program<'hir> {
    pub fn new(defs: &'hir [Definition<'hir>]) -> Self {
        Self {
            defs,
            id: HirId::DUMMY,
        }
    }
}

impl<'hir> HirNode<'hir> for Program<'hir> {
    fn get_hir_id(&self) -> HirId { self.id }

    fn get_hir_node_kind(&'hir self) -> HirNodeKind<'hir> {
        HirNodeKind::Prog(self)
    }

    fn set_hir_id(&mut self, id: HirId) {
        self.id = id;
    }

}
