use std::fmt::Debug;
use crate::hir_id::{HirId, HirNode};
use crate::impl_hir_node;
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

#[derive(Debug,Clone,Copy)]
pub struct Module<'hir> {
    pub id: HirId,
    pub items: &'hir [ModuleItem<'hir>],
}

impl<'hir> Module<'hir> {
    pub fn new(items: &'hir [ModuleItem<'hir>]) -> Self {
        Self { items, id: HirId::DUMMY }
    }
}

impl_hir_node!(Module<'hir>, Module);

#[derive(Debug,Clone,Copy)]
pub enum ModuleItem<'hir> {
    Def(Definition<'hir>),
    Module(Module<'hir>),
}

impl<'hir> HirNode<'hir> for ModuleItem<'hir> {
    fn get_hir_id(&self) -> HirId {
        match self {
            ModuleItem::Def(d) => d.get_hir_id(),
            ModuleItem::Module(m) => m.get_hir_id(),
        }
    }

    fn get_hir_node_kind(&'hir self) -> HirNodeKind<'hir> {
        match self {
            ModuleItem::Def(d) => HirNodeKind::Def(d),
            ModuleItem::Module(m) => HirNodeKind::Module(m),
        }
    }

    fn set_hir_id(&mut self, id: HirId) {
        match self {
            ModuleItem::Def(d) => d.set_hir_id(id),
            ModuleItem::Module(m) => m.set_hir_id(id),
        }
    }

}

/* #[derive(Debug)] */
/* pub struct Program<'hir> { */
/*     pub id: HirId, */
/*     pub defs: &'hir [Definition<'hir>], */
/* } */

/* impl<'hir> Program<'hir> { */
/*     pub const fn new(defs: &'hir [Definition<'hir>]) -> Self { */
/*         Self { */
/*             defs, */
/*             id: HirId::DUMMY, */
/*         } */
/*     } */
/* } */

