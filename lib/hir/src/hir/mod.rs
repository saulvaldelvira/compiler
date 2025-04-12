use std::collections::HashMap;
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
pub use expr::Expression;
use session::Symbol;
use span::Span;
pub use def::Definition;

pub mod def;
mod node_ref;
pub mod path;
pub use path::{PathDef, Path};
pub use node_ref::{NodeRef, NodeRefKind};

#[derive(Debug,Clone)]
pub struct Module<'hir> {
    pub id: HirId,
    pub defs: &'hir [Definition<'hir>],
    pub name: Symbol,
    pub span: Span,
    items: HashMap<Symbol, &'hir Definition<'hir>>,
}

impl<'hir> Module<'hir> {
    pub fn new(name: Symbol, defs: &'hir [Definition<'hir>], span: Span) -> Self {
        let mut m = Self { name, defs, span, id: HirId::DUMMY, items: HashMap::new() };
        for def in defs {
            m.items.insert(def.name.ident.sym, def);
        }
        m
    }

    pub fn find_definition(&self, name: &Symbol) -> Option<&'hir Definition<'hir>> {
        self.items.get(name).map(|v| &**v)
    }
}

impl_hir_node!(Module<'hir>, Module);
