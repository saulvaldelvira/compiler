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

#[derive(Debug,Clone,Copy)]
pub enum ModItemKind<'hir> {
    Mod(&'hir Module<'hir>),
    Def(&'hir Definition<'hir>),
}

#[derive(Debug,Clone,Copy)]
pub struct ModItem<'hir> {
    pub kind: ModItemKind<'hir>,
    pub id: HirId,
}

impl<'hir> ModItem<'hir> {
    #[inline(always)]
    pub const fn new(kind: ModItemKind<'hir>) -> Self {
        Self { kind, id: HirId::DUMMY }
    }
}

impl_hir_node!(ModItem<'hir>, ModItem);

#[derive(Debug,Clone)]
pub struct Module<'hir> {
    pub id: HirId,
    pub items: &'hir [ModItem<'hir>],
    pub name: Symbol,
    pub span: Span,
    item_map: HashMap<Symbol, &'hir ModItem<'hir>>,
}

impl<'hir> Module<'hir> {
    pub fn new(name: Symbol, defs: &'hir [ModItem<'hir>], span: Span) -> Self {
        let mut m = Self { name, items: defs, span, id: HirId::DUMMY, item_map: HashMap::new() };
        for item in defs {
            match item.kind {
                ModItemKind::Mod(module) => m.item_map.insert(module.name, item),
                ModItemKind::Def(definition) => m.item_map.insert(definition.name.ident.sym, item),
            };
        }
        m
    }

    pub fn find_definition(&self, name: &Symbol) -> Option<&'hir ModItem<'hir>> {
        self.item_map.get(name).map(|v| &**v)
    }
}

impl_hir_node!(Module<'hir>, Module);
