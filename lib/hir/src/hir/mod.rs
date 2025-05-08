use std::{collections::HashMap, fmt::Debug};

use crate::{
    hir_id::{HirId, HirNode},
    impl_hir_node,
    node_map::HirNodeKind,
};

#[derive(Debug, Clone, Copy)]
pub struct Ident {
    pub sym: Symbol,
    pub span: Span,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool { self.sym == other.sym }
}

#[derive(Debug, Clone, Copy)]
pub enum Constness {
    Const,
    Default,
}

pub mod types;
pub use types::Type;

pub mod stmt;
pub use stmt::Statement;

pub mod expr;
pub use def::Definition;
pub use expr::Expression;
use session::Symbol;
use span::Span;

pub mod def;
mod node_ref;
pub mod path;
pub use node_ref::{NodeRef, NodeRefKind};
pub use path::{Path, PathDef};

#[derive(Debug, Clone, Copy)]
pub enum ModItemKind<'hir> {
    Mod(&'hir Module<'hir>),
    Def(&'hir Definition<'hir>),
}

#[derive(Debug, Clone, Copy)]
pub struct ModItem<'hir> {
    pub kind: ModItemKind<'hir>,
    pub id: HirId,
}

impl<'hir> ModItem<'hir> {
    #[inline(always)]
    pub const fn new(kind: ModItemKind<'hir>) -> Self {
        Self {
            kind,
            id: HirId::DUMMY,
        }
    }

    pub const fn inner_id(&self) -> HirId {
        match self.kind {
            ModItemKind::Mod(module) => module.id,
            ModItemKind::Def(definition) => definition.id,
        }
    }
}

impl_hir_node!(ModItem<'hir>, ModItem);

#[derive(Clone)]
pub struct Module<'hir> {
    pub id: HirId,
    pub items: &'hir [ModItem<'hir>],
    pub name: Symbol,
    pub span: Span,
    item_map: HashMap<Symbol, &'hir ModItem<'hir>>,
}

impl Debug for Module<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Module")
            .field("id", &self.id)
            .field("items", &self.items)
            .field("name", &self.name)
            .field("span", &self.span)
            .finish()
    }
}

impl<'hir> Module<'hir> {
    pub fn new(name: Symbol, defs: &'hir [ModItem<'hir>], span: Span) -> Self {
        let mut m = Self {
            name,
            items: defs,
            span,
            id: HirId::DUMMY,
            item_map: HashMap::new(),
        };
        for item in defs {
            match item.kind {
                ModItemKind::Mod(module) => m.item_map.insert(module.name, item),
                ModItemKind::Def(definition) => m.item_map.insert(definition.name.ident.sym, item),
            };
        }
        m
    }

    pub fn find_item(&self, name: &Symbol) -> Option<&'hir ModItem<'hir>> {
        self.item_map.get(name).map(|v| &**v)
    }
}

impl_hir_node!(Module<'hir>, Module);
