pub mod hir_id;
mod hir;
use std::cell::{Cell, RefCell};
pub mod node_map;

pub use hir_id::HirId;

pub mod visitor;

pub use hir::*;
use hir_id::HirNode;
use node_map::{HirNodeKind, NodeMap};

mod _arena {
    use super::*;

    ::arena::define_arenas!(
        [visibility = pub]
        expr : Expression<'ctx>,
        types: Type<'ctx>,
        modules: Module<'ctx>,
    );
}

pub use _arena::ArenaAllocable;
pub use ::arena::markers;

/// Hir session
///
/// This struct represents a hir compilation session
/// It holds an arena with all the nodes of the tree, and
/// a map to find nodes by their [HirId]
pub struct Session<'hir> {
    root: Cell<Option<&'hir Module<'hir>>>,
    node_map: RefCell<NodeMap<'hir>>,
    arena: _arena::Arena<'hir>,
}

impl<'hir> Session<'hir> {
    pub fn set_root(&self, m: &'hir Module<'hir>) {
        self.root.set(Some(m));
    }

    pub fn get_root(&self) -> &'hir Module<'hir> {
        self.root.get().unwrap()
    }

    /// Gets a node by their [HirId].
    /// This functions assumes that the id is valid, and exists
    /// in the tree.
    ///
    /// # Panics
    /// - If the id dosn't match a node in the graph
    pub fn get_node(&self, id: &HirId) -> HirNodeKind<'hir> {
        self.node_map.borrow().get_by_id(id).unwrap_or_else(|| {
            unreachable!()
        })
    }
}

impl Default for Session<'_> {
    fn default() -> Self {
        Self {
            arena: _arena::Arena::new(),
            root: Cell::new(None),
            node_map: RefCell::default(),
        }
    }
}

impl<'hir> Session<'hir> {
    pub fn alloc<T,C>(&self, val: T) -> &'hir T
    where
        T: ArenaAllocable<'hir, C> + HirNode<'hir>
    {
        let elem = val.alloc_into(&self.arena);
        let mut map = self.node_map.borrow_mut();
        elem.set_hir_id(map.get_next_id());
        map.define(elem.get_hir_id(), elem.get_hir_node_kind());
        elem
    }
    pub fn alloc_iter<T, I, C>(&self, val: I) -> &'hir [T]
    where
        T: ArenaAllocable<'hir, C> + HirNode<'hir>,
        I: IntoIterator<Item = T>,
        <I as IntoIterator>::IntoIter: ExactSizeIterator
    {
        let items = T::alloc_iter(val, &self.arena);
        let mut node_map = self.node_map.borrow_mut();
        for elem in &mut *items {
            elem.set_hir_id(node_map.get_next_id());
        }
        for elem in &*items {
            node_map.define(elem.get_hir_id(), elem.get_hir_node_kind());
        }
        items
    }
}


