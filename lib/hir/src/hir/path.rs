use crate::hir_id::HirNode;
use crate::node_map::HirNodeKind;

use super::{Definition, Ident, NodeRef};
use super::HirId;

#[derive(Debug,Clone)]
pub struct PathSegment<'hir> {
    pub ident: Ident,
    pub def: NodeRef<&'hir Definition<'hir>>,
}

#[derive(Debug,Clone)]
pub struct Path<'hir> {
    pub segments: Box<[PathSegment<'hir>]>,
}

impl<'hir> Path<'hir> {
    pub fn new(segments: Box<[PathSegment<'hir>]>) -> Self {
        debug_assert!(!segments.is_empty());
        Self { segments }
    }

    pub fn from_ident(ident: Ident) -> Self {
        let segment = PathSegment {
            ident,
            def: NodeRef::pending()
        };
        Self { segments: Box::new([segment]) }
    }

    pub fn def(&self) -> &NodeRef<&'hir Definition<'hir>> {
        &self.segments.last().unwrap().def
    }
}

#[derive(Debug,Clone,Copy)]
pub struct PathDef {
    pub ident: Ident,
    pub id: HirId,
}

impl PathDef {
    pub fn new(ident: Ident) -> Self {
        Self {
            ident,
            id: HirId::DUMMY,
        }
    }
}

impl<'hir> HirNode<'hir> for PathDef {
    fn get_hir_id(&self) -> HirId {
        self.id
    }

    fn set_hir_id(&mut self, id: HirId) {
        self.id = id;
    }

    fn get_hir_node_kind(&'hir self) -> HirNodeKind<'hir> {
        HirNodeKind::PathDef(self)
    }
}

