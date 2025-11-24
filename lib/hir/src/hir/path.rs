use super::{HirId, Ident, NodeRef};
use crate::{hir_id::HirNode, node_map::HirNodeKind};

#[derive(Debug, Clone)]
pub struct PathSegment {
    pub ident: Ident,
    pub def: NodeRef<HirId>,
}

#[derive(Debug, Clone)]
pub struct Path {
    pub is_absolute: bool,
    pub segments: Box<[PathSegment]>,
}

impl Path {
    pub fn new(is_absolute: bool, segments: Box<[PathSegment]>) -> Self {
        debug_assert!(!segments.is_empty());
        Self { is_absolute, segments }
    }

    pub fn from_ident(ident: Ident) -> Self {
        let segment = PathSegment {
            ident,
            def: NodeRef::pending(),
        };
        Self {
            is_absolute: false,
            segments: Box::new([segment]),
        }
    }

    pub fn last_segment(&self) -> &PathSegment {
        self.segments.last().unwrap_or_else(|| {
            unreachable!("We assert that segments is NOT empty when building it")
        })
    }

    pub fn def(&self) -> &NodeRef<HirId> { &self.segments.last().unwrap().def }
}

#[derive(Debug, Clone, Copy)]
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
    fn get_hir_id(&self) -> HirId { self.id }

    fn set_hir_id(&mut self, id: HirId) { self.id = id; }

    fn get_hir_node_kind(&'hir self) -> HirNodeKind<'hir> { HirNodeKind::PathDef(self) }
}
