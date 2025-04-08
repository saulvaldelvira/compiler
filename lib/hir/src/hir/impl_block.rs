use crate::hir_id::HirNode;
use crate::node_map::HirNodeKind;
use crate::HirId;

use super::{Definition, Path};

#[derive(Debug,Clone)]
pub struct ImplBlock<'hir> {
    pub path: Path<'hir>,
    pub defs: &'hir [Definition<'hir>],
    pub id: HirId,
}

impl<'hir> ImplBlock<'hir> {
    #[inline(always)]
    pub fn new(path: Path<'hir>, defs: &'hir [Definition<'hir>]) -> Self {
        Self { path, defs, id: HirId::DUMMY }
    }
}

impl<'hir> HirNode<'hir> for ImplBlock<'hir> {
    fn get_hir_id(&self) -> HirId { self.id }

    fn set_hir_id(&mut self, id: HirId) {
        self.id = id;
    }

    fn get_hir_node_kind(&'hir self) -> HirNodeKind<'hir> {
        HirNodeKind::Impl(self)
    }
}
