use std::fmt;

use crate::node_map::HirNodeKind;


#[derive(Clone,Copy,Hash,Eq,PartialEq)]
pub struct HirId(pub(crate) usize);

impl HirId {
    pub const DUMMY: Self = HirId(usize::MAX);
}

impl fmt::Debug for HirId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub trait HirNode<'hir> {
    fn get_hir_id(&self) -> HirId;
    fn set_hir_id(&mut self, id: HirId);
    fn get_hir_node_kind(&'hir self) -> HirNodeKind<'hir>;
}

#[macro_export]
macro_rules! impl_hir_node {
    ($s:ty, $var:ident) => {
        impl<'hir> HirNode<'hir> for $s {
            fn get_hir_id(&self) -> HirId { self.id }

            fn get_hir_node_kind(&'hir self) -> HirNodeKind<'hir> {
                HirNodeKind:: $var (self)
            }

            fn set_hir_id(&mut self, id: HirId) {
                self.id = id;
            }
        }
    };
}
