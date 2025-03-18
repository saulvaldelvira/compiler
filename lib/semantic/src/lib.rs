mod __arena {
    ::arena::define_arenas!([visibility = pub]);
}

use std::cell::RefCell;
use std::collections::HashMap;

use __arena::Arena;

pub mod types;
pub use types::*;
pub mod type_lowering;
use hir::hir_id::HirNode;
use hir::HirId;
pub use type_lowering::*;
pub mod errors;
pub mod rules;

pub struct Semantic<'sem> {
    arena: Arena<'sem>,
    types: RefCell<HashMap<TypeId, &'sem Ty<'sem>>>,
    hir_to_typeid_assoc: RefCell<HashMap<HirId, TypeId>>,
    kind_to_id_assoc: RefCell<HashMap<TypeKind<'sem>, TypeId>>,
}

impl<'sem> Default for Semantic<'sem> {
    fn default() -> Self {
        Self {
            arena: Arena::new(),
            types: Default::default(),
            hir_to_typeid_assoc: Default::default(),
            kind_to_id_assoc: Default::default(),
        }
    }
}

impl<'sem> Semantic<'sem> {
    pub fn get_arena(&self) -> &Arena<'sem> { &self.arena }

    pub fn set_type_of(&self, node: &dyn HirNode<'_>, id: TypeId) {
        debug_assert!(!self.hir_to_typeid_assoc.borrow().contains_key(&node.get_hir_id()));
        self.hir_to_typeid_assoc.borrow_mut().insert(node.get_hir_id(), id);
    }

    pub fn resolve_type(&self, id: &TypeId) -> Option<&'sem Ty<'sem>> {
        self.types.borrow().get(id).map(|v| {
            debug_assert_eq!(v.id, *id);
            &**v
        })
    }

    pub fn register_type(&self, ty: &'sem Ty<'sem>) {
        debug_assert!(!self.types.borrow().contains_key(&ty.id));
        self.types.borrow_mut().insert(ty.id, ty);
        self.kind_to_id_assoc.borrow_mut().insert(ty.kind, ty.id);
    }

    pub fn type_id_of(&self, node: &HirId) -> Option<TypeId> {
        self.hir_to_typeid_assoc.borrow().get(node).copied()
    }

    pub fn type_of(&self, node: &HirId) -> Option<&'sem Ty<'sem>> {
        self.hir_to_typeid_assoc.borrow().get(node).map(|id| {
            self.resolve_type(id).unwrap_or_else(|| {
                unreachable!("If we have a TypeId on the hir_to_typeid_assoc table, it MUST be also interned into the types table")
            })
        })
    }

    pub fn find_id_of_type_kind(&self, kind: &TypeKind<'sem>) -> Option<TypeId> {
        self.kind_to_id_assoc.borrow().get(kind).copied()
    }

    pub fn get_primitive_type(&self, prim: PrimitiveType) -> &'sem Ty<'sem> {
        self.find_id_of_type_kind(&TypeKind::Primitive(prim)).and_then(|id| {
            self.resolve_type(&id)
        }).unwrap_or_else(|| {
            unreachable!("Primitive types should've been interned when creating the TypeLowerer instance.");
        })
    }
}
