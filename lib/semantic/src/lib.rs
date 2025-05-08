mod __arena {
    ::arena::define_arenas!([visibility = pub]);
}

use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
};

use __arena::Arena;

pub mod types;
pub use types::*;
pub mod type_lowering;
use hir::HirId;
pub use type_lowering::*;
pub mod errors;
pub mod rules;

pub struct Semantic<'sem> {
    arena: Arena<'sem>,
    types: RefCell<HashMap<TypeId, &'sem Ty<'sem>>>,
    kind_to_id_assoc: RefCell<HashMap<TypeKind<'sem>, TypeId>>,
    hir_to_typeid_assoc: RefCell<HashMap<HirId, TypeId>>,

    next_id: Cell<usize>,
}

impl Default for Semantic<'_> {
    fn default() -> Self {
        Self {
            arena: Arena::new(),
            types: Default::default(),
            hir_to_typeid_assoc: Default::default(),
            kind_to_id_assoc: Default::default(),
            next_id: Cell::new(100),
        }
    }
}

impl<'sem> Semantic<'sem> {
    #[inline(always)]
    pub const fn get_arena(&self) -> &Arena<'sem> { &self.arena }

    pub fn set_type_of(&self, hir_id: HirId, id: TypeId) {
        debug_assert!(!self.hir_to_typeid_assoc.borrow().contains_key(&hir_id));
        self.hir_to_typeid_assoc.borrow_mut().insert(hir_id, id);
    }

    pub fn resolve_type(&self, id: &TypeId) -> Option<&'sem Ty<'sem>> {
        self.types.borrow().get(id).map(|v| {
            debug_assert_eq!(v.id, *id);
            &**v
        })
    }

    #[inline]
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

    #[inline]
    pub fn find_id_of_type_kind(&self, kind: &TypeKind<'sem>) -> Option<TypeId> {
        self.kind_to_id_assoc.borrow().get(kind).copied()
    }

    pub fn get_or_intern_type(&self, kind: TypeKind<'sem>) -> &'sem Ty<'sem> {
        if let Some(ty) = self.kind_to_id_assoc.borrow().get(&kind) {
            return self.types.borrow().get(ty).unwrap();
        }

        let id = self.next_id.get();
        let ty = Ty {
            kind,
            id: TypeId(id),
        };
        self.next_id.set(id + 1);

        let ty: &'sem Ty<'sem> = self.arena.alloc(ty);
        self.types.borrow_mut().insert(ty.id, ty);
        self.kind_to_id_assoc.borrow_mut().insert(ty.kind, ty.id);
        ty
    }
}
