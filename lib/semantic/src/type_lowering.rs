//! Type lowering
//!
//! Hir types hold no semantic meaning, they just represent a Type
//! that has been written in the program.
//!
//! # Example
//! ```text
//!
//! fn foo() -> int {
//!   let a: int;
//!   ...
//! }
//! ```
//!
//! In the example above, we have a funtion 'foo' that returns int, and
//! inside it, a variable 'a' with type int.
//!
//! In the HIR, those two types are different HIR nodes, and have different
//! [HirId](hir::HirId)s.
//!
//! Prior to type checking, we need to lower all those HIR Types into the
//! semantic types.
//! So foo's return type and a's type will have the same [TypeId].

use std::collections::HashMap;

use crate::{PrimitiveType, Ty, TypeId, TypeKind};

pub struct TypeLowering<'low, 'ty, 'hir> {
    map: HashMap<&'hir hir::Type<'hir>, TypeId>,
    reverse_map: HashMap<TypeId, &'hir hir::Type<'hir>>,
    sem: &'low crate::Semantic<'ty>,
}

impl<'low, 'ty, 'hir> TypeLowering<'low, 'ty, 'hir> {
    pub fn new(sem: &'low crate::Semantic<'ty>) -> Self {
        let mut tl = Self {
            map: HashMap::new(),
            reverse_map: HashMap::new(),
            sem
        };

        /* Make sure all primitive types are lowered, even if they
         * don't appear on the hir tree */
        tl.lower_hir_types(hir::Type::primitive_array());

        tl
    }

    pub fn lower_hir_types_iter(&mut self, tys: impl ExactSizeIterator<Item = &'hir hir::Type<'hir>>) -> &'ty [&'ty Ty<'ty>] {
        let tys = self.sem.arena.alloc_iter(
            tys.map(|ty| self.lower_hir_type(ty))
        );
        tys
    }

    pub fn lower_hir_types(&mut self, tys: &'hir [hir::Type<'hir>]) -> &'ty [&'ty Ty<'ty>] {
        let tys = self.sem.arena.alloc_iter(
            tys.iter().map(|ty| self.lower_hir_type(ty))
        );
        tys
    }

    pub fn lower_hir_type(&mut self, ty: &'hir hir::Type<'hir>) -> &'ty Ty<'ty> {
        if let Some(ty) = self.map.get(ty) {
            return self.sem.types.borrow().get(ty).unwrap_or_else(|| {
                unreachable!("We should NEVER add a TypeId to map that hasn't been interned into types.");
            })
        };
        let sem_ty = self.lower_hir_type_owned(ty);
        let sem_ty = self.sem.get_or_intern_type(sem_ty);
        self.map.insert(ty, sem_ty.id);
        self.reverse_map.insert(sem_ty.id, ty);
        sem_ty
    }

    pub fn get_hir_type_from_semantic_id(&self, id: &TypeId) -> &'hir hir::Type<'hir> {
        self.reverse_map.get(id).unwrap_or_else(|| {
            unreachable!("If we have a TypeId, it must've been interned, thus it MUST BE HERE!")
        })
    }

    pub fn lower_fields(&mut self, fields: &'hir [hir::def::Field<'hir>]) -> &'ty [crate::Field<'ty>] {
        self.sem.arena.alloc_iter(
            fields.iter().map(|f| self.lower_field_owned(f))
        )
    }

    fn lower_field_owned(&mut self, field: &'hir hir::def::Field<'hir>) -> crate::Field<'ty> {
        crate::Field {
            name: field.name.ident.sym,
            ty: self.lower_hir_type(field.ty),
        }
    }

    fn lower_hir_type_owned(&mut self, ty: &'hir hir::Type<'hir>) -> TypeKind<'ty> {
        use hir::types::TypeKind as HTK;
        let kind = match &ty.kind {
            HTK::Primitive(primitive_type) => TypeKind::Primitive(PrimitiveType::from(primitive_type)),
            HTK::Ref(t) => TypeKind::Ref(self.lower_hir_type(t)),
            HTK::Array(arr, index) => TypeKind::Array(self.lower_hir_type(arr), *index),
            HTK::Function { params, ret_ty } => {
                let params = self.lower_hir_types(params);
                let ret_ty = self.lower_hir_type(ret_ty);
                TypeKind::Function { params, ret_ty }
            },
            HTK::Path(_) => unreachable!(),
        };

        kind
    }

}

