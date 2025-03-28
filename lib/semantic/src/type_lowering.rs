use std::collections::HashMap;

use crate::{PrimitiveType, Ty, TypeId, TypeKind};

pub struct TypeLowering<'low, 'ty, 'hir> {
    map: HashMap<&'hir hir::Type<'hir>, TypeId>,
    sem: &'low crate::Semantic<'ty>,
    next_id: usize,
}

impl<'low, 'ty, 'hir> TypeLowering<'low, 'ty, 'hir> {
    pub fn new(sem: &'low crate::Semantic<'ty>) -> Self {
        let mut tl = Self {
            map: HashMap::new(),
            next_id: 100,
            sem
        };

        /* Make sure all primitive types are lowered, even if they
         * don't appear on the hir tree */
        tl.lower_hir_types(hir::Type::primitive_array());

        tl
    }

    pub fn lower_hir_types(&mut self, tys: &'hir [hir::Type<'hir>]) -> &'ty [Ty<'ty>] {
        let tys = self.sem.arena.alloc_iter(
            tys.iter().map(|ty| self.lower_hir_type_owned(ty))
        );
        for t in &*tys {
            self.sem.register_type(t);
        }
        tys
    }

    pub fn lower_hir_type(&mut self, ty: &'hir hir::Type<'hir>) -> &'ty Ty<'ty> {
        if let Some(ty) = self.map.get(ty) {
            return self.sem.types.borrow().get(ty).unwrap_or_else(|| {
                unreachable!("We should NEVER add a TypeId to map that hasn't been interned into types.");
            })
        };
        let ty = self.lower_hir_type_owned(ty);
        let ty = self.sem.arena.alloc(ty);
        self.sem.register_type(ty);
        ty
    }


    fn lower_fields(&mut self, fields: &'hir [hir::def::Field<'hir>]) -> &'ty [crate::Field<'ty>] {
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

    fn lower_hir_type_owned(&mut self, ty: &'hir hir::Type<'hir>) -> Ty<'ty> {
        use hir::types::TypeKind as HTK;
        let kind = match &ty.kind {
            HTK::Primitive(primitive_type) => TypeKind::Primitive(PrimitiveType::from(primitive_type)),
            HTK::Ref(t) => TypeKind::Ref(self.lower_hir_type(t)),
            HTK::Array(arr, index) => TypeKind::Array(self.lower_hir_type(arr), *index),
            HTK::Struct(s) => {
                let (name,fields) = s.def.expect_resolved().as_struct_def().expect("SHOULD BE STRUCT");
                let fields = self.lower_fields(fields);
                TypeKind::Struct {
                    name,
                    fields
                }
            },
            HTK::Function { params, ret_ty } => {
                let params = self.lower_hir_types(params);
                let ret_ty = self.lower_hir_type(&ret_ty);
                TypeKind::Function { params, ret_ty }
            }
        };

        let ty = Ty {
            kind,
            id: TypeId(self.next_id),
        };
        self.next_id += 1;

        ty
    }

}

