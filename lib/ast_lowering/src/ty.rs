use hir::types::PrimitiveType;

use crate::AstLowering;

impl<'low, 'hir: 'low> AstLowering<'low, 'hir> {
    pub(super) fn lower_type(&mut self, ty: &ast::types::Type) -> &'hir hir::Type<'hir> {
        self.sess.alloc(self.lower_type_owned(ty))
    }

    pub(super) fn lower_types(&mut self, ty: &[ast::types::Type]) -> &'hir [hir::Type<'hir>] {
        self.sess.alloc_iter(ty.iter().map(|ty| self.lower_type_owned(ty)))
    }

    pub(super) fn lower_type_owned(&mut self, ty: &ast::types::Type) -> hir::Type<'hir> {
        use ast::types::TypeKind;
        use hir::types::TypeKind as HTK;
        let kind = match &ty.kind {
            TypeKind::I8(_) => HTK::Primitive(PrimitiveType::I8),
            TypeKind::I16(_) => HTK::Primitive(PrimitiveType::I16),
            TypeKind::I32(_) => HTK::Primitive(PrimitiveType::I32),
            TypeKind::I64(_) => HTK::Primitive(PrimitiveType::I64),
            TypeKind::U8(_) => HTK::Primitive(PrimitiveType::U8),
            TypeKind::U16(_) => HTK::Primitive(PrimitiveType::U16),
            TypeKind::U32(_) => HTK::Primitive(PrimitiveType::U32),
            TypeKind::U64(_) => HTK::Primitive(PrimitiveType::U64),
            TypeKind::F32(_) => HTK::Primitive(PrimitiveType::F32),
            TypeKind::F64(_) => HTK::Primitive(PrimitiveType::F64),
            TypeKind::Bool(_) => HTK::Primitive(PrimitiveType::Bool),
            TypeKind::Char(_) => HTK::Primitive(PrimitiveType::Char),
            TypeKind::Tuple(t) => {
                let tys = self.lower_types(t);
                HTK::Tuple(tys)
            }
            TypeKind::Array { ty, length, .. } => {
                let ty = self.lower_type(ty);
                HTK::Array(ty, *length)
            }
            TypeKind::Path(spanned) => HTK::Path(Self::lower_path(spanned)),
            TypeKind::Ref { of, .. } => HTK::Ref(self.lower_type(of)),
        };
        hir::Type::new(kind)
    }
}
