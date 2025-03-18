use hir::types::PrimitiveType;

use crate::AstLowering;

impl<'low, 'hir: 'low> AstLowering<'low, 'hir> {

    pub (super) fn lower_type(&mut self, ty: &ast::types::Type) -> &'hir hir::Type<'hir> {
        self.sess.alloc(self.lower_type_owned(ty))
    }

    pub (super) fn lower_types<'a, I>(&mut self, ty: I) -> &'hir [hir::Type<'hir>]
    where
        I: IntoIterator<Item = &'a ast::types::Type>,
        <I as IntoIterator>::IntoIter: ExactSizeIterator,
    {
        self.sess.alloc_iter(ty.into_iter().map(|t| self.lower_type_owned(t)))
    }

    pub (super) fn lower_type_owned(&mut self, ty: &ast::types::Type) -> hir::Type<'hir> {
        use ast::types::TypeKind;
        use hir::types::TypeKind as HTK;
        let kind = match &ty.kind {
            TypeKind::Int(_) => HTK::Primitive(PrimitiveType::Int),
            TypeKind::Float(_) => HTK::Primitive(PrimitiveType::Float),
            TypeKind::Bool(_) => HTK::Primitive(PrimitiveType::Bool),
            TypeKind::Char(_) => HTK::Primitive(PrimitiveType::Char),
            TypeKind::Empty(_) => HTK::Primitive(PrimitiveType::Empty),
            TypeKind::Array { ty, length, .. } => {
                let ty = self.lower_type(ty);
                HTK::Array(ty, *length)
            },
            TypeKind::Struct(spanned) => {
                HTK::Struct(self.lower_path(spanned))
            },
            TypeKind::Ref { of, .. } => HTK::Ref(self.lower_type(of)),
        };
        hir::Type::new(kind)
    }
}
