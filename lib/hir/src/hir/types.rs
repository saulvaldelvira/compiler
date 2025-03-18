use crate::HirId;

use super::Ident;

#[derive(Debug)]
pub enum PrimitiveType {
    Int,
    Char,
    Float,
    Bool,
    Empty,
}

#[derive(Debug)]
pub enum TypeKind<'hir> {
    Primitive(PrimitiveType),
    Ref(&'hir Type<'hir>),
    Array(&'hir Type<'hir>, usize),
    Struct(Ident),
    Function { params: &'hir [Type<'hir>], ret_ty: Option<&'hir Type<'hir>> }
}

#[derive(Debug)]
pub struct Type<'hir> {
    pub id: HirId,
    pub kind: TypeKind<'hir>
}
