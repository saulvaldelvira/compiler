use span::Span;

use crate::HirId;

use super::types::Type;
use super::{Constness, Expression, Ident, Res, Statement};

#[derive(Debug)]
pub struct PathDef {
    pub ident: Ident,
    pub id: HirId,
}

#[derive(Debug)]
pub struct Field<'hir> {
    pub name: Ident,
    pub ty: &'hir Type<'hir>,
    pub id: HirId,
}

#[derive(Debug)]
pub enum DefinitionKind<'hir> {
    Variable { constness: Constness, init: Option<&'hir Expression<'hir>> },
    Function { body: &'hir [Statement<'hir>] },
    Struct { fields: &'hir [Field<'hir>] },
}

#[derive(Debug)]
pub struct Definition<'hir> {
    pub id: HirId,
    pub name: PathDef,
    pub ty: Res<&'hir Type<'hir>>,
    pub kind: DefinitionKind<'hir>,
    pub span: Span,
}
