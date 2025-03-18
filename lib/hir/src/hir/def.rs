use session::Symbol;
use span::Span;

use crate::hir_id::HirNode;
use crate::node_map::HirNodeKind;
use crate::{impl_hir_node, HirId};

use super::types::Type;
use super::{Constness, Expression, Ident, Statement};

#[derive(Debug,Clone,Copy)]
pub struct PathDef {
    pub ident: Ident,
    pub id: HirId,
}

impl PathDef {
    pub fn new(ident: Ident) -> Self {
        Self {
            ident,
            id: HirId::DUMMY,
        }
    }
}

impl<'hir> HirNode<'hir> for PathDef {
    fn get_hir_id(&self) -> HirId {
        self.id
    }

    fn set_hir_id(&mut self, id: HirId) {
        self.id = id;
    }

    fn get_hir_node_kind(&'hir self) -> HirNodeKind<'hir> {
        HirNodeKind::PathDef(self)
    }
}

#[derive(Debug)]
pub struct Field<'hir> {
    pub name: &'hir PathDef,
    pub ty: &'hir Type<'hir>,
    pub id: HirId,
    pub span: Span,
}

impl_hir_node!(Field<'hir>, Field);

impl<'hir> Field<'hir> {
    pub fn new(name: &'hir PathDef, ty: &'hir Type<'hir>, span: Span) -> Self {
        Self {
            name,
            ty,
            span,
            id: HirId::DUMMY,
        }
    }
}

#[derive(Debug,Clone,Copy)]
pub enum DefinitionKind<'hir> {
    Variable { constness: Constness, init: Option<&'hir Expression<'hir>> },
    Function { params: &'hir [Definition<'hir>], body: &'hir [Statement<'hir>] },
    Struct { fields: &'hir [Field<'hir>] },
}

#[derive(Debug,Clone,Copy)]
pub struct Definition<'hir> {
    pub id: HirId,
    pub name: &'hir PathDef,
    pub ty: Option<&'hir Type<'hir>>,
    pub kind: DefinitionKind<'hir>,
    pub span: Span,
}

impl<'hir> Definition<'hir> {
    pub fn new(kind: DefinitionKind<'hir>, name: &'hir PathDef, ty: impl Into<Option<&'hir Type<'hir>>>, span: Span) -> Self {
        Self {
            kind,
            span,
            ty: ty.into(),
            name,
            id: HirId::DUMMY
        }
    }

    pub fn as_struct_def(&self) -> Option<(Symbol, &'hir [Field<'hir>])> {
        match self.kind {
            DefinitionKind::Struct { fields } => Some((self.name.ident.sym, fields)),
            _ => None
        }
    }
}

impl<'hir> HirNode<'hir> for Definition<'hir> {
    fn get_hir_id(&self) -> HirId { self.id }

    fn get_hir_node_kind(&'hir self) -> HirNodeKind<'hir> {
        HirNodeKind::Def(self)
    }

    fn set_hir_id(&mut self, id: HirId) {
        self.id = id;
    }
}
