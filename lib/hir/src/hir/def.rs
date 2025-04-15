use core::fmt;

use session::Symbol;
use span::Span;

use crate::hir_id::HirNode;
use crate::node_map::HirNodeKind;
use crate::{impl_hir_node, HirId};
use crate::path::PathDef;

use super::types::Type;
use super::{Constness, Expression, Module, Statement};

#[derive(Debug,Clone,Copy)]
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
    Variable { constness: Constness, ty: Option<&'hir Type<'hir>>, init: Option<&'hir Expression<'hir>> },
    Function { params: &'hir [Definition<'hir>], ret_ty: &'hir Type<'hir>, body: &'hir [Statement<'hir>] },
    Struct { fields: &'hir [Field<'hir>] },
    Module(&'hir Module<'hir>),
}

#[derive(Clone,Copy)]
pub struct Definition<'hir> {
    pub id: HirId,
    pub name: &'hir PathDef,
    pub kind: DefinitionKind<'hir>,
    pub span: Span,
}

impl fmt::Debug for Definition<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut d = f.debug_struct("Definition");
        d.field("id", &self.id)
         .field("name", &self.name)
         .field("kind", &self.kind)
         .field("span", &self.span)
         .finish()
    }
}

impl<'hir> Definition<'hir> {
    pub fn new(kind: DefinitionKind<'hir>, name: &'hir PathDef, span: Span) -> Self {
        Self {
            kind,
            span,
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

    pub fn as_variable_def(&self) -> Option<(Constness, Option<&'hir Type<'hir>>, Option<&'hir Expression<'hir>>)> {
        match self.kind {
            DefinitionKind::Variable { constness, ty, init } => Some((constness, ty, init)),
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
