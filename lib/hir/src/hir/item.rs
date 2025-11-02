use core::fmt;
use std::collections::HashMap;

use interner::Symbol;
use span::source::FileId;
use span::Span;

use super::{Constness, Expression, Statement, types::Type};
use crate::{NodeRef, Path};
use crate::{HirId, hir_id::HirNode, impl_hir_node, node_map::HirNodeKind, path::PathDef};

#[derive(Debug, Clone)]
pub struct UseItem<'hir> {
    pub path: Path,
    pub new_name: &'hir PathDef,
}

impl<'hir> UseItem<'hir> {
    pub fn new(path: Path, new_name: &'hir PathDef) -> Self {
        Self { path, new_name }
    }

    pub fn get_name(&self) -> Symbol {
        self.new_name.ident.sym
    }

    #[inline]
    pub fn def(&self) -> &NodeRef<HirId> {
        self.path.def()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Param<'hir> {
    pub name: &'hir PathDef,
    pub ty: &'hir Type<'hir>,
    pub id: HirId,
    pub span: Span,
}

impl<'hir> Param<'hir> {
    pub fn new(name: &'hir PathDef, ty: &'hir Type<'hir>, span: Span) -> Self {
        Self { name, ty, span, id: HirId::DUMMY }
    }

    pub fn get_name(&self) -> Symbol { self.name.ident.sym }
}

impl_hir_node!(Param<'hir>, Param);

#[derive(Debug, Clone, Copy)]
pub enum ItemKind<'hir> {
    Mod(&'hir Module<'hir>),
    Variable {
        name: &'hir PathDef,
        ty: Option<&'hir Type<'hir>>,
        init: Option<&'hir Expression<'hir>>,
        constness: Constness,
    },
    Function {
        name: &'hir PathDef,
        params: &'hir [Param<'hir>],
        ret_ty: &'hir Type<'hir>,
        body: &'hir [Statement<'hir>],
    },
    Struct {
        name: &'hir PathDef,
        fields: &'hir [Field<'hir>],
    },
    Use(&'hir UseItem<'hir>),
}

impl ItemKind<'_> {
    pub fn get_repr(&self) -> &'static str {
        match self {
            ItemKind::Mod(_) => "module",
            ItemKind::Variable { .. } => "variable",
            ItemKind::Function { .. } => "function",
            ItemKind::Struct { .. } => "struct",
            ItemKind::Use(_) => "use",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Item<'hir> {
    pub kind: ItemKind<'hir>,
    pub span: Span,
    pub id: HirId,
}

impl_hir_node!(Item<'hir>, Item);

impl<'hir> Item<'hir> {
    pub fn new(kind: ItemKind<'hir>, span: Span) -> Self {
        Self { kind, span, id: HirId::DUMMY }
    }

    pub fn new_param(name: &'hir PathDef, ty: &'hir Type<'hir>, span: Span) -> Self {
        let kind = ItemKind::Variable {
            name,
            ty: Some(ty),
            init: None,
            constness: Constness::Default
        };
        Self::new(kind, span)
    }

    pub fn as_struct_def(&self) -> Option<(Symbol, &'hir [Field<'hir>])> {
        match self.kind {
            ItemKind::Struct { fields, name } => Some((name.ident.sym, fields)),
            _ => None,
        }
    }

    pub fn as_variable_def(
        &self,
    ) -> Option<(
        Constness,
        Option<&'hir Type<'hir>>,
        Option<&'hir Expression<'hir>>,
    )> {
        match self.kind {
            ItemKind::Variable {
                constness,
                ty,
                init,
                ..
            } => Some((constness, ty, init)),
            _ => None,
        }
    }

    pub fn as_module(&self) -> Option<&'hir Module<'_>> {
        match &self.kind {
            ItemKind::Mod(module) => Some(module),
            _ => None,
        }
    }

    pub fn as_use(&self) -> Option<&'hir UseItem<'hir>> {
        match &self.kind {
            ItemKind::Use(u) => Some(u),
            _ => None,
        }
    }

    pub fn get_name(&self) -> Symbol {
        match &self.kind {
            ItemKind::Mod(module) => module.name.ident.sym,
            ItemKind::Use(u) => u.get_name(),
            ItemKind::Variable{ name, .. } |
            ItemKind::Function { name, .. } |
            ItemKind::Struct { name, .. } => name.ident.sym
        }
    }

    pub const fn is_definition(&self) -> bool {
        matches!(self.kind, ItemKind::Variable {..}
                            | ItemKind::Function { .. }
                            | ItemKind::Struct {.. })
    }
}

#[derive(Clone)]
pub struct Module<'hir> {
    pub id: HirId,
    pub items: &'hir [Item<'hir>],
    pub name: &'hir PathDef,
    pub span: Span,
    pub extern_file: Option<FileId>,
    item_map: HashMap<Symbol, &'hir Item<'hir>>,
}

impl fmt::Debug for Module<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Module")
            .field("id", &self.id)
            .field("items", &self.items)
            .field("name", &self.name)
            .field("span", &self.span)
            .finish()
    }
}

impl<'hir> Module<'hir> {
    pub fn new(name: &'hir PathDef, defs: &'hir [Item<'hir>], span: Span, extern_file: impl Into<Option<FileId>>) -> Self {
        let mut m = Self {
            name,
            items: defs,
            span,
            extern_file: extern_file.into(),
            id: HirId::DUMMY,
            item_map: HashMap::new(),
        };
        for item in defs {
            m.item_map.insert(item.get_name(), item);
        }
        m
    }

    pub fn find_item(&self, name: Symbol) -> Option<&'hir Item<'hir>> {
        self.item_map.get(&name).map(|v| &**v)
    }
}

impl_hir_node!(Module<'hir>, Module);

#[derive(Debug, Clone, Copy)]
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
