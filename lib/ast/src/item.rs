use core::fmt;

use session::Symbol;
use span::{Span, Spanned};

use crate::Path;
use crate::{types::Type, Block, Expression, Statement};

#[derive(Debug)]
pub enum VariableConstness {
    Const(Span),
    Let(Span),
}

#[derive(Debug)]
pub struct Field {
    pub name: Spanned<Symbol>,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug)]
pub struct Param {
    pub name: Spanned<Symbol>,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug)]
pub enum ItemKind {
    Variable {
        constness: VariableConstness,
        name: Spanned<Symbol>,
        ty: Option<Type>,
        init: Option<Expression>,
        semicolon: Span,
    },
    Function {
        kw_fn: Span,
        name: Spanned<Symbol>,
        params: Box<[Param]>,
        return_type: Option<Type>,
        body: Block<Statement>,
    },
    Struct {
        kw_struct: Span,
        name: Spanned<Symbol>,
        fields: Block<Field>,
    },
    Use {
        kw_use: Span,
        path: Path,
        kw_as: Option<Span>,
        as_name: Option<Spanned<Symbol>>,
        semicolon: Span,
    },
    Mod(Module),
}

#[derive(Debug)]
pub struct Module {
    pub elems: Box<[Item]>,
    pub name: Spanned<Symbol>,
    pub span: Span,
}

pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
}

impl fmt::Debug for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{:#?}", self.kind) }
}
