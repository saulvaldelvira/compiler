use core::fmt;

use interner::Symbol;
use span::source::FileId;
use span::{Span, Spanned};

use crate::{Path, Statement};
use crate::{types::Type, Block, Expression};

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
pub enum UseTarget {
    Path(Path),
    Type(Type),
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
        kw_extern: Option<Span>,
        kw_fn: Span,
        name: Spanned<Symbol>,
        params: Box<[Param]>,
        variadic_span: Option<Span>,
        return_type: Option<Type>,
        body: Option<Block<Statement, Expression>>,
        semicolon: Option<Span>,
    },
    Struct {
        kw_struct: Span,
        name: Spanned<Symbol>,
        fields: Block<Field>,
    },
    Use {
        kw_use: Span,
        src: UseTarget,
        kw_as: Option<Span>,
        as_name: Option<Spanned<Symbol>>,
        semicolon: Span,
    },
    UseWildCard {
        kw_use: Span,
        src: Path,
        double_colon: Span,
        wildard_span: Span,
        semicolon: Span,
    },
    Mod(Module),
}

#[derive(Debug)]
pub enum ModuleBody {
    Inline(Block<Item>),
    Slf(Box<[Item]>, FileId),
    Extern {
        semicolon: Span,
        items: Box<[Item]>,
        id: FileId,
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: Spanned<Symbol>,
    pub span: Span,
    pub body: ModuleBody,
}

pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
}

impl fmt::Debug for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{:#?}", self.kind) }
}
