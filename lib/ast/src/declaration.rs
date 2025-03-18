use core::fmt;

use session::Symbol;
use span::{Span, Spanned};

use crate::{Block, Expression, Statement};
use crate::types::Type;

#[derive(Debug,Clone,PartialEq)]
pub enum MemoryAddress {
    Absolute(u16),
    Relative(i16),
    FieldOffset(u16),
}

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
pub enum DeclarationKind {
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
}

pub struct Declaration {
    pub kind: DeclarationKind,
    pub span: Span,
}

impl fmt::Debug for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self.kind)
    }
}
