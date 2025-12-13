use std::fmt::Debug;

use span::Span;

use crate::Path;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    I8(Span),
    I16(Span),
    I32(Span),
    I64(Span),
    U8(Span),
    U16(Span),
    U32(Span),
    U64(Span),
    F32(Span),
    F64(Span),
    Bool(Span),
    Char(Span),
    Array {
        open_brace: Span,
        ty: Box<Type>,
        semicollon: Span,
        length: u32,
        close_brace: Span,
    },
    Path(Path),
    Ref {
        ampersand: Span,
        of: Box<Type>,
    },
    Tuple(Box<[Type]>),
}

#[derive(Clone, PartialEq)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.kind)
    }
}
