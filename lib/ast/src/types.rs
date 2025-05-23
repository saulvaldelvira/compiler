use std::fmt::Debug;

use span::Span;

use crate::expr::Path;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Int(Span),
    Float(Span),
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
    Empty(Span),
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
