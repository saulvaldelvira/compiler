use core::fmt;

use lexer::Span;

use crate::Expression;

#[derive(Debug)]
pub struct VariableDecl {
    pub is_const: bool,
    pub name: Box<str>,
    pub init: Option<Expression>
}

#[derive(Debug)]
pub enum DeclarationKind {
    Variable(VariableDecl),
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
