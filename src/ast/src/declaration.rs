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

#[derive(Debug)]
pub struct Declaration {
    pub kind: DeclarationKind,
    pub span: Span,
}

