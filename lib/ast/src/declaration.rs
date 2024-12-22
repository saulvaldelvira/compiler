use core::fmt;

use lexer::Span;

use crate::stmt::BlockStmt;
use crate::Expression;
use crate::types::Type;

#[derive(Debug)]
pub struct VariableDecl {
    pub is_const: bool,
    pub name: Box<str>,
    pub init: Option<Expression>
}

#[derive(Debug)]
pub struct FunctionArgument {
    pub name: Box<str>,
    pub ty: Type,
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: Box<str>,
    pub args: Box<[FunctionArgument]>,
    pub return_type: Type,
    pub body: BlockStmt,
}

#[derive(Debug)]
pub enum DeclarationKind {
    Variable(VariableDecl),
    Function(FunctionDecl),
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
