//! Statements
//!
use core::fmt;

use crate::Expression;
use lexer::Span;

use super::declaration::Declaration;

type Stmt = Box<Statement>;

/// Wraps an [Expression](crate::expr::Expression) as a statement
#[derive(Debug)]
pub struct ExprAsStmt {
    pub expr: Expression,
}

#[derive(Debug)]
pub struct PrintStmt {
    pub expr: Expression,
}

#[derive(Debug)]
pub struct DeclarationStmt {
    pub inner: Declaration,
}

#[derive(Debug)]
pub struct BlockStmt {
    pub stmts: Vec<Statement>,
}

#[derive(Debug)]
pub struct IfStmt {
    pub cond: Expression,
    pub if_true: Stmt,
    pub if_false: Option<Stmt>,
}

#[derive(Debug)]
pub struct WhileStmt {
    pub cond: Expression,
    pub stmts: Stmt,
}

#[derive(Debug)]
pub struct ForStmt {
    pub init: Option<Declaration>,
    pub cond: Option<Expression>,
    pub inc: Option<Expression>,
    pub body: Stmt,
}

#[derive(Debug)]
pub struct EmptyStmt;

#[derive(Debug)]
pub struct BreakStmt;

#[derive(Debug)]
pub struct ContinueStmt;

#[derive(Debug)]
pub enum StatementKind {
    Expression(ExprAsStmt),
    Print(PrintStmt),
    Decl(DeclarationStmt),
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    Empty(EmptyStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
}

pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self.kind)
    }
}
