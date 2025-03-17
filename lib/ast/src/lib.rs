//! Abstract Syntax Tree
//!
//! This crate contains the AST for the language.

pub mod expr;
pub mod stmt;

use std::fmt::Debug;

pub use expr::Expression;
pub use stmt::Statement;
pub mod types;
pub mod declaration;
pub use declaration::Declaration;
pub mod visitor;
pub use visitor::Visitor;

pub use lexer::Span;
mod decorated;
pub use decorated::{AstRef,AstDecorated};

#[derive(Debug)]
pub struct Program {
    pub decls: Box<[Declaration]>,
}
