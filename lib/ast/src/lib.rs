//! Abstract Syntax Tree
//!
//! This crate contains the AST for the language.

pub mod expr;
pub mod stmt;

pub use expr::Expression;
pub use stmt::Statement;
pub mod types;
pub mod declaration;
pub use declaration::Declaration;
pub mod visitor;
pub use visitor::Visitor;

#[derive(Debug)]
pub struct Program {
    pub decls: Box<[Declaration]>,
}

