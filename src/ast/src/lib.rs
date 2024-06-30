//! Abstract Syntax Tree
//!
//! This crate contains the AST for the language.

pub mod expr;
pub use expr::Expr;
pub mod stmt;
pub use stmt::Stmt;
pub mod types;
pub mod declaration;
pub mod visitor;
pub use visitor::Visitor;

use builders::{Constructor, Getters};

#[derive(Debug,Constructor,Getters)]
pub struct Program {
    stmts: Vec<Stmt>,
}
