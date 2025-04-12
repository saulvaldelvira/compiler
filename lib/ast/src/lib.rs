//! Abstract Syntax Tree
//!
//! This crate contains the AST for the language.

pub mod expr;
pub mod stmt;

use std::fmt::Debug;

pub use expr::Expression;
use span::{Span, Spanned};
pub use stmt::Statement;
pub mod types;
pub mod declaration;
pub use declaration::Declaration;
pub mod visitor;
pub use visitor::Visitor;

pub use session::Symbol;

#[derive(Debug)]
pub struct Block<T> {
    pub open_brace: Span,
    pub val: Box<[T]>,
    pub close_brace: Span,
}

#[derive(Debug)]
pub struct Parenthesized<T> {
    pub open_paren: Span,
    pub val: T,
    pub close_paren: Span,
}

impl<T> Parenthesized<T> {
    pub fn span(&self) -> Span {
        self.open_paren.join(&self.close_paren)
    }
}

#[derive(Debug)]
pub struct Module {
    pub elems: Box<[Declaration]>,
    pub name: Spanned<Symbol>,
    pub span: Span,
}
