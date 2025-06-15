//! Abstract Syntax Tree
//!
//! This crate contains the AST for the language.

pub mod expr;
pub mod stmt;

use std::fmt::Debug;

pub use expr::Expression;
use span::{Span, Spanned};
pub use stmt::Statement;
pub mod item;
pub use item::*;
pub mod types;
pub mod visitor;
pub use interner::Symbol;
pub use visitor::Visitor;

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

#[derive(Debug, Clone, PartialEq)]
pub struct Path {
    pub segments: Box<[Spanned<Symbol>]>,
    pub span: Span,
}

impl<T> Parenthesized<T> {
    pub fn span(&self) -> Span { self.open_paren.join(&self.close_paren) }
}
