//! # Abstract Syntax Tree
//!
//! The AST is a syntactical representation of a program.
//! It is the output of the `parser`.
//!
//! This AST doesn't have any semantic information.
//! For example, it doesn't know the different types of binary operations
//! (arithmetic, logical, etc.). It just sees `<OPERAND> <OPERATOR> <OPERAND>`.
//!
//! We try to presserve all the syntactic information of the program
//! (semicolons, parenthesis, etc.), and we don't make up things that aren't there.
//!
//! For example, see [`ItemKind::Function`]. Its return type is an [Option],
//! beacuse this program is syntactically correct:
//! ```text
//! fn foo() { }
//! ```
//! but this one is also correct:
//! ```text
//! fn foo() -> int { ... }
//! ```
//!
//! So the AST doesn't infer anything. It's job is to represent the input program as
//! faithfully as possible. That's why the return type is optional, beacuse it can
//! be ommited.
//!
//! Later, we'll lower this AST to a HIR Tree, which will desugar some things, like
//! return types. It'll also remove parenthesis, since their only job is to explicit
//! the precedence of operations, and they become useless after building the AST.

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
