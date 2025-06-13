use std::fmt::Debug;

use crate::hir_id::HirId;

#[derive(Debug, Clone, Copy)]
pub struct Ident {
    pub sym: Symbol,
    pub span: Span,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool { self.sym == other.sym }
}

#[derive(Debug, Clone, Copy)]
pub enum Constness {
    Const,
    Default,
}

pub mod types;
pub use types::Type;

pub mod stmt;
pub use stmt::Statement;

pub mod expr;
pub use expr::Expression;
use session::Symbol;
use span::Span;

pub mod item;
pub use item::*;

mod node_ref;
pub mod path;
pub use node_ref::{NodeRef, NodeRefKind};
pub use path::{Path, PathDef};

