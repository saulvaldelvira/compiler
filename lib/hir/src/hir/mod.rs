use crate::hir_id::HirId;

#[derive(Debug)]
pub struct Ident {
    pub sym: Symbol,
    pub span: Span,
}

#[derive(Debug)]
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
pub use def::Definition;

pub mod def;

#[derive(Debug)]
pub enum Res<T> {
    Resolved(T),
    Pending,
    Err
}

impl<T> From<Option<T>> for Res<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(val) => Self::Resolved(val),
            None => Self::Pending,
        }
    }
}

#[derive(Debug)]
pub struct Program<'hir> {
    pub id: HirId,
    pub defs: &'hir [Definition<'hir>],
}
