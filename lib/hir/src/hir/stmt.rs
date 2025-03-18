use span::Span;

use crate::HirId;

use super::{Definition, Expression};

#[derive(Debug)]
pub enum StatementKind<'hir> {
    Expr(&'hir Expression<'hir>),
    Def(&'hir Definition<'hir>),
}

#[derive(Debug)]
pub struct Statement<'hir> {
    pub kind: StatementKind<'hir>,
    pub span: Span,
    pub id: HirId,
}
