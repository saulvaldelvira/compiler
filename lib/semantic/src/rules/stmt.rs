use hir::{Definition, Statement};
use span::Span;

use crate::errors::{SemanticError, SemanticErrorKind};
use crate::Ty;

use super::SemanticRule;

pub struct CheckFunctionReturns<'hir, 'sem> {
    pub def: &'hir Definition<'hir>,
    pub body: &'hir [Statement<'hir>],
    pub ret_type: &'sem Ty<'sem>,
    pub span: Span,
}

impl SemanticRule for CheckFunctionReturns<'_,'_> {
    type Result = ();

    fn apply(&self, _sem: &crate::Semantic<'_>, em: &mut error_manager::ErrorManager) -> Self::Result {
        if !self.ret_type.is_empty_type()
         && !self.body.iter().any(HasReturn::has_return)
        {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::FunctionNeedsReturn(self.def.name.ident.sym),
                span: self.span,
            });
        }
    }
}

pub trait HasReturn {
    fn has_return(&self) -> bool;
}

impl HasReturn for hir::Statement<'_> {
    fn has_return(&self) -> bool {
        use hir::stmt::StatementKind;
        match self.kind {
            StatementKind::If { if_true, if_false, .. } => {
                if_true.has_return()
                && if_false.is_some_and(HasReturn::has_return)
            },
            StatementKind::Block(stmts) => {
                stmts.iter().any(HasReturn::has_return)
            },
            StatementKind::Return(_) => true,
            StatementKind::While { .. } |
            StatementKind::Expr(_) |
            StatementKind::For { .. } |
            StatementKind::Empty |
            StatementKind::Break |
            StatementKind::Continue |
            StatementKind::Print(_) |
            StatementKind::Read(_) |
            StatementKind::Def(_) => false
        }
    }
}
