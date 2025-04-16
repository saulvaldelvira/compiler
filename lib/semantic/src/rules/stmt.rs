use hir::{Definition, Expression, Statement};
use span::Span;

use crate::errors::{SemanticError, SemanticErrorKind};
use crate::{PrimitiveType, TypeKind};

use super::SemanticRule;

pub struct CheckFunctionReturns<'hir> {
    pub def: &'hir Definition<'hir>,
    pub body: &'hir [Statement<'hir>],
    pub span: Span,
}

impl SemanticRule<'_> for CheckFunctionReturns<'_> {
    type Result = ();

    fn apply(&self, sem: &crate::Semantic<'_>, em: &mut error_manager::ErrorManager) -> Self::Result {

        let Some(ty) = sem.type_of(&self.def.id) else { return };
        let (_, ret_type) = ty.as_function_type().expect("Expected function's type to be of FuncType");

        if !ret_type.is_empty_type()
         && !self.body.iter().any(HasReturn::has_return)
        {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::FunctionNeedsReturn(self.def.name.ident.sym),
                span: self.span,
            });
        }
    }
}

pub struct CheckReturnStmt<'sem> {
    pub definition: &'sem Definition<'sem>,
    pub found: Option<&'sem Expression<'sem>>,
    pub span: Span,
}

impl SemanticRule<'_> for CheckReturnStmt<'_> {
    type Result = ();

    fn apply(&self, sem: &crate::Semantic<'_>, em: &mut error_manager::ErrorManager) -> Self::Result {

        let Some(found) = self.found.map_or_else(
                        ||     Some(sem.get_or_intern_type(TypeKind::Primitive(PrimitiveType::Empty))),
                        |expr| sem.type_of(&expr.id)) else { return };

        let def_ty = sem.type_of(&self.definition.id).unwrap();
        let (_, expected) = def_ty.as_function_type().unwrap();

        if !found.kind.can_be_promoted_to(&expected.kind) {
            let expected = format!("{}", expected.kind);
            let got = format!("{}", found.kind);
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::MistmatchedReturn { expected, got },
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
