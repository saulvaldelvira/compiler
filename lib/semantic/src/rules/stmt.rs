use hir::{BlockExpr, Expression, Item};
use span::Span;

use super::SemanticRule;
use crate::{
    TypeKind,
    errors::{SemanticError, SemanticErrorKind},
};

pub struct CheckFunctionReturns<'hir> {
    pub def: &'hir Item<'hir>,
    pub body: &'hir BlockExpr<'hir>,
    pub span: Span,
}

impl SemanticRule<'_> for CheckFunctionReturns<'_> {
    type Result = ();

    fn apply(
        &self,
        sem: &crate::Semantic<'_>,
        em: &mut error_manager::ErrorManager,
    ) -> Self::Result {
        let Some(ty) = sem.type_of(&self.def.id) else {
            return;
        };
        let (_, _, ret_type) = ty
            .as_function_type()
            .expect("Expected function's type to be of FuncType");

        if let Some(tail) = self.body.tail {
            if let Some(tailty) = sem.type_of(&tail.id)
                && !tailty.kind.can_be_promoted_to(&ret_type.kind)
            {
                let expected = format!("{}", ret_type.kind);
                let got = format!("{}", tailty.kind);
                em.emit_error(SemanticError {
                    kind: SemanticErrorKind::MistmatchedReturn { expected, got },
                    span: self.span,
                });
            }
        } else if !ret_type.is_empty_type() && !self.body.has_return() {
                em.emit_error(SemanticError {
                    kind: SemanticErrorKind::FunctionNeedsReturn(self.def.get_name().unwrap()),
                    span: self.span,
                });
            }
        }
}

pub struct CheckReturnStmt<'sem> {
    pub definition: &'sem Item<'sem>,
    pub found: Option<&'sem Expression<'sem>>,
    pub span: Span,
}

impl SemanticRule<'_> for CheckReturnStmt<'_> {
    type Result = ();

    fn apply(
        &self,
        sem: &crate::Semantic<'_>,
        em: &mut error_manager::ErrorManager,
    ) -> Self::Result {
        let Some(found) = self.found.map_or_else(
            || Some(sem.get_or_intern_type(TypeKind::empty())),
            |expr| sem.type_of(&expr.id),
        ) else {
            return;
        };

        let def_ty = sem.type_of(&self.definition.id).unwrap();
        let (_, _, expected) = def_ty.as_function_type().unwrap();

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

impl HasReturn for hir::Expression<'_> {
    fn has_return(&self) -> bool {
        use hir::expr::ExpressionKind;
        match &self.kind {
            ExpressionKind::Block(b) => b.has_return(),
            ExpressionKind::If { if_true, if_false, .. } =>
                if_true.has_return() && if_false.as_deref().is_some_and(HasReturn::has_return),
            _ => false,
        }
    }
}

impl HasReturn for hir::BlockExpr<'_> {
    fn has_return(&self) -> bool {
        self.stmts.iter().any(HasReturn::has_return)
    }
}

impl HasReturn for hir::Statement<'_> {
    fn has_return(&self) -> bool {
        use hir::stmt::StatementKind;
        match self.kind {
            StatementKind::Return(_) => true,
            StatementKind::While { .. }
            | StatementKind::Expr(_)
            | StatementKind::For { .. }
            | StatementKind::Empty
            | StatementKind::Break
            | StatementKind::Continue
            | StatementKind::Item(_) => false,
        }
    }
}
