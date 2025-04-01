use error_manager::ErrorManager;
use hir::{Expression, Ident};
use span::Span;
use crate::errors::{SemanticError, SemanticErrorKind};
use crate::{PrimitiveType, Ty, TypeId, TypeKind};

use super::SemanticRule;

use hir::expr::ExpressionKind;

pub trait Lvalue {
    fn is_lvalue(&self) -> bool;
}

impl Lvalue for hir::Expression<'_> {
    fn is_lvalue(&self) -> bool {
        match &self.kind {
            ExpressionKind::ArrayAccess { .. }  |
            ExpressionKind::Deref(_) |
            ExpressionKind::StructAccess { .. } |
            ExpressionKind::Variable(_) => true,
            ExpressionKind::Assignment { left, .. } => left.is_lvalue(),
            ExpressionKind::Array(_) |
            ExpressionKind::Unary { .. } |
            ExpressionKind::Ref(_) |
            ExpressionKind::Logical { .. } |
            ExpressionKind::Comparison { .. } |
            ExpressionKind::Arithmetic { .. } |
            ExpressionKind::Ternary { .. } |
            ExpressionKind::Literal(_) |
            ExpressionKind::Cast { .. } |
            ExpressionKind::Call { .. } => false,
        }
    }
}

pub struct ValidateArrayAccess<'hir> {
    pub arr: &'hir Expression<'hir>,
    pub index: &'hir Expression<'hir>,
}

impl SemanticRule<'_> for ValidateArrayAccess<'_> {
    type Result = Option<TypeId>;

    fn apply(&self, sem: &crate::Semantic<'_>, em: &mut ErrorManager) -> Self::Result {
        let arr_ty = sem.type_of(&self.arr.id);
        let index_ty = sem.type_of(&self.index.id);

        if arr_ty.is_some_and(|arr| !matches!(arr.kind, TypeKind::Array(_,_))) {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::IndexToNonArray,
                span: self.arr.span,
            });
        }

        if let Some(ind) = index_ty {
            if !matches!(ind.kind, TypeKind::Primitive(PrimitiveType::Int)) {
                em.emit_error(SemanticError {
                    kind: SemanticErrorKind::NonIntegerIndex,
                    span: self.index.span
                });
            }
        }

        match arr_ty {
            Some(Ty { kind: TypeKind::Array(of, _), ..}) => Some(of.id),
            _ => None
        }
    }
}

pub struct ValidateAssignment<'hir> {
    pub left: &'hir Expression<'hir>,
    pub right: &'hir Expression<'hir>,
    pub span: Span,
}

impl SemanticRule<'_> for ValidateAssignment<'_> {
    type Result = Option<TypeId>;

    fn apply(&self, sem: &crate::Semantic<'_>, em: &mut ErrorManager) -> Self::Result {
        if !self.left.is_lvalue() {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::LValue,
                span: self.left.span,
            });
        }

        let left_ty = sem.type_of(&self.left.id)?;
        let right_ty = sem.type_of(&self.right.id)?;

        right_ty.promote_to(left_ty).map(|t| Some(t.id)).unwrap_or_else(|| {
            let l = format!("{}", left_ty.kind);
            let r = format!("{}", right_ty.kind);
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::CantPromote(r, l),
                span: self.span,
            });
            Default::default()
        })
    }
}

pub struct ValidateFieldAccess<'hir> {
    pub st: &'hir Expression<'hir>,
    pub field: Ident,
}

impl SemanticRule<'_> for ValidateFieldAccess<'_> {
    type Result = Option<TypeId>;

    fn apply(&self, sem: &crate::Semantic<'_>, em: &mut ErrorManager) -> Self::Result {

        let struct_ty = sem.type_of(&self.st.id)?;

        struct_ty
        .access_field(&self.field.sym)
        .map(|ty| Some(ty.id))
        .unwrap_or_else(|kind| {
            em.emit_error(SemanticError {
                kind,
                span: self.field.span,
            });
            Default::default()
        })
    }
}

pub struct ValidateArithmetic<'hir> {
    pub left: &'hir Expression<'hir>,
    pub right: &'hir Expression<'hir>,
    pub span: Span,
}

impl SemanticRule<'_> for ValidateArithmetic<'_> {
    type Result = Option<TypeId>;

    fn apply(&self, sem: &crate::Semantic<'_>, em: &mut ErrorManager) -> Self::Result {
        let left_ty = sem.type_of(&self.left.id)?;
        let right_ty = sem.type_of(&self.right.id)?;

        left_ty.arithmetic(right_ty).map(|t| Some(t.id)).unwrap_or_else(|| {
            let l = format!("{}", left_ty.kind);
            let r = format!("{}", right_ty.kind);
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::Arithmetic(l, r),
                span: self.span
            });
            Default::default()
        })
    }
}

pub struct ValidateLogical<'hir> {
    pub left: &'hir Expression<'hir>,
    pub right: &'hir Expression<'hir>,
    pub span: Span,
}

impl SemanticRule<'_> for ValidateLogical<'_> {
    type Result = Option<TypeId>;

    fn apply(&self, sem: &crate::Semantic<'_>, em: &mut ErrorManager) -> Self::Result {
        let left_ty = sem.type_of(&self.left.id)?;
        let right_ty = sem.type_of(&self.right.id)?;

        left_ty.logical(right_ty, sem).map(|t| Some(t.id)).unwrap_or_else(|| {
            let l = format!("{}", left_ty.kind);
            let r = format!("{}", right_ty.kind);
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::Logical(l, r),
                span: self.span
            });
            Default::default()
        })
    }
}

pub struct ValidateComparison<'hir> {
    pub left: &'hir Expression<'hir>,
    pub right: &'hir Expression<'hir>,
    pub span: Span,
}

impl SemanticRule<'_> for ValidateComparison<'_> {
    type Result = Option<TypeId>;

    fn apply(&self, sem: &crate::Semantic<'_>, em: &mut ErrorManager) -> Self::Result {
        let left_ty = sem.type_of(&self.left.id)?;
        let right_ty = sem.type_of(&self.right.id)?;

        left_ty.comparison(right_ty, sem).map(|t| Some(t.id)).unwrap_or_else(|| {
            let l = format!("{}", left_ty.kind);
            let r = format!("{}", right_ty.kind);
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::Compare(l, r),
                span: self.span
            });
            Default::default()
        })
    }
}

pub struct ValidateCall<'hir> {
    pub callee: &'hir Expression<'hir>,
    pub args: &'hir [hir::Expression<'hir>],
    pub span: Span,
}

impl SemanticRule<'_> for ValidateCall<'_> {
    type Result = Option<TypeId>;

    fn apply(&self, sem: &crate::Semantic<'_>, em: &mut ErrorManager) -> Self::Result {
        let expr_ty = sem.type_of(&self.callee.id)?;

        let TypeKind::Function { params, ret_ty } = expr_ty.kind else {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::CallToNonFunction,
                span: self.callee.span,
            });
            return Default::default()
        };

        if params.len() !=  self.args.len() {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::MismatchedArgsNum { expected: params.len(), received: self.args.len() },
                span: self.span
            });
            return Default::default()
        }

        let args_tys = self.args.iter().map(|expr| {
            sem.type_of(&expr.id).map(|ty| (ty, expr.span))
        });
        let mut error = false;
        for (param, arg) in params.iter().zip(args_tys) {
            let Some((arg,arg_span)) = arg else { return Default::default() };

            if !arg.kind.can_be_promoted_to(&param.kind) {
                let l = format!("{}", param.kind);
                let r = format!("{}", arg.kind);
                em.emit_error(SemanticError {
                    kind: SemanticErrorKind::CantPromote(l, r),
                    span: arg_span
                });
                error = true;
            }
        }

        if error {
            Default::default()
        } else {
            Some(ret_ty.id)
        }

    }
}

pub struct ValidateCast<'hir, 'sem> {
    pub expr: &'hir Expression<'hir>,
    pub to: &'sem Ty<'sem>,
    pub span: Span,
}

impl<'sem> SemanticRule<'sem> for ValidateCast<'_, 'sem> {
    type Result = Option<TypeId>;

    fn apply(&self, sem: &crate::Semantic<'sem>, em: &mut ErrorManager) -> Self::Result {
        let to_ty = sem.type_of(&self.expr.id)?;

        if !matches!(to_ty.kind, TypeKind::Primitive(_)) {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::NonPrimitiveCast(to_ty.kind.to_string()),
                span: self.span
            });
        }

        if !matches!(self.to.kind, TypeKind::Primitive(_)) {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::NonPrimitiveCast(self.to.kind.to_string()),
                span: self.span
            });
        }

        let ty = to_ty.promote_to(self.to);
        if ty.is_none() {
            let from = format!("{}", to_ty.kind);
            let to = format!("{}", self.to.kind);
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::InvalidCast { from, to },
                span: self.span,
            });
        }

        ty.map(|t| t.id)
    }
}

