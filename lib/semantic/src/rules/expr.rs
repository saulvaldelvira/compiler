use error_manager::ErrorManager;
use hir::{Expression, Ident, expr::ExpressionKind};
use span::Span;

use super::SemanticRule;
use crate::{
    PrimitiveType, Ty, TypeId, TypeKind,
    errors::{SemanticError, SemanticErrorKind},
};

pub trait Lvalue {
    fn is_lvalue(&self) -> bool;
}

impl Lvalue for hir::Expression<'_> {
    fn is_lvalue(&self) -> bool {
        match &self.kind {
            ExpressionKind::ArrayAccess { .. }
            | ExpressionKind::Deref(_)
            | ExpressionKind::StructAccess { .. }
            | ExpressionKind::Variable(_) => true,
            ExpressionKind::Assignment { left, .. } => left.is_lvalue(),
            ExpressionKind::Array(_)
            | ExpressionKind::Unary { .. }
            | ExpressionKind::Ref(_)
            | ExpressionKind::Logical { .. }
            | ExpressionKind::Comparison { .. }
            | ExpressionKind::Arithmetic { .. }
            | ExpressionKind::Ternary { .. }
            | ExpressionKind::Literal(_)
            | ExpressionKind::Cast { .. }
            | ExpressionKind::Call { .. } => false,
        }
    }
}

pub trait SideEffect {
    fn has_side_effect(&self) -> bool;
}

impl SideEffect for hir::Expression<'_> {
    fn has_side_effect(&self) -> bool {
        match &self.kind {
            ExpressionKind::Assignment { .. } | ExpressionKind::Call { .. } => true,

            ExpressionKind::ArrayAccess { arr, index } => {
                arr.has_side_effect() || index.has_side_effect()
            }
            ExpressionKind::Deref(e) => e.has_side_effect(),
            ExpressionKind::StructAccess { st, .. } => st.has_side_effect(),
            ExpressionKind::Array(arr) => arr.iter().any(SideEffect::has_side_effect),
            ExpressionKind::Unary { expr, .. }
            | ExpressionKind::Cast { expr, .. } => expr.has_side_effect(),
            ExpressionKind::Ref(r) => r.has_side_effect(),
            ExpressionKind::Logical { left, right, .. }
            | ExpressionKind::Comparison { left, right, .. }
            | ExpressionKind::Arithmetic { left, right, .. } => {
                left.has_side_effect() || right.has_side_effect()
            }
            ExpressionKind::Ternary {
                cond,
                if_true,
                if_false,
                ..
            } => cond.has_side_effect() || if_true.has_side_effect() || if_false.has_side_effect(),
            ExpressionKind::Literal(_) | ExpressionKind::Variable(_) => false,
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

        if arr_ty.is_some_and(|arr| !matches!(arr.kind, TypeKind::Array(_, _))) {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::IndexToNonArray,
                span: self.arr.span,
            });
        }

        if let Some(ind) = index_ty {
            if !matches!(ind.kind, TypeKind::Primitive(PrimitiveType::Int)) {
                em.emit_error(SemanticError {
                    kind: SemanticErrorKind::NonIntegerIndex,
                    span: self.index.span,
                });
            }
        }

        match arr_ty {
            Some(Ty {
                kind: TypeKind::Array(of, _),
                ..
            }) => Some(of.id),
            _ => None,
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

        right_ty
            .promote_to(left_ty)
            .map(|t| Some(t.id))
            .unwrap_or_else(|| {
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

        left_ty
            .arithmetic(right_ty)
            .map(|t| Some(t.id))
            .unwrap_or_else(|| {
                let l = format!("{}", left_ty.kind);
                let r = format!("{}", right_ty.kind);
                em.emit_error(SemanticError {
                    kind: SemanticErrorKind::Arithmetic(l, r),
                    span: self.span,
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

        left_ty
            .logical(right_ty, sem)
            .map(|t| Some(t.id))
            .unwrap_or_else(|| {
                let l = format!("{}", left_ty.kind);
                let r = format!("{}", right_ty.kind);
                em.emit_error(SemanticError {
                    kind: SemanticErrorKind::Logical(l, r),
                    span: self.span,
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

        left_ty
            .comparison(right_ty, sem)
            .map(|t| Some(t.id))
            .unwrap_or_else(|| {
                let l = format!("{}", left_ty.kind);
                let r = format!("{}", right_ty.kind);
                em.emit_error(SemanticError {
                    kind: SemanticErrorKind::Compare(l, r),
                    span: self.span,
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
            return Default::default();
        };

        if params.len() != self.args.len() {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::MismatchedArgsNum {
                    expected: params.len(),
                    received: self.args.len(),
                },
                span: self.span,
            });
            return Default::default();
        }

        let args_tys = self
            .args
            .iter()
            .map(|expr| sem.type_of(&expr.id).map(|ty| (ty, expr.span)));
        let mut error = false;
        for (param, arg) in params.iter().zip(args_tys) {
            let Some((arg, arg_span)) = arg else {
                return Default::default();
            };

            if !arg.kind.can_be_promoted_to(&param.kind) {
                let l = format!("{}", param.kind);
                let r = format!("{}", arg.kind);
                em.emit_error(SemanticError {
                    kind: SemanticErrorKind::CantPromote(l, r),
                    span: arg_span,
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
        let from_ty = sem.type_of(&self.expr.id)?;

        let mut errored = false;

        if !matches!(from_ty.kind, TypeKind::Primitive(_) | TypeKind::Ref(_)) {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::NonPrimitiveCast(from_ty.kind.to_string()),
                span: self.span,
            });
            errored = true;
        }

        if !matches!(self.to.kind, TypeKind::Primitive(_) | TypeKind::Ref(_)) {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::NonPrimitiveCast(self.to.kind.to_string()),
                span: self.span,
            });
            errored = true;
        }

        if !errored && !from_ty.kind.can_cast(&self.to.kind) {
            let from = format!("{}", from_ty.kind);
            let to = format!("{}", self.to.kind);
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::InvalidCast { from, to },
                span: self.span,
            });
        }

        Some(self.to.id)
    }
}

pub struct ValidateTernary<'hir> {
    pub cond: &'hir Expression<'hir>,
    pub if_true: &'hir Expression<'hir>,
    pub if_false: &'hir Expression<'hir>,
    pub span: Span,
}

impl<'sem> SemanticRule<'sem> for ValidateTernary<'_> {
    type Result = Option<TypeId>;

    fn apply(&self, sem: &crate::Semantic<'sem>, em: &mut ErrorManager) -> Self::Result {
        let cond_ty = sem.type_of(&self.cond.id)?;
        let ift_ty = sem.type_of(&self.if_true.id)?;
        let iff_ty = sem.type_of(&self.if_false.id)?;

        if !cond_ty
            .kind
            .can_be_promoted_to(&TypeKind::Primitive(PrimitiveType::Bool))
        {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::CantPromote(cond_ty.kind.to_string(), "bool".to_string()),
                span: self.span,
            });
        }

        let ty = iff_ty.promote_to(ift_ty);
        if ty.is_none() {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::CantPromote(iff_ty.kind.to_string(), ift_ty.to_string()),
                span: self.span,
            });
        }

        ty.map(|ty| ty.id)
    }
}
