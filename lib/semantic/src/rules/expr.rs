use error_manager::ErrorManager;
use hir::expr::StructAccess;
use hir::stmt::ForStmt;
use hir::{Expression, Ident, expr::ExpressionKind};
use span::Span;

use super::SemanticRule;
use crate::{
    Ty, TypeId, TypeKind,
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
            | ExpressionKind::TupleAccess { .. }
            | ExpressionKind::Variable(_) => true,
            ExpressionKind::Assignment { left, .. } => left.is_lvalue(),
            ExpressionKind::Array(_)
            | ExpressionKind::Unary { .. }
            | ExpressionKind::Ref(_)
            | ExpressionKind::Logical { .. }
            | ExpressionKind::Comparison { .. }
            | ExpressionKind::Arithmetic { .. }
            | ExpressionKind::If { .. }
            | ExpressionKind::Block { .. }
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
            ExpressionKind::StructAccess(StructAccess { st, ..}) => st.has_side_effect(),
            ExpressionKind::TupleAccess { tuple, .. } => tuple.has_side_effect(),
            ExpressionKind::Array(arr) => arr.iter().any(SideEffect::has_side_effect),
            ExpressionKind::Unary { expr, .. }
            | ExpressionKind::Cast { expr, .. } => expr.has_side_effect(),
            ExpressionKind::Ref(r) => r.has_side_effect(),
            ExpressionKind::Logical { left, right, .. }
            | ExpressionKind::Comparison { left, right, .. }
            | ExpressionKind::Arithmetic { left, right, .. } => {
                left.has_side_effect() || right.has_side_effect()
            }
            ExpressionKind::If {
                cond,
                if_true,
                if_false,
                ..
            } => cond.has_side_effect() || if_true.has_side_effect() || if_false.as_ref().is_some_and(|i| i.has_side_effect()),
            ExpressionKind::Block(b) => b.has_side_effect(),
            ExpressionKind::Literal(_) | ExpressionKind::Variable(_) => false,
        }
    }
}

impl SideEffect for hir::BlockExpr<'_> {
    fn has_side_effect(&self) -> bool {
        self.stmts.iter().any(SideEffect::has_side_effect)
        || self.tail.is_none_or(SideEffect::has_side_effect)
    }
}

impl SideEffect for hir::Statement<'_> {
    fn has_side_effect(&self) -> bool {
        use hir::stmt::StatementKind;
        match &self.kind {
            StatementKind::Expr(expr) => expr.has_side_effect(),
            StatementKind::While { cond, body } => cond.has_side_effect() || body.has_side_effect(),
            StatementKind::For(ForStmt { init, cond, inc, body }) => {
                init.is_some_and(SideEffect::has_side_effect) ||
                cond.is_some_and(SideEffect::has_side_effect) ||
                inc.is_some_and(SideEffect::has_side_effect) ||
                body.has_side_effect()
            }
            StatementKind::Empty |
            StatementKind::Break |
            StatementKind::Continue => false,
            StatementKind::Return(expr) => expr.is_some_and(SideEffect::has_side_effect),
            StatementKind::Item(i) => i.has_side_effect(),
        }
    }
}

impl SideEffect for hir::Item<'_> {
    fn has_side_effect(&self) -> bool {
        match &self.kind {
            hir::ItemKind::Variable { init, .. } => init.is_some_and(SideEffect::has_side_effect),
            _ => false
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

        if arr_ty.is_some_and(|arr| !matches!(arr.kind, TypeKind::Array(_, _) | TypeKind::Ref(_))) {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::IndexToNonArray,
                span: self.arr.span,
            });
        }

        if let Some(ind) = index_ty && !ind.is_integer() {
                em.emit_error(SemanticError {
                    kind: SemanticErrorKind::NonIntegerIndex,
                    span: self.index.span,
                });
        }

        match arr_ty {
            Some(Ty {
                kind: TypeKind::Array(of, _)
                    | TypeKind::Ref(of), ..
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

        let TypeKind::Function { is_variadic, params, ret_ty } = expr_ty.kind else {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::CallToNonFunction,
                span: self.callee.span,
            });
            return Default::default();
        };

        let mut src_params = self.args.iter();
        let dst_params = params.iter();

        let mut error = false;
        for expected_param in dst_params {
            let Some(src_param) = src_params.next() else {
                em.emit_error(SemanticError {
                    kind: SemanticErrorKind::MismatchedArgsNum {
                        expected: params.len(),
                        received: self.args.len(),
                    },
                    span: self.span,
                });
                return Default::default();
            };

            let Some(src_ty) = sem.type_of(&src_param.id) else { return Default::default() };

            if !src_ty.kind.can_be_promoted_to(&expected_param.kind) {
                let l = format!("{}", expected_param.kind);
                let r = format!("{}", src_ty.kind);
                em.emit_error(SemanticError {
                    kind: SemanticErrorKind::CantPromote(r, l),
                    span: src_param.span,
                });
                error = true;
            }
        }

        if src_params.next().is_some() && !is_variadic {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::MismatchedArgsNum { expected: self.args.len(), received: params.len() },
                span: self.span,
            });
            error = true;
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

pub struct ValidateIf<'hir> {
    pub if_true: &'hir Expression<'hir>,
    pub if_false: Option<&'hir Expression<'hir>>,
    pub span: Span,
}

impl<'sem> SemanticRule<'sem> for ValidateIf<'_> {
    type Result = Option<TypeId>;

    fn apply(&self, sem: &crate::Semantic<'sem>, em: &mut ErrorManager) -> Self::Result {
        let iftrue_ty = sem.type_of(&self.if_true.id)?;
        if let Some(if_false) = self.if_false {
            let iffalse_ty = sem.type_of(&if_false.id)?;

            if iftrue_ty != iffalse_ty {
                let ift = format!("{}", iftrue_ty.kind);
                let iff = format!("{}", iffalse_ty.kind);
                em.emit_error(SemanticError {
                    kind: SemanticErrorKind::MismatchedIfTypes(ift, iff),
                    span: self.span
                });
                return Default::default();
            }
        } else if !iftrue_ty.is_empty_type() {
            let ift = format!("{}", iftrue_ty.kind);
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::NonEmptyThenWithoutElse(ift),
                span: self.span
            });
            return Default::default();
        }
        Some(iftrue_ty.id)
    }
}

pub struct ValidateTupleAccess<'hir> {
    pub tuple: &'hir Expression<'hir>,
    pub index: u16,
    pub span: Span,
}

impl<'sem> SemanticRule<'sem> for ValidateTupleAccess<'_> {
    type Result = Option<TypeId>;

    fn apply(&self, sem: &crate::Semantic<'sem>, em: &mut ErrorManager) -> Self::Result {
        let ty = sem.type_of(&self.tuple.id)?;

        let tys = match ty.kind {
            TypeKind::Tuple(tys) if !tys.is_empty() => tys,
            _ => {
                let ty = format!("{}", ty.kind);
                em.emit_error(SemanticError {
                    kind: SemanticErrorKind::TupleAccessOnNonTuple(ty),
                    span: self.span,
                });
                return None
            }
        };

        if self.index as usize >= tys.len() {
            em.emit_error(SemanticError {
                kind: SemanticErrorKind::InvalidIndexForTuple(self.index, u16::try_from(tys.len()).unwrap() ),
                span: self.span,
            });
            return None
        }


        Some(tys[self.index as usize].id)
    }
}

pub struct ValidateArrayExpr<'hir> {
    pub exprs: &'hir [Expression<'hir>],
}

impl<'sem> SemanticRule<'sem> for ValidateArrayExpr<'_> {
    type Result = Option<TypeId>;

    fn apply(&self, sem: &crate::Semantic<'sem>, em: &mut ErrorManager) -> Self::Result {
        if self.exprs.is_empty() { return None }

        let expected_ty = sem.type_of(&self.exprs[0].id)?;
        for other in self.exprs.get(1..).unwrap_or(&[]) {
            let Some(ty) = sem.type_of(&other.id) else { continue };
            if ty != expected_ty {
                em.emit_error(SemanticError {
                    kind: SemanticErrorKind::MismatchedArrayTypes(
                              format!("{}", expected_ty.kind),
                              format!("{}", ty.kind)
                    ),
                    span: other.span,
                });
            }
        }

        let arr_ty = sem.get_or_intern_type(TypeKind::Array(expected_ty, u32::try_from(self.exprs.len()).unwrap()));
        Some(arr_ty.id)
    }
}
