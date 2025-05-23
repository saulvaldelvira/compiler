use ast::Symbol;
use hir::{
    NodeRef,
    expr::{ArithmeticOp, CmpOp, ExpressionKind as HExprKind, LogicalOp, UnaryOp},
    path::PathSegment,
};
use span::Spanned;

use crate::{AstLowering, ident};

impl<'low, 'hir: 'low> AstLowering<'low, 'hir> {
    pub(super) fn lower_path(spanned: &[Spanned<Symbol>]) -> hir::Path {
        let mut segments = vec![];
        for sp in spanned {
            segments.push(Self::lower_path_segment(sp));
        }
        hir::Path::new(segments.into_boxed_slice())
    }

    #[allow(clippy::too_many_lines)]
    fn lower_path_segment(seg: &Spanned<Symbol>) -> PathSegment {
        PathSegment {
            ident: ident(seg),
            def: NodeRef::pending(),
        }
    }

    pub(super) fn lower_expressions(
        &mut self,
        expr: &[ast::Expression],
    ) -> &'hir [hir::Expression<'hir>] {
        self.sess
            .alloc_iter(expr.iter().map(|expr| self.lower_expression_owned(expr)))
    }

    pub(super) fn lower_expression(
        &mut self,
        expr: &ast::Expression,
    ) -> &'hir hir::Expression<'hir> {
        self.sess.alloc(self.lower_expression_owned(expr))
    }

    #[allow(clippy::too_many_lines)]
    fn lower_expression_owned(&mut self, expr: &ast::Expression) -> hir::Expression<'hir> {
        use ast::expr::{ExpressionKind as EK, UnaryExprOp};
        let kind = match &expr.kind {
            EK::Unary { op, expr } => {
                match op.val {
                    UnaryExprOp::Plus => {
                        return self.lower_expression_owned(expr);
                    }
                    UnaryExprOp::Negation => {
                        HExprKind::Unary {
                            op: UnaryOp::Neg,
                            expr: self.lower_expression(expr),
                        }
                    }
                    UnaryExprOp::Not => {
                        HExprKind::Unary {
                            op: UnaryOp::Not,
                            expr: self.lower_expression(expr),
                        }
                    }
                    UnaryExprOp::Deref => HExprKind::Deref(self.lower_expression(expr)),
                    UnaryExprOp::Ref => HExprKind::Ref(self.lower_expression(expr)),
                }
            }
            EK::Paren(parenthesized) => return self.lower_expression_owned(&parenthesized.val),
            EK::Cast { expr, ty, .. } => {
                let expr = self.lower_expression(expr);
                let to = self.lower_type(ty);
                HExprKind::Cast { expr, to }
            }
            EK::Binary { op, left, right } => {
                use ast::expr::BinaryExprOp as BOP;

                let left = self.lower_expression(left);
                let right = self.lower_expression(right);
                match op.val {
                    BOP::Add | BOP::Sub | BOP::Mul | BOP::Div | BOP::Mod => {
                        let op = match op.val {
                            BOP::Add => ArithmeticOp::Add,
                            BOP::Sub => ArithmeticOp::Sub,
                            BOP::Mul => ArithmeticOp::Mul,
                            BOP::Div => ArithmeticOp::Div,
                            BOP::Mod => ArithmeticOp::Mod,
                            _ => unreachable!(),
                        };
                        HExprKind::Arithmetic { left, op, right }
                    }
                    BOP::Gt | BOP::Ge | BOP::Lt | BOP::Le | BOP::Eq | BOP::Neq => {
                        let op = match op.val {
                            BOP::Gt => CmpOp::Gt,
                            BOP::Ge => CmpOp::Ge,
                            BOP::Lt => CmpOp::Lt,
                            BOP::Le => CmpOp::Le,
                            BOP::Eq => CmpOp::Eq,
                            BOP::Neq => CmpOp::Neq,
                            _ => unreachable!(),
                        };
                        HExprKind::Comparison { left, op, right }
                    }
                    BOP::And | BOP::Or => {
                        let op = match op.val {
                            BOP::And => LogicalOp::And,
                            BOP::Or => LogicalOp::Or,
                            _ => unreachable!(),
                        };
                        HExprKind::Logical { left, op, right }
                    }
                    BOP::Assign => HExprKind::Assignment { left, right },
                }
            }
            EK::Ternary {
                cond,
                if_true,
                if_false,
            } => {
                let cond = self.lower_expression(cond);
                let if_true = self.lower_expression(if_true);
                let if_false = self.lower_expression(if_false);
                HExprKind::Ternary {
                    cond,
                    if_true,
                    if_false,
                }
            }
            EK::Path(spanned) => HExprKind::Variable(Self::lower_path(spanned)),
            EK::Literal(spanned) => {
                use ast::expr::LitValue as ALV;
                use hir::expr::LitValue as HLV;
                let val = match spanned.val {
                    ALV::Int(n) => HLV::Int(n),
                    ALV::Float(n) => HLV::Float(n),
                    ALV::Str(symbol) => HLV::Str(symbol),
                    ALV::Bool(n) => HLV::Bool(n),
                    ALV::Char(n) => HLV::Char(n),
                };
                HExprKind::Literal(val)
            }
            EK::Call { callee, args } => {
                let callee = self.lower_expression(callee);
                let args = self.lower_expressions(&args.val);

                HExprKind::Call { callee, args }
            }
            EK::ArrayAccess { arr, index, .. } => {
                let arr = self.lower_expression(arr);
                let index = self.lower_expression(index);

                HExprKind::ArrayAccess { arr, index }
            }
            EK::StructAccess { st, field } => {
                let st = self.lower_expression(st);

                HExprKind::StructAccess {
                    st,
                    field: ident(field),
                }
            }
        };
        hir::Expression::new(kind, expr.span)
    }
}
