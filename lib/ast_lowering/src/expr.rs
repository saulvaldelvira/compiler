use ast::Symbol;
use hir::expr::{ArithmeticOp, CmpOp};
use hir::{Ident, Res};
use span::Spanned;
use hir::expr::{ExpressionKind as HExprKind, LogicalOp};

use crate::{ident, AstLowering};

impl<'low, 'hir: 'low> AstLowering<'low, 'hir> {

    pub (super) fn lower_expressions(&mut self, expr: &[ast::Expression]) -> &'hir [hir::Expression<'hir>] {
        self.arena.alloc_iter(
            expr.iter().map(|expr| self.lower_expression_owned(expr))
        )
    }

    pub (super) fn lower_expression(&mut self, expr: &ast::Expression) -> &'hir hir::Expression<'hir> {
        self.arena.alloc(self.lower_expression_owned(expr))
    }

    fn lower_to_path(&mut self, spanned: &Spanned<Symbol>) -> hir::expr::Path {
        hir::expr::Path {
            id: self.next_id(),
            def: Res::Pending,
            ident: ident(spanned),
        }
    }

    fn lower_expression_owned(&mut self, expr: &ast::Expression) -> hir::Expression<'hir> {
        use ast::expr::ExpressionKind as EK;
        let kind = match &expr.kind {
            EK::Unary { op, expr } => todo!(),
            EK::Paren(parenthesized) => todo!(),
            EK::Binary { op, left, right } => {
                let left = self.lower_expression(left);
                let right = self.lower_expression(right);
                use ast::expr::BinaryExprOp as BOP;
                match op.val {
                    BOP::Add |
                        BOP::Sub |
                        BOP::Mul |
                        BOP::Div |
                        BOP::Mod => {
                            let op = match op.val {
                                BOP::Add => ArithmeticOp::Add,
                                BOP::Sub => ArithmeticOp::Sub,
                                BOP::Mul => ArithmeticOp::Mul,
                                BOP::Div => ArithmeticOp::Div,
                                BOP::Mod => ArithmeticOp::Mod,
                                _ => unreachable!(),
                            };
                            HExprKind::Arithmetic { left, op, right }
                        },
                        BOP::Gt |
                            BOP::Ge |
                            BOP::Lt |
                            BOP::Le |
                            BOP::Eq |
                            BOP::Neq =>  {
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
                            },
                            BOP::And |
                                BOP::Or =>  {
                                    let op = match op.val {
                                        BOP::And => LogicalOp::And,
                                        BOP::Or => LogicalOp::Or,
                                        _ => unreachable!(),
                                    };
                                    HExprKind::Logical { left, op, right }
                                },
                            BOP::Assign => {
                                HExprKind::Assignment { left, right }
                            }
                }
            },
            EK::Ternary { cond, if_true, if_false } => todo!(),
            EK::Path(spanned) => {
                HExprKind::Variable(self.lower_to_path(spanned))
            },
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
            },
            EK::Call { callee, args } => todo!(),
            EK::ArrayAccess { arr, index, .. } =>  {
                let arr = self.lower_expression(arr);
                let index = self.lower_expression(index);

                HExprKind::ArrayAccess { arr, index }
            },
            EK::StructAccess { st, field } => {
                let st = self.lower_expression(st);

                HExprKind::StructAccess { st, field: self.lower_to_path(field) }
            },
        };
        hir::Expression {
            kind,
            id: self.next_id(),
            span: expr.span,
        }
    }
}
