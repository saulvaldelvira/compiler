use hir::{BlockExpr, Expression};
use semantic::Semantic;

use super::Eval;
use crate::{
    code_generator::CodeGenerator,
    codefuncs::Address,
    mir::{MaplArithmetic, MaplComparison, MaplInstruction, MaplLiteral, MaplLogical, MaplType},
};

fn cast(
    semantic: &Semantic<'_>,
    ins: MaplInstruction,
    from: &Expression,
    to: &Expression,
) -> MaplInstruction {
    let from_ty = semantic.type_of(&from.id).unwrap();
    let to_ty = semantic.type_of(&to.id).unwrap();

    let from_ty = MaplType::from(from_ty);
    let to_ty = MaplType::from(to_ty);

    if from_ty == to_ty {
        return ins;
    }

    match (from_ty, to_ty) {
        (MaplType::Float, MaplType::Byte) => {
            let c1 = MaplInstruction::Cast {
                mapl: Box::new(ins),
                from: MaplType::Float,
                to: MaplType::Int,
            };
            MaplInstruction::Cast {
                mapl: Box::new(c1),
                from: MaplType::Int,
                to: MaplType::Byte,
            }
        }
        (MaplType::Byte, MaplType::Float) => {
            let c1 = MaplInstruction::Cast {
                mapl: Box::new(ins),
                from: MaplType::Byte,
                to: MaplType::Int,
            };
            MaplInstruction::Cast {
                mapl: Box::new(c1),
                from: MaplType::Int,
                to: MaplType::Float,
            }
        }
        _ => {
            MaplInstruction::Cast {
                mapl: Box::new(ins),
                from: from_ty,
                to: to_ty,
            }
        }
    }
}

impl Eval for BlockExpr<'_> {
    fn eval(&self, _cg: &mut CodeGenerator<'_, '_, '_>) -> MaplInstruction {
        todo!()
    }
}

impl Eval for Expression<'_> {

    #[expect(clippy::too_many_lines)]
    fn eval(&self, cg: &mut CodeGenerator) -> MaplInstruction {
        use hir::expr::ExpressionKind;
        match &self.kind {
            ExpressionKind::Array(_) => todo!(),
            ExpressionKind::Unary { op, expr } => {
                use hir::expr::UnaryOp;
                match op {
                    UnaryOp::Not => MaplInstruction::Not(Box::new(expr.eval(cg))),
                    UnaryOp::Neg => {
                        let ty = cg.sem.type_of(&expr.id).unwrap();
                        let ty = MaplType::from(ty);
                        let lit = match ty {
                            MaplType::Int => MaplLiteral::Int(0),
                            MaplType::Float => MaplLiteral::Float(0.0),
                            MaplType::Byte => MaplLiteral::Byte(0),
                        };

                        let lit = MaplInstruction::Push(lit);

                        MaplInstruction::Arithmetic {
                            left: Box::new(lit),
                            right: Box::new(expr.eval(cg)),
                            op: MaplArithmetic::Sub,
                            ty,
                        }
                    }
                }
            }
            ExpressionKind::Ref(expr) => expr.address(cg),
            ExpressionKind::Deref(expr) => {
                let ty = cg.sem.type_of(&self.id).unwrap();
                let ty = MaplType::from(ty);

                MaplInstruction::Compose(Box::new([expr.eval(cg), MaplInstruction::Load(ty)]))
            }
            ExpressionKind::Arithmetic { left, op, right } => {
                let left = left.eval(cg).into();
                let right = right.eval(cg).into();
                let op = MaplArithmetic::from(*op);
                let ty = cg.sem.type_of(&self.id).unwrap();
                let ty = MaplType::from(ty);

                MaplInstruction::Arithmetic {
                    left,
                    right,
                    op,
                    ty,
                }
            }
            ExpressionKind::Comparison { left, op, right } => {
                let ty = cg.sem.type_of(&left.id).unwrap();
                let ty = MaplType::from(ty);

                let left = left.eval(cg).into();
                let right = right.eval(cg).into();
                let op = MaplComparison::from(*op);

                MaplInstruction::Comparison {
                    left,
                    right,
                    op,
                    ty,
                }
            }
            ExpressionKind::Logical { left, op, right } => {
                let left = left.eval(cg).into();
                let right = right.eval(cg).into();
                let op = MaplLogical::from(*op);

                MaplInstruction::Logical { left, right, op }
            }
            ExpressionKind::If {
                cond,
                if_true,
                if_false,
            } => {
                let else_label = cg.next_label();
                let end_label = cg.next_label();
                MaplInstruction::Compose(Box::new([
                    cond.eval(cg),
                    MaplInstruction::Jz(else_label.clone()),
                    if_true.eval(cg),
                    MaplInstruction::Jmp(end_label.clone()),
                    MaplInstruction::DefineLabel(else_label),
                    if_false.unwrap().eval(cg),
                    MaplInstruction::DefineLabel(end_label),
                ]))
            }
            ExpressionKind::Assignment { left, right } => {
                let ty = cg.sem.type_of(&left.id).unwrap().into();
                let ins = [
                    left.address(cg),
                    right.eval(cg),
                    MaplInstruction::Store(ty),
                    left.address(cg),
                    MaplInstruction::Load(ty),
                ];
                MaplInstruction::Compose(Box::from(ins))
            }
            ExpressionKind::Literal(lit_value) => {
                use hir::expr::LitValue;
                let lit = match lit_value {
                    LitValue::Int(n) => MaplLiteral::Int(*n as i16),
                    LitValue::Float(f) => MaplLiteral::Float(*f as f32),
                    LitValue::Bool(b) => MaplLiteral::Int(i16::from(*b)),
                    LitValue::Char(c) => MaplLiteral::Byte(u8::try_from(*c).unwrap()),
                    LitValue::Str(_) => unreachable!(),
                };
                MaplInstruction::Push(lit)
            }
            ExpressionKind::Call { callee, args } => {
                let mut vec = Vec::new();
                for arg in *args {
                    vec.push(arg.eval(cg));
                }
                vec.push(callee.address(cg));
                MaplInstruction::Compose(vec.into_boxed_slice())
            }
            ExpressionKind::Cast { expr, .. } => {
                let ins = expr.eval(cg);
                cast(cg.sem, ins, expr, self)
            }
            ExpressionKind::Variable(_)
            | ExpressionKind::ArrayAccess { .. }
            | ExpressionKind::StructAccess { .. } => {
                let ty = cg.sem.type_of(&self.id).unwrap();
                let ty = MaplType::from(ty);
                MaplInstruction::Compose(Box::new([self.address(cg), MaplInstruction::Load(ty)]))
            },
            _ => todo!(),
        }
    }
}
