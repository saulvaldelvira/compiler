use hir::Expression;
use semantic::Semantic;

use crate::code_generator::CodeGenerator;
use crate::codefuncs::Address;
use crate::mir::{MaplArithmetic, MaplComparison, MaplInstruction, MaplLiteral, MaplLogical, MaplType};

use super::Eval;

impl Eval for Expression<'_> {
    fn eval(&self, cg: &mut CodeGenerator, sem: &Semantic<'_>) -> MaplInstruction {
        use hir::expr::ExpressionKind;
        match &self.kind {
            ExpressionKind::Array(_) => todo!(),
            ExpressionKind::Unary { op, expr } => {
                use hir::expr::UnaryOp;
                match op {
                    UnaryOp::Not => MaplInstruction::Not(Box::new(expr.eval(cg, sem))),
                    UnaryOp::Neg => {
                        let ty = sem.type_of(&expr.id).unwrap();
                        let ty = MaplType::from(ty);
                        let lit = match ty {
                            MaplType::Int => MaplLiteral::Int(0),
                            MaplType::Float => MaplLiteral::Float(0.0),
                            MaplType::Byte => MaplLiteral::Byte(0),
                        };

                        let lit = MaplInstruction::Push(lit);

                        MaplInstruction::Arithmetic {
                            left: Box::new(lit),
                            right: Box::new(expr.eval(cg, sem)),
                            op: MaplArithmetic::Sub,
                            ty,
                        }
                    }
                }
            },
            ExpressionKind::Ref(expr) => {
                expr.address(cg, sem)
            },
            ExpressionKind::Deref(expr) => {
                let ty = sem.type_of(&self.id).unwrap();
                let ty = MaplType::from(ty);

                MaplInstruction::Compose(Box::new([
                    expr.eval(cg, sem),
                    MaplInstruction::Load(ty),
                ]))
            },
            ExpressionKind::Arithmetic { left, op, right } => {
                let left = left.eval(cg, sem).into();
                let right = right.eval(cg, sem).into();
                let op = MaplArithmetic::from(*op);
                let ty = sem.type_of(&self.id).unwrap();
                let ty = MaplType::from(ty);

                MaplInstruction::Arithmetic { left, right, op, ty }
            },
            ExpressionKind::Comparison { left, op, right } => {
                let ty = sem.type_of(&left.id).unwrap();
                let ty = MaplType::from(ty);

                let left = left.eval(cg, sem).into();
                let right = right.eval(cg, sem).into();
                let op = MaplComparison::from(*op);

                MaplInstruction::Comparison { left, right, op, ty }
            },
            ExpressionKind::Logical { left, op, right } => {
                let left = left.eval(cg, sem).into();
                let right = right.eval(cg, sem).into();
                let op = MaplLogical::from(*op);

                MaplInstruction::Logical { left, right, op }
            }
            ExpressionKind::Ternary { cond, if_true, if_false } => {
                let else_label = cg.next_label();
                let end_label = cg.next_label();
                MaplInstruction::Compose(Box::new([
                    cond.eval(cg, sem),
                    MaplInstruction::Jz(else_label.clone()),
                    if_true.eval(cg, sem),
                    MaplInstruction::Jmp(end_label.clone()),
                    MaplInstruction::DefineLabel(else_label),
                    if_false.eval(cg, sem),
                    MaplInstruction::DefineLabel(end_label),
                ]))
            },
            ExpressionKind::Assignment { left, right } => {
                let ty = sem.type_of(&left.id).unwrap().into();
                let ins = [
                    left.address(cg, sem),
                    right.eval(cg, sem),
                    MaplInstruction::Store(ty),
                    left.address(cg, sem),
                    MaplInstruction::Load(ty),
                ];
                MaplInstruction::Compose(Box::from(ins))
            },
            ExpressionKind::Literal(lit_value) => {
                use hir::expr::LitValue;
                let lit = match lit_value {
                    LitValue::Int(n) => MaplLiteral::Int(*n as i16),
                    LitValue::Float(f) => MaplLiteral::Float(*f as f32),
                    LitValue::Bool(b) => MaplLiteral::Int( if *b { 1 } else { 0 }),
                    LitValue::Char(c) => MaplLiteral::Byte(u8::try_from(*c).unwrap()),
                    _ => unreachable!()
                };
                MaplInstruction::Push(lit)
            }
            ExpressionKind::Call { callee, args } => {
                let mut vec = Vec::new();
                for arg in *args {
                    vec.push(arg.eval(cg, sem));
                }
                vec.push(callee.address(cg, sem));
                MaplInstruction::Compose(vec.into_boxed_slice())
            },
            ExpressionKind::Cast { expr, .. } => {
                let from_ty = sem.type_of(&expr.id).unwrap();
                let from_ty = MaplType::from(from_ty);
                let to = sem.type_of(&self.id).unwrap();
                let to = MaplType::from(to);
                MaplInstruction::Cast {
                    mapl: Box::new(expr.eval(cg, sem)),
                    from: from_ty,
                    to,
                }
            },
            ExpressionKind::Variable(_) |
            ExpressionKind::ArrayAccess { .. } |
            ExpressionKind::StructAccess { .. } => {
                let ty = sem.type_of(&self.id).unwrap();
                let ty = MaplType::from(ty);
                MaplInstruction::Compose(Box::new([
                        self.address(cg, sem),
                        MaplInstruction::Load(ty),
                ]))
            }
        }
    }
}
