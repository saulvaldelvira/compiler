use hir::def::DefinitionKind;
use hir::{Definition, Expression};
use semantic::types::TypeKind;

use crate::code_generator::CodeGenerator;
use crate::codefuncs::Eval;
use crate::mir::{MaplArithmetic, MaplInstruction, MaplLiteral, MaplType};
use crate::size::SizeOf;

use super::Address;

impl Address for Expression<'_> {
    fn address(&self, cg: &mut CodeGenerator<'_>, sem: &semantic::Semantic<'_>) -> MaplInstruction {
        use hir::expr::ExpressionKind;
        match &self.kind {
            ExpressionKind::Variable(path) => {
                path.def.expect_resolved().address(cg, sem)
            },
            ExpressionKind::ArrayAccess { arr, index } => {
                let addr = arr.address(cg, sem);
                let index = index.eval(cg, sem);

                let ty = sem.type_of(&self.id).unwrap();
                let size = ty.size_of();
                let size = MaplInstruction::Push(MaplLiteral::Int(size as i16));

                let offset = MaplInstruction::Arithmetic {
                    left: Box::new(index),
                    right: Box::new(size),
                    op: MaplArithmetic::Mul,
                    ty: MaplType::Int,
                };

                MaplInstruction::Arithmetic {
                    left: Box::new(addr),
                    right: Box::new(offset),
                    op: MaplArithmetic::Add,
                    ty: MaplType::Int
                }
            },
            ExpressionKind::StructAccess { st, field } => {
                let addr = st.address(cg, sem);
                let TypeKind::Struct { fields, .. }
                         = sem.type_of(&st.id).unwrap().kind else { unreachable!() };

                let offset = fields.iter()
                                   .take_while(|f| f.name != field.sym)
                                   .map(|f| f.ty.size_of() as i16)
                                   .sum();

                MaplInstruction::Arithmetic {
                    left: Box::new(addr),
                    right: Box::new(MaplInstruction::Push(MaplLiteral::Int(offset))),
                    op: MaplArithmetic::Add,
                    ty: MaplType::Int,
                }
            },
            ExpressionKind::Deref(r) => {
                r.eval(cg, sem)
            },
            ExpressionKind::Ref(_) |
            ExpressionKind::Array(_) |
            ExpressionKind::Unary { .. } |
            ExpressionKind::Logical { .. } |
            ExpressionKind::Comparison { .. } |
            ExpressionKind::Arithmetic { .. } |
            ExpressionKind::Ternary { .. } |
            ExpressionKind::Assignment { .. } |
            ExpressionKind::Literal(_) |
            ExpressionKind::Cast { .. } |
            ExpressionKind::Call { .. } => {
                unreachable!("We can't address a {:#?}", self.kind);
            }
        }
    }
}

impl Address for Definition<'_> {
    fn address(&self, ctx: &mut CodeGenerator<'_>, _sem: &semantic::Semantic<'_>) -> MaplInstruction {
        match &self.kind {
            DefinitionKind::Variable { .. } => {
                let addr = ctx.address_of(&self.id).unwrap();
                MaplInstruction::Pushaddr(addr)
            }
            DefinitionKind::Function { .. } => {
                MaplInstruction::Call(self.name.ident.sym.to_string())
            },
            DefinitionKind::Struct { .. } => unreachable!()
        }
    }
}
