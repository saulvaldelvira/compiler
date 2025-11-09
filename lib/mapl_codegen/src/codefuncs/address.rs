use hir::{Item, ItemKind, Param};
use hir::{Expression, node_map::HirNodeKind};
use semantic::types::TypeKind;

use super::Address;
use crate::{
    code_generator::CodeGenerator,
    codefuncs::Eval,
    mir::{MaplArithmetic, MaplInstruction, MaplLiteral, MaplType},
    size::SizeOf,
};

impl Address for Expression<'_> {
    fn address(&self, cg: &mut CodeGenerator<'_, '_, '_>) -> MaplInstruction {
        use hir::expr::ExpressionKind;
        match &self.kind {
            ExpressionKind::Variable(path) => {
                let def_id = path.def().expect_resolved();
                let node = cg.hir.get_node(&def_id);
                match node {
                    HirNodeKind::Item(item) => item.address(cg),
                    HirNodeKind::Param(param) => param.address(cg),
                    _ => unreachable!()
                }
            }
            ExpressionKind::ArrayAccess { arr, index } => {
                let addr = arr.address(cg);
                let index = index.eval(cg);

                let ty = cg.sem.type_of(&self.id).unwrap();
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
                    ty: MaplType::Int,
                }
            }
            ExpressionKind::StructAccess { st, field } => {
                let addr = st.address(cg);
                let TypeKind::Struct { fields, .. } = cg.sem.type_of(&st.id).unwrap().kind else {
                    unreachable!()
                };

                let offset = fields
                    .iter()
                    .take_while(|f| f.name != field.sym)
                    .map(|f| f.ty.size_of() as i16)
                    .sum();

                MaplInstruction::Arithmetic {
                    left: Box::new(addr),
                    right: Box::new(MaplInstruction::Push(MaplLiteral::Int(offset))),
                    op: MaplArithmetic::Add,
                    ty: MaplType::Int,
                }
            }
            ExpressionKind::Deref(r) => r.eval(cg),
            ExpressionKind::Ref(_)
            | ExpressionKind::Array(_)
            | ExpressionKind::Unary { .. }
            | ExpressionKind::Logical { .. }
            | ExpressionKind::Comparison { .. }
            | ExpressionKind::Arithmetic { .. }
            | ExpressionKind::Ternary { .. }
            | ExpressionKind::Assignment { .. }
            | ExpressionKind::Literal(_)
            | ExpressionKind::Cast { .. }
            | ExpressionKind::Call { .. } => {
                unreachable!("We can't address a {:#?}", self.kind);
            }
        }
    }
}

impl Address for Param<'_> {
    fn address(&self, cg: &mut CodeGenerator<'_, '_, '_>) -> MaplInstruction {
        let addr = cg.address_of(self.id).unwrap();
        MaplInstruction::Pushaddr(addr)
    }
}

impl Address for Item<'_> {
    fn address(&self, cg: &mut CodeGenerator<'_, '_, '_>) -> MaplInstruction {
        match &self.kind {
            ItemKind::Variable { .. } => {
                let addr = cg.address_of(self.id).unwrap();
                MaplInstruction::Pushaddr(addr)
            }
            ItemKind::Function { .. } => {
                let name = cg.get_mangled_symbol(self.id).unwrap();
                MaplInstruction::Call(name)
            }
            ItemKind::Use(u) => {
                let def = u.path.def().expect_resolved();
                let node = cg.hir.get_node(&def).expect_item();
                node.address(cg)
            }
            ItemKind::TypeAlias { .. } |
            ItemKind::Mod(_) |
            ItemKind::Struct { .. } => unreachable!(),
        }
    }
}
