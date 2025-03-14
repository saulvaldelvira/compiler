use std::ops::Deref;

use ast::expr::{ArrayAccess, AssignmentExpr, ExpressionKind, VariableExpr};
use ast::types::{ArrayType, Type, TypeKind};
use ast::Expression;
use crate::memory::{MaplSizeStrategy, SizeStrategy};

use super::{Address, Eval};

impl Address for Expression {
    fn address(&self, cg: &mut super::MaplCodeGenerator) {
        match &self.kind {
            ExpressionKind::Variable(variable_expr) => variable_expr.address(cg),
            ExpressionKind::Assignment(assignment_expr) => assignment_expr.address(cg),
            ExpressionKind::ArrayAccess(arr) => arr.address(cg),
            ExpressionKind::Unary(_unary_expr) => todo!(),
            ExpressionKind::Binary(_binary_expr) => todo!(),
            ExpressionKind::Ternary(_ternary_expr) => todo!(),
            ExpressionKind::Literal(_lit_expr) => todo!(),
            ExpressionKind::Call(_call_expr) => todo!(),
        }
    }
}

impl Address for ArrayAccess {
    fn address(&self, cg: &mut super::MaplCodeGenerator) {
        self.array.address(cg);
        self.index.eval(cg);
        let ty = self.array.ty.unwrap();
        let Type { kind:
            TypeKind::Array(ArrayType { of, ..} )
        } = ty.deref() else { unreachable!() };
        cg.base.write_fmt(format_args!("PUSHI {}", MaplSizeStrategy::size_of(of)));
        cg.base.write("MULI");
        cg.base.write("ADDI");
    }
}

impl Address for VariableExpr {
    fn address(&self, cg: &mut super::MaplCodeGenerator) {
        cg.pushaddr(&self.decl.unwrap().address.unwrap());
    }
}

impl Address for AssignmentExpr {
    fn address(&self, cg: &mut super::MaplCodeGenerator) {
        if self.left.has_side_effect() {
            self.left.eval(cg);
            cg.discard_type(&self.left.ty.unwrap());
        }
        self.left.address(cg);
    }
}
