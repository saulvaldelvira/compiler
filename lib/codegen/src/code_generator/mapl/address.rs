use std::ops::Deref;

use ast::expr::{ArrayAccess, AssignmentExpr, ExpressionKind, StructAccess, VariableExpr};
use ast::types::{ArrayType, Type, TypeKind};
use ast::Expression;
use crate::memory::{MaplSizeStrategy, SizeStrategy};

use super::{Address, Eval, MaplCodeGenerator};

impl Address for Expression {
    fn address(&self, cg: &mut super::MaplCodeGenerator) {
        match &self.kind {
            ExpressionKind::Variable(variable_expr) => variable_expr.address(cg),
            ExpressionKind::Assignment(assignment_expr) => assignment_expr.address(cg),
            ExpressionKind::ArrayAccess(arr) => arr.address(cg),
            ExpressionKind::StructAccess(sa) => sa.address(cg),
            ExpressionKind::Deref(d) => d.of.eval(cg),
            ExpressionKind::Ref(_) => unreachable!(),
            ExpressionKind::Unary(_unary_expr) => unreachable!(),
            ExpressionKind::Binary(_binary_expr) => unreachable!(),
            ExpressionKind::Ternary(_ternary_expr) => unreachable!(),
            ExpressionKind::Literal(_lit_expr) => unreachable!(),
            ExpressionKind::Call(_call_expr) => unreachable!(),
        }
    }
}

impl Address for StructAccess {
    fn address(&self, cg: &mut MaplCodeGenerator) {
        self.st.address(cg);
        let TypeKind::Struct(s) = &self.st.ty.unwrap().kind else { unreachable!() };
        let decl = s.decl.unwrap();
        let field = decl.fields.iter().find(|f| f.name == self.field).unwrap();
        cg.pushaddr(&field.address.unwrap());
        cg.base.write("ADDI");
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
