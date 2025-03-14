use ast::expr::{AssignmentExpr, ExpressionKind, VariableExpr};
use ast::Expression;
use super::{Address, Eval};

impl Address for Expression {
    fn address(&self, cg: &mut super::MaplCodeGenerator) {
        match &self.kind {
            ExpressionKind::Variable(variable_expr) => variable_expr.address(cg),
            ExpressionKind::Assignment(assignment_expr) => assignment_expr.address(cg),
            ExpressionKind::Unary(_unary_expr) => todo!(),
            ExpressionKind::Binary(_binary_expr) => todo!(),
            ExpressionKind::Ternary(_ternary_expr) => todo!(),
            ExpressionKind::Literal(_lit_expr) => todo!(),
            ExpressionKind::Call(_call_expr) => todo!(),
        }
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
