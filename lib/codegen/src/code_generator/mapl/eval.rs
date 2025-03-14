use ast::expr::{ExpressionKind, LitExpr, LitValue};
use ast::Expression;

use super::{Eval, MaplCodeGenerator};

impl Eval for Expression {
    fn eval(&self, cg: &mut MaplCodeGenerator) {
        match &self.kind {
            ExpressionKind::Literal(lit_expr) => lit_expr.eval(cg),
            ExpressionKind::Unary(_unary_expr) => todo!(),
            ExpressionKind::Binary(_binary_expr) => todo!(),
            ExpressionKind::Ternary(_ternary_expr) => todo!(),
            ExpressionKind::Assignment(_assignment_expr) => todo!(),
            ExpressionKind::Variable(_variable_expr) => todo!(),
            ExpressionKind::Call(_call_expr) => todo!(),
        }
    }
}

impl Eval for LitExpr {
    fn eval(&self, cg: &mut MaplCodeGenerator) {
        match self.value {
            LitValue::Int(n) => cg.pushi(n),
            LitValue::Float(_) => todo!(),
            LitValue::Str(_) => todo!(),
            LitValue::Bool(_) => todo!(),
            LitValue::Char(_) => todo!(),
        }
    }
}

