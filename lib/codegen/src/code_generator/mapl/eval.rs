use ast::expr::{BinaryExpr, BinaryExprKind, BinaryExprOp, ExpressionKind, LitExpr, LitValue};
use ast::Expression;

use super::{Eval, MaplCodeGenerator};

impl Eval for Expression {
    fn eval(&self, cg: &mut MaplCodeGenerator) {
        match &self.kind {
            ExpressionKind::Literal(lit_expr) => lit_expr.eval(cg),
            ExpressionKind::Unary(_unary_expr) => todo!(),
            ExpressionKind::Binary(binary_expr) => binary_expr.eval(cg),
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
            LitValue::Float(f) => cg.pushf(f),
            LitValue::Char(c) => cg.pushb(c as u8),
            LitValue::Bool(b) => cg.pushb( if b { 1 } else { 0 } ),
            LitValue::Str(_) => todo!(),
        }
    }
}

fn eval_arithmetic(expr: &BinaryExpr, cg: &mut MaplCodeGenerator) {
    let t = expr.left.ty.unwrap().arithmetic(&expr.right.ty.unwrap());
    match expr.op {
        BinaryExprOp::Add => cg.add(&t),
        BinaryExprOp::Sub => cg.subs(&t),
        BinaryExprOp::Mul => cg.mul(&t),
        BinaryExprOp::Div => cg.div(&t),
        _ => unreachable!()
    }
}

fn eval_comparison(expr: &BinaryExpr, cg: &mut MaplCodeGenerator) {
    let t = expr.left.ty.unwrap().arithmetic(&expr.right.ty.unwrap());
    let op = match expr.op {
        BinaryExprOp::Gt => "GT",
        BinaryExprOp::Ge => "GE",
        BinaryExprOp::Lt => "LT",
        BinaryExprOp::Le => "LE",
        BinaryExprOp::Eq => "EQ",
        BinaryExprOp::Neq => "NEQ",
        _ => unreachable!()
    };
    cg.sufixed_op(op, &t);
}

fn eval_logical(expr: &BinaryExpr, cg: &mut MaplCodeGenerator) {
    let t = expr.left.ty.unwrap().arithmetic(&expr.right.ty.unwrap());
    let op = match expr.op {
        BinaryExprOp::And => "AND",
        BinaryExprOp::Or => "OR",
        _ => unreachable!()
    };
    cg.sufixed_op(op, &t);
}

impl Eval for BinaryExpr {
    fn eval(&self, cg: &mut MaplCodeGenerator) {
        self.left.eval(cg);
        self.right.eval(cg);
        match self.kind {
            BinaryExprKind::Logical => eval_logical(self, cg),
            BinaryExprKind::Arithmetic => eval_arithmetic(self, cg),
            BinaryExprKind::Comparison => eval_comparison(self, cg),
            BinaryExprKind::Comma => self.right.eval(cg),
        }
    }
}
