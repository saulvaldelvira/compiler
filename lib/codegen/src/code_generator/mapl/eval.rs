use ast::expr::{AssignmentExpr, BinaryExpr, BinaryExprKind, BinaryExprOp, CallExpr, ExpressionKind, LitExpr, LitValue, TernaryExpr, UnaryExpr, UnaryExprOp, VariableExpr};
use ast::Expression;
use session::with_symbol;

use super::{get_type_suffix, Address, Eval, MaplCodeGenerator};

impl Eval for Expression {
    fn eval(&self, cg: &mut MaplCodeGenerator) {
        match &self.kind {
            ExpressionKind::Literal(lit_expr) => lit_expr.eval(cg),
            ExpressionKind::Unary(unary_expr) => unary_expr.eval(cg),
            ExpressionKind::Binary(binary_expr) => binary_expr.eval(cg),
            ExpressionKind::Ternary(ternary_expr) => ternary_expr.eval(cg),
            ExpressionKind::Assignment(assignment_expr) => assignment_expr.eval(cg),
            ExpressionKind::Variable(variable_expr) => variable_expr.eval(cg),
            ExpressionKind::Call(call_expr) => call_expr.eval(cg),
        }
    }
}

impl Eval for TernaryExpr {
    fn eval(&self, cg: &mut MaplCodeGenerator) {
        let else_label = cg.anon_label();
        let end_label = cg.anon_label();

        self.cond.eval(cg);
        cg.base.write_fmt(format_args!("JZ {else_label}"));
        self.if_true.eval(cg);
        cg.base.write_fmt(format_args!("JMP {end_label}"));
        cg.base.write_fmt(format_args!("{else_label}:"));
        self.if_false.eval(cg);
        cg.base.write_fmt(format_args!("{end_label}:"));
    }
}

impl Eval for UnaryExpr {
    fn eval(&self, cg: &mut MaplCodeGenerator) {
        self.expr.eval(cg);
        match self.op {
            UnaryExprOp::Negation => {
                let ty = self.expr.ty.unwrap();
                let ty = get_type_suffix(&ty);
                cg.base.write_fmt(format_args!("PUSH{ty} -1\nMUL{ty}"));
            },
            UnaryExprOp::Plus => {},
            UnaryExprOp::Not => cg.base.write("NOT"),
        }
    }
}

impl Eval for CallExpr {
    fn eval(&self, cg: &mut MaplCodeGenerator) {
        for param in &self.args {
            param.eval(cg);
        }
        with_symbol(self.callee, |s| {
            cg.base.write_fmt(format_args!("call {s}"));
        })
    }
}

impl Eval for AssignmentExpr {
    fn eval(&self, cg: &mut MaplCodeGenerator) {
        self.left.address(cg);
        self.right.eval(cg);
        cg.sufixed_op("STORE", &self.left.ty.unwrap());
        self.left.address(cg);
        cg.sufixed_op("LOAD", &self.left.ty.unwrap());
    }
}

impl Eval for VariableExpr {
    fn eval(&self, cg: &mut MaplCodeGenerator) {
        self.address(cg);
        cg.sufixed_op("LOAD", &self.decl.unwrap().ty.unwrap());
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
