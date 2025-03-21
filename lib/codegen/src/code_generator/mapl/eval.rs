use std::ops::Deref;

use ast::expr::{ArrayAccess, AssignmentExpr, BinaryExpr, BinaryExprKind, BinaryExprOp, CallExpr, Dereference, ExpressionKind, LitExpr, LitValue, Reference, StructAccess, TernaryExpr, UnaryExpr, UnaryExprOp, VariableExpr};
use ast::types::{ArrayType, Type, TypeKind};
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
            ExpressionKind::ArrayAccess(arr) => arr.eval(cg),
            ExpressionKind::StructAccess(sa) => sa.eval(cg),
            ExpressionKind::Ref(r) => r.eval(cg),
            ExpressionKind::Deref(dr) => dr.eval(cg),
        }
    }
}

impl Eval for Dereference {
    fn eval(&self, cg: &mut MaplCodeGenerator) {
        self.of.eval(cg);
        cg.sufixed_op("LOAD", &self.of.dereference());
    }
}

impl Eval for Reference {
    fn eval(&self, cg: &mut MaplCodeGenerator) {
        self.of.address(cg);
    }
}

impl Eval for StructAccess {
    fn eval(&self, cg: &mut MaplCodeGenerator) {
        self.address(cg);
        let s = self.st.get_type().access_field(self.field);
        cg.sufixed_op("LOAD", &s);
    }
}

impl Eval for ArrayAccess {
    fn eval(&self, cg: &mut MaplCodeGenerator) {
        self.address(cg);
        let ty = self.array.get_type();
        let Type { kind:
            TypeKind::Array(ArrayType { of, ..} )
        } = ty.deref() else { unreachable!() };
        cg.sufixed_op("LOAD", of);
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
                let ty = self.expr.get_type();
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
        cg.sufixed_op("STORE", &self.left.get_type());
        self.left.address(cg);
        cg.sufixed_op("LOAD", &self.left.get_type());
    }
}

impl Eval for VariableExpr {
    fn eval(&self, cg: &mut MaplCodeGenerator) {
        self.address(cg);
        cg.sufixed_op("LOAD", &self.decl.unwrap().ty.as_ref().unwrap());
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
    let t = expr.left.get_type().arithmetic(&expr.right.get_type());
    match expr.op {
        BinaryExprOp::Add => cg.add(&t),
        BinaryExprOp::Sub => cg.subs(&t),
        BinaryExprOp::Mul => cg.mul(&t),
        BinaryExprOp::Div => cg.div(&t),
        BinaryExprOp::Mod => cg.sufixed_op("MOD", &t),
        _ => unreachable!()
    }
}

fn eval_comparison(expr: &BinaryExpr, cg: &mut MaplCodeGenerator) {
    let t = expr.left.get_type().arithmetic(&expr.right.get_type());
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
    let t = expr.left.get_type().arithmetic(&expr.right.get_type());
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
        }
    }
}
