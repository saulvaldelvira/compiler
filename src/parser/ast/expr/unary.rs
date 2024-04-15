use super::{Expr, Expression};
use crate::lexer::token::Token;

pub struct Unary {
    op: Token,
    expr: Expr,
}

impl Unary {
    pub fn new(op: Token, expr: Expr) -> Self {
        Self {op,expr}
    }
    pub fn as_box(self) -> Box<Self> {
        Box::new(self)
    }
}

impl Expression for Unary {
    fn print(&self) {
        print!("({}", self.op.get_lexem());
        self.expr.print();
        print!(")");
    }
    fn eval(&self) -> f64 {
        let expr = self.expr.eval();
        match self.op.get_lexem() {
            "!" => if expr == 1.0 { 0.0 } else { 1.0 }
            _ => panic!("Unreachable")
        }
    }
}
