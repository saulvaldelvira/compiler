use super::{Expr, Expression};
use crate::lexer::token::Token;

pub struct Binary {
    left: Expr,
    op: Token,
    right: Expr,
}

impl Binary {
    pub fn new(
        left: Expr,
        op: Token,
        right: Expr,
    ) -> Self {
        Self {left,op,right}
    }
    pub fn as_box(self) -> Box<Self> {
        Box::new(self)
    }
}

impl Expression for Binary {
    fn print(&self) {
        print!("(");
        self.left.print();
        print!(" {} ", self.op.get_lexem());
        self.right.print();
        print!(")");
    }
    fn eval(&self) -> f64 {
        macro_rules! tern  {
            ($cond:expr) => {
                if $cond { 1.0 } else { 0.0 }
            };
        }

        let left = self.left.eval();
        let right = self.right.eval();
        match self.op.get_lexem() {
            "*" => left * right,
            "+" => left + right,
            "-" => left - right,
            "/" => left / right,
            ">" => tern!(left > right),
            "<" => tern!(left < right),
            ">=" => tern!(left >= right),
            "<=" => tern!(left <= right),
            "==" => tern!(left == right),
            "!=" => tern!(left != right),
            "," => right,
            _ => panic!("Unreachable")
        }
    }
    fn truthy(&self) -> bool {
        self.eval() != 0.0
    }
}

