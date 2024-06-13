use super::{Expr, Expression};

pub struct Ternary {
    cond: Expr,
    if_true: Expr,
    if_false: Expr,
}

impl Ternary {
    pub fn new(cond: Expr, if_true: Expr, if_false: Expr) -> Self {
        Self {cond, if_true, if_false}
    }
    pub fn as_box(self) -> Box<Self> {
        Box::new(self)
    }
}

impl Expression for Ternary {
    fn print(&self) {
        print!("(");
        self.cond.print();
        print!(" ? ");
        self.if_true.print();
        print!(" : ");
        self.if_false.print();
        print!(")");
    }
    fn eval(&self) -> f64 {
        if self.cond.eval() == 1.0 {
            self.if_true.eval()
        }else {
            self.if_false.eval()
        }
    }
    fn truthy(&self) -> bool {
        self.eval() != 0.0
    }
}
