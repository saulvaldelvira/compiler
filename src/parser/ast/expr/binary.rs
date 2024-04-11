use super::Expr;
use crate::lexer::token::Token;

pub struct Binary {
    left: Box<dyn Expr>,
    op: Token,
    right: Box<dyn Expr>,
}

impl Binary {
    pub fn new(
        left: Box<dyn Expr>,
        op: Token,
        right: Box<dyn Expr>,
    ) -> Self {
        Self {left,op,right}
    }
    pub fn as_box(self) -> Box<Self> {
        Box::new(self)
    }
}

impl Expr for Binary {
    fn print(&self) {
        print!("(");
        self.left.print();
        print!(" {} ", self.op.get_lexem());
        self.right.print();
        print!(")");
    }
}

