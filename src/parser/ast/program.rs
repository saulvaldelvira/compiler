use super::expr::Expr;

pub struct Program {
    expressions: Vec<Expr>,
}

impl Program {
    pub fn new(expressions: Vec<Expr>) -> Self {
        Self {expressions}
    }
    pub fn get_expressions(&self) -> &[Expr] {
        &self.expressions
    }
}