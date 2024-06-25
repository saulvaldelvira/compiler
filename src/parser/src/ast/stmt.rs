use builders::{Constructor,Getters,AsBox};
use super::expr::Expr;

pub type Stmt = Box<dyn Statement>;

pub trait Statement {
    fn execute(&self);
}

#[derive(Constructor,Getters,AsBox)]
pub struct ExprAsStmt {
    inner: Expr,
}

impl Statement for ExprAsStmt {
    fn execute(&self) {
        self.inner.print();
        println!(" = {} ({})", self.inner.eval(), self.inner.truthy());
    }
}
