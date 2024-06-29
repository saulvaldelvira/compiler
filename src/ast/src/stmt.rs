use builders::AsBox;

use super::{declaration::Declaration, expr::Expr};

pub type Stmt = Box<Statement>;

#[derive(AsBox)]
pub enum Statement {
    ExprAsStmt(Expr),
    Print(Expr),
    Declaration(Declaration)
}
