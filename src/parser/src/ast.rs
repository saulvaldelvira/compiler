pub mod expr;
pub use expr::Expr;
pub mod stmt;
pub use stmt::Stmt;

use builders::{Constructor, Getters};

#[derive(Constructor,Getters)]
pub struct Program {
    stmts: Vec<Stmt>,
}
