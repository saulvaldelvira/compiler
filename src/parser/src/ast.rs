pub mod expr;
pub use expr::Expr;
pub mod stmt;
pub use stmt::Stmt;
pub mod types;
pub mod declaration;

use builders::{Constructor, Getters};

use crate::visitor::Visitor;

#[derive(Constructor,Getters)]
pub struct Program {
    stmts: Vec<Stmt>,
}
