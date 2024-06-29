use builders::AsBox;

use crate::Result;

use super::{declaration::Declaration, expr::Expr};

pub type Stmt = Box<Statement>;

#[derive(AsBox)]
pub enum Statement {
    ExprAsStmt(Expr),
    Print(Expr),
    Declaration(Declaration)
}

impl Statement {
    pub fn execute(&self) -> Result<()> {
        match self {
            Statement::ExprAsStmt(e) => {
                if e.has_side_effect() {
                    e.eval()?;
                }
            },
            Statement::Print(e) => e.eval()?.print(),
            Statement::Declaration(_) => todo!(),
        }
        Ok(())
    }
}
