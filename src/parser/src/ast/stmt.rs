use builders::{Constructor,Getters,AsBox};
use crate::Result;

use super::expr::{Expr, LitValue};

pub type Stmt = Box<dyn Statement>;

pub trait Statement {
    fn execute(&self) -> Result<()>;
}

#[derive(Constructor,Getters,AsBox)]
pub struct ExprAsStmt {
    inner: Expr,
}

impl Statement for ExprAsStmt {
    fn execute(&self) -> Result<()> {
        if self.inner.has_side_effect() {
            self.inner.eval()?;
        }
        Ok(())
    }
}

#[derive(Constructor,Getters,AsBox)]
pub struct Print {
    expr: Expr
}

impl Statement for Print {
    fn execute(&self) -> Result<()> {
        match self.expr.eval()? {
            LitValue::Number(n) => print!("{n}"),
            LitValue::Str(s) => {
                let s = s.strip_prefix("\"").ok_or("Can't strip prefix of string literal")?;
                let s = s.strip_suffix("\"").ok_or("Can't strip suffix of string literal")?;
                let s = s.replace("\\n", "\n");
                print!("{s}");
            },
            LitValue::Bool(b) => print!("{b}"),
            LitValue::Nil => print!("nil")
        }
        Ok(())
    }
}
