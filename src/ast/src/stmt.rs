use builders::{AsBox, IntoEnum};

use super::{declaration::Declaration, expr::Expr};

pub type Stmt = Box<Statement>;

#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Statement, field = Expression)]
pub struct ExprAsStmt {
    pub expr: Expr,
}

#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Statement, field = Print)]
pub struct PrintStmt {
    pub expr: Expr,
}

#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Statement, field = Decl)]
pub struct DeclarationStmt {
    pub inner: Declaration,
}

#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Statement, field = Block)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug,AsBox)]
pub enum Statement {
    Expression(ExprAsStmt),
    Print(PrintStmt),
    Decl(DeclarationStmt),
    Block(BlockStmt)
}

#[doc(hidden)]
#[macro_export]
macro_rules! __stmt {
    ($e:expr) => {
        {
            let stmt: Statement = $e.into();
            stmt.as_box()
        }
    };
}

pub use __stmt as stmt;
