//! Statements
//!
use builders::{AsBox, IntoEnum};
use lexer::{spanned, Spanned};

use crate::AST;

use super::{declaration::Declaration, expr::Expr};

pub type Stmt = Box<Statement>;

/// Wraps an [Expression](crate::expr::Expression) as a statement
#[spanned]
#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Statement, field = Expression)]
pub struct ExprAsStmt {
    pub expr: Expr,
}

#[spanned]
#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Statement, field = Print)]
pub struct PrintStmt {
    pub expr: Expr,
}

#[spanned]
#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Statement, field = Decl)]
pub struct DeclarationStmt {
    pub inner: Declaration,
}

#[spanned]
#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Statement, field = Block)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
}

#[spanned]
#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Statement, field = If)]
pub struct IfStmt {
    pub cond: Expr,
    pub if_true: Stmt,
    pub if_false: Option<Stmt>,
}

#[spanned]
#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Statement, field = While)]
pub struct WhileStmt {
    pub cond: Expr,
    pub stmts: Stmt,
}

#[derive(Debug,AsBox,IntoEnum,Spanned)]
#[into_enum(enum_name = AST)]
pub enum Statement {
    Expression(ExprAsStmt),
    Print(PrintStmt),
    Decl(DeclarationStmt),
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt)
}

#[doc(hidden)]
#[macro_export]
macro_rules! __stmt {
    ($variant:ident { $( $i:ident $( : $val:expr )?  ),*  } ) => {
        $crate::ast!(Statement : $variant { $( $i $( : $val )?  ),* , span: None })
    };
}

pub use __stmt as stmt;
