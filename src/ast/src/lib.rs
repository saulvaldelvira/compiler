//! Abstract Syntax Tree
//!
//! This crate contains the AST for the language.

pub mod expr;
use declaration::Declaration;
pub use expr::Expr;
pub mod stmt;
use expr::Expression;
use stmt::Statement;
pub use stmt::Stmt;
pub mod types;
pub mod declaration;
pub mod visitor;
use types::Type;
pub use visitor::Visitor;

use builders::{Constructor, Getters, IntoEnum};

pub enum AST {
    Program(Program),
    Expression(Expression),
    Statement(Statement),
    Declaration(Declaration),
    Type(Type),
}

#[derive(Debug,Constructor,Getters,IntoEnum)]
#[into_enum(enum_name = AST, field = Program)]
pub struct Program {
    stmts: Vec<Stmt>,
}

#[doc(hidden)]
#[macro_export]
macro_rules! __ast {
    ($type:ident : $variant:ident { $( $i:ident $( : $val:expr )?  ),*  } ) => {
        {
            let expr: $type = $variant { $( $i $( : $val  )? ),* } .into();
            expr.as_box()
        }
    };
    ($type:ident : $variant:ident ( $( $e:expr ),* ) ) => {
        {
            let expr: $type = $variant ( $( $e ),* ) .into();
            expr.as_box()
        }
    };
}

pub use __ast as ast;
