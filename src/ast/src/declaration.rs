use builders::{AsBox, IntoEnum};
use super::Expr;

#[derive(Debug,IntoEnum)]
#[into_enum(enum_name = Declaration, field = Variable)]
pub struct VariableDecl {
    pub name: String,
    pub init: Option<Expr>
}

#[derive(Debug,AsBox)]
pub enum Declaration {
    Variable(VariableDecl),
}

#[doc(hidden)]
#[macro_export]
macro_rules! __decl {
    ($e:expr) => {
        {
            let d: Declaration = $e.into();
            d.as_box()
        }
    };
}

pub use __decl as decl;
