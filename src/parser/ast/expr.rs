pub mod binary;
pub mod literal;
pub mod unary;

pub use binary::Binary;
pub use literal::Literal;
pub use unary::Unary;

pub trait Expr {
    fn print(&self);
}
