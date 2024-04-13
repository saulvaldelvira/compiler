pub mod literal;
pub mod binary;
pub mod unary;
pub mod ternary;

pub use literal::Literal;
pub use unary::Unary;
pub use binary::Binary;
pub use ternary::Ternary;

pub type Expr = Box<dyn Expression>;

pub trait Expression {
    fn print(&self);
    fn eval(&self) -> f64;
}
