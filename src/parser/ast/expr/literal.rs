use std::fmt::Display;

use super::Expr;

pub struct Nil;

impl Display for Nil {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "nil")
    }
}

pub struct Literal<T>
where
    T: Display
{
    value: T,
}

impl<T> Literal<T>
where
    T: Display
{
    pub fn new(value: T) -> Self {
        Self {value}
    }
    pub fn as_box(self) -> Box<Self> {
        Box::new(self)
    }
}

impl Literal<Nil> {
    pub fn nil() -> Self {
        Self {value:Nil{}}
    }
}

impl<T> Expr for Literal<T>
where
    T: Display
{
    fn print(&self) {
        print!("{}", self.value);
    }
}
