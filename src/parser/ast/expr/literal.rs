use std::fmt::Display;

use super::Expression;

pub struct Nil;

pub trait LiteralValue {}
impl LiteralValue for Nil {}
impl<T: Display> LiteralValue for T {}

pub struct Literal<T: LiteralValue> {
    value: T,
}

impl<T: LiteralValue> Literal<T> {
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

impl<T: Display> Expression for Literal<T> {
    fn print(&self) {
        print!("{}", self.value);
    }
    fn eval(&self) -> f64 {
        self.value.to_string().parse::<f64>().unwrap()
    }
}

impl Expression for Literal<Nil> {
    fn print(&self) {
        print!("nil");
    }
    fn eval(&self) -> f64 { 0.0 }
    fn truthy(&self) -> bool { false }
}

