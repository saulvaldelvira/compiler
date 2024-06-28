use builders::{Constructor, Getters,AsBox};
use lexer::token::Token;

use super::types::{BoolType, NumberType, StringType, Type};

pub type Expr = Box<dyn Expression>;

pub trait Expression {
    fn print(&self);
    fn eval(&self) -> f64;
    fn truthy(&self) -> bool { false }
    fn get_type(&self) -> Box<dyn Type>;
}

#[derive(Constructor,Getters,AsBox)]
pub struct Unary {
    op: Token,
    expr: Expr,
}

impl Expression for Unary {
    fn print(&self) {
        print!("({}", self.op.get_lexem());
        self.expr.print();
        print!(")");
    }
    fn eval(&self) -> f64 {
        let expr = self.expr.eval();
        match self.op.get_lexem() {
            "!" => if expr == 1.0 { 0.0 } else { 1.0 }
            _ => panic!("Unreachable")
        }
    }
    fn get_type(&self) -> Box<dyn Type> {
        Box::new(BoolType)
    }
}

#[derive(Constructor,Getters,AsBox)]
pub struct Binary {
    left: Expr,
    op: Token,
    right: Expr,
}

impl Expression for Binary {
    fn print(&self) {
        print!("(");
        self.left.print();
        print!(" {} ", self.op.get_lexem());
        self.right.print();
        print!(")");
    }
    fn eval(&self) -> f64 {
        macro_rules! tern  {
            ($cond:expr) => {
                if $cond { 1.0 } else { 0.0 }
            };
        }

        let left = self.left.eval();
        let right = self.right.eval();
        match self.op.get_lexem() {
            "*" => left * right,
            "+" => left + right,
            "-" => left - right,
            "/" => left / right,
            ">" => tern!(left > right),
            "<" => tern!(left < right),
            ">=" => tern!(left >= right),
            "<=" => tern!(left <= right),
            "==" => tern!(left == right),
            "!=" => tern!(left != right),
            "," => right,
            _ => unreachable!("Unknown operator")
        }
    }
    fn truthy(&self) -> bool {
        self.eval() != 0.0
    }
    fn get_type(&self) -> Box<dyn Type> {
        self.left.get_type().arithmetic(self.right.get_type())
    }
}

#[derive(Constructor,Getters,AsBox)]
pub struct Ternary {
    cond: Expr,
    if_true: Expr,
    if_false: Expr,
}

impl Expression for Ternary {
    fn print(&self) {
        print!("(");
        self.cond.print();
        print!(" ? ");
        self.if_true.print();
        print!(" : ");
        self.if_false.print();
        print!(")");
    }
    fn eval(&self) -> f64 {
        if self.cond.eval() == 1.0 {
            self.if_true.eval()
        }else {
            self.if_false.eval()
        }
    }
    fn truthy(&self) -> bool {
        self.eval() != 0.0
    }
    fn get_type(&self) -> Box<dyn Type> {
        self.if_true.get_type()
    }
}

pub enum LitValue {
    Number(f64),
    Str(String),
    Bool(bool),
    Nil
}

#[derive(AsBox,Constructor)]
pub struct Literal {
    value: LitValue,
}

impl Expression for Literal {
    fn print(&self) {
        match &self.value {
            LitValue::Nil => print!("nil"),
            LitValue::Str(s) => print!("{s}"),
            LitValue::Bool(b) => print!("{b}"),
            LitValue::Number(n) => print!("{n}"),
        }
    }
    fn eval(&self) -> f64 {
        match &self.value {
            LitValue::Nil => 0.0,
            LitValue::Bool(b) => if *b { 1.0 }  else { 0.0 },
            LitValue::Number(n) => *n,
            LitValue::Str(_) => unreachable!(),
        }
    }
    fn truthy(&self) -> bool {
        if let LitValue::Nil = self.value {
            false
        } else {
            Expression::truthy(self)
        }
    }
    fn get_type(&self) -> Box<dyn Type> {
        match self.value {
            LitValue::Number(_) => Box::new(NumberType),
            LitValue::Str(_) => Box::new(StringType),
            LitValue::Bool(_) => Box::new(BoolType),
            LitValue::Nil => Box::new(NumberType),
        }
    }
}
