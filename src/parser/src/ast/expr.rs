use builders::{Constructor, Getters,AsBox};
use lexer::token::Token;
use crate::Result;

use super::types::{BoolType, NumberType, StringType, Type};

pub type Expr = Box<dyn Expression>;

pub trait Expression {
    fn print(&self);
    fn eval(&self) -> Result<LitValue>;
    fn truthy(&self) -> Result<bool> {
        Ok(self.eval()?.truthy())
    }
    fn get_type(&self) -> Box<dyn Type>;
    fn has_side_effect(&self) -> bool { false }
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
    fn eval(&self) -> Result<LitValue> {
        let expr = self.expr.eval()?;
        let expr = match expr {
            LitValue::Number(n) => n != 0.0,
            LitValue::Bool(b) => b,
            LitValue::Str(_) | LitValue::Nil => return Ok(expr),
        };
        Ok(match self.op.get_lexem() {
            "!" => LitValue::Bool(if expr { false } else { true }),
            _ => panic!("Unreachable")
        })
    }
    fn get_type(&self) -> Box<dyn Type> {
        Box::new(BoolType)
    }
    fn has_side_effect(&self) -> bool {
        self.expr.has_side_effect()
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
    fn eval(&self) -> Result<LitValue> {
        macro_rules! tern  {
            ($cond:expr) => {
                LitValue::Bool(if $cond { true } else { false })
            };
        }

        let left = match self.left.eval()? {
            LitValue::Str(_) => return Err("Can't use a string in a binary expression".into()),
            LitValue::Nil => return Ok(LitValue::Nil),
            LitValue::Number(n) => n,
            LitValue::Bool(b) => b as u8 as f64,
        };
        let right = match self.right.eval()? {
            LitValue::Str(_) => return Err("Can't use a string in a binary expression".into()),
            LitValue::Nil => return Ok(LitValue::Nil),
            LitValue::Number(n) => n,
            LitValue::Bool(b) => b as u8 as f64,
        };
        macro_rules! num {
            ($e:expr) => {
                LitValue::Number($e)
            };
        }
        Ok(match self.op.get_lexem() {
            "*" => num!(left * right),
            "+" => num!(left + right),
            "-" => num!(left - right),
            "/" => num!(left / right),
            ">" => tern!(left > right),
            "<" => tern!(left < right),
            ">=" => tern!(left >= right),
            "<=" => tern!(left <= right),
            "==" => tern!(left == right),
            "!=" => tern!(left != right),
            "," => num!(right),
            _ => unreachable!("Unknown operator")
        })
    }
    fn get_type(&self) -> Box<dyn Type> {
        self.left.get_type().arithmetic(self.right.get_type())
    }
    fn has_side_effect(&self) -> bool {
        self.left.has_side_effect() || self.right.has_side_effect()
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
    fn eval(&self) -> Result<LitValue> {
        if self.cond.eval()?.truthy() {
            self.if_true.eval()
        }else {
            self.if_false.eval()
        }
    }
    fn get_type(&self) -> Box<dyn Type> {
        self.if_true.get_type()
    }
    fn has_side_effect(&self) -> bool {
        self.cond.has_side_effect()
        || self.if_true.has_side_effect()
        || self.if_false.has_side_effect()
    }
}

#[derive(Clone)]
pub enum LitValue {
    Number(f64),
    Str(String),
    Bool(bool),
    Nil
}

impl LitValue {
    fn truthy(&self) -> bool {
        match self {
            LitValue::Number(n) => *n != 0.0,
            LitValue::Bool(b) => *b,
            LitValue::Nil | LitValue::Str(_) => false,
        }
    }
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
    fn eval(&self) -> Result<LitValue> {
        Ok(self.value.clone())
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
