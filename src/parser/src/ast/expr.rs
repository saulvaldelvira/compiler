use builders::AsBox;
use lexer::token::Token;
use crate::Result;

use super::types::Type;

pub type Expr = Box<Expression>;

#[derive(AsBox)]
pub enum Expression {
    Unary {
        op: Token,
        expr: Expr
    },
    Binary {
        left: Expr,
        op: Token,
        right: Expr,
    },
    Ternary {
        cond: Expr,
        if_true: Expr,
        if_false: Expr,
    },
    Literal(LitValue),
}

impl Expression {
    pub fn print(&self) {
        print!("(");
        match self {
            Expression::Unary { op, expr } => {
                print!("{}", op.get_lexem());
                expr.print();
            },
            Expression::Binary { left, op, right } => {
                left.print();
                print!("{}", op.get_lexem());
                right.print();
            },
            Expression::Ternary { cond, if_true, if_false } => {
                cond.print();
                print!(" ? ");
                if_true.print();
                print!(":");
                if_false.print();
            },
            Expression::Literal(value) => {
                value.print();
            }
        }
        print!(")");
    }
    pub fn eval(&self) -> Result<LitValue> {
        match self {
            Expression::Unary { op, expr } => {
                let val = expr.eval()?;
                Ok(match val {
                    LitValue::Number(n) => LitValue::Bool(n != 0.0),
                    LitValue::Bool(b) => LitValue::Bool(!b),
                    LitValue::Nil => LitValue::Nil,
                    LitValue::Str(_) => return Err("Can't use operator on a String".into()),
                })
            },
            Expression::Binary { left, op, right } => {
                macro_rules! tern  {
                    ($cond:expr) => {
                        LitValue::Bool(if $cond { true } else { false })
                    };
                }
                let left = match left.eval()? {
                    LitValue::Str(_) => return Err("Can't use a string in a binary expression".into()),
                    LitValue::Nil => return Ok(LitValue::Nil),
                    LitValue::Number(n) => n,
                    LitValue::Bool(b) => b as u8 as f64,
                };
                let right = match right.eval()? {
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
                Ok(match op.get_lexem() {
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
            },
            Expression::Ternary { cond, if_true, if_false } => {
                if cond.eval()?.truthy() {
                    if_true.eval()
                }else {
                    if_false.eval()
                }
            },
            Expression::Literal(value) => {
                Ok(value.clone())
            },
        }
    }
    pub fn truthy(&self) -> Result<bool> {
        Ok(self.eval()?.truthy())
    }
    pub fn get_type(&self) -> Type {
        match self {
            Expression::Unary { op, expr } => Type::Bool,
            Expression::Binary { left, op, right } => {
                left.get_type().arithmetic(right.get_type())
            },
            Expression::Ternary { cond, if_true, if_false } => {
                if_true.get_type()
            },
            Expression::Literal(value) => self.get_type(),
        }
    }
    pub fn has_side_effect(&self) -> bool {
        match self {
            Expression::Unary { op, expr } => expr.has_side_effect(),
            Expression::Binary { left, op, right } => left.has_side_effect() || right.has_side_effect(),
            Expression::Ternary { cond, if_true, if_false } =>
                cond.has_side_effect() || if_true.has_side_effect() || if_false.has_side_effect(),
            Expression::Literal(_) => false,
        }
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
    pub fn truthy(&self) -> bool {
        match self {
            LitValue::Number(n) => *n != 0.0,
            LitValue::Bool(b) => *b,
            LitValue::Nil | LitValue::Str(_) => false,
        }
    }
    pub fn print(&self) {
        match self {
            LitValue::Number(n) => print!("{n}"),
            LitValue::Str(s) => print!("{s}"),
            LitValue::Bool(b) => print!("{b}"),
            LitValue::Nil => print!("nil"),
        }
    }
}
