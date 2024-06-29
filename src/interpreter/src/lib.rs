use ast::{declaration::Declaration, expr::{Expression, LitValue}, stmt::Statement, Program, Visitor};

use self::enviroment::Enviroment;
mod enviroment;

pub struct Interpreter {
    enviroment: Enviroment,
}

impl Interpreter {
    pub fn new() -> Self { Self { enviroment: Enviroment::new() } }
    pub fn interpret(&mut self, p: &Program) { self.visit_program(p, ()); }
}

impl Visitor<(),LitValue> for Interpreter {
    fn visit_statement(&mut self, s: &Statement, _p: ()) -> Option<LitValue> {
        match s {
            Statement::ExprAsStmt(e) => {
                if e.has_side_effect() {
                    self.walk_expression(e, _p);
                }
            },
            Statement::Print(e) => {
                let expr = self.visit_expression(e, _p)?;
                expr.print();
            },
            Statement::Declaration( decl ) => {
                self.visit_declaration(decl, _p);
            }
        }
        None
    }
    fn visit_declaration(&mut self, d: &Declaration, p: ()) -> Option<LitValue> {
        match d {
            Declaration::VariableDecl { name, init } => {
                let init = match init {
                    Some(i) => self.visit_expression(&i, p)?,
                    None => LitValue::Nil,
                };
                self.enviroment.define(name, init);
            },
        }
        None
    }
    fn visit_expression(&mut self, a: &Expression, p: ()) -> Option<LitValue> {
        match a {
            Expression::Unary {op, expr } => {
                Some(match op.get_lexem() {
                    "!" => match self.visit_expression(expr, p)? {
                        LitValue::Number(n) => LitValue::Bool(n != 0.0),
                        LitValue::Bool(b) => LitValue::Bool(!b),
                        LitValue::Nil => LitValue::Nil,
                        LitValue::Str(_) => panic!("Can't use operator on a String"),
                    },
                    _ => unreachable!()
                })
            },
            Expression::Binary { left, op, right } => {
                macro_rules! tern  {
                    ($cond:expr) => {
                        LitValue::Bool(if $cond { true } else { false })
                    };
                }
                let left = match self.visit_expression(left, p)? {
                    LitValue::Str(_) => panic!("Can't use a string in a binary expression"),
                    LitValue::Nil => return Some(LitValue::Nil),
                    LitValue::Number(n) => n,
                    LitValue::Bool(b) => b as u8 as f64,
                };
                let right = match self.visit_expression(right, p)? {
                    LitValue::Str(_) => panic!("Can't use a string in a binary expression"),
                    LitValue::Nil => return Some(LitValue::Nil),
                    LitValue::Number(n) => n,
                    LitValue::Bool(b) => b as u8 as f64,
                };
                macro_rules! num {
                    ($e:expr) => {
                        LitValue::Number($e)
                    };
                }
                Some(match op.get_lexem() {
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
                if self.visit_expression(cond, p)?.truthy() {
                    self.visit_expression(if_true, p)
                }else {
                    self.visit_expression(if_false, p)
                }
            },
            Expression::Literal(value) => {
                Some(value.clone())
            },
            Expression::Variable { .. } => todo!(),
        }
    }
}

