use core::panic;

use ast::{expr::{Expression, LitValue, VariableExpr}, Program, Visitor};

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
    fn visit_unary(&mut self, u: &ast::expr::UnaryExpr, p: ()) -> Option<LitValue> {
        Some( match u.op.get_lexem() {
            "!" => match self.visit_expression(&u.expr, p)? {
                LitValue::Number(n) => LitValue::Bool(n != 0.0),
                LitValue::Bool(b) => LitValue::Bool(!b),
                LitValue::Nil => LitValue::Nil,
                LitValue::Str(_) => panic!("Can't use operator on a String"),
            },
            _ => unreachable!()
        })
    }
    fn visit_binary(&mut self, b: &ast::expr::BinaryExpr, p: ()) -> Option<LitValue> {
        let ref left = b.left;
        let ref op = b.op;
        let ref right = b.right;
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
    }
    fn visit_ternary(&mut self, t: &ast::expr::TernaryExpr, p: ()) -> Option<LitValue> {
        if self.visit_expression(&t.cond, p)?.truthy() {
            self.visit_expression(&t.if_true, p)
        }else {
            self.visit_expression(&t.if_false, p)
        }
    }
    fn visit_assignment(&mut self, a: &ast::expr::AssignmentExpr, p: ()) -> Option<LitValue> {
        if !a.left.lvalue() {
            panic!("Trying to assign to non-lvalue: {:#?}", a);
        }
        let right = self.visit_expression(&a.right, p)?;
        match a.left.as_ref() {
            Expression::Variable(VariableExpr{ name }) => {
                let variable = self.enviroment.get(name).expect("Assigning to an undefined variable");
                *variable = right.clone();
            },
            _ => unreachable!(),
        }
        Some(right)
    }
    fn visit_variable_expr(&mut self, _v: &VariableExpr, _p: ()) -> Option<LitValue> {
        self.enviroment.get(&_v.name).cloned()
    }
    fn visit_vardecl(&mut self, v: &ast::declaration::VariableDecl, p: ()) -> Option<LitValue> {
        let init = match &v.init {
            Some(i) => self.visit_expression(&i, p)?,
            None => LitValue::Nil,
        };
        self.enviroment.define(&v.name, init);
        None
    }
    fn visit_print(&mut self, pr: &ast::stmt::PrintStmt, p: ()) -> Option<LitValue> {
        let expr = self.visit_expression(&pr.expr, p).expect("Expected valid expression on print");
        expr.print();
        None
    }
    fn visit_literal(&mut self, _l: &LitValue, _p: ()) -> Option<LitValue> { Some(_l.clone()) }
    fn visit_block(&mut self, b: &ast::stmt::BlockStmt, p: ()) -> Option<LitValue> {
        self.enviroment.set();
        for stmt in &b.stmts {
            self.visit_statement(stmt, p);
        }
        self.enviroment.reset();
        None
    }
}

