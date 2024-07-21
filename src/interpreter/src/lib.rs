use ast::{expr::{Expression, LitExpr, LitValue, VariableExpr}, stmt::{ForStmt, WhileStmt}, Program, Visitor};

use self::enviroment::Enviroment;
use ast::Spannable;
mod enviroment;

pub struct Interpreter {
    enviroment: Enviroment,
    ctx: Ctx,
}

impl Interpreter {
    pub fn new() -> Self {
        let ctx = Ctx { inside_loop: false, is_break: false, is_continue: false };
        Self { enviroment: Enviroment::new(), ctx }
    }
    pub fn interpret(&mut self, p: &Program) {
        self.visit_program(p, ());
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

macro_rules! err {
    ($($arg:expr),* ; $( $ast:expr )? ) => {
        panic!("{}: {}", $( $ast.get_span().map(|s| s.to_string()).unwrap_or_else(|| "".to_owned()) )?, format!($($arg),*))
    }
}

#[derive(Clone,Copy)]
struct Ctx {
    inside_loop: bool,
    is_break: bool,
    is_continue: bool,
}

impl Ctx {
    fn enter_loop(&mut self) {
        self.inside_loop = true;
        self.is_break = false;
        self.is_continue = false;
    }
    fn exit_loop(&mut self) {
        self.inside_loop = false;
        self.is_break = false;
        self.is_continue = false;
    }
    fn loop_iter(&mut self) {
        self.is_break = false;
        self.is_continue = false;
    }
    fn must_break_block(&self) -> bool {
        self.is_break || self.is_continue
    }
}

impl Visitor<(),LitValue> for Interpreter {
    fn visit_unary(&mut self, u: &ast::expr::UnaryExpr, p: ()) -> Option<LitValue> {
        Some( match u.op.as_str() {
            "!" => match self.visit_expression(&u.expr, p)? {
                LitValue::Number(n) => LitValue::Bool(n != 0.0),
                LitValue::Bool(b) => LitValue::Bool(!b),
                LitValue::Nil => LitValue::Nil,
                LitValue::Str(_) => err!("Can't use operator on a String" ; &u.expr),
            },
            "-" => LitValue::Number(match self.visit_expression(&u.expr, p)? {
                LitValue::Number(n) => -n,
                LitValue::Bool(b) => (0 - b as u8) as f64,
                LitValue::Nil => return Some(LitValue::Nil),
                LitValue::Str(_) => err!("Can't use operator on a String" ; &u.expr),
            }),
            "+" => self.visit_expression(&u.expr, p)?,
            _ => unreachable!()
        })
    }
    fn visit_binary(&mut self, b: &ast::expr::BinaryExpr, p: ()) -> Option<LitValue> {
        let left = &b.left;
        let op = &b.op;
        let right = &b.right;
        macro_rules! tern  {
            ($cond:expr) => {
                LitValue::Bool(if $cond { true } else { false })
            };
        }
        let left = match self.visit_expression(left, p)? {
            LitValue::Str(_) => err!("Can't use a string in a binary expression" ; left),
            LitValue::Nil => return Some(LitValue::Nil),
            LitValue::Number(n) => n,
            LitValue::Bool(b) => b as u8 as f64,
        };
        let right = match self.visit_expression(right, p)? {
            LitValue::Str(_) => err!("Can't use a string in a binary expression" ; right),
            LitValue::Nil => return Some(LitValue::Nil),
            LitValue::Number(n) => n,
            LitValue::Bool(b) => b as u8 as f64,
        };
        macro_rules! num {
            ($e:expr) => {
                LitValue::Number($e)
            };
        }
        Some(match op.as_str() {
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
    fn visit_if(&mut self, i: &ast::stmt::IfStmt, p: ()) -> Option<LitValue> {
        let cond = self.visit_expression(&i.cond, p).unwrap();
        if cond.truthy() {
            self.visit_statement(&i.if_true, p)
        } else if let Some(if_false) = &i.if_false {
            self.visit_statement(if_false, p)
        } else { None }
    }
    fn visit_while(&mut self, w: &WhileStmt, p: ()) -> Option<LitValue> {
        self.ctx.enter_loop();
        while self.visit_expression(&w.cond, p).unwrap().truthy() {
            self.ctx.loop_iter();

            self.visit_statement(&w.stmts, p);
            if self.ctx.is_break { break }
            if self.ctx.is_continue { continue }
        }
        self.ctx.exit_loop();
        None
    }
    fn visit_for(&mut self, f: &ForStmt, p: ()) -> Option<LitValue> {
        self.ctx.enter_loop();
        if let Some(init) = &f.init { self.visit_vardecl(init, p); }
        loop {
            self.ctx.loop_iter();

            if let Some(cond) = &f.cond {
                match self.visit_expression(cond, p) {
                    Some(s) => if !s.truthy() { break },
                    None => err!("Expression must evaluate" ; cond),
                }
            }
            self.visit_statement(&f.body, p);
            if let Some(inc) = &f.inc { self.visit_expression(inc, p); }
            if self.ctx.is_break { break }
            if self.ctx.is_continue { continue }
        }
        self.ctx.exit_loop();
        None
    }
    fn visit_break_stmt(&mut self, b: &ast::stmt::BreakStmt, _p: ()) -> Option<LitValue> {
        if !self.ctx.inside_loop {
            err!("Break statement outside loop" ; b);
        }
        self.ctx.is_break = true;
        None
    }
    fn visit_continue_stmt(&mut self, c: &ast::stmt::ContinueStmt, _p: ()) -> Option<LitValue> {
        if !self.ctx.inside_loop {
            err!("Continue statement outside loop" ; c);
        }
        self.ctx.is_continue = true;
        None
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
            err!("Trying to assign to non-lvalue: {:#?}" , a.left ; a);
        }
        let right = self.visit_expression(&a.right, p)?;
        match a.left.as_ref() {
            Expression::Variable(VariableExpr{ name, .. }) => {
                if self.enviroment.is_const(name) {
                    err!("Assignment to const variable \"{name}\""; &a.left);
                }
                let variable = self.enviroment
                                   .get_val(name)
                                   .unwrap_or_else(|| err!("Assignment to an undefined variable \"{name}\"" ; &a.left));
                *variable = right.clone();
            },
            _ => unreachable!(),
        }
        Some(right)
    }
    fn visit_variable_expr(&mut self, _v: &VariableExpr, _p: ()) -> Option<LitValue> {
        self.enviroment.get_val(&_v.name).cloned()
    }
    fn visit_vardecl(&mut self, v: &ast::declaration::VariableDecl, p: ()) -> Option<LitValue> {
        if v.is_const && v.init.is_none() {
            err!("Const variable declaration needs to be initialized" ; v);
        }
        let init = match &v.init {
            Some(i) => self.visit_expression(i, p)?,
            None => LitValue::Nil,
        };
        self.enviroment.define(&v.name, init, v);
        None
    }
    fn visit_print(&mut self, pr: &ast::stmt::PrintStmt, p: ()) -> Option<LitValue> {
        let expr = self.visit_expression(&pr.expr, p).expect("Expected valid expression on print");
        expr.print();
        None
    }
    fn visit_literal(&mut self, _l: &LitExpr, _p: ()) -> Option<LitValue> { Some(_l.value.clone()) }
    fn visit_block(&mut self, b: &ast::stmt::BlockStmt, p: ()) -> Option<LitValue> {
        self.enviroment.set();
        for stmt in &b.stmts {
            self.visit_statement(stmt, p);
            if self.ctx.must_break_block() { break }
        }
        self.enviroment.reset();
        None
    }
}

