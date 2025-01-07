use core::panic;
use std::ops::ControlFlow;
use std::rc::Rc;

use ast::declaration::{DeclarationKind, VariableDecl};
use ast::expr::CallExpr;
use ast::visitor::{walk_call, VisitorResult};
use ast::AstRef;
use ast::{expr::{ExpressionKind, LitExpr, LitValue, VariableExpr}, stmt::{ForStmt, WhileStmt}, Program, Visitor};
use session::with_symbol;

use self::enviroment::Enviroment;
mod enviroment;

pub struct Interpreter {
    enviroment: Enviroment,
    ctx: Ctx,
}

impl Interpreter {
    pub fn new() -> Self {
        let ctx = Ctx {
            inside_loop: false,
            is_break: false,
            is_continue: false,
            values: Vec::new()
        };
        Self { enviroment: Enviroment::new(), ctx }
    }
    pub fn interpret(&mut self, p: &Program) {
        self.visit_program(p);
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

macro_rules! err {
    ($($arg:expr),* ; $ast:expr ) => {
        {
             {
                 panic!("{}: {}", $ast, format!($($arg),*))
             }
        }
    };
    ($($arg:expr),* ) => {
        {
             {
                 panic!("{}", format!($($arg),*))
             }
        }
    }
}

struct Ctx {
    inside_loop: bool,
    is_break: bool,
    is_continue: bool,
    values: Vec<LitValue>,
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
    fn pop_val(&mut self) -> LitValue {
        self.values.pop().unwrap()
    }
}

impl Visitor<'_> for Interpreter {
    type Result = ControlFlow<()>;

    fn visit_program(&mut self, prog: &'_ Program) -> Self::Result {

        for decl in &prog.decls {
            if let DeclarationKind::Function(f) = &decl.kind {
                if with_symbol(f.name, |name| { name == "main" }) {
                    let callee = f.name;
                    let main = CallExpr {
                        decl: AstRef::from(Rc::clone(f)),
                        args: Vec::new().into_boxed_slice(),
                        callee
                    };
                    self.visit_call(&main)?;
                }
            }
        }

        Self::Result::output()
    }

    fn visit_unary(&mut self, u: &ast::expr::UnaryExpr) -> Self::Result {
        self.visit_expression(&u.expr)?;
        let val = self.ctx.pop_val();

        let val = with_symbol(u.op, |op| {
            match op {
                "!" => match val {
                    LitValue::Number(n) => LitValue::Bool(n != 0.0),
                    LitValue::Char(c) => LitValue::Bool(c as u32 != 0),
                    LitValue::Bool(b) => LitValue::Bool(!b),
                    LitValue::Nil => LitValue::Nil,
                    LitValue::Str(_) => err!("Can't use operator on a String" ; u.expr.span),
                },
                "-" => match val {
                    LitValue::Number(n) => LitValue::Number(-n),
                    LitValue::Char(c) => LitValue::Number(-(c as i32) as f64),
                    LitValue::Bool(b) => LitValue::Number((0 - b as u8) as f64),
                    LitValue::Nil => LitValue::Nil,
                    LitValue::Str(_) => err!("Can't use operator on a String" ; &u.expr.span),
                },
                "+" => val,
                _ => unreachable!()
            }
        });

        self.ctx.values.push(val);

        Self::Result::output()
    }
    fn visit_binary(&mut self, b: &ast::expr::BinaryExpr) -> Self::Result {
        self.visit_expression(&b.right);
        self.visit_expression(&b.left);
        macro_rules! tern  {
            ($cond:expr) => {
                LitValue::Bool(if $cond { true } else { false })
            };
        }
        let left_val = self.ctx.pop_val();
        let right_val = self.ctx.pop_val();

        let left = match left_val {
            LitValue::Str(_) => err!("Can't use a string in a binary expression" ; b.left.span),
            LitValue::Nil => {
                self.ctx.values.push(LitValue::Nil);
                return Self::Result::output()
            },
            LitValue::Number(n) => n,
            LitValue::Bool(b) => b as u8 as f64,
            LitValue::Char(c) => c as u32 as f64,
        };
        let right = match right_val {
            LitValue::Str(_) => err!("Can't use a string in a binary expression" ; b.right.span),
            LitValue::Nil => {
                self.ctx.values.push(LitValue::Nil);
                return Self::Result::output()
            },
            LitValue::Number(n) => n,
            LitValue::Bool(b) => b as u8 as f64,
            LitValue::Char(c) => c as u32 as f64,
        };
        macro_rules! num {
            ($e:expr) => {
                LitValue::Number($e)
            };
        }
        let res = with_symbol(b.op, |op| {
            match op {
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
            }
        });

        self.ctx.values.push(res);

        Self::Result::output()
    }
    fn visit_if(&mut self, i: &ast::stmt::IfStmt) -> Self::Result {
        self.visit_expression(&i.cond);
        let cond = self.ctx.pop_val();

        if cond.truthy() {
            self.visit_statement(&i.if_true)
        } else if let Some(if_false) = &i.if_false {
            self.visit_statement(if_false)
        } else { Self::Result::output() }
    }
    fn visit_while(&mut self, w: &WhileStmt) -> Self::Result {
        self.ctx.enter_loop();
        loop {
            self.visit_expression(&w.cond)?;
            if !self.ctx.pop_val().truthy() {
                break
            }

            self.ctx.loop_iter();

            self.visit_statement(&w.stmts);
            if self.ctx.is_break { break }
            if self.ctx.is_continue { continue }
        }
        self.ctx.exit_loop();
        Self::Result::output()
    }
    fn visit_for(&mut self, f: &ForStmt) -> Self::Result {
        self.ctx.enter_loop();
        if let Some(init) = &f.init { self.visit_declaration(init); }
        loop {
            self.ctx.loop_iter();

            if let Some(cond) = &f.cond {
                self.visit_expression(cond)?;
                if !self.ctx.pop_val().truthy() {
                    break
                }
            }
            self.visit_statement(&f.body);
            if let Some(inc) = &f.inc { self.visit_expression(inc); }
            if self.ctx.is_break { break }
            if self.ctx.is_continue { continue }
        }
        self.ctx.exit_loop();

        Self::Result::output()
    }
    fn visit_break_stmt(&mut self, _b: &ast::stmt::BreakStmt) -> Self::Result {
        if !self.ctx.inside_loop {
            err!("Break statement outside loop");
        }
        self.ctx.is_break = true;
        Self::Result::output()
    }
    fn visit_continue_stmt(&mut self, _c: &ast::stmt::ContinueStmt) -> Self::Result {
        if !self.ctx.inside_loop {
            err!("Continue statement outside loop");
        }
        self.ctx.is_continue = true;
        Self::Result::output()
    }
    fn visit_ternary(&mut self, t: &ast::expr::TernaryExpr) -> Self::Result {
        self.visit_expression(&t.cond)?;
        if self.ctx.pop_val().truthy() {
            self.visit_expression(&t.if_true)
        }else {
            self.visit_expression(&t.if_false)
        }
    }
    fn visit_assignment(&mut self, a: &ast::expr::AssignmentExpr) -> Self::Result {
        if !a.left.lvalue() {
            err!("Trying to assign to non-lvalue: {:#?}" , a.left ; a.left.span);
        }
        self.visit_expression(&a.right)?;
        let right = self.ctx.values.last().unwrap();
        match &a.left.as_ref().kind {
            ExpressionKind::Variable(VariableExpr{ name, .. }) => {
                if self.enviroment.is_const(name) {
                    with_symbol(*name, |name| {
                        err!("Assignment to const variable \"{name}\""; a.left.span);
                    });
                }
                let variable = self.enviroment
                                   .get_val(name)
                                   .unwrap_or_else(|| {
                                        with_symbol(*name, |name| {
                                            err!("Assignment to an undefined variable \"{name}\"" ; a.left.span)
                                        })
                                    });
                *variable = right.clone();
            },
            _ => unreachable!(),
        };

        Self::Result::output()
    }
    fn visit_variable_expr(&mut self, _v: &VariableExpr) -> Self::Result {
        let val = self.enviroment.get_val(&_v.name).cloned().unwrap_or_else(|| {
            panic!("Use of unexisting variable");
        });
        self.ctx.values.push(val);

        Self::Result::output()
    }
    fn visit_vardecl(&mut self, v: &Rc<VariableDecl>) -> Self::Result {
        if v.is_const && v.init.is_none() {
            err!("Const variable declaration needs to be initialized");
        }
        let init = match &v.init {
            Some(i) => {
                self.visit_expression(i)?;
                self.ctx.pop_val()
            },
            None => LitValue::Nil,
        };
        self.enviroment.define_var(v.name, init, v);
        Self::Result::output()
    }
    fn visit_print(&mut self, pr: &ast::stmt::PrintStmt) -> Self::Result {
        self.visit_expression(&pr.expr)?;
        let expr = self.ctx.pop_val();
        expr.print();
        Self::Result::output()
    }
    fn visit_literal(&mut self, l: &LitExpr) -> Self::Result {
        self.ctx.values.push(l.value.clone());
        Self::Result::output()
    }
    fn visit_block(&mut self, b: &ast::stmt::BlockStmt) -> Self::Result {
        self.enviroment.set();
        for stmt in &b.stmts {
            self.visit_statement(stmt);
            if self.ctx.must_break_block() { break }
        }
        self.enviroment.reset();
        Self::Result::output()
    }
    fn visit_call(&mut self, call: &CallExpr) -> Self::Result {
        self.enviroment.set();

        walk_call(self, call);

        call.decl.with(|func| {
            self.visit_block(&func.body);
        });

        self.enviroment.reset();
        Self::Result::output()
    }
}

