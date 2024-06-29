use crate::ast::{declaration::Declaration, expr::Expression, stmt::{self, Statement}, Program};

pub trait Visitor<P: Copy,R> {
    fn visit_expression(&mut self, a: &Expression, p: P) -> Option<R> { self.walk_expression(a, p); None  }
    fn visit_statement(&mut self, s: &Statement, p: P) -> Option<R> { self.walk_statement(s, p); None }
    fn visit_declaration(&mut self, d: &Declaration, p: P) -> Option<R> { self.walk_declaration(d, p); None }
    fn visit_program(&mut self, prog: &Program, p: P) -> Option<R> { self.walk_program(prog, p); None }
    fn walk_expression(&mut self, a: &Expression, p: P) {
        match a {
            Expression::Unary { op, expr } => { self.visit_expression(expr, p); },
            Expression::Binary { left, op, right } => {
                self.visit_expression(left, p);
                self.visit_expression(right, p);
            },
            Expression::Ternary { cond, if_true, if_false } => {
                self.visit_expression(cond, p);
                self.visit_expression(if_true, p);
                self.visit_expression(if_false, p);
            },
            Expression::Literal(_) => {},
        };
    }
    fn walk_statement(&mut self, s: &Statement, p: P) {
        match s {
            Statement::ExprAsStmt(e) => { self.visit_expression(e, p);  },
            Statement::Print(e) => { self.visit_expression(e, p); },
            Statement::Declaration(d) => { self.walk_declaration(d, p); },
        }
    }
    fn walk_declaration(&mut self, d: &Declaration, p: P) {
        match d {
            Declaration::VariableDecl { name, init } => {
                if let Some(expr) = init {
                    self.visit_expression(expr, p);
                };
            },
        }
    }
    fn walk_program(&mut self, prog: &Program, p: P) {
        prog.get_stmts().iter().for_each(|stmt| { self.visit_statement(stmt, p); });
    }
}

pub struct Interpreter {

}

impl Interpreter {
    pub fn new() -> Self { Self {} }
    pub fn interpret(&mut self, p: &Program) { self.visit_program(p, ()); }
}

impl Visitor<(),()> for Interpreter {
    fn visit_statement(&mut self, s: &Statement, p: ()) -> Option<()> {
        s.execute().ok()
    }
    fn visit_declaration(&mut self, d: &Declaration, p: ()) -> Option<()> {
        todo!()
    }
}

