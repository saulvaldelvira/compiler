use crate::{declaration::{Declaration, VariableDecl}, expr::{AssignmentExpr, BinaryExpr, Expression, LitExpr, TernaryExpr, UnaryExpr, VariableExpr}, stmt::{BlockStmt, DeclarationStmt, ExprAsStmt, IfStmt, PrintStmt, Statement, WhileStmt}, Program};

pub trait Visitor<P: Copy,R> {
    fn visit_unary(&mut self, u: &UnaryExpr, p: P) -> Option<R> { self.visit_expression(&u.expr, p) }
    fn visit_binary(&mut self, b: &BinaryExpr, p: P) -> Option<R> {
        self.visit_expression(&b.left, p);
        self.visit_expression(&b.right, p);
        None
    }
    fn visit_ternary(&mut self, t: &TernaryExpr, p: P) -> Option<R> {
        self.visit_expression(&t.cond, p);
        self.visit_expression(&t.if_true, p);
        self.visit_expression(&t.if_false, p);
        None
    }
    fn visit_assignment(&mut self, a: &AssignmentExpr, p: P) -> Option<R> {
        self.visit_expression(&a.left, p);
        self.visit_expression(&a.right, p);
        None
    }
    fn visit_variable_expr(&mut self, _v: &VariableExpr, _p: P) -> Option<R> { None }
    fn visit_literal(&mut self, _l: &LitExpr, _p: P) -> Option<R> { None }

    fn visit_expression(&mut self, a: &Expression, p: P) -> Option<R> {
        match a {
            Expression::Unary(u) => self.visit_unary(u, p),
            Expression::Binary(b) => self.visit_binary(b, p),
            Expression::Ternary(t) => self.visit_ternary(t, p),
            Expression::Assignment(a) => self.visit_assignment(a, p),
            Expression::Variable(v) => self.visit_variable_expr(v, p),
            Expression::Literal(l) => self.visit_literal(l, p),
        }
    }

    fn visit_vardecl(&mut self, v: &VariableDecl, p: P) -> Option<R> {
        if let Some(ref init) = v.init {
            self.visit_expression(init, p);
        }
        None
    }
    fn visit_expr_as_stmt(&mut self, s: &ExprAsStmt, p: P) -> Option<R> {
        self.visit_expression(&s.expr, p);
        None
    }
    fn visit_print(&mut self, pr: &PrintStmt, p: P) -> Option<R> { self.visit_expression(&pr.expr, p); None  }
    fn visit_decl_stmt(&mut self, d: &DeclarationStmt, p: P) -> Option<R> { self.visit_declaration(&d.inner, p); None  }
    fn visit_block(&mut self, b: &BlockStmt, p: P) -> Option<R> {
        for stmt in &b.stmts {
            self.visit_statement(stmt,p);
        }
        None
    }
    fn visit_if(&mut self, i: &IfStmt, p: P) -> Option<R> {
        self.visit_expression(&i.cond, p);
        self.visit_statement(&i.if_true, p);
        if let Some(if_false) = &i.if_false {
            self.visit_statement(if_false, p);
        }
        None
    }
    fn visit_while(&mut self, w: &WhileStmt, p: P) -> Option<R> {
        self.visit_expression(&w.cond, p);
        self.visit_statement(&w.stmts, p);
        None
    }
    fn visit_statement(&mut self, s: &Statement, p: P) -> Option<R> {
        match s {
            Statement::Expression(e) => self.visit_expr_as_stmt(e, p),
            Statement::Print(e) => self.visit_print(e, p),
            Statement::Decl(d) => self.visit_decl_stmt(d, p),
            Statement::Block(b) => self.visit_block(b, p),
            Statement::If(i) => self.visit_if(i, p),
            Statement::While(w) => self.visit_while(w, p),
        }
    }
    fn visit_declaration(&mut self, d: &Declaration, p: P) -> Option<R> {
        match d {
            Declaration::Variable(v) => self.visit_vardecl(v, p)
        }
    }
    fn visit_program(&mut self, prog: &Program, p: P) -> Option<R> {
        prog.get_stmts().iter().for_each(|stmt| { self.visit_statement(stmt, p); });
        None
    }
}
