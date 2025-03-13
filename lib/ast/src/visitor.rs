use std::ops::ControlFlow;
use std::rc::Rc;

use crate::declaration::{DeclarationKind, FunctionDecl};
use crate::expr::CallExpr;
use crate::stmt::StatementKind;
use crate::types::Type;
use crate::Expression;
use crate::{declaration::{Declaration, VariableDecl}, expr::{AssignmentExpr, BinaryExpr, ExpressionKind, LitExpr, TernaryExpr, UnaryExpr, VariableExpr}, stmt::{BlockStmt, BreakStmt, ContinueStmt, DeclarationStmt, EmptyStmt, ExprAsStmt, ForStmt, IfStmt, PrintStmt, Statement, WhileStmt}, Program};

pub trait Visitor<'ast> : Sized {
    type Param: Copy;
    type Result: VisitorResult;

    fn visit_unary(&mut self, u: &'ast UnaryExpr, p: Self::Param) -> Self::Result { self.visit_expression(&u.expr, p) }
    fn visit_binary(&mut self, b: &'ast BinaryExpr, p: Self::Param) -> Self::Result {
        walk_binary(self, b, p)
    }
    fn visit_ternary(&mut self, t: &'ast TernaryExpr, p: Self::Param) -> Self::Result {
        self.visit_expression(&t.cond, p);
        self.visit_expression(&t.if_true, p);
        self.visit_expression(&t.if_false, p);
        Self::Result::output()
    }
    fn visit_assignment(&mut self, a: &'ast AssignmentExpr, p: Self::Param) -> Self::Result {
        self.visit_expression(&a.left, p);
        self.visit_expression(&a.right, p);
        Self::Result::output()
    }
    fn visit_variable_expr(&mut self, _v: &'ast VariableExpr, _p: Self::Param) -> Self::Result { Self::Result::output() }
    fn visit_literal(&mut self, _l: &'ast LitExpr, _p: Self::Param) -> Self::Result { Self::Result::output() }

    fn visit_expression(&mut self, a: &'ast Expression, p: Self::Param) -> Self::Result {
        use ExpressionKind as EK;
        match &a.kind {
            EK::Unary(u) => self.visit_unary(u, p),
            EK::Binary(b) => self.visit_binary(b, p),
            EK::Ternary(t) => self.visit_ternary(t, p),
            EK::Assignment(a) => self.visit_assignment(a, p),
            EK::Variable(v) => self.visit_variable_expr(v, p),
            EK::Literal(l) => self.visit_literal(l, p),
            EK::Call(c) => self.visit_call(c, p),
        }
    }
    fn visit_vardecl(&mut self, v: &'ast Rc<VariableDecl>, p: Self::Param) -> Self::Result {
        if let Some(ref init) = v.init {
            self.visit_expression(init, p);
        }
        Self::Result::output()
    }
    fn visit_expr_as_stmt(&mut self, s: &'ast ExprAsStmt, p: Self::Param) -> Self::Result {
        self.visit_expression(&s.expr, p);
        Self::Result::output()
    }
    fn visit_print(&mut self, pr: &'ast PrintStmt, p: Self::Param) -> Self::Result {
        self.visit_expression(&pr.expr, p);
        Self::Result::output()
    }
    fn visit_decl_stmt(&mut self, d: &'ast DeclarationStmt, p: Self::Param) -> Self::Result {
        self.visit_declaration(&d.inner, p);
        Self::Result::output()
    }
    fn visit_block(&mut self, b: &'ast BlockStmt, p: Self::Param) -> Self::Result {
        for stmt in &b.stmts {
            self.visit_statement(stmt, p);
        }
        Self::Result::output()
    }
    fn visit_if(&mut self, i: &'ast IfStmt, p: Self::Param) -> Self::Result {
        walk_if_statement(self, i, p)
    }
    fn visit_while(&mut self, w: &'ast WhileStmt, p: Self::Param) -> Self::Result {
        self.visit_expression(&w.cond, p);
        self.visit_statement(&w.stmts, p);
        Self::Result::output()
    }
    fn visit_for(&mut self, f: &'ast ForStmt, p: Self::Param) -> Self::Result {
        if let Some(init) = &f.init { self.visit_declaration(init, p); }
        if let Some(cond) = &f.cond { self.visit_expression(cond, p); }
        if let Some(inc) = &f.cond { self.visit_expression(inc, p); }
        self.visit_statement(&f.body, p);
        Self::Result::output()
    }
    fn visit_empty_stmt(&mut self, _e: &'ast EmptyStmt, _p: Self::Param) -> Self::Result { Self::Result::output() }
    fn visit_break_stmt(&mut self, _b: &'ast BreakStmt, _p: Self::Param) -> Self::Result { Self::Result::output() }
    fn visit_continue_stmt(&mut self, _c: &'ast ContinueStmt, _p: Self::Param) -> Self::Result { Self::Result::output() }

    fn visit_statement(&mut self, s: &'ast Statement, p: Self::Param) -> Self::Result {
        use StatementKind as SK;
        match &s.kind {
            SK::Expression(e) => self.visit_expr_as_stmt(e, p),
            SK::Print(e) => self.visit_print(e, p),
            SK::Decl(d) => self.visit_decl_stmt(d, p),
            SK::Block(b) => self.visit_block(b, p),
            SK::If(i) => self.visit_if(i, p),
            SK::While(w) => self.visit_while(w, p),
            SK::For(f) => self.visit_for(f, p),
            SK::Empty(e) => self.visit_empty_stmt(e, p),
            SK::Break(b) => self.visit_break_stmt(b, p),
            SK::Continue(c) => self.visit_continue_stmt(c, p),
        }
    }
    fn visit_type(&mut self, _ty: &'ast Type, _p: Self::Param) -> Self::Result {
        Self::Result::output()
    }
    fn visit_function_decl(&mut self, f: &'ast Rc<FunctionDecl>, p: Self::Param) -> Self::Result {
        walk_function_decl(self, f, p)
    }
    fn visit_declaration(&mut self, d: &'ast Declaration, p: Self::Param) -> Self::Result {
        use DeclarationKind as DK;
        match &d.kind {
            DK::Variable(v) => self.visit_vardecl(v, p),
            DK::Function(function_decl) => self.visit_function_decl(function_decl, p),
        }
    }
    fn visit_program(&mut self, prog: &'ast Program, p: Self::Param) -> Self::Result {
        walk_program(self, prog, p)
    }
    fn visit_call(&mut self, call: &'ast CallExpr, p: Self::Param) -> Self::Result {
        walk_call(self, call, p)
    }
}

pub fn walk_binary<'ast, V: Visitor<'ast>>(v: &mut V, b: &'ast BinaryExpr, p: V::Param) -> V::Result {
        v.visit_expression(&b.left, p);
        v.visit_expression(&b.right, p);
        V::Result::output()
}

pub fn walk_call<'ast, V: Visitor<'ast>>(v: &mut V, call: &'ast CallExpr, p: V::Param) -> V::Result {
        for arg in &call.args {
            v.visit_expression(arg, p);
        }
        V::Result::output()
}

pub fn walk_program<'ast, V: Visitor<'ast>>(v: &mut V, program: &'ast Program, p: V::Param) -> V::Result {
        program.decls.iter().for_each(|decl| { v.visit_declaration(decl, p); });
        V::Result::output()
}

pub fn walk_function_decl<'ast, V: Visitor<'ast>>(v: &mut V, f: &'ast FunctionDecl, p: V::Param) -> V::Result {
    f.args.iter().for_each(|vardecl| { v.visit_vardecl(vardecl, p); } );
    v.visit_type(&f.return_type, p);
    v.visit_block(&f.body, p);

    V::Result::output()
}

pub fn walk_if_statement<'ast, V: Visitor<'ast>>(v: &mut V, i: &'ast IfStmt, p: V::Param) -> V::Result {
    v.visit_expression(&i.cond, p);
    v.visit_statement(&i.if_true, p);
    if let Some(if_false) = &i.if_false {
        v.visit_statement(if_false, p);
    }
    V::Result::output()
}


pub trait VisitorResult {
    type T;
    type Residual;

    fn output() -> Self;
    fn from_residual(residual: Self::Residual) -> Self;
    fn from_branch(b: ControlFlow<Self::Residual, Self::T>) -> Self;
    fn branch(self) -> ControlFlow<Self::Residual, Self::T>;
}

impl VisitorResult for () {
    type T = ();
    type Residual = core::convert::Infallible;

    fn output() -> Self {}
    fn from_residual(_residual: Self::Residual) -> Self {}
    fn from_branch(_b: ControlFlow<Self::Residual>) -> Self {}
    fn branch(self) -> ControlFlow<Self::Residual> {
        ControlFlow::Continue(())
    }
}

impl<B,C: Default> VisitorResult for ControlFlow<B,C> {
    type T = C;
    type Residual = B;

    fn output() -> Self {
        ControlFlow::Continue(Self::T::default())
    }

    fn from_residual(residual: Self::Residual) -> Self {
        ControlFlow::Break(residual)
    }

    fn from_branch(b: ControlFlow<Self::Residual, Self::T>) -> Self {
        b
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::T> {
        self
    }
}

impl<T: Default,E> VisitorResult for Result<T,E> {
    type T = T;
    type Residual = E;

    fn output() -> Self {
        Result::Ok(T::default())
    }

    fn from_residual(residual: Self::Residual) -> Self {
        Self::Err(residual)
    }

    fn from_branch(b: ControlFlow<Self::Residual, Self::T>) -> Self {
        match b{
            ControlFlow::Continue(c) => Ok(c),
            ControlFlow::Break(b) => Err(b)
        }
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::T> {
        match self {
            Ok(t) => ControlFlow::Continue(t),
            Err(err) => ControlFlow::Break(err)
        }
    }
}

impl<T> VisitorResult for Option<T> {
    type T = T;
    type Residual = ();

    fn output() -> Self {
        None
    }

    fn from_residual(_residual: Self::Residual) -> Self {
        None
    }

    fn from_branch(b: ControlFlow<Self::Residual, Self::T>) -> Self {
        match b {
            ControlFlow::Continue(c) => Some(c),
            ControlFlow::Break(_) => None,
        }
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::T> {
        match self {
            Some(e) => ControlFlow::Continue(e),
            None => ControlFlow::Break(()),
        }
    }
}
