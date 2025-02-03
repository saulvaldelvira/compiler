use std::ops::ControlFlow;
use std::rc::Rc;

use crate::declaration::{DeclarationKind, FunctionDecl};
use crate::expr::CallExpr;
use crate::stmt::StatementKind;
use crate::types::Type;
use crate::Expression;
use crate::{declaration::{Declaration, VariableDecl}, expr::{AssignmentExpr, BinaryExpr, ExpressionKind, LitExpr, TernaryExpr, UnaryExpr, VariableExpr}, stmt::{BlockStmt, BreakStmt, ContinueStmt, DeclarationStmt, EmptyStmt, ExprAsStmt, ForStmt, IfStmt, PrintStmt, Statement, WhileStmt}, Program};

pub trait Visitor<'ast> : Sized {
    type Result: VisitorResult;

    fn visit_unary(&mut self, u: &'ast UnaryExpr) -> Self::Result { self.visit_expression(&u.expr) }
    fn visit_binary(&mut self, b: &'ast BinaryExpr) -> Self::Result {
        walk_binary(self, b)
    }
    fn visit_ternary(&mut self, t: &'ast TernaryExpr) -> Self::Result {
        self.visit_expression(&t.cond);
        self.visit_expression(&t.if_true);
        self.visit_expression(&t.if_false);
        Self::Result::output()
    }
    fn visit_assignment(&mut self, a: &'ast AssignmentExpr) -> Self::Result {
        self.visit_expression(&a.left);
        self.visit_expression(&a.right);
        Self::Result::output()
    }
    fn visit_variable_expr(&mut self, _v: &'ast VariableExpr) -> Self::Result { Self::Result::output() }
    fn visit_literal(&mut self, _l: &'ast LitExpr) -> Self::Result { Self::Result::output() }

    fn visit_expression(&mut self, a: &'ast Expression) -> Self::Result {
        use ExpressionKind as EK;
        match &a.kind {
            EK::Unary(u) => self.visit_unary(u),
            EK::Binary(b) => self.visit_binary(b),
            EK::Ternary(t) => self.visit_ternary(t),
            EK::Assignment(a) => self.visit_assignment(a),
            EK::Variable(v) => self.visit_variable_expr(v),
            EK::Literal(l) => self.visit_literal(l),
            EK::Call(c) => self.visit_call(c),
        }
    }
    fn visit_vardecl(&mut self, v: &'ast Rc<VariableDecl>) -> Self::Result {
        if let Some(ref init) = v.init {
            self.visit_expression(init);
        }
        Self::Result::output()
    }
    fn visit_expr_as_stmt(&mut self, s: &'ast ExprAsStmt) -> Self::Result {
        self.visit_expression(&s.expr);
        Self::Result::output()
    }
    fn visit_print(&mut self, pr: &'ast PrintStmt) -> Self::Result { self.visit_expression(&pr.expr); Self::Result::output()  }
    fn visit_decl_stmt(&mut self, d: &'ast DeclarationStmt) -> Self::Result { self.visit_declaration(&d.inner); Self::Result::output()  }
    fn visit_block(&mut self, b: &'ast BlockStmt) -> Self::Result {
        for stmt in &b.stmts {
            self.visit_statement(stmt);
        }
        Self::Result::output()
    }
    fn visit_if(&mut self, i: &'ast IfStmt) -> Self::Result {
        walk_if_statement(self, i)
    }
    fn visit_while(&mut self, w: &'ast WhileStmt) -> Self::Result {
        self.visit_expression(&w.cond);
        self.visit_statement(&w.stmts);
        Self::Result::output()
    }
    fn visit_for(&mut self, f: &'ast ForStmt) -> Self::Result {
        if let Some(init) = &f.init { self.visit_declaration(init); }
        if let Some(cond) = &f.cond { self.visit_expression(cond); }
        if let Some(inc) = &f.cond { self.visit_expression(inc); }
        self.visit_statement(&f.body);
        Self::Result::output()
    }
    fn visit_empty_stmt(&mut self, _e: &'ast EmptyStmt) -> Self::Result { Self::Result::output() }
    fn visit_break_stmt(&mut self, _b: &'ast BreakStmt) -> Self::Result { Self::Result::output() }
    fn visit_continue_stmt(&mut self, _c: &'ast ContinueStmt) -> Self::Result { Self::Result::output() }
    fn visit_statement(&mut self, s: &'ast Statement) -> Self::Result {
        use StatementKind as SK;
        match &s.kind {
            SK::Expression(e) => self.visit_expr_as_stmt(e),
            SK::Print(e) => self.visit_print(e),
            SK::Decl(d) => self.visit_decl_stmt(d),
            SK::Block(b) => self.visit_block(b),
            SK::If(i) => self.visit_if(i),
            SK::While(w) => self.visit_while(w),
            SK::For(f) => self.visit_for(f),
            SK::Empty(e) => self.visit_empty_stmt(e),
            SK::Break(b) => self.visit_break_stmt(b),
            SK::Continue(c) => self.visit_continue_stmt(c),
        }
    }
    fn visit_type(&mut self, ty: &'ast Type) -> Self::Result {
        let _todo = ty;
        Self::Result::output()
    }
    fn visit_function_decl(&mut self, f: &'ast Rc<FunctionDecl>) -> Self::Result {
        walk_function_decl(self, f)
    }
    fn visit_declaration(&mut self, d: &'ast Declaration) -> Self::Result {
        use DeclarationKind as DK;
        match &d.kind {
            DK::Variable(v) => self.visit_vardecl(v),
            DK::Function(function_decl) => self.visit_function_decl(function_decl),
        }
    }
    fn visit_program(&mut self, prog: &'ast Program) -> Self::Result {
        walk_program(self, prog)
    }
    fn visit_call(&mut self, call: &'ast CallExpr) -> Self::Result {
        walk_call(self, call)
    }
}

pub fn walk_binary<'ast, V: Visitor<'ast>>(v: &mut V, b: &'ast BinaryExpr) -> V::Result {
        v.visit_expression(&b.left);
        v.visit_expression(&b.right);
        V::Result::output()
}

pub fn walk_call<'ast, V: Visitor<'ast>>(v: &mut V, call: &'ast CallExpr) -> V::Result {
        for arg in &call.args {
            v.visit_expression(arg);
        }
        V::Result::output()
}

pub fn walk_program<'ast, V: Visitor<'ast>>(v: &mut V, program: &'ast Program) -> V::Result {
        program.decls.iter().for_each(|decl| { v.visit_declaration(decl); });
        V::Result::output()
}

pub fn walk_function_decl<'ast, V: Visitor<'ast>>(v: &mut V, f: &'ast FunctionDecl) -> V::Result {
    f.args.iter().for_each(|vardecl| { v.visit_vardecl(vardecl); } );
    v.visit_type(&f.return_type);
    v.visit_block(&f.body);

    V::Result::output()
}

pub fn walk_if_statement<'ast, V: Visitor<'ast>>(v: &mut V, i: &'ast IfStmt) -> V::Result {
    v.visit_expression(&i.cond);
    v.visit_statement(&i.if_true);
    if let Some(if_false) = &i.if_false {
        v.visit_statement(if_false);
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
