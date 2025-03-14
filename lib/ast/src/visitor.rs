use std::ops::ControlFlow;
use std::rc::Rc;

use crate::declaration::{DeclarationKind, FunctionDecl};
use crate::expr::CallExpr;
use crate::stmt::{ReadStmt, ReturnStmt, StatementKind};
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

    fn visit_expression(&mut self, expr: &'ast Expression) -> Self::Result {
        walk_expression(self, expr)
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
    fn visit_read_stmt(&mut self, r: &'ast ReadStmt) -> Self::Result {
        walk_read_stmt(self, r)
    }
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
    fn visit_statement(&mut self, stmt: &'ast Statement) -> Self::Result {
        walk_statement(self, stmt)
    }
    fn visit_return(&mut self, ret: &'ast ReturnStmt) -> Self::Result { walk_return(self, ret) }

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

pub fn walk_statement<'ast, V: Visitor<'ast>>(v: &mut V, stmt: &'ast Statement) -> V::Result {
        use StatementKind as SK;
        match &stmt.kind {
            SK::Expression(e) => v.visit_expr_as_stmt(e),
            SK::Print(e) => v.visit_print(e),
            SK::Read(r) => v.visit_read_stmt(r),
            SK::Decl(d) => v.visit_decl_stmt(d),
            SK::Block(b) => v.visit_block(b),
            SK::If(i) => v.visit_if(i),
            SK::While(w) => v.visit_while(w),
            SK::For(f) => v.visit_for(f),
            SK::Empty(e) => v.visit_empty_stmt(e),
            SK::Break(b) => v.visit_break_stmt(b),
            SK::Continue(c) => v.visit_continue_stmt(c),
            SK::Return(r) => v.visit_return(r),
        }
}

pub fn walk_read_stmt<'ast, V: Visitor<'ast>>(v: &mut V, r: &'ast ReadStmt) -> V::Result {
    v.visit_expression(&r.expr);
    V::Result::output()
}

pub fn walk_expression<'ast, V: Visitor<'ast>>(v: &mut V, expr: &'ast Expression) -> V::Result {
        use ExpressionKind as EK;
        match &expr.kind {
            EK::Unary(u) => v.visit_unary(u),
            EK::Binary(b) => v.visit_binary(b),
            EK::Ternary(t) => v.visit_ternary(t),
            EK::Assignment(a) => v.visit_assignment(a),
            EK::Variable(var) => v.visit_variable_expr(var),
            EK::Literal(l) => v.visit_literal(l),
            EK::Call(c) => v.visit_call(c),
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

pub fn walk_return<'ast, V: Visitor<'ast>>(v: &mut V, ret: &'ast ReturnStmt) -> V::Result {
        if let Some(ret) = &ret.expr {
            v.visit_expression(ret);
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
