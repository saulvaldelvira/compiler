use std::ops::ControlFlow;
use std::rc::Rc;

use crate::declaration::{DeclarationKind, FunctionDecl, StructDecl, StructField};
use crate::expr::{ArrayAccess, CallExpr, Dereference, Reference, StructAccess};
use crate::stmt::{ReadStmt, ReturnStmt, StatementKind};
use crate::types::{ArrayType, RefType, StructType, Type, TypeKind};
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
        walk_variable_decl(self, v)
    }
    fn visit_array_access(&mut self, a: &'ast ArrayAccess) -> Self::Result {
        walk_array_access(self, a)
    }
    fn visit_struct_access(&mut self, a: &'ast StructAccess) -> Self::Result {
        walk_struct_access(self, a)
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
        walk_type(self, ty)
    }

    fn visit_ref_type(&mut self, rty: &'ast RefType) -> Self::Result {
        walk_ref_type(self, rty)
    }

    fn visit_ref_expr(&mut self, rexpr: &'ast Reference) -> Self::Result {
        walk_ref_expr(self, rexpr)
    }

    fn visit_deref_expr(&mut self, rexpr: &'ast Dereference) -> Self::Result {
        walk_deref_expr(self, rexpr)
    }

    fn visit_array_type(&mut self, aty: &'ast ArrayType) -> Self::Result {
        walk_array_type(self, aty)
    }

    fn visit_struct_type(&mut self, sty: &'ast StructType) -> Self::Result {
        walk_struct_type(self, sty)
    }

    fn visit_function_decl(&mut self, f: &'ast Rc<FunctionDecl>) -> Self::Result {
        walk_function_decl(self, f)
    }
    fn visit_declaration(&mut self, decl: &'ast Declaration) -> Self::Result {
        walk_declaration(self, decl)
    }
    fn visit_struct_decl(&mut self, s: &'ast Rc<StructDecl>) -> Self::Result {
        walk_struct_decl(self, s)
    }
    fn visit_struct_field(&mut self, field: &'ast StructField) -> Self::Result {
        walk_struct_field(self, field)
    }
    fn visit_program(&mut self, prog: &'ast Program) -> Self::Result {
        walk_program(self, prog)
    }
    fn visit_call(&mut self, call: &'ast CallExpr) -> Self::Result {
        walk_call(self, call)
    }
}

pub fn walk_ref_type<'ast, V: Visitor<'ast>>(v: &mut V, rty: &'ast RefType) -> V::Result {
    v.visit_type(&rty.of);
    V::Result::output()
}

pub fn walk_type<'ast, V: Visitor<'ast>>(v: &mut V, ty: &'ast Type) -> V::Result {
        match &ty.kind {
            TypeKind::Array(aty) => v.visit_array_type(aty),
            TypeKind::Struct(sty) => v.visit_struct_type(sty),
            TypeKind::Ref(rty) => v.visit_ref_type(rty),
            TypeKind::Int |
            TypeKind::Float |
            TypeKind::Bool |
            TypeKind::Char |
            TypeKind::String |
            TypeKind::Error(_) |
            TypeKind::Empty => { V::Result::output() }
        }
}

pub fn walk_variable_decl<'ast, V: Visitor<'ast>>(v: &mut V, vdecl: &'ast VariableDecl) -> V::Result {
    if let Some(ref ty) = vdecl.ty {
        v.visit_type(ty);
    }
    if let Some(ref init) = vdecl.init {
        v.visit_expression(init);
    }
    V::Result::output()
}

pub fn walk_array_type<'ast, V: Visitor<'ast>>(v: &mut V, aty: &'ast ArrayType) -> V::Result {
    v.visit_type(&aty.of);
    V::Result::output()
}

pub fn walk_struct_type<'ast, V: Visitor<'ast>>(_v: &mut V, _sty: &'ast StructType) -> V::Result {
    V::Result::output()
}

pub fn walk_declaration<'ast, V: Visitor<'ast>>(v: &mut V, decl: &'ast Declaration) -> V::Result {
        use DeclarationKind as DK;
        match &decl.kind {
            DK::Variable(var) => v.visit_vardecl(var),
            DK::Function(function_decl) => v.visit_function_decl(function_decl),
            DK::Struct(s) => v.visit_struct_decl(s),
        }
}

pub fn walk_struct_field<'ast, V: Visitor<'ast>>(v: &mut V, s: &'ast StructField) -> V::Result {
    v.visit_type(&s.ty);
    V::Result::output()
}

pub fn walk_struct_decl<'ast, V: Visitor<'ast>>(v: &mut V, s: &'ast StructDecl) -> V::Result {
    for field in &s.fields {
        v.visit_struct_field(field);
    }
    V::Result::output()
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

pub fn walk_array_access<'ast, V: Visitor<'ast>>(v: &mut V, ac: &'ast ArrayAccess) -> V::Result {
    v.visit_expression(&ac.array);
    v.visit_expression(&ac.index);
    V::Result::output()
}

pub fn walk_struct_access<'ast, V: Visitor<'ast>>(v: &mut V, sa: &'ast StructAccess) -> V::Result {
    v.visit_expression(&sa.st);
    V::Result::output()
}

pub fn walk_ref_expr<'ast, V: Visitor<'ast>>(v: &mut V, r: &'ast Reference) -> V::Result {
    v.visit_expression(&r.of);
    V::Result::output()
}

pub fn walk_deref_expr<'ast, V: Visitor<'ast>>(v: &mut V, r: &'ast Dereference) -> V::Result {
    v.visit_expression(&r.of);
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
            EK::ArrayAccess(a) => v.visit_array_access(a),
            EK::StructAccess(sa) => v.visit_struct_access(sa),
            EK::Ref(rexpr) => v.visit_ref_expr(rexpr),
            EK::Deref(rexpr) => v.visit_deref_expr(rexpr),
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
