use std::fmt::{self,Write};

use ast::expr::{AssignmentExpr, BinaryExpr, CallExpr, ExpressionKind, LitExpr, TernaryExpr, UnaryExpr, VariableExpr};
use ast::types::Type;
use ast::{declaration, stmt, visitor, Declaration, Expression, Program, Statement};
use ast::{self, Visitor, AST, visitor::VisitorResult};
use session::with_symbol;

pub struct PrintVisitor<'a>(pub &'a mut dyn Write);

impl Visitor<'_> for PrintVisitor<'_> {
    type Result = Result<(),fmt::Error>;

    fn visit_ast(&mut self, a: &'_ AST) -> Self::Result { visitor::walk_ast(self, a) }

    fn visit_unary(&mut self, u: &'_ UnaryExpr) -> Self::Result { self.visit_expression(&u.expr) }

    fn visit_binary(&mut self, b: &'_ BinaryExpr) -> Self::Result {
        self.visit_expression(&b.left)?;
        self.visit_expression(&b.right)?;
        Self::Result::output()
    }

    fn visit_ternary(&mut self, t: &'_ TernaryExpr) -> Self::Result {
        self.visit_expression(&t.cond)?;
        self.visit_expression(&t.if_true)?;
        self.visit_expression(&t.if_false)?;
        Self::Result::output()
    }

    fn visit_assignment(&mut self, a: &'_ AssignmentExpr) -> Self::Result {
        self.visit_expression(&a.left)?;
        self.0.write_str(" = ")?;
        self.visit_expression(&a.right)?;
        Self::Result::output()
    }

    fn visit_variable_expr(&mut self, v: &'_ VariableExpr) -> Self::Result {
        with_symbol(v.name, |name| {
            self.0.write_str(name)
        })
    }

    fn visit_literal(&mut self, _l: &'_ LitExpr) -> Self::Result { Self::Result::output() }

    fn visit_expression(&mut self, a: &'_ Expression) -> Self::Result {
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

    fn visit_vardecl(&mut self, v: &'_ std::rc::Rc<declaration::VariableDecl>) -> Self::Result {
        if let Some(ref init) = v.init {
            self.visit_expression(init)?;
        }
        Self::Result::output()
    }

    fn visit_expr_as_stmt(&mut self, s: &'_ stmt::ExprAsStmt) -> Self::Result {
        self.visit_expression(&s.expr)?;
        Self::Result::output()
    }

    fn visit_print(&mut self, pr: &'_ stmt::PrintStmt) -> Self::Result {
        self.0.write_str("print ")?;
        self.visit_expression(&pr.expr)
    }

    fn visit_decl_stmt(&mut self, d: &'_ stmt::DeclarationStmt) -> Self::Result { self.visit_declaration(&d.inner)?; Self::Result::output()  }

    fn visit_block(&mut self, b: &'_ stmt::BlockStmt) -> Self::Result {
        for stmt in &b.stmts {
            self.visit_statement(stmt)?;
        }
        Self::Result::output()
    }

    fn visit_if(&mut self, i: &'_ stmt::IfStmt) -> Self::Result {
        self.visit_expression(&i.cond)?;
        self.visit_statement(&i.if_true)?;
        if let Some(if_false) = &i.if_false {
            self.visit_statement(if_false)?;
        }
        Self::Result::output()
    }

    fn visit_while(&mut self, w: &'_ stmt::WhileStmt) -> Self::Result {
        self.visit_expression(&w.cond)?;
        self.visit_statement(&w.stmts)?;
        Self::Result::output()
    }

    fn visit_for(&mut self, f: &'_ stmt::ForStmt) -> Self::Result {
        if let Some(init) = &f.init { self.visit_declaration(init)?; }
        if let Some(cond) = &f.cond { self.visit_expression(cond)?; }
        if let Some(inc) = &f.cond { self.visit_expression(inc)?; }
        self.visit_statement(&f.body)?;
        Self::Result::output()
    }

    fn visit_empty_stmt(&mut self, _e: &'_ stmt::EmptyStmt) -> Self::Result { Self::Result::output() }

    fn visit_break_stmt(&mut self, _b: &'_ stmt::BreakStmt) -> Self::Result { Self::Result::output() }

    fn visit_continue_stmt(&mut self, _c: &'_ stmt::ContinueStmt) -> Self::Result { Self::Result::output() }

    fn visit_statement(&mut self, s: &'_ Statement) -> Self::Result {
        use stmt::StatementKind as SK;
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
        }?;
        self.0.write_str(";")
    }

    fn visit_type(&mut self, ty: &'_ Type) -> Self::Result {
        let _todo = ty;
        Self::Result::output()
    }

    fn visit_function_decl(&mut self, f: &'_ std::rc::Rc<declaration::FunctionDecl>) -> Self::Result {
        visitor::walk_function_decl(self, f)
    }

    fn visit_declaration(&mut self, d: &'_ Declaration) -> Self::Result {
        use declaration::DeclarationKind as DK;
        match &d.kind {
            DK::Variable(v) => self.visit_vardecl(v),
            DK::Function(function_decl) => self.visit_function_decl(function_decl),
        }
    }

    fn visit_program(&mut self, prog: &'_ Program) -> Self::Result {
        visitor::walk_program(self, prog)
    }

    fn visit_call(&mut self, call: &'_ CallExpr) -> Self::Result {
        visitor::walk_call(self, call)
    }


}
