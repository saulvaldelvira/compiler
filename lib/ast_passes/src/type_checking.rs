use std::borrow::Cow;
use std::rc::Rc;

use ast::declaration::FunctionDecl;
use ast::stmt::ReturnStmt;
use ast::types::{ErrorType, Type, TypeKind};
use ast::visitor::{walk_binary, walk_if_statement};
use ast::{Program, Visitor, visitor::VisitorResult};
use error_manager::ErrorManager;

pub struct TypeCheking {
    pub (super) error_manager: ErrorManager,

    current_function: Option<Rc<FunctionDecl>>,
}

impl TypeCheking {
    pub fn new() -> Self {
        Self {
            error_manager: ErrorManager::new(),
            current_function: None
        }
    }
    pub fn process(&mut self, prog: &Program) {
        self.visit_program(prog);
    }

}

fn error(msg: impl Into<Cow<'static, str>>) -> Type {
    let msg = msg.into();
    Type {
        kind: TypeKind::Error(ErrorType { msg })
    }
}

impl Visitor<'_> for TypeCheking {
    type Result = Option<Type>;

    fn visit_unary(&mut self, u: &'_ ast::expr::UnaryExpr) -> Self::Result { self.visit_expression(&u.expr) }

    fn visit_binary(&mut self, b: &'_ ast::expr::BinaryExpr) -> Self::Result {
        use ast::expr::BinaryExprKind;

        walk_binary(self, b);

        let ty = match b.kind {
            BinaryExprKind::Logical => b.left.ty.unwrap().logical(&b.right.ty.unwrap()),
            BinaryExprKind::Arithmetic => b.left.ty.unwrap().arithmetic(&b.right.ty.unwrap()),
            BinaryExprKind::Comparison => b.left.ty.unwrap().comparison(&b.right.ty.unwrap()),
            BinaryExprKind::Comma => b.left.ty.cloned().unwrap(),
        };

        Some(ty)
    }

    fn visit_ternary(&mut self, t: &'_ ast::expr::TernaryExpr) -> Self::Result {
        self.visit_expression(&t.cond);
        self.visit_expression(&t.if_true);
        self.visit_expression(&t.if_false);

        Some (
            if t.if_true.ty.unwrap().kind != t.if_false.ty.unwrap().kind {
                error("Incompatible types on ternary operator")
            } else {
                t.if_true.ty.cloned().unwrap()
            }
        )
    }

    fn visit_assignment(&mut self, a: &'_ ast::expr::AssignmentExpr) -> Self::Result {
        self.visit_expression(&a.left);
        self.visit_expression(&a.right);

        let lt = a.left.ty.unwrap();
        let rt = a.right.ty.unwrap();

        if lt.kind != rt.kind {
            self.error_manager.error(format!("Assignment of type {:#?} to {:#?}", rt.kind, lt.kind), a.right.span.join(&a.left.span));
        }

        Some(a.left.ty.cloned().unwrap())
    }

    fn visit_variable_expr(&mut self, _v: &'_ ast::expr::VariableExpr) -> Self::Result {
        let ty = _v.decl.get().unwrap().ty.cloned().unwrap();
        Some(ty)
    }

    fn visit_literal(&mut self, _l: &'_ ast::expr::LitExpr) -> Self::Result {
        use ast::expr::LitValue;
        let t = match _l.value {
            LitValue::Int(_) => Type::int(),
            LitValue::Float(_) => Type::float(),
            LitValue::Str(_) => Type::string(),
            LitValue::Bool(_) => Type::bool(),
            LitValue::Char(_) => Type::char(),
        };
        Some(t)
    }

    fn visit_expression(&mut self, a: &'_ ast::Expression) -> Self::Result {
        use ast::expr::ExpressionKind as EK;
        let ty = match &a.kind {
            EK::Unary(u) => self.visit_unary(u),
            EK::Binary(b) => self.visit_binary(b),
            EK::Ternary(t) => self.visit_ternary(t),
            EK::Assignment(a) => self.visit_assignment(a),
            EK::Variable(v) => self.visit_variable_expr(v),
            EK::Literal(l) => self.visit_literal(l),
            EK::Call(c) => self.visit_call(c),
        };
        match ty.clone() {
            Some(ty) => {
                if let TypeKind::Error(err) = &ty.kind {
                    self.error_manager.error(err.msg.clone(), a.span);
                }
                a.ty.set(ty);
            },
            None => {
                self.error_manager.error("Unknown type", a.span);
            },
        };
        ty
    }

    fn visit_call(&mut self, call: &'_ ast::expr::CallExpr) -> Self::Result {
        ast::visitor::walk_call(self, call);
        let ty = call.decl.unwrap().return_type.clone();
        Some(ty)
    }

    fn visit_vardecl(&mut self, v: &'_ std::rc::Rc<ast::declaration::VariableDecl>) -> Self::Result {
        if let Some(ref init) = v.init {
            self.visit_expression(init);
        }
        Self::Result::output()
    }

    fn visit_expr_as_stmt(&mut self, s: &'_ ast::stmt::ExprAsStmt) -> Self::Result {
        self.visit_expression(&s.expr);
        Self::Result::output()
    }

    fn visit_print(&mut self, pr: &'_ ast::stmt::PrintStmt) -> Self::Result {
        self.visit_expression(&pr.expr);
        Self::Result::output()
    }

    fn visit_decl_stmt(&mut self, d: &'_ ast::stmt::DeclarationStmt) -> Self::Result {
        self.visit_declaration(&d.inner);
        Self::Result::output()
    }

    fn visit_block(&mut self, b: &'_ ast::stmt::BlockStmt) -> Self::Result {
        for stmt in &b.stmts {
            self.visit_statement(stmt);
        }
        Self::Result::output()
    }

    fn visit_if(&mut self, i: &'_ ast::stmt::IfStmt) -> Self::Result {
        walk_if_statement(self, i);
        let ty = i.cond.ty.unwrap();
        if !matches!(ty.kind, TypeKind::Bool) {
            self.error_manager.error("If condition must be bool", i.cond.span);
        }
        None
    }

    fn visit_while(&mut self, w: &'_ ast::stmt::WhileStmt) -> Self::Result {
        self.visit_expression(&w.cond);
        self.visit_statement(&w.stmts);

        let ty = w.cond.ty.unwrap();
        if !matches!(ty.kind, TypeKind::Bool) {
            self.error_manager.error("While condition must be bool", w.cond.span);
        }

        Self::Result::output()
    }

    fn visit_for(&mut self, f: &'_ ast::stmt::ForStmt) -> Self::Result {
        if let Some(init) = &f.init { self.visit_declaration(init); }
        if let Some(cond) = &f.cond {
            self.visit_expression(cond);
            if !cond.ty.unwrap().is_boolean() {
                self.error_manager.error("For condition must be boolean", cond.span);
            }
        }
        if let Some(inc) = &f.cond { self.visit_expression(inc); }
        self.visit_statement(&f.body);
        Self::Result::output()
    }

    fn visit_empty_stmt(&mut self, _e: &'_ ast::stmt::EmptyStmt) -> Self::Result { Self::Result::output() }

    fn visit_break_stmt(&mut self, _b: &'_ ast::stmt::BreakStmt) -> Self::Result { Self::Result::output() }

    fn visit_continue_stmt(&mut self, _c: &'_ ast::stmt::ContinueStmt) -> Self::Result { Self::Result::output() }

    fn visit_statement(&mut self, s: &'_ ast::Statement) -> Self::Result {
        use ast::stmt::StatementKind as SK;
        let ty = match &s.kind {
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
            SK::Return(r) => self.visit_return(r),
        };

        if let Some(Type { kind: TypeKind::Error(err)}) = ty {
            self.error_manager.error(err.msg, s.span);
        }

        Self::Result::output()
    }

    fn visit_type(&mut self, ty: &'_ Type) -> Self::Result {
        let _todo = ty;
        Self::Result::output()
    }

    fn visit_function_decl(&mut self, f: &'_ std::rc::Rc<ast::declaration::FunctionDecl>) -> Self::Result {
        let oldf = self.current_function.take();
        self.current_function = Some(Rc::clone(f));
        ast::visitor::walk_function_decl(self, f);
        self.current_function = oldf;
        Self::Result::output()
    }

    fn visit_declaration(&mut self, d: &'_ ast::Declaration) -> Self::Result {
        use ast::declaration::DeclarationKind as DK;
        match &d.kind {
            DK::Variable(v) => self.visit_vardecl(v),
            DK::Function(function_decl) => self.visit_function_decl(function_decl),
        }
    }

    fn visit_program(&mut self, prog: &'_ Program) -> Self::Result {
        ast::visitor::walk_program(self, prog)
    }

    fn visit_return(&mut self, ret: &'_ ReturnStmt) -> Self::Result {
        if let Some(expr) = ret.expr.as_ref() {
            self.visit_expression(expr);
        }

        let Some(expected) = &self.current_function else { panic!() };

        if let Some(expr) = ret.expr.as_ref() {
            if expr.ty.unwrap().kind != expected.return_type.kind {
                let msg = format!("Incompatible type {:#?} on return, inside function with type {:#?}", expr.ty.unwrap().kind, expected.return_type.kind);
                self.error_manager.error(msg, expr.span);
            }
        } else if !matches!(expected.return_type.kind, TypeKind::Empty) {
            return Some(error(format!("Return needs an expression of type {:#?}", expected.return_type.kind)));
        }

        Self::Result::output()
    }

}
