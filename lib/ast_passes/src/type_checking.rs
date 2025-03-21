use std::borrow::Cow;
use std::rc::Rc;

use ast::declaration::FunctionDecl;
use ast::expr::{ArrayAccess, Dereference, Reference, StructAccess};
use ast::stmt::{ReadStmt, ReturnStmt};
use ast::types::{ErrorType, Type, TypeKind};
use ast::visitor::{walk_array_access, walk_binary, walk_declaration, walk_deref_expr, walk_expression, walk_if_statement, walk_read_stmt, walk_ref_expr, walk_statement};
use ast::{Program, Visitor, visitor::VisitorResult};
use util::ErrorManager;

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
        kind: TypeKind::Error(ErrorType { msg, span: None })
    }
}

impl Visitor<'_> for TypeCheking {
    type Result = Option<Type>;

    fn visit_unary(&mut self, u: &'_ ast::expr::UnaryExpr) -> Self::Result { self.visit_expression(&u.expr) }

    fn visit_binary(&mut self, b: &'_ ast::expr::BinaryExpr) -> Self::Result {
        use ast::expr::BinaryExprKind;

        walk_binary(self, b);

        let ty = match b.kind {
            BinaryExprKind::Logical => b.left.get_type().logical(&b.right.get_type()),
            BinaryExprKind::Arithmetic => b.left.get_type().arithmetic(&b.right.get_type()),
            BinaryExprKind::Comparison => b.left.get_type().comparison(&b.right.get_type()),
        };

        Some(ty)
    }

    fn visit_ternary(&mut self, t: &'_ ast::expr::TernaryExpr) -> Self::Result {
        self.visit_expression(&t.cond);
        self.visit_expression(&t.if_true);
        self.visit_expression(&t.if_false);

        Some (
            if t.if_true.get_type().kind != t.if_false.get_type().kind {
                error("Incompatible types on ternary operator")
            } else {
                t.if_true.ty.cloned().unwrap()
            }
        )
    }

    fn visit_assignment(&mut self, a: &'_ ast::expr::AssignmentExpr) -> Self::Result {
        self.visit_expression(&a.left);
        self.visit_expression(&a.right);

        let lt = a.left.get_type();
        let rt = a.right.get_type();

        if lt.kind != rt.kind
           && !matches!(rt.kind, TypeKind::Error(_))
           && !matches!(lt.kind, TypeKind::Error(_))
        {
            self.error_manager.error(format!("Assignment of type {:#?} to {:#?}", rt.kind, lt.kind), a.right.span.join(&a.left.span));
        }

        Some(a.left.ty.cloned().unwrap())
    }

    fn visit_ref_expr(&mut self, rexpr: &'_ Reference) -> Self::Result {
        walk_ref_expr(self, rexpr);
        let ty = rexpr.of.reference();
        Some(ty)
    }

    fn visit_variable_expr(&mut self, _v: &'_ ast::expr::VariableExpr) -> Self::Result {
        let ty = _v.decl.unwrap().ty.clone().unwrap();
        Some(ty)
    }

    fn visit_deref_expr(&mut self, rexpr: &'_ Dereference) -> Self::Result {
        walk_deref_expr(self, rexpr);
        let t = rexpr.of.dereference();
        Some(t)
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
        let ty = walk_expression(self, a);
        match ty {
            Some(mut ty) => {
                if let TypeKind::Error(err) = &mut ty.kind {
                    if err.span.is_none() {
                        err.span = Some(a.span);
                        self.error_manager.error(err.msg.clone(), a.span);
                    }
                }
                a.ty.set(ty);
            },
            None => {
                a.ty.set(error("Missing type"));
            },
        }
        a.ty.cloned()
    }

    fn visit_array_access(&mut self, ac: &'_ ArrayAccess) -> Self::Result {
        walk_array_access(self, ac);
        let ty = ac.array.get_type().square_brackets(&ac.index);
        Some(ty)
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

    fn visit_struct_access(&mut self, a: &'_ StructAccess) -> Self::Result {
        self.visit_expression(&a.st);
        let ty = a.st.get_type().access_field(a.field);
        Some(ty)
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
        let ty = i.cond.get_type();
        if !matches!(ty.kind, TypeKind::Bool) {
            self.error_manager.error("If condition must be bool", i.cond.span);
        }
        None
    }

    fn visit_while(&mut self, w: &'_ ast::stmt::WhileStmt) -> Self::Result {
        self.visit_expression(&w.cond);
        self.visit_statement(&w.stmts);

        let ty = w.cond.get_type();
        if !matches!(ty.kind, TypeKind::Bool) {
            self.error_manager.error("While condition must be bool", w.cond.span);
        }

        Self::Result::output()
    }

    fn visit_for(&mut self, f: &'_ ast::stmt::ForStmt) -> Self::Result {
        if let Some(init) = &f.init { self.visit_declaration(init); }
        if let Some(cond) = &f.cond {
            self.visit_expression(cond);
            if !cond.get_type().is_boolean() {
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
        let ty = walk_statement(self, s);

        if let Some(Type { kind: TypeKind::Error(err)}) = ty {
            if err.span.is_none() {
                self.error_manager.error(err.msg, s.span);
            }
        }

        Self::Result::output()
    }

    fn visit_read_stmt(&mut self, r: &'_ ReadStmt) -> Self::Result {
        walk_read_stmt(self, r);
        if !r.expr.lvalue() {
            Some(error("Cannot read into non-lvalue"))
        } else {
            None
        }
    }

    fn visit_type(&mut self, ty: &'_ Type) -> Self::Result {
        let _todo = ty;
        Self::Result::output()
    }

    fn visit_declaration(&mut self, decl: &'_ ast::Declaration) -> Self::Result {
       if let Some(Type { kind: TypeKind::Error(err), .. }) = walk_declaration(self, decl) {
           if err.span.is_none() {
               self.error_manager.error(err.msg, decl.span);
           }
       }
       Self::Result::output()
    }

    fn visit_function_decl(&mut self, f: &'_ std::rc::Rc<ast::declaration::FunctionDecl>) -> Self::Result {
        let oldf = self.current_function.take();
        self.current_function = Some(Rc::clone(f));
        ast::visitor::walk_function_decl(self, f);
        for arg in &f.args {
            if !arg.ty.as_ref().unwrap().is_primitive() {
                return Some(error(format!("Non-primitive type on function param '{:#?}'", arg.name)))
            }
        }
        self.current_function = oldf;
        Self::Result::output()
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
            if expr.get_type().kind != expected.return_type.kind {
                let msg = format!("Incompatible type {:#?} on return, inside function with type {:#?}", expr.get_type().kind, expected.return_type.kind);
                self.error_manager.error(msg, expr.span);
            }
        } else if !matches!(expected.return_type.kind, TypeKind::Empty) {
            return Some(error(format!("Return needs an expression of type {:#?}", expected.return_type.kind)));
        }

        Self::Result::output()
    }

}
