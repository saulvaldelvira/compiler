use std::collections::HashMap;
use std::rc::{Rc, Weak};

use ast::declaration::{DeclarationKind, FunctionDecl, VariableDecl};
use ast::expr::{CallExpr, VariableExpr};
use ast::visitor::{self, walk_function_decl};
use ast::{Program, Visitor};
use util::ErrorManager;
use session::Symbol;

pub struct Identification {
    pub (super) error_manager: ErrorManager,
    scopes: Scopes,
}

impl Identification {
    pub fn new() -> Self {
        Identification {
            scopes: Scopes::new(),
            error_manager: ErrorManager::new(),
        }
    }

    pub fn process(&mut self, program: &Program) {
        for decl in &program.decls {
            match &decl.kind {
                DeclarationKind::Variable(vdecl) => self.scopes.def_var(vdecl.name, Rc::downgrade(&vdecl)),
                DeclarationKind::Function(fdecl) => self.scopes.def_fn(fdecl.name, Rc::downgrade(&fdecl)),
            }
        }

        self.visit_program(program).unwrap();
    }
}

impl<'ast> Visitor<'ast> for Identification {
    type Result = Result<(),String>;

    fn visit_expression(&mut self, a: &'ast ast::Expression) -> Self::Result {
        visitor::walk_expression(self, a).unwrap_or_else(|err| {
            self.error_manager.error(err, a.span);
        });
        Ok(())
    }

    fn visit_vardecl(&mut self, v: &'ast Rc<VariableDecl>) -> Self::Result {
        self.scopes.def_var(v.name, Rc::downgrade(v));
        Ok(())
    }

    fn visit_variable_expr(&mut self, v: &'ast VariableExpr) -> Self::Result {
        match self.scopes.get_var(&v.name) {
            Some(decl) => v.decl.set(Weak::clone(decl)),
            None => return Err(format!("Undefined variable \"{:?}\"", v.name)),
        }
        Ok(())
    }

    fn visit_function_decl(&mut self, f: &'ast Rc<FunctionDecl>) -> Self::Result {
        self.scopes.def_fn(f.name, Rc::downgrade(f));
        self.scopes.set();
        walk_function_decl(self, f).unwrap();
        self.scopes.reset();
        Ok(())
    }

    fn visit_call(&mut self, call: &'ast CallExpr) -> Self::Result {
        match self.scopes.get_func(&call.callee) {
            Some(decl) => {
                call.decl.set(Weak::clone(decl));
            },
            None => return Err(format!("Undefined function \"{:?}\"", call.callee)),
        }

        Ok(())
    }
}

#[derive(Default)]
pub struct ScopeLevel {
    variables: HashMap<Symbol,Weak<VariableDecl>>,
    functions: HashMap<Symbol,Weak<FunctionDecl>>,
}

pub struct Scopes(Vec<ScopeLevel>);

impl Scopes {
    pub fn new() -> Self {
        let mut slf = Self(Vec::new());
        slf.set();
        slf
    }
    pub fn set(&mut self) {
        self.0.push(ScopeLevel::default());
    }
    pub fn reset(&mut self) {
        self.0.pop().unwrap_or_else(||
            unreachable!("Called reset on level 0")
        );
    }
    pub fn def_fn(&mut self, name: Symbol, func: Weak<FunctionDecl>) {
        self.0.last_mut().unwrap().functions.insert(name, func);
    }
    pub fn def_var(&mut self, name: Symbol, variable: Weak<VariableDecl>) {
        self.0.last_mut().unwrap().variables.insert(name, variable);
    }
    pub fn get_func(&self, sym: &Symbol) -> Option<&Weak<FunctionDecl>> {
        for lev in self.0.iter().rev() {
            let val = lev.functions.get(sym);
            if val.is_some() { return val }
        }
        None
    }
    pub fn get_var(&self, sym: &Symbol) -> Option<&Weak<VariableDecl>> {
        for lev in self.0.iter().rev() {
            let val = lev.variables.get(sym);
            if val.is_some() { return val }
        }
        None
    }
}
