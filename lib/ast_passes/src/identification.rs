use core::fmt;
use std::collections::HashMap;
use std::rc::Rc;

use ast::declaration::{FunctionDecl, VariableDecl};
use ast::expr::{CallExpr, VariableExpr};
use ast::visitor::{walk_function_decl, walk_program};
use ast::{Program, Visitor};
use session::Symbol;

enum Pass {
    Discover,
    Global,
    Link,
    Unset
}

pub struct Identification {
    scopes: Scopes,
    n_errors: usize,
    pass: Pass,
}

impl Identification {
    pub fn new() -> Self {
        Identification {
            scopes: Scopes::new(),
            n_errors: 0,
            pass: Pass::Unset
        }
    }

    pub fn n_errors(&self) -> usize {
        self.n_errors
    }

    fn error_fmt(&mut self, msg: fmt::Arguments<'_>) {
        self.n_errors += 1;
        eprintln!("IDENTIFICATION: {msg}");
    }

    pub fn process(&mut self, program: &Program) {
        self.pass = Pass::Global;
        self.visit_program(program);
        if self.n_errors > 0 { return }

        self.pass = Pass::Discover;
        self.visit_program(program);
        if self.n_errors > 0 { return }

        self.pass = Pass::Link;
        self.visit_program(program);
    }
}

impl<'ast> Visitor<'ast> for Identification {
    type Result = ();

    fn visit_program(&mut self, prog: &'ast Program) {
       if matches!(self.pass, Pass::Unset) {
           panic!("Fatal Error!: Don't call Identification::visit_program, call Identification::process!");
       } else {
           walk_program(self, prog)
       }
    }

    fn visit_vardecl(&mut self, v: &'ast Rc<VariableDecl>) {
        if !matches!(self.pass, Pass::Link) { return }
        self.scopes.def_var(v.name, Rc::clone(v));


    }

    fn visit_variable_expr(&mut self, v: &'ast VariableExpr) {
        if !matches!(self.pass, Pass::Link) { return }
        match self.scopes.get_var(&v.name) {
            Some(decl) => v.decl.set(Rc::clone(decl)),
            None => self.error_fmt(format_args!("Undefined variable \"{:?}\"", v.name)),
        }
    }

    fn visit_function_decl(&mut self, f: &'ast Rc<FunctionDecl>) {
        self.scopes.def_fn(f.name, Rc::clone(f));
        if matches!(self.pass, Pass::Global) { return }
        self.scopes.set();
        walk_function_decl(self, f);
        self.scopes.reset();
    }

    fn visit_call(&mut self, call: &'ast CallExpr) {
        if !matches!(self.pass, Pass::Link) { return }

        match self.scopes.get_func(&call.callee) {
            Some(decl) => {
                call.decl.set(Rc::clone(decl));
            },
            None => self.error_fmt(format_args!("Undefined function \"{:?}\"", call.callee)),
        }
    }
}

#[derive(Default)]
pub struct ScopeLevel {
    variables: HashMap<Symbol,Rc<VariableDecl>>,
    functions: HashMap<Symbol,Rc<FunctionDecl>>,
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
    pub fn def_fn(&mut self, name: Symbol, func: Rc<FunctionDecl>) {
        self.0.last_mut().unwrap().functions.insert(name, func);
    }
    pub fn def_var(&mut self, name: Symbol, variable: Rc<VariableDecl>) {
        self.0.last_mut().unwrap().variables.insert(name, variable);
    }
    pub fn get_func(&self, sym: &Symbol) -> Option<&Rc<FunctionDecl>> {
        for lev in self.0.iter().rev() {
            let val = lev.functions.get(sym);
            if val.is_some() { return val }
        }
        None
    }
    pub fn get_var(&self, sym: &Symbol) -> Option<&Rc<VariableDecl>> {
        for lev in self.0.iter().rev() {
            let val = lev.variables.get(sym);
            if val.is_some() { return val }
        }
        None
    }
}
