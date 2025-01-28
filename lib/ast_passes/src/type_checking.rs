use std::fmt::Arguments;

use ast::types::{Type, TypeKind};
use ast::visitor::prelude::*;
use ast::Program;
use session::with_symbol;

#[derive(Default)]
pub struct TypeChecking {
    pub n_errors: u32,
}

impl TypeChecking {
    pub fn process(&mut self, prog: &Program) {
        self.visit_program(prog);
    }
    fn error(&mut self, args: Arguments<'_>) {
        self.n_errors += 1;
        eprintln!("{args}")
    }
}

impl Visitor<'_> for TypeChecking {
    type Result = ();

    fn visit_assignment(&mut self, a: &'_ ast::expr::AssignmentExpr) {
        let ltype = a.left.ty.unwrap();
        let rtype = a.right.ty.unwrap();
        if !rtype.eq(&ltype) {
            self.error(format_args!("Can't assign {rtype} to {ltype}"));
        }
    }

    fn visit_vardecl(&mut self, v: &'_ std::rc::Rc<ast::declaration::VariableDecl>) -> Self::Result {
        let tkind = with_symbol(v.name, |n| {
            TypeKind::from(n)
        });
        let ty = Type {
            span: Default::default(),
            kind: tkind
        };
        v.ty.set(ty);
        if let Some(ref init) = v.init {
            self.visit_expression(init);
        }
        Self::Result::output()
    }
}
