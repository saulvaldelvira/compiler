use std::ops::Deref;

use ast::declaration::{DeclarationKind, FunctionDecl, MemoryAddress};
use ast::{Declaration, Program};

use super::{Define, Execute, MaplCodeGenerator};

impl Define for FunctionDecl {
    fn define(&self, cg: &mut MaplCodeGenerator) {
        session::with_symbol(self.name, |name| {
            cg.0.write_fmt(format_args!("{name}:\n"));
        });
        if let Some(last) = self.args.last() {
            let addr = last.address.unwrap();
            let MemoryAddress::Relative(addr) = addr.deref() else { unreachable!() };
            cg.0.write_fmt(format_args!("enter {}", -addr));
        }

        for stmt in &self.body.stmts {
            stmt.execute(cg);
        }
        cg.0.write("ret\n");
    }
}

impl Define for Program {
    fn define(&self, cg: &mut MaplCodeGenerator) {
        cg.0.write("call main\nhalt\n");
        for decl in &self.decls {
            decl.define(cg);
        }
    }
}

impl Define for Declaration {
    fn define(&self, cg: &mut MaplCodeGenerator) {
        match &self.kind {
            DeclarationKind::Variable(_variable_decl) => todo!(),
            DeclarationKind::Function(function_decl) => function_decl.define(cg),
        }
    }
}

