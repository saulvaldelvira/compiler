use std::ops::Deref;
use std::rc::Rc;

use ast::declaration::{DeclarationKind, FunctionDecl, MemoryAddress, VariableDecl};
use ast::stmt::ReturnStmt;
use ast::types::TypeKind;
use ast::{Declaration, Program};

use super::{get_last_variable_decl, Define, Eval, Execute, MaplCodeGenerator};

impl Define for Rc<FunctionDecl> {
    fn define(&self, cg: &mut MaplCodeGenerator) {
        session::with_symbol(self.name, |name| {
            cg.base.write_fmt(format_args!("{name}:"));
        });

        get_last_variable_decl(self).inspect(|vd| {
            let addr = vd.address.unwrap();
            let MemoryAddress::Relative(addr) = addr.deref() else { unreachable!() };
            cg.base.write_fmt(format_args!("enter {}", -addr));
        });

        let f = cg.current_function.take();
        cg.current_function = Some(Rc::clone(self));

        for stmt in &self.body.stmts {
            stmt.execute(cg);
        }

        if matches!(self.return_type.kind, TypeKind::Empty) {
            ReturnStmt { expr: None }.execute(cg);
        }

        cg.current_function = f;

    }
}

impl Define for Program {
    fn define(&self, cg: &mut MaplCodeGenerator) {
        self.decls.iter()
            .filter_map(|d| {
                let Declaration { kind: DeclarationKind::Variable(v), .. } = d else { return None };
                Some(v)
            }).for_each(|v| {
                v.define(cg);
            });

        cg.base.write("call main\nhalt");

        self.decls.iter()
            .filter_map(|d| {
                let Declaration { kind: DeclarationKind::Function(f), .. } = d else { return None };
                Some(f)
            }).for_each(|f| {
                f.define(cg);
            });
    }
}

impl Define for Declaration {
    fn define(&self, cg: &mut MaplCodeGenerator) {
        match &self.kind {
            DeclarationKind::Variable(variable_decl) => variable_decl.define(cg),
            DeclarationKind::Function(function_decl) => function_decl.define(cg),
        }
    }
}

impl Define for VariableDecl {
    fn define(&self, cg: &mut MaplCodeGenerator) {
        if let Some(init) = &self.init {
            cg.pushaddr(&self.address.unwrap());
            init.eval(cg);
            cg.sufixed_op("STORE", &self.ty.unwrap());
        }
    }
}
