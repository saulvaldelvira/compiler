use std::marker::PhantomData;
use std::rc::Rc;

use ast::declaration::{DeclarationKind, FunctionDecl, MemoryAddress};
use ast::stmt::{DeclarationStmt, StatementKind};
use ast::visitor::{walk_function_decl, walk_program};
use ast::{Declaration, Program, Visitor};

use ast::types::Type;

mod mapl;
pub use mapl::MaplSizeStrategy;

pub trait SizeStrategy {
    const CALL_FRAME: usize;
    fn size_of(t: &Type) -> usize;
}

pub struct MemoryAllocation<S: SizeStrategy> {
    _marker: PhantomData<S>,
}

impl<S: SizeStrategy> MemoryAllocation<S> {
    pub fn new() -> Self {
        Self {
            _marker: PhantomData
        }
    }
}

pub fn assign_memory<S: SizeStrategy>(program: &Program) {
    let mut mem = MemoryAllocation::<S>::new();
    mem.visit_program(program);
}

impl<S: SizeStrategy> Visitor<'_> for MemoryAllocation<S> {
    type Result = ();

    fn visit_program(&mut self, prog: &'_ ast::Program) -> Self::Result {
        let mut addr = 0_usize;
        prog.decls.iter()
            .filter_map(|decl| {
                let DeclarationKind::Variable(v) = &decl.kind else { return None };
                Some(v)
            })
            .for_each(|vd| {
                vd.address.set(MemoryAddress::Absolute(addr as u16));
                addr += S::size_of(&vd.ty.unwrap());
        });
        walk_program(self, prog)
    }

    fn visit_function_decl(&mut self, f: &'_ Rc<FunctionDecl>) -> Self::Result {
        let mut addr = S::CALL_FRAME;

        f.args.iter().rev().for_each(|vdecl| {
            vdecl.address.set(MemoryAddress::Relative(addr as i16));
            addr += S::size_of(&vdecl.ty.unwrap());
        });

        let mut addr = 0_i16;

        f.body.stmts.iter().filter_map(|stmt| {
            let StatementKind::Decl(DeclarationStmt {
                inner: Declaration { kind: DeclarationKind::Variable(vd), ..}
            }) = &stmt.kind else { return None };
            Some(vd)
        })
        .for_each(|vd| {
                addr -= S::size_of(&vd.ty.unwrap()) as i16;
                vd.address.set(MemoryAddress::Relative(addr));
        });

        walk_function_decl(self, f)
    }

}
