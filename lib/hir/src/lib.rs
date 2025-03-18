pub mod hir;
pub mod hir_id;
use std::cell::Cell;

use hir::def::Field;
pub use hir_id::HirId;

pub mod visitor;

pub use hir::*;

arena::define_arenas!(
    expr : Expression<'ctx>,
    defs: Definition<'ctx>,
    types: Type<'ctx>,
    fields: Field<'ctx>,
    stmts: Statement<'ctx>,
    prog: Program<'ctx>
);

pub struct Session<'hir> {
    root: Cell<Option<&'hir Program<'hir>>>,
    arena: Arena<'hir>,
}

impl<'hir> Session<'hir> {
    pub fn new() -> Self {
        Session {
            arena: Arena::new(),
            root: Cell::new(None),
        }
    }

    pub fn with_arena<T>(&self, f: impl FnOnce(&Arena<'hir>) -> T) -> T{
        f(&self.arena)
    }

    pub fn set_root(&self, prog: &'hir Program<'hir>) {
        self.root.set(Some(prog));
    }

    pub fn get_root_program(&self) -> &Program<'hir> {
        self.root.get().unwrap()
    }

}

impl<'hir> Session<'hir> {
    pub fn alloc<T>(&'hir self, val: T) -> &'hir mut T
    where
        T: ArenaAllocable<'hir>
    {
        val.alloc_into(&self.arena)
    }
}


