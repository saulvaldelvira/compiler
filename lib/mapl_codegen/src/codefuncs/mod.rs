use crate::{code_generator::CodeGenerator, mir::MaplInstruction};

mod address;
mod define;
mod eval;
mod execute;
mod metadata;

pub trait Execute {
    fn execute(&self, cg: &mut CodeGenerator<'_, '_, '_>) -> MaplInstruction;
}

pub trait Address {
    fn address(&self, cg: &mut CodeGenerator<'_, '_, '_>) -> MaplInstruction;
}

pub trait Eval {
    fn eval(&self, cg: &mut CodeGenerator<'_, '_, '_>) -> MaplInstruction;
}

pub trait Define {
    fn define(&self, cg: &mut CodeGenerator<'_, '_, '_>) -> MaplInstruction;
}

pub trait Metadata {
    fn metadata(&self, cg: &mut CodeGenerator<'_, '_, '_>) -> MaplInstruction;
}
