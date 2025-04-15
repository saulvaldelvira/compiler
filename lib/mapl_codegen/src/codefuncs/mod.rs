use crate::code_generator::CodeGenerator;
use crate::mir::MaplInstruction;

mod execute;
mod eval;
mod address;
mod define;
mod metadata;

pub trait Execute {
    fn execute(&self, cg: &mut CodeGenerator<'_,'_,'_>) -> MaplInstruction;
}

pub trait Address {
    fn address(&self, cg: &mut CodeGenerator<'_,'_,'_>) -> MaplInstruction;
}

pub trait Eval {
    fn eval(&self, cg: &mut CodeGenerator<'_,'_,'_>) -> MaplInstruction;
}

pub trait Define {
    fn define(&self, cg: &mut CodeGenerator<'_,'_,'_>) -> MaplInstruction;
}

pub trait Metadata {
    fn metadata(&self, cg: &mut CodeGenerator<'_,'_,'_>) -> MaplInstruction;
}
