use ast::{Program, Visitor};
use error_manager::ErrorManager;
use memory::MemoryAllocation;
use size_strategy::SizeStrategy;

mod memory;
pub mod size_strategy;

pub fn assign_memory<S: SizeStrategy>(program: &Program) -> Result<(), ErrorManager> {
    let mut mem = MemoryAllocation::<S>::new();
    mem.visit_program(program);
    Ok(())
}
