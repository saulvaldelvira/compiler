use ast::Program;
use code_generator::gen_code;
use memory::assign_memory;

mod memory;
mod code_generator;

pub mod target;
pub use target::Target;

pub fn process<T: Target>(program: &Program) -> String {
    assign_memory::<T::SizeStrategy>(program);
    gen_code::<T::CodeGenerator>(program)
}
