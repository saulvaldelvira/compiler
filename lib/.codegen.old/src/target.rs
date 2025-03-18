use crate::code_generator::CodeGenerator;
use crate::memory::SizeStrategy;

pub trait Target {
    type CodeGenerator: CodeGenerator;
    type SizeStrategy: SizeStrategy;
}

use crate::memory::MaplSizeStrategy;
use crate::code_generator::MaplCodeGenerator;

pub struct MaplTarget;
impl Target for MaplTarget {
    type CodeGenerator = MaplCodeGenerator;

    type SizeStrategy = MaplSizeStrategy;
}
