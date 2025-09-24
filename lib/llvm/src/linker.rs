use crate::ffi::LLVMLinkModules2;
use crate::Module;

impl<'ctx> Module<'ctx> {
    pub fn link(&mut self, other: Module<'ctx>) -> bool {
        let ret = unsafe {
            LLVMLinkModules2(self.as_raw(), other.as_raw())
        };
        ret != 0
    }
}
