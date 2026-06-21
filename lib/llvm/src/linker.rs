use core::mem::ManuallyDrop;

use crate::ffi::LLVMLinkModules2;
use crate::Module;

impl<'ctx> Module<'ctx> {
    pub fn link(&mut self, other: Module<'ctx>) -> bool {
        let md = ManuallyDrop::new(other);
        let ret = unsafe {
            LLVMLinkModules2(self.as_raw(), md.as_raw())
        };
        ret != 0
    }
}
