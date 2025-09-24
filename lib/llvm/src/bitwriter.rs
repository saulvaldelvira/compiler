use crate::core::Module;
use crate::ffi::LLVMWriteBitcodeToFile;


impl Module<'_> {
    pub fn write_to_file(&self, path: &str) -> Result<(), i32> {
       cstr!(path);
       let ret = unsafe { LLVMWriteBitcodeToFile(self.as_raw(), path) };
       if ret != 0 {
           Err(ret as _)
       } else {
           Ok(())
       }
    }
}
