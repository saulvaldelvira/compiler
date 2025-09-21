use core::ffi::{c_char, CStr};
use core::ptr;

use crate::core::Module;
use crate::ffi::{LLVMDisposeMessage, LLVMVerifierFailureAction, LLVMVerifyModule};

#[derive(Clone, Copy)]
pub enum VeryfierFailureAction {
    AbortProcess,
    Print,
}

impl From<VeryfierFailureAction> for LLVMVerifierFailureAction {
    fn from(value: VeryfierFailureAction) -> Self {
        match value {
            VeryfierFailureAction::AbortProcess => LLVMVerifierFailureAction::LLVMAbortProcessAction,
            VeryfierFailureAction::Print => LLVMVerifierFailureAction::LLVMPrintMessageAction,
        }
    }
}

impl Module {
    pub fn verify(&mut self, action: VeryfierFailureAction) -> Result<(), String> {
        let mut msg: *mut c_char = ptr::null_mut();
        let res = unsafe { LLVMVerifyModule(self.as_raw(), action.into(), &raw mut msg) };

        let ret = if res == 0 {
            Ok(())
        } else {
            unsafe {
                let s = CStr::from_ptr(msg);
                let s = s.to_str().unwrap().to_string();
                Err(s)
            }
        };

        unsafe { LLVMDisposeMessage(msg); }

        ret
    }
}

