use core::ffi::{c_char, CStr};
use core::ptr;

use crate::core::{Function, Module};
use crate::ffi::{LLVMDisposeMessage, LLVMVerifierFailureAction, LLVMVerifyFunction, LLVMVerifyModule};

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

impl Module<'_> {
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

impl Function<'_> {
    pub fn verify(&mut self, action: VeryfierFailureAction) -> Result<(), i32> {
        let res = unsafe { LLVMVerifyFunction(self.as_value().raw(), action.into()) };
        if res == 0 {
            Ok(())
        } else {
            Err(res)
        }
    }
}
