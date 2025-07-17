use core::ffi::{c_char, CStr};
use core::fmt::Display;
use core::ptr;
use std::ffi::CString;

use super::{Function, Type, Value};
use crate::ffi::{LLVMAddFunction, LLVMDisposeMessage, LLVMDisposeModule, LLVMModuleCreateWithName, LLVMModuleRef, LLVMPrintModuleToFile, LLVMPrintModuleToString, LLVMTypeKind};

pub struct Module {
    raw: LLVMModuleRef,
}

impl Module {
    pub fn new(name: &str) -> Self {
        cstr!(name);
        let raw = unsafe {
            LLVMModuleCreateWithName(name)
        };
        Self { raw }
    }

    /// Adds a function to this module, with the given `name`.
    ///
    /// `func_ty` must be an [`LLVMFunctionTypeKind`]
    ///
    /// [`LLVMFunctionTypeKind`]: LLVMTypeKind::LLVMFunctionTypeKind
    pub fn add_function(&mut self, name: &str, func_ty: Type) -> Function {
        debug_assert_eq!(
            func_ty.kind(),
            LLVMTypeKind::LLVMFunctionTypeKind,
            "Non-function type passed to Module::add_function",
        );
        cstr!(name);
        Function(Value(unsafe {
            LLVMAddFunction(self.raw, name, func_ty.0)
        }))
    }

    /// Gets the raw LLVM module reference
    #[inline]
    pub (crate) fn as_raw(&self) -> LLVMModuleRef { self.raw }

    /// Prints `self` to the given `path`
    pub fn print(&self, path: &str) -> Result<(), String> {
        cstr!(path);
        let mut err: *mut c_char = ptr::null_mut();
        let ret = unsafe { LLVMPrintModuleToFile(self.raw, path, &raw mut err) };

        let ret = if ret == 0 {
            Ok(())
        } else {
            let msg = unsafe { CStr::from_ptr(err) }.to_str().unwrap().to_string();
            Err(msg)
        };

        unsafe { LLVMDisposeMessage(err); }

        ret
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            let module_str = LLVMPrintModuleToString(self.raw);
            let s = CStr::from_ptr(module_str).to_str().unwrap();
            write!(f, "{s}")?;
            LLVMDisposeMessage(module_str);
            Ok(())
        }
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        unsafe { LLVMDisposeModule(self.raw); }
    }
}

