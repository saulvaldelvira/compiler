use core::ffi::{c_char, CStr};
use core::fmt::Display;
use core::marker::PhantomData;
use core::ptr;

use super::{Function, Type, Value};
use crate::ffi::{LLVMAddFunction, LLVMDisposeMessage, LLVMDisposeModule, LLVMGetNamedFunction, LLVMModuleCreateWithNameInContext, LLVMModuleRef, LLVMPrintModuleToFile, LLVMPrintModuleToString, LLVMTypeKind};
use crate::Context;

pub struct Module<'ctx> {
    raw: LLVMModuleRef,
    ctx: &'ctx Context,
}

impl<'ctx> Module<'ctx> {
    pub fn new(name: &str, ctx: &'ctx Context) -> Self {
        cstr!(name);
        let raw = unsafe {
            LLVMModuleCreateWithNameInContext(name, ctx.raw)
        };
        Self { raw, ctx }
    }

    /// Adds a function to this module, with the given `name`.
    ///
    /// `func_ty` must be an [`LLVMFunctionTypeKind`]
    ///
    /// [`LLVMFunctionTypeKind`]: LLVMTypeKind::LLVMFunctionTypeKind
    pub fn add_function(&mut self, name: &str, func_ty: Type<'ctx>) -> Function<'ctx> {
        debug_assert_eq!(
            func_ty.kind(),
            LLVMTypeKind::LLVMFunctionTypeKind,
            "Non-function type passed to Module::add_function",
        );
        cstr!(name);
        Function(Value(unsafe {
            LLVMAddFunction(self.raw, name, func_ty.0)
        }, PhantomData), self.ctx)
    }

    pub fn get_function(&self, name: &str) -> Option<Function<'ctx>> {
        cstr!(name);
        let ptr = unsafe { LLVMGetNamedFunction(self.raw, name) };
        (!ptr.is_null()).then_some(Function(Value(ptr, PhantomData), self.ctx))
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

impl Display for Module<'_> {
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

impl Drop for Module<'_> {
    fn drop(&mut self) {
        unsafe { LLVMDisposeModule(self.raw); }
    }
}

