#![allow(
    clippy::cast_lossless,
    clippy::cast_possible_truncation,
    clippy::needless_pass_by_value,
    clippy::ptr_cast_constness,
)]

#[allow(
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    clippy::pedantic,
)]
/// FFI bindings to the LLVM C API
pub mod ffi;

macro_rules! cstr {
    ($s:ident) => {
        let __tmp = ::std::ffi::CString::new($s)
                           .expect("No null bytes should exist");
        let $s = __tmp.as_c_str().as_ptr() as _;
    };
}

pub mod core;
pub use core::{Builder, Value, Type, Module, BasicBlock};

use crate::ffi::{LLVMContextCreate, LLVMContextDispose, LLVMContextRef, LLVMGetGlobalContext};

pub mod analysis;
pub mod bitwriter;
pub mod linker;

#[derive(Debug)]
pub struct Context {
    pub raw: LLVMContextRef,
}

impl Context {

    pub fn global() -> Self {
        Self::from(unsafe { LLVMGetGlobalContext() })
    }

    pub fn new() -> Self {
        Self { raw: unsafe { LLVMContextCreate() } }
    }

    pub fn create_builder(&self) -> Builder<'_> {
        Builder::new(self)
    }

    pub fn create_module(&self, name: &str) -> Module<'_> {
        Module::new(name, self)
    }

    pub fn create_basic_block(&self, name: &str) -> BasicBlock<'_> {
        BasicBlock::new(name, self)
    }
}

impl From<LLVMContextRef> for Context {
    fn from(value: LLVMContextRef) -> Self {
        Self { raw: value }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe { LLVMContextDispose(self.raw); }
    }
}
