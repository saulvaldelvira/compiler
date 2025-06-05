use core::ffi::c_int;

use crate::ffi::{LLVMAppendBasicBlock, LLVMBasicBlockRef, LLVMFunctionType, LLVMGetParam, LLVMGetTypeKind, LLVMInt32Type, LLVMTypeKind, LLVMTypeRef, LLVMValueRef};

mod module;
pub use module::Module;

mod builder;
pub use builder::Builder;

#[repr(transparent)]
pub struct Type(LLVMTypeRef);

impl Type {
    pub fn int_32() -> Self {
        Self(unsafe { LLVMInt32Type() })
    }

    pub fn function(
        ret_ty: Type,
        param_tys: &mut [Type],
        is_variadic: bool,
    ) -> Self {
        let len = param_tys.len() as u32;
        Self(unsafe {
          LLVMFunctionType(
              ret_ty.0,
              param_tys.as_mut_ptr() as *mut _,
              len,
              is_variadic as c_int
        )})
    }

    pub (crate) fn kind(&self) -> LLVMTypeKind {
        unsafe { LLVMGetTypeKind(self.0) }
    }
}

pub struct Value(LLVMValueRef);

pub struct Function(Value);

impl Function {
    pub fn param(&self, idx: u32) -> Value {
        Value(unsafe { LLVMGetParam(self.0.0, idx) })
    }

    pub fn append_basic_block(&mut self, name: &str) -> BasicBlock {
        cstr!(name);
        BasicBlock(unsafe { LLVMAppendBasicBlock(self.0.0, name)})
    }
}

pub struct BasicBlock(LLVMBasicBlockRef);

