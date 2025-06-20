use core::ffi::c_char;

use crate::core::{BasicBlock, Value};
use crate::ffi::{LLVMBuildAdd, LLVMBuildMul, LLVMBuildRet, LLVMBuildSub, LLVMBuilderRef, LLVMCreateBuilder, LLVMPositionBuilderAtEnd, LLVMValueRef};

pub struct Builder {
    raw: LLVMBuilderRef,
}

impl Builder {
    pub fn new() -> Self {
        Self { raw: unsafe { LLVMCreateBuilder() } }
    }

    pub fn position_at_end(&mut self, block: &mut BasicBlock) {
        unsafe { LLVMPositionBuilderAtEnd(self.raw, block.0); }
    }

    pub fn binop(&mut self,
        left: Value,
        right: Value,
        name: &str,
        f: unsafe extern "C" fn (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, *const c_char) -> LLVMValueRef,
    ) -> Value {
        cstr!(name);
        Value(unsafe {
            f(self.raw, left.0, right.0, name)
        })
    }

    pub fn add(&mut self, left: Value, right: Value, name: &str) -> Value {
        self.binop(left, right, name, LLVMBuildAdd)
    }

    pub fn sub(&mut self, left: Value, right: Value, name: &str) -> Value {
        self.binop(left, right, name, LLVMBuildSub)
    }

    pub fn mul(&mut self, left: Value, right: Value, name: &str) -> Value {
        self.binop(left, right, name, LLVMBuildMul)
    }

    pub fn ret(&mut self, val: Value) -> Value {
        Value(unsafe { LLVMBuildRet(self.raw, val.0) })
    }
}

impl Default for Builder {
    fn default() -> Self {
        Self::new()
    }
}
