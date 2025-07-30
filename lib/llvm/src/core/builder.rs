use core::ffi::c_char;

use crate::core::{BasicBlock, Value};
use crate::ffi::{LLVMBuildAdd, LLVMBuildAlloca, LLVMBuildAnd, LLVMBuildCall2, LLVMBuildGEP2, LLVMBuildICmp, LLVMBuildLoad2, LLVMBuildMul, LLVMBuildNeg, LLVMBuildNot, LLVMBuildOr, LLVMBuildRet, LLVMBuildRetVoid, LLVMBuildStore, LLVMBuildSub, LLVMBuilderRef, LLVMCreateBuilder, LLVMIntPredicate, LLVMPositionBuilderAtEnd, LLVMValueRef};
use crate::Type;

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

    pub fn and(&mut self, left: Value, right: Value, name: &str) -> Value {
        self.binop(left, right, name, LLVMBuildAnd)
    }

    pub fn or(&mut self, left: Value, right: Value, name: &str) -> Value {
        self.binop(left, right, name, LLVMBuildOr)
    }

    pub fn neg(&mut self, value: Value, name: &str) -> Value {
        cstr!(name);
        Value(unsafe {
            LLVMBuildNeg(self.raw, value.0, name)
        })
    }

    pub fn signed_integer_cmp(&mut self,
        left: Value,
        right: Value,
        int_pred: LLVMIntPredicate,
        name: &str,
    ) -> Value {
        cstr!(name);
        Value(unsafe {
            LLVMBuildICmp(self.raw, int_pred, left.0, right.0, name)
        })
    }

    pub fn not(&mut self, value: Value, name: &str) -> Value {
        cstr!(name);
        Value(unsafe {
            LLVMBuildNot(self.raw, value.0, name)
        })
    }

    pub fn ret(&mut self, val: impl Into<Option<Value>>) -> Value {
        Value(unsafe {
            match val.into() {
                Some(val) => LLVMBuildRet(self.raw, val.0),
                None => LLVMBuildRetVoid(self.raw),
            }
        })
    }

    pub fn call(&mut self, func_ty: Type, func: Value, args: &mut [Value], name: &str) -> Value {
        cstr!(name);
        unsafe {
            let len = args.len();
            let args = args.as_mut_ptr().cast();
            Value(LLVMBuildCall2(
                self.raw,
                func_ty.0,
                func.0,
                args,
                len as u32,
                name
            ))
        }
    }

    pub fn alloca(&mut self, ty: Type, name: &str) -> Value {
        cstr!(name);
        unsafe {
            Value(LLVMBuildAlloca(self.raw, ty.0, name))
        }
    }

    pub fn load(&mut self, alloca: Value, ty: Type, name: &str) -> Value {
        cstr!(name);
        unsafe {
            Value(LLVMBuildLoad2(self.raw, ty.0, alloca.0, name))
        }
    }

    pub fn store(&mut self, value: Value, ptr: Value) -> Value {
        unsafe {
            Value(LLVMBuildStore(self.raw, value.0, ptr.0))
        }
    }

    pub fn gep(&mut self, ty: Type, ptr: Value, indices: &mut [Value], name: &str) -> Value {
        cstr!(name);
        unsafe {
            let len = indices.len();
            let indices = indices.as_mut_ptr().cast();
            Value(LLVMBuildGEP2(
                self.raw,
                ty.0,
                ptr.0,
                indices,
                len as _,
                name
            ))
        }
    }
}

impl Default for Builder {
    fn default() -> Self {
        Self::new()
    }
}
