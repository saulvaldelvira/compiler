use core::ffi::c_int;

use crate::ffi::{LLVMAppendBasicBlock, LLVMBasicBlockRef, LLVMBuildLoad2, LLVMConstInt, LLVMConstReal, LLVMCountParams, LLVMDoubleType, LLVMFloatType, LLVMFunctionType, LLVMGetParam, LLVMGetTypeKind, LLVMInt1Type, LLVMInt32Type, LLVMInt8Type, LLVMSetValueName, LLVMTypeKind, LLVMTypeOf, LLVMTypeRef, LLVMValueRef, LLVMVoidType};

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

    pub fn int_1() -> Self {
        Self(unsafe { LLVMInt1Type() })
    }

    pub fn float_32() -> Self {
        Self(unsafe { LLVMFloatType() })
    }

    pub fn float_64() -> Self {
        Self(unsafe { LLVMDoubleType() })
    }

    pub fn void() -> Self {
        Self(unsafe { LLVMVoidType()})
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

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Value(LLVMValueRef);

impl Value {

    pub fn const_int(ty: Type, val: u64) -> Self {
        unsafe {
            Self(LLVMConstInt(ty.0, val, 1))
        }
    }

    pub fn const_uint(ty: Type, val: u64) -> Self {
        unsafe {
            Self(LLVMConstInt(ty.0, val, 0))
        }
    }

    pub fn const_float(ty: Type, val: f64) -> Self {
        unsafe {
            Self(LLVMConstReal(ty.0, val))
        }
    }

    pub fn const_int1(val: u64) -> Self {
        Self::const_int(Type::int_1(), val)
    }

    pub fn const_int32(val: u64) -> Self {
        Self::const_int(Type::int_32(), val)
    }

    pub fn const_f64(val: f64) -> Self {
        Self::const_float(Type::float_64(), val)
    }

    pub fn const_f32(val: f32) -> Self {
        Self::const_float(Type::float_32(), val as f64)
    }

    pub fn set_name(&mut self, name: &str) {
        cstr!(name);
        unsafe { LLVMSetValueName(self.0, name); }
    }

    pub fn get_type(&self) -> Type {
        unsafe {
            Type(LLVMTypeOf(self.0))
        }
    }
}

pub struct Function(Value);

impl Function {
    pub fn into_value(self) -> Value {
        self.0
    }

    pub fn param(&self, idx: u32) -> Value {
        Value(unsafe { LLVMGetParam(self.0.0, idx) })
    }

    pub fn n_params(&self) -> u32 {
        unsafe { LLVMCountParams(self.0.0) }
    }

    pub fn append_basic_block(&mut self, name: &str) -> BasicBlock {
        cstr!(name);
        BasicBlock(unsafe { LLVMAppendBasicBlock(self.0.0, name)})
    }
}

pub struct BasicBlock(LLVMBasicBlockRef);

