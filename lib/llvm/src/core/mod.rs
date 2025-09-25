use core::ffi::c_int;
use core::marker::PhantomData;

use crate::ffi::{LLVMAppendBasicBlockInContext, LLVMAppendExistingBasicBlock, LLVMArrayType, LLVMBasicBlockRef, LLVMConstInt, LLVMConstIntGetZExtValue, LLVMConstReal, LLVMCountParams, LLVMCreateBasicBlockInContext, LLVMDoubleTypeInContext, LLVMFloatTypeInContext, LLVMFunctionType, LLVMGetParam, LLVMGetTypeKind, LLVMInt1TypeInContext, LLVMInt32TypeInContext, LLVMIntTypeInContext, LLVMIsConstant, LLVMSetValueName, LLVMSizeOf, LLVMStructCreateNamed, LLVMStructSetBody, LLVMTypeKind, LLVMTypeOf, LLVMTypeRef, LLVMValueRef, LLVMVoidTypeInContext};
use crate::Context;

mod module;
pub use module::{Module, Global};

mod builder;
pub use builder::Builder;


#[repr(transparent)]
/* ^ Needed so we cast *mut Type to *mut LLVMTypeRef
 * to avoid allocating temporary arrays */
#[derive(Clone, Copy, Debug)]
pub struct Type<'ctx>(LLVMTypeRef, PhantomData<&'ctx Context>);

impl<'ctx> Type<'ctx> {
    pub fn int(n_bytes: u32, ctx: &'ctx Context) -> Self {
        Self(unsafe { LLVMIntTypeInContext(ctx.raw, n_bytes) }, PhantomData)
    }

    pub fn int_32(ctx: &'ctx Context) -> Self {
        Self(unsafe { LLVMInt32TypeInContext(ctx.raw) }, PhantomData)
    }

    pub fn int_1(ctx: &'ctx Context) -> Self {
        Self(unsafe { LLVMInt1TypeInContext(ctx.raw) }, PhantomData)
    }

    pub fn float_32(ctx: &'ctx Context) -> Self {
        Self(unsafe { LLVMFloatTypeInContext(ctx.raw) }, PhantomData)
    }

    pub fn float_64(ctx: &'ctx Context) -> Self {
        Self(unsafe { LLVMDoubleTypeInContext(ctx.raw) }, PhantomData)
    }

    pub fn void(ctx: &'ctx Context) -> Self {
        Self(unsafe { LLVMVoidTypeInContext(ctx.raw)}, PhantomData)
    }

    pub fn array(ty: Type<'ctx>, len: u32) -> Self {
        Self(unsafe {
            LLVMArrayType(ty.0, len)
        }, PhantomData)
    }

    pub fn function(
        ret_ty: Type<'ctx>,
        param_tys: &mut [Type<'ctx>],
        is_variadic: bool,
    ) -> Self {
        let len = param_tys.len() as u32;
        Self(unsafe {
          LLVMFunctionType(
              ret_ty.0,
              param_tys.as_mut_ptr() as *mut _,
              len,
              is_variadic as c_int
        )},
        PhantomData)
    }

    pub fn struct_named(
        name: &str,
        types: &mut [Type<'ctx>],
        packed: bool,
        ctx: &'ctx Context
    ) -> Self
    {
        cstr!(name);
        unsafe {
            let sty = LLVMStructCreateNamed(ctx.raw, name);
            let count = types.len() as u32;
            let types = types.as_mut_ptr().cast();
            LLVMStructSetBody(sty, types, count, packed as i32);
            Self(sty, PhantomData)
        }
    }

    pub fn size_of(&self) -> Value<'ctx> {
        unsafe { Value(LLVMSizeOf(self.0), self.1) }
    }

    pub (crate) fn kind(&self) -> LLVMTypeKind {
        unsafe { LLVMGetTypeKind(self.0) }
    }

    pub fn raw(&self) -> LLVMTypeRef { self.0 }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Value<'ctx>(LLVMValueRef, PhantomData<&'ctx Context>);

impl<'ctx> Value<'ctx> {

    pub fn is_constant(&self) -> bool {
        unsafe { LLVMIsConstant(self.0) != 0 }
    }

    pub fn as_const_int(&self) -> Option<u64> {
        if !matches!(self.get_type().kind(), LLVMTypeKind::LLVMIntegerTypeKind) {
            return None
        }
        unsafe {
            self.is_constant().then(|| {
                LLVMConstIntGetZExtValue(self.0)
            })
        }
    }

    pub fn const_int(ty: Type<'ctx>, val: u64) -> Self {
        unsafe {
            Self(LLVMConstInt(ty.0, val, 1), ty.1)
        }
    }

    pub fn const_uint(ty: Type<'ctx>, val: u64) -> Self {
        unsafe {
            Self(LLVMConstInt(ty.0, val, 0), ty.1)
        }
    }

    pub fn const_float(ty: Type<'ctx>, val: f64) -> Self {
        unsafe {
            Self(LLVMConstReal(ty.0, val), ty.1)
        }
    }

    pub fn const_int1(val: u64, ctx: &'ctx Context) -> Self {
        Self::const_int(Type::int_1(ctx), val)
    }

    pub fn const_int32(val: u64, ctx: &'ctx Context) -> Self {
        Self::const_int(Type::int_32(ctx), val)
    }

    pub fn const_f64(val: f64, ctx: &'ctx Context) -> Self {
        Self::const_float(Type::float_64(ctx), val)
    }

    pub fn const_f32(val: f32, ctx: &'ctx Context) -> Self {
        Self::const_float(Type::float_32(ctx), val as f64)
    }

    pub fn set_name(&mut self, name: &str) {
        cstr!(name);
        unsafe { LLVMSetValueName(self.0, name); }
    }

    pub fn get_type(&self) -> Type<'ctx> {
        unsafe {
            Type(LLVMTypeOf(self.0), self.1)
        }
    }

    pub fn raw(&self) -> LLVMValueRef { self.0 }
}

pub struct Function<'ctx>(Value<'ctx>, &'ctx Context);

impl<'ctx> Function<'ctx> {
    pub fn as_value(&self) -> &Value<'ctx> {
        &self.0
    }

    pub fn param(&self, idx: u32) -> Value<'ctx> {
        Value(unsafe { LLVMGetParam(self.0.0, idx) }, self.0.1)
    }

    pub fn n_params(&self) -> u32 {
        unsafe { LLVMCountParams(self.0.0) }
    }

    pub fn append_basic_block(&mut self, name: &str) -> BasicBlock<'ctx> {
        cstr!(name);
        BasicBlock(unsafe { LLVMAppendBasicBlockInContext(self.1.raw, self.0.0, name)}, self.1)
    }

    pub fn append_existing_basic_block(&mut self, b: &BasicBlock) {
        unsafe { LLVMAppendExistingBasicBlock(self.0.0, b.0)}
    }
}

#[derive(Clone, Copy)]
pub struct BasicBlock<'ctx>(LLVMBasicBlockRef, &'ctx Context);

impl<'ctx> BasicBlock<'ctx> {
    pub fn new(name: &str, ctx: &'ctx Context) -> Self {
        cstr!(name);
        BasicBlock(unsafe {
            LLVMCreateBasicBlockInContext(ctx.raw, name)
        }, ctx)
    }

    pub fn get_context(&self) -> &'ctx Context { self.1 }
}

