use core::ffi::{c_char, c_int};
use core::marker::PhantomData;

use crate::core::{BasicBlock, Value};
use crate::ffi::{LLVMBuildAdd, LLVMBuildAlloca, LLVMBuildAnd, LLVMBuildBitCast, LLVMBuildBr, LLVMBuildCall2, LLVMBuildCondBr, LLVMBuildFCmp, LLVMBuildFDiv, LLVMBuildFRem, LLVMBuildGEP2, LLVMBuildGlobalStringPtr, LLVMBuildICmp, LLVMBuildIntCast2, LLVMBuildLoad2, LLVMBuildMul, LLVMBuildNeg, LLVMBuildNot, LLVMBuildOr, LLVMBuildRet, LLVMBuildRetVoid, LLVMBuildSDiv, LLVMBuildSRem, LLVMBuildStore, LLVMBuildSub, LLVMBuildUDiv, LLVMBuildURem, LLVMBuildUnreachable, LLVMBuilderRef, LLVMCreateBuilderInContext, LLVMDisposeBuilder, LLVMIntPredicate, LLVMPositionBuilderAtEnd, LLVMRealPredicate, LLVMValueRef};
use crate::{Context, Type};

pub struct Builder<'ctx> {
    raw: LLVMBuilderRef,
    ctx: &'ctx Context,
}

impl<'ctx> Builder<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Self {
        Self { raw: unsafe { LLVMCreateBuilderInContext(ctx.raw) }, ctx }
    }

    pub fn position_at_end(&mut self, block: &mut BasicBlock) {
        unsafe { LLVMPositionBuilderAtEnd(self.raw, block.0); }
    }

    pub fn binop(&mut self,
        left: Value<'ctx>,
        right: Value<'ctx>,
        name: &str,
        f: unsafe extern "C" fn (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, *const c_char) -> LLVMValueRef,
    ) -> Value<'ctx> {
        cstr!(name);
        Value(unsafe {
            f(self.raw, left.0, right.0, name)
        }, PhantomData)
    }

    pub fn add(&mut self,
        left: Value<'ctx>,
        right: Value<'ctx>,
        name: &str
    ) -> Value<'ctx>
    {
        self.binop(left, right, name, LLVMBuildAdd)
    }

    pub fn sub(&mut self,
        left: Value<'ctx>,
        right: Value<'ctx>,
        name: &str
    ) -> Value<'ctx>
    {
        self.binop(left, right, name, LLVMBuildSub)
    }

    pub fn mul(&mut self,
        left: Value<'ctx>,
        right: Value<'ctx>,
        name: &str
    ) -> Value<'ctx>
    {
        self.binop(left, right, name, LLVMBuildMul)
    }

    pub fn and(&mut self,
        left: Value<'ctx>,
        right: Value<'ctx>,
        name: &str
    ) -> Value<'ctx>
    {
        self.binop(left, right, name, LLVMBuildAnd)
    }

    pub fn or(&mut self,
        left: Value<'ctx>,
        right: Value<'ctx>,
        name: &str
    ) -> Value<'ctx>
    {
        self.binop(left, right, name, LLVMBuildOr)
    }

    pub fn uint_div(&mut self,
        left: Value<'ctx>,
        right: Value<'ctx>,
        name: &str
    ) -> Value<'ctx>
    {
        self.binop(left, right, name, LLVMBuildUDiv)
    }

    pub fn sint_div(&mut self,
        left: Value<'ctx>,
        right: Value<'ctx>,
        name: &str
    ) -> Value<'ctx>
    {
        self.binop(left, right, name, LLVMBuildSDiv)
    }

    pub fn uint_rem(&mut self,
        left: Value<'ctx>,
        right: Value<'ctx>,
        name: &str
    ) -> Value<'ctx>
    {
        self.binop(left, right, name, LLVMBuildURem)
    }

    pub fn sint_rem(&mut self,
        left: Value<'ctx>,
        right: Value<'ctx>,
        name: &str
    ) -> Value<'ctx>
    {
        self.binop(left, right, name, LLVMBuildSRem)
    }

    pub fn fdiv(&mut self,
        left: Value<'ctx>,
        right: Value<'ctx>,
        name: &str
    ) -> Value<'ctx>
    {
        self.binop(left, right, name, LLVMBuildFDiv)
    }

    pub fn frem(&mut self,
        left: Value<'ctx>,
        right: Value<'ctx>,
        name: &str
    ) -> Value<'ctx>
    {
        self.binop(left, right, name, LLVMBuildFRem)
    }

    pub fn neg(&mut self,
        value: Value<'ctx>,
        name: &str
    ) -> Value<'ctx>
    {
        cstr!(name);
        Value(unsafe {
            LLVMBuildNeg(self.raw, value.0, name)
        }, PhantomData)
    }

    pub fn integer_cmp(&mut self,
        left: Value,
        right: Value,
        int_pred: LLVMIntPredicate,
        name: &str,
    ) -> Value<'ctx>
    {
        cstr!(name);
        Value(unsafe {
            LLVMBuildICmp(self.raw, int_pred, left.0, right.0, name)
        }, PhantomData)
    }

    pub fn real_cmp(&mut self,
        left: Value,
        right: Value,
        real_pred: LLVMRealPredicate,
        name: &str,
    ) -> Value<'ctx>
    {
        cstr!(name);
        Value(unsafe {
            LLVMBuildFCmp(self.raw, real_pred, left.0, right.0, name)
        }, PhantomData)
    }

    pub fn not(&mut self,
        value: Value<'ctx>,
        name: &str
    ) -> Value<'ctx>
    {
        cstr!(name);
        Value(unsafe {
            LLVMBuildNot(self.raw, value.0, name)
        }, PhantomData)
    }

    pub fn ret<Val>(&mut self, val: Val) -> Value<'ctx>
    where
        Val: Into<Option<Value<'ctx>>>
    {
        Value(unsafe {
            match val.into() {
                Some(val) => LLVMBuildRet(self.raw, val.0),
                None => LLVMBuildRetVoid(self.raw),
            }
        }, PhantomData)
    }

    pub fn call(&mut self,
        func_ty: Type<'ctx>,
        func: Value<'ctx>,
        args: &mut [Value<'ctx>],
        name: &str
    ) -> Value<'ctx>
    {
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
            ), PhantomData)
        }
    }

    pub fn alloca(&mut self, ty: Type<'ctx>, name: &str) -> Value<'ctx> {
        cstr!(name);
        unsafe {
            Value(LLVMBuildAlloca(self.raw, ty.0, name), PhantomData)
        }
    }

    pub fn load(&mut self,
        alloca: Value<'ctx>,
        ty: Type<'ctx>,
        name: &str
    ) -> Value<'ctx>
    {
        cstr!(name);
        unsafe {
            Value(LLVMBuildLoad2(self.raw, ty.0, alloca.0, name), PhantomData)
        }
    }

    pub fn store(&mut self, value: Value<'ctx>, ptr: Value<'ctx>) -> Value<'ctx> {
        unsafe {
            Value(LLVMBuildStore(self.raw, value.0, ptr.0), PhantomData)
        }
    }

    pub fn gep(&mut self,
        ty: Type<'ctx>,
        ptr: Value<'ctx>,
        indices: &mut [Value<'ctx>],
        name: &str
    ) -> Value<'ctx>
    {
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
            ), PhantomData)
        }
    }

    pub fn global_string_ptr(&mut self, content: &str, name: &str) -> Value<'ctx> {
        cstr!(content);
        cstr!(name);
        Value(unsafe {
            LLVMBuildGlobalStringPtr(self.raw, content, name)
        }, PhantomData)
    }

    pub fn bit_cast(&mut self, val: &Value<'ctx>, to: &Type<'ctx>, name: &str) -> Value<'ctx> {
        cstr!(name);
        unsafe { Value(LLVMBuildBitCast(self.raw, val.0, to.0, name), PhantomData) }
    }

    pub fn int_cast(&mut self, val: &Value<'ctx>, to: &Type<'ctx>, is_signed: bool, name: &str) -> Value<'ctx> {
        cstr!(name);
        unsafe { Value(LLVMBuildIntCast2(self.raw, val.0, to.0, is_signed as c_int, name), PhantomData) }
    }

    pub fn branch(&mut self, block: &mut BasicBlock) -> Value<'ctx> {
        Value(unsafe { LLVMBuildBr(self.raw, block.0) }, PhantomData)
    }

    pub fn cond_br(&mut self,
        val: Value<'ctx>,
        then: &BasicBlock,
        else_br: &BasicBlock
    ) -> Value<'ctx>
    {
        Value(unsafe {
            LLVMBuildCondBr(self.raw, val.0, then.0, else_br.0)
        }, PhantomData)
    }

    pub fn build_unreachable(&mut self) -> Value<'ctx> {
        Value(unsafe {
            LLVMBuildUnreachable(self.raw)
        }, PhantomData)
    }

    pub fn get_context(&self) -> &'ctx Context { self.ctx }
}

impl Drop for Builder<'_> {
    fn drop(&mut self) {
        unsafe { LLVMDisposeBuilder(self.raw); }
    }
}
