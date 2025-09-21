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
pub use core::{Builder, Value, Type, Module};

pub mod analysis;
pub mod bitwriter;
pub mod linker;
