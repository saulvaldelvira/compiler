#!/bin/sh

# Generates Rust bindings to the LLVM C API, using bindgen

libs="core bitwriter analysis"

clang_args="$(llvm-config --cflags --ldflags --system-libs --libs $libs)"

bindgen \
    --allowlist-file ".*llvm-c.*" \
    --merge-extern-blocks \
    --rustified-enum ".*" \
    wrapper.h \
    -o src/ffi.rs \
    -- $clang_args
