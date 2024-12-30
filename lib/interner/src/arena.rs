use core::{slice, str};
use std::alloc;
use std::alloc::Layout;
use std::ptr::{self,NonNull};

pub struct Arena {
    buf: NonNull<u8>,
    len: usize,
    cap: usize,
}

#[derive(Clone,Copy,Debug,Hash,Eq,PartialEq)]
pub struct Span {
    pub offset: usize,
    pub len: usize,
}

impl Arena {
    pub fn new() -> Self {
        Self {
            buf: NonNull::dangling(),
            len: 0,
            cap: 0,
        }
    }
    fn resize(&mut self) {
        let old_layout = Layout::array::<u8>(self.cap).unwrap();
        let old_cap = self.cap;
        if self.cap == 0 {
            self.cap = 8;
        } else {
            self.cap *= 2;
        }
        let new_layout = Layout::array::<u8>(self.cap).unwrap();

        let ptr =
        if old_cap == 0 {
            unsafe { alloc::alloc(new_layout) }
        } else {
            unsafe {
                alloc::realloc(self.buf.as_ptr(), old_layout, new_layout.size())
            }
        };
        self.buf = NonNull::new(ptr).unwrap();
    }
    pub fn alloc(&mut self, src: &str) -> Span {
        let rem = self.cap - self.len;
        if rem < src.len() {
            self.resize();
        }

        let offset = self.len;

        unsafe {
            ptr::copy_nonoverlapping(
                src.as_bytes().as_ptr(),
                self.buf.as_ptr().add(offset),
                src.len());
        }

        self.len += src.len();

        Span {
            offset,
            len: src.len()
        }

    }

    pub fn get(&self, span: Span) -> Option<&'static str> {
        if span.offset + span.len > self.len {
            return None
        }
        let start = unsafe { self.buf.add(span.offset).as_ptr() };
        let slice = unsafe { slice::from_raw_parts(start, span.len) };
        Some(str::from_utf8(slice).unwrap())
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        let ptr = self.buf.as_ptr();
        let layout = Layout::array::<u8>(self.cap).unwrap();
        unsafe { alloc::dealloc(ptr, layout) };
    }
}
