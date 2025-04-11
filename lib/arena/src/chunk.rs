use std::mem::{self, MaybeUninit};
use std::ptr::{slice_from_raw_parts_mut, NonNull};

pub (crate) struct ArenaChunk<T> {
    elements: NonNull<[MaybeUninit<T>]>,
    len: usize,
}

impl<T> ArenaChunk<T> {
    pub fn new(len: usize) -> Self {
        let array = Box::new_uninit_slice(len);
        let elements = Box::into_raw(array);
        let elements = unsafe { NonNull::new_unchecked(elements) };
        Self {
            elements,
            len: 0,
        }
    }

    pub fn capacity(&self) -> usize {
        self.elements.len()
    }

    #[inline(always)]
    pub fn start(&mut self) -> *mut T {
        self.elements.as_ptr() as *mut T
    }

    #[inline(always)]
    pub fn end(&mut self) -> *mut T {
        unsafe {
            if size_of::<T>() == 0 {
                core::ptr::without_provenance_mut(!0)
            } else {
                self.start().add(self.elements.len())
            }
        }

    }

    pub fn add_len(&mut self, len: usize) {
        self.len += len;
    }

    pub fn can_alloc(&self, ammount: usize) -> bool {
        self.len + ammount <= unsafe { self.elements.as_ref().len() }
    }
}

impl<T> Drop for ArenaChunk<T> {
    fn drop(&mut self) {
        let ptr = unsafe { self.elements.as_mut() };
        let ptr = slice_from_raw_parts_mut(ptr.as_mut_ptr(), ptr.len());
        if mem::needs_drop::<T>() {
            for i in 0..self.len {
                unsafe {
                    (*ptr)[i].assume_init_drop();
                }
            }
        }
        let elems = unsafe { Box::from_raw(ptr) };
        drop(elems);
    }
}
