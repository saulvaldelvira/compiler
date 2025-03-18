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

    pub fn start(&self) -> *mut T {
        self.elements.as_ptr() as *mut T
    }

    pub fn end(&self) -> *mut T {
        assert!(size_of::<T>() != 0);
        unsafe { self.start().add(self.elements.len()) }
    }

    pub fn alloc<'ctx>(&mut self, value: T) -> &'ctx mut T {
        unsafe {
            let ptr = self.start().add(self.len);
            ptr.write(value);
            self.len += 1;
            &mut *ptr
        }
    }

    pub fn alloc_slice<'ctx>(&mut self, mut values: Vec<T>) -> &'ctx mut [T] {
        unsafe {
            let ptr = self.start().add(self.len);

            let start = ptr.add(self.len);
            let len = values.len();
            if values.is_empty() { return &mut [] }

            values.as_ptr().copy_to_nonoverlapping(start, len);
            values.set_len(0);

            self.len += len;

            let slice = slice_from_raw_parts_mut(start, len);
            &mut *slice
        }
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
