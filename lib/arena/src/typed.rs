use std::{
    cell::{Cell, RefCell},
    cmp,
    marker::PhantomData,
    ptr,
    ptr::slice_from_raw_parts_mut,
};

use crate::chunk::ArenaChunk;

pub struct TypedArena<'ctx, T> {
    elems: RefCell<Vec<ArenaChunk<T>>>,
    _marker: PhantomData<&'ctx T>,

    start: Cell<*mut T>,
    end: Cell<*mut T>,
}

impl<'ctx, T> TypedArena<'ctx, T> {
    fn must_grow(&'ctx self, amount: usize) -> bool {
        self.elems
            .borrow()
            .last()
            .is_none_or(|l| !l.can_alloc(amount))
    }

    fn grow(&'ctx self, amount: usize) {
        let mut chunk = ArenaChunk::new(amount);
        self.start.set(chunk.start());
        self.end.set(chunk.end());
        self.elems.borrow_mut().push(chunk);
    }

    /// Allocs an slice of elements from the given [Iterator]
    ///
    /// The iterator must be an [`ExactSizeIterator`]
    #[allow(clippy::mut_from_ref)]
    pub fn alloc_iter<I>(&self, values: I) -> &'ctx mut [T]
    where
        I: IntoIterator<Item = T>,
        <I as IntoIterator>::IntoIter: ExactSizeIterator,
    {
        let mut values = values.into_iter().collect::<Vec<_>>();
        let len = values.len();

        if self.must_grow(len) {
            self.grow(cmp::max(32, len));
        }

        self.elems
            .borrow_mut()
            .last_mut()
            .expect("We've grown the vector, so it's imposible it doesn't have, at least, one element")
            .add_len(len);

        let ptr = self.start.get();
        unsafe {
            values.as_ptr().copy_to_nonoverlapping(ptr, len);
            values.set_len(0);

            self.start.set(ptr.add(len));

            &mut *slice_from_raw_parts_mut(ptr, len)
        }
    }

    /// Allocs an element, and returns a reference to it
    #[allow(clippy::mut_from_ref)]
    pub fn alloc(&self, value: T) -> &'ctx mut T {
        if self.must_grow(1) {
            self.grow(32);
        }

        self.elems
            .borrow_mut()
            .last_mut()
            .expect("We've grown the vector, so it's imposible it doesn't have, at least, one element")
            .add_len(1);

        let ptr = self.start.get();
        unsafe {
            ptr::write(ptr, value);
            self.start.set(ptr.add(1));
            &mut *ptr
        }
    }
}

impl<T> Default for TypedArena<'_, T> {
    fn default() -> Self {
        Self {
            elems: RefCell::default(),
            start: Cell::new(ptr::null_mut()),
            end: Cell::new(ptr::null_mut()),
            _marker: PhantomData,
        }
    }
}
