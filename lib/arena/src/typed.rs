use core::{
    cell::{Cell, RefCell},
    marker::PhantomData,
    cmp, ptr, slice,
};

use tiny_vec::TinyVec;

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
    {
        /* We can't pre-alloc the space like we do on the DroplessArena.
         * This arena is reentrant, which means that the iterator may
         * call alloc_iter again. If we pre-allocated the space we may
         * have initialized elements after uninitialized ones.
         *
         * Example:
         * An initial call to alloc_iter pre-allocates space for 4 elements.
         * While filling the space, the iterator calls alloc_iter again,
         * which causes it to re-alloc more space.
         * On the third call to iterator.next(), a panic occurs.
         *
         * In that case, we would have the following structure in memory.
         *
         *     (alloc_iter_1) -- calls -- (alloc_iter2)
         * [ INIT INIT UNINIT UNINIT ] [INIT INIT PANIC! ]
         *                |------------------------^
         *                We poll the iterator to get the third item,
         *                but the iterator panics.
         *
         * Since we have mixed init and uninit data, we either drop uninitalized
         * elements, or leak memory
         *
         * Collecting the elements beforehand takes care of panic safety and
         * reentrancy. Also, this function is called less often that its dropless
         * counterpart.
         * The TinyVec can hold 8 elements on the stack, so small iterators won't
         * even cause heap allocations
         * */
        let mut values: TinyVec<_, 8> = values.into_iter().collect();
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

            slice::from_raw_parts_mut(ptr, len)
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
