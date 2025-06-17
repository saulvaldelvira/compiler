use core::mem;
use core::{
    cell::{Cell, RefCell},
    marker::PhantomData,
    ptr, slice,
};

use tiny_vec::TinyVec;

use crate::chunk::ArenaChunk;
use crate::{PAGE_SIZE, HUGE_PAGE};

pub struct TypedArena<'ctx, T> {
    elems: RefCell<Vec<ArenaChunk<T>>>,
    start: Cell<*mut T>,
    end: Cell<*mut T>,
    _marker: PhantomData<&'ctx T>,
}

impl<'ctx, T> TypedArena<'ctx, T> {

    fn reserve(&self, amount: usize) {
        let mut chunks = self.elems.borrow_mut();
        let last_chunk = chunks.last();

        if last_chunk.is_some_and(|c| c.can_alloc(amount)) {
            return
        }

        let elem_size = mem::size_of::<T>().max(1);

        let mut new_cap =
        if let Some(last) = last_chunk {
            let len = last.capacity();
            let max_size = HUGE_PAGE / elem_size;
            usize::min(len, max_size / 2) * 2
        } else {
            PAGE_SIZE / elem_size
        };

        new_cap = usize::max(amount, new_cap);

        let mut chunk = ArenaChunk::new(new_cap);
        self.start.set(chunk.start());
        self.end.set(chunk.end());
        chunks.push(chunk);
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

        self.reserve(len);

        self.elems
            .borrow_mut()
            .last_mut()
            .expect("We've grown the vector, so it's imposible\
                it doesn't have, at least, one element")
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
        self.reserve(1);

        self.elems
            .borrow_mut()
            .last_mut()
            .expect("We've grown the vector, so it's imposible\
                     it doesn't have, at least, one element")
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
