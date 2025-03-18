use std::cell::RefCell;
use std::cmp;
use std::marker::PhantomData;
use crate::chunk::ArenaChunk;

pub struct TypedArena<'ctx, T> {
    elems: RefCell<Vec<ArenaChunk<T>>>,
    _marker: PhantomData<&'ctx T>,
}

impl<'ctx, T> TypedArena<'ctx, T> {
    fn must_grow(&'ctx self, amount: usize) -> bool {
        self.elems.borrow().last().is_none_or(|l| !l.can_alloc(amount))
    }

    fn grow(&'ctx self, amount: usize) {
        self.elems.borrow_mut().push(ArenaChunk::new(amount));
    }

    /// Allocs an slice of elements from the given [Iterator]
    ///
    /// The iterator must be an [ExactSizeIterator]
    #[allow(clippy::mut_from_ref)]
    pub fn alloc_iter<I>(&self, values: I) -> &'ctx mut [T]
    where
        I: IntoIterator<Item = T>,
        <I as IntoIterator>::IntoIter: ExactSizeIterator
    {
        let values = values.into_iter().collect::<Vec<_>>();

        if self.must_grow(values.len()) {
            self.grow(cmp::max(32, values.len()));
        }
        let mut elems = self.elems.borrow_mut();
        let chunk = elems.last_mut().unwrap();
        chunk.alloc_slice(values)
    }

    /// Allocs an element, and returns a reference to it
    #[allow(clippy::mut_from_ref)]
    pub fn alloc(&self, value: T) -> &'ctx mut T {
        if self.must_grow(1) {
            self.grow(32);
        }
        let mut elems = self.elems.borrow_mut();
        let chunk = elems.last_mut().unwrap();
        chunk.alloc(value)
    }
}

impl<T> Default for TypedArena<'_, T> {
    fn default() -> Self {
        Self {
            elems: Default::default(),
            _marker: Default::default(),
        }
    }
}
