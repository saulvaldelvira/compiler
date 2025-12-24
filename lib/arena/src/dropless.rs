use core::{
    alloc::Layout,
    cell::{Cell, RefCell},
    marker::PhantomData,
    cmp, mem, ptr, slice,
};

use crate::chunk::ArenaChunk;
use crate::{HUGE_PAGE, PAGE_SIZE};

pub struct DroplessArena<'ctx> {
    elems: RefCell<Vec<ArenaChunk<u8>>>,
    start: Cell<*mut u8>,
    end: Cell<*mut u8>,
    _marker: PhantomData<&'ctx u8>,
}

const ALIGNMENT: usize = mem::size_of::<usize>();

#[expect(clippy::inline_always)]
#[inline(always)]
const fn align_down(val: usize, align: usize) -> usize {
    debug_assert!(align.is_power_of_two());
    val & !(align - 1)
}

#[expect(clippy::inline_always)]
#[inline(always)]
const fn align_up(val: usize, align: usize) -> usize {
    debug_assert!(align.is_power_of_two());
    (val + align - 1) & !(align - 1)
}

impl<'ctx> DroplessArena<'ctx> {
    fn __alloc_raw(&self, layout: Layout) -> Option<*mut u8> {
        let start = self.start.get().addr();
        let old_end = self.end.get();
        let end = old_end.addr();

        let bytes = align_up(layout.size(), ALIGNMENT);

        let sub = end.checked_sub(bytes)?;

        let new_end = align_down(sub, layout.align());
        if start <= new_end {
            let new_end = old_end.with_addr(new_end);
            self.end.set(new_end);
            Some(new_end)
        } else {
            None
        }
    }

    /// Allocs a chunk of bytes for the given [Layout]
    pub fn alloc_raw(&self, layout: Layout) -> *mut u8 {
        if layout.size() == 0 {
            return ptr::without_provenance_mut(!0);
        }
        loop {
            if let Some(ptr) = self.__alloc_raw(layout) {
                debug_assert!(!ptr.is_null());
                return ptr;
            }
            self.grow(layout);
        }
    }

    /// Allocs an element, and returns a reference to it
    #[expect(clippy::missing_panics_doc)]
    pub fn alloc<T>(&self, value: T) -> &'ctx mut T {
        assert!(!mem::needs_drop::<T>());

        let buf = self.alloc_raw(Layout::new::<T>()) as *mut T;

        unsafe {
            ptr::write(buf, value);
            &mut *buf
        }
    }

/// Writes len elements from the given iterator into ptr
fn fill_array<T, I>(mut iter: I, ptr: *mut T, len: usize) -> &'ctx mut [T]
    where
        I: Iterator<Item = T>,
    {
        for i in 0..len {
            let Some(elem) = iter.next() else {
                /* We call this function with len == iter.len()
                 * so next should always return Some */
                unreachable!()
            };

            unsafe { ptr.add(i).write(elem) };
        }
        unsafe { slice::from_raw_parts_mut(ptr, len) }
    }

    /// Allocs an slice of elements from the given [Iterator]
    ///
    /// The iterator must be an [`ExactSizeIterator`]
    pub fn alloc_iter<T, I>(&self, iter: I) -> &'ctx mut [T]
    where
        I: IntoIterator<
            Item = T,
            IntoIter: ExactSizeIterator,
            >,
    {
        assert!(!mem::needs_drop::<T>());
        assert!(mem::size_of::<T>() != 0);

        let iter = iter.into_iter();
        let length = iter.len();

        if length == 0 {
            return &mut [];
        }

        let ptr = self.alloc_raw(Layout::array::<T>(length).unwrap()) as *mut T;

        Self::fill_array(iter, ptr, length)
    }

    fn grow(&self, layout: Layout) {
        let additional = layout.size() + cmp::max(ALIGNMENT, layout.align()) - 1;

        let mut elems = self.elems.borrow_mut();
        let mut new_cap;
        if let Some(last) = elems.last_mut() {
            new_cap = last.capacity().min(HUGE_PAGE / 2);
            new_cap *= 2;
        } else {
            new_cap = PAGE_SIZE;
        }

        new_cap = cmp::max(additional, new_cap);

        let mut chunk = ArenaChunk::new(align_up(new_cap, PAGE_SIZE));

        self.start.set(chunk.start());

        let end = align_down(chunk.end().addr(), ALIGNMENT);

        debug_assert!(self.start.get().addr() <= end);

        self.end.set(chunk.end().with_addr(end));

        elems.push(chunk);
    }
}

impl Default for DroplessArena<'_> {
    fn default() -> Self {
        Self {
            elems: RefCell::default(),
            start: Cell::new(ptr::null_mut()),
            end: Cell::new(ptr::null_mut()),
            _marker: PhantomData,
        }
    }
}
