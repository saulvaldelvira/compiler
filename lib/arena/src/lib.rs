use std::borrow::Borrow;
use std::cell::{Cell, RefCell};
use std::cmp;
use std::marker::PhantomData;
use std::mem::{self, MaybeUninit};
use std::ops::{Deref, DerefMut};
use std::ptr::{slice_from_raw_parts_mut, NonNull};

struct ArenaChunk<T> {
    elements: NonNull<[MaybeUninit<T>]>,
    len: usize,
}

impl<T> ArenaChunk<T> {
    fn new(len: usize) -> Self {
        let array = Box::new_uninit_slice(len);
        let elements = Box::into_raw(array);
        let elements = unsafe { NonNull::new_unchecked(elements) };
        Self {
            elements,
            len: 0,
        }
    }

    fn alloc<'ctx>(&mut self, value: T) -> &'ctx mut T {
        let elems = unsafe { self.elements.as_mut() };
        elems[self.len].write(value);
        let ret = unsafe { elems[self.len].assume_init_mut() };
        self.len += 1;
        ret
    }

    fn alloc_iter<'ctx, I>(&mut self, value: I) -> &'ctx mut [T]
    where
        I: IntoIterator<Item = T>,
    {
        let elems = unsafe { self.elements.as_mut() };
        let start = self.len;

        for elem in value.into_iter() {
            elems[self.len].write(elem);
            self.len += 1;
        }

        unsafe {
            /*
             * SAFETY
             * We've initialized the elements on range [start..self.len]
             * AND MaybeUninit<T> is guaranteed to have the same memory layout
             * than T, so transmuting is safe.
             *
             * TODO: Change to asume_init_mut when stabilized
             */
            mem::transmute::<_, &'ctx mut [T]>(  &mut elems[start..self.len] )
        }
    }

    fn can_alloc(&self, ammount: usize) -> bool {
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

pub struct Arena<'ctx, T> {
    elems: RefCell<Vec<ArenaChunk<T>>>,
    _marker: PhantomData<&'ctx T>,
}

impl<'ctx, T> Arena<'ctx, T> {
    pub fn new() -> Self {
        Self {
            elems: RefCell::new(Vec::new()),
            _marker: PhantomData,
        }
    }

    fn with_chunk<R>(&self, c: impl for <'a> FnOnce(&'a Vec<ArenaChunk<T>>) -> R) -> R {
        let b = self.elems.borrow();
        let r = c(b.deref());
        drop(b);
        r
    }

    fn with_mut_chunk<R>(&self, c: impl for <'a> FnOnce(&'a mut Vec<ArenaChunk<T>>) -> R) -> R {
        let mut b = self.elems.borrow_mut();
        let r = c(b.deref_mut());
        drop(b);
        r
    }

    fn must_grow(&'ctx self, amount: usize) -> bool {
        self.with_chunk(|c| {
            c.last().is_none_or(|l| !l.can_alloc(amount))
        })
    }

    fn grow(&'ctx self, amount: usize) {
        self.with_mut_chunk(|c| {
            c.push(ArenaChunk::new(amount));
        });
    }

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
        let ret =
        {
            self.with_mut_chunk(|elems| {
                let chunk = elems.last_mut().unwrap();
                let ret = chunk.alloc_iter(values);
                unsafe {
                    mem::transmute::<_, &'ctx mut [T]>(ret)
                }
            })
        };

        ret
    }

    #[allow(clippy::mut_from_ref)]
    pub fn alloc(&self, value: T) -> &'ctx mut T {
        if self.must_grow(1) {
            self.grow(32);
        }
        let ret =
        {
            self.with_mut_chunk(|elems| {
                let chunk = elems.last_mut().unwrap();
                let ret = chunk.alloc(value);
                unsafe {
                    mem::transmute::<_, &'ctx mut T>(ret)
                }
            })
        };

        ret
    }
}

#[macro_export]
macro_rules! define_arenas {
    ($(
            $name:ident : $ty:ty
    ),*) => {

        pub struct Arena<'ctx> {
            $( $name:  $crate::Arena<'ctx, $ty>,)*
        }

        impl<'ctx> Arena<'ctx> {
            pub fn new() -> Self {
                Self {
                    $(
                        $name : $crate::Arena::new(),
                    )*
                }
            }

            pub fn alloc<T>(&self, val: T) -> &'ctx mut T
            where
                T: ArenaAllocable<'ctx>
            {
                val.alloc_into(self)
            }

            pub fn alloc_iter<T, I>(&self, val: I) -> &'ctx mut [T]
            where
                T: ArenaAllocable<'ctx>,
                I: IntoIterator<Item = T>,
                <I as IntoIterator>::IntoIter: ExactSizeIterator
            {
                T::alloc_iter(val, self)
            }
        }

        pub trait ArenaAllocable<'ctx> : Sized {
            #[allow(clippy::mut_from_ref)]
            fn alloc_into(self, arena: &Arena<'ctx>) -> &'ctx mut Self;

            #[allow(clippy::mut_from_ref)]
            fn alloc_iter<I>(it: I, arena: &Arena<'ctx>) -> &'ctx mut [Self]
            where
                I: IntoIterator<Item = Self>,
                <I as IntoIterator>::IntoIter: ExactSizeIterator;
        }

        $(
            impl<'ctx> ArenaAllocable<'ctx> for $ty {
                fn alloc_into(self, arena: &Arena<'ctx>) -> &'ctx mut Self {
                    arena . $name .alloc(self)
                }

                fn alloc_iter<I>(it: I, arena: &Arena<'ctx>) -> &'ctx mut [Self]
                where
                    I: IntoIterator<Item = Self>,
                    <I as IntoIterator>::IntoIter: ExactSizeIterator
                {
                    arena . $name .alloc_iter(it)
                }
            }
        )*
    };
}
