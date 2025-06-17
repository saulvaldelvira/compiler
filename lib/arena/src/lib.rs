//! Arena
//!
//! An arena allows to allocate objects dynamically, and bind them to a
//! specific common lifetime.
//! All objects allocated on the arena live as long as the arena does.
//! This allows us to easily link nodes together.
//!
//! # Example
//! ```rust
#![doc = include_str!("../examples/friends.rs")]
//! ```

mod chunk;
mod dropless;
mod typed;

pub use dropless::DroplessArena;
pub use typed::TypedArena;

#[doc(hidden)]
pub mod markers {
    pub struct Copy;
    pub struct NoCopy;
}

const PAGE_SIZE: usize = 4096;
const HUGE_PAGE: usize = 2 * 1024 * 1024;

#[macro_export]
macro_rules! define_arenas {
    ( $([visibility = $vis:vis ])? $(
           $name:ident : $ty:ty
    ),* $(,)?) => {

        $($vis)? struct Arena<'ctx> {
            dropless: $crate::DroplessArena<'ctx>,
            $( $name:  $crate::TypedArena<'ctx, $ty>,)*
        }

        impl<'ctx> Arena<'ctx> {
            $($vis)? fn new() -> Self {
                Self {
                    dropless: $crate::DroplessArena::default(),
                    $(
                        $name : $crate::TypedArena::default(),
                    )*
                }
            }

            $($vis)? fn alloc<T,C>(&self, val: T) -> &'ctx mut T
            where
                T: ArenaAllocable<'ctx, C>
            {
                val.alloc_into(self)
            }

            $($vis)? fn alloc_iter<T, I, C>(&self, val: I) -> &'ctx mut [T]
            where
                T: ArenaAllocable<'ctx, C>,
                I: IntoIterator<Item = T>,
                <I as IntoIterator>::IntoIter: ExactSizeIterator
            {
                T::alloc_iter(val, self)
            }
        }

        $($vis)? trait ArenaAllocable<'ctx, C = $crate::markers::Copy> : Sized {
            #[allow(clippy::mut_from_ref)]
            fn alloc_into(self, arena: &Arena<'ctx>) -> &'ctx mut Self;

            #[allow(clippy::mut_from_ref)]
            fn alloc_iter<I>(it: I, arena: &Arena<'ctx>) -> &'ctx mut [Self]
            where
                I: IntoIterator<Item = Self>,
                <I as IntoIterator>::IntoIter: ExactSizeIterator;
        }

        impl<'ctx, T: Copy> ArenaAllocable<'ctx> for T {
            #[allow(clippy::mut_from_ref)]
            fn alloc_into(self, arena: &Arena<'ctx>) ->  &'ctx mut Self {
                arena.dropless.alloc(self)
            }

            #[allow(clippy::mut_from_ref)]
            fn alloc_iter<I>(it:I, arena: &Arena<'ctx>) ->  &'ctx mut [Self]
            where
                I:IntoIterator<Item = Self>,
                <I as IntoIterator>::IntoIter:ExactSizeIterator
            {
                arena.dropless.alloc_iter(it)
            }
        }

        $(
            impl<'ctx> ArenaAllocable<'ctx, $crate::markers::NoCopy> for $ty {
                fn alloc_into(self, arena: &Arena<'ctx>) -> &'ctx mut Self {
                    if !::core::mem::needs_drop::<Self>() {
                        arena.dropless.alloc(self)
                    } else {
                        arena . $name .alloc(self)
                    }
                }

                fn alloc_iter<I>(it: I, arena: &Arena<'ctx>) -> &'ctx mut [Self]
                where
                    I: IntoIterator<Item = Self>,
                    <I as IntoIterator>::IntoIter: ExactSizeIterator
                {
                    if !::core::mem::needs_drop::<Self>() {
                        arena.dropless.alloc_iter(it)
                    } else {
                        arena . $name .alloc_iter(it)
                    }
                }
            }
        )*
    };
}

#[cfg(test)]
mod test;
