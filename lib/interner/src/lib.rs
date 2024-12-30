use std::collections::HashMap;

use arena::{Span,Arena};

mod arena;

#[derive(Clone,Copy,Debug,Hash,Eq,PartialEq)]
#[repr(transparent)]
pub struct Symbol(Span);

pub struct Interner {
    arena: Arena,
    set: HashMap<&'static str, Span>,
}

impl Interner {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            set: HashMap::new(),
        }
    }

    fn intern(&mut self, src: &str) -> Symbol {
        let span = self.arena.alloc(src);

        /* SAFETY: we can extend the arena allocation to `'static` because we
           only access these while the arena is still alive. */
        let src: &'static str = unsafe { &*(src as *const str) };

        self.set.insert(src, span);
        Symbol(span)
    }

    pub fn get_or_intern(&mut self, src: &str) -> Symbol {
        /* SAFETY: we can extend the arena allocation to `'static` because we
           only access these while the arena is still alive. */
        let src: &'static str = unsafe { &*(src as *const str) };
        self.set.get(src).map(|sp| Symbol(*sp)).unwrap_or_else(|| {
            self.intern(src)
        })
    }

    pub fn get_str(&self, sym: Symbol) -> Option<&'static str> {
        self.arena.get(sym.0)
    }
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test() {
        let mut interner = Interner::new();

        let a = interner.get_or_intern("hello");
        let b = interner.get_or_intern("world");
        let c = interner.get_or_intern("hello");

        let mut map = HashMap::new();
        map.insert(a, "hello");
        map.insert(b, "world");

        let hello = * map.get(&c).unwrap();
        assert_eq!(hello, "hello");

        assert_eq!(a,c);
        assert_ne!(a,b);
        assert_ne!(b,c);

    }
}
