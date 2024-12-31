use std::collections::HashMap;

#[derive(Clone,Copy,Debug,Hash,Eq,PartialEq)]
pub struct Symbol {
    pub offset: usize,
    pub len: usize,
}

pub struct Interner {
    arena: String,
    set: HashMap<&'static str, Symbol>,
}

impl Interner {
    pub fn new() -> Self {
        Self {
            arena: String::new(),
            set: HashMap::new(),
        }
    }

    fn intern(&mut self, src: &str) -> Symbol {
        let offset = self.arena.len();
        let len = src.len();
        self.arena.push_str(src);

        /* SAFETY: we can extend the arena allocation to `'static` because we
           only access these while the arena is still alive. */
        let src: &'static str = unsafe { &*(src as *const str) };

        let sym = Symbol {
            offset,
            len
        };
        self.set.insert(src, sym);
        sym
    }

    pub fn get_or_intern(&mut self, src: &str) -> Symbol {
        /* SAFETY: we can extend the arena allocation to `'static` because we
           only access these while the arena is still alive. */
        let src: &'static str = unsafe { &*(src as *const str) };
        self.set.get(src).cloned().unwrap_or_else(|| {
            self.intern(src)
        })
    }

    pub fn get_str(&self, sym: Symbol) -> Option<&'static str> {
        let src = &self.arena[sym.offset..sym.offset + sym.len];
        let src: &'static str = unsafe { &*(src as *const str) };
        Some(src)
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
