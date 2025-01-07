use hashbrown::hash_map::RawEntryMut;
use hashbrown::{DefaultHashBuilder, HashMap};
use std::hash::{BuildHasher, Hash};

pub struct Span {
    pub offset: usize,
    pub len: usize,
}

#[derive(Clone,Copy,Debug,Hash,Eq,PartialEq)]
#[repr(transparent)]
pub struct Symbol(usize);

#[derive(Default)]
struct Arena {
    buf: String,
    spans: Vec<Span>,
}

impl Arena {
    fn get_str(&self, sym: Symbol) -> Option<&str> {
        let span = self.spans.get(sym.0)?;
        let src = &self.buf[span.offset..span.offset + span.len];
        Some(src)
    }

    fn push_str(&mut self, src: &str) -> Symbol {
        let offset = self.buf.len();
        let len = src.len();
        self.buf.push_str(src);

        let span = Span {
            offset,
            len
        };
        let sym = Symbol(self.spans.len());
        self.spans.push(span);
        sym
    }
}

pub struct Interner<H = DefaultHashBuilder>
where H: BuildHasher + Default
{
    arena: Arena,
    set: HashMap<Symbol,(),()>,
    hasher: H,
}

impl<H: BuildHasher + Default> Interner<H> {
    pub fn new() -> Self {
        Self {
            arena: Arena::default(),
            set: HashMap::default(),
            hasher: H::default()
        }
    }

    pub fn get_or_intern(&mut self, src: &str) -> Symbol {
        let Self { arena, set, hasher } = self;

        let hash = hasher.hash_one(src);

        let entry = set.raw_entry_mut().from_hash(hash, |&sym| {
            src == unsafe { arena.get_str(sym).unwrap_unchecked() }
        });

        let k = match entry {
            RawEntryMut::Occupied(occupied) => {
                occupied.into_key()
            },
            RawEntryMut::Vacant(vacant) => {
                let sym = arena.push_str(src);
                vacant.insert_with_hasher(hash, sym, (), |sym| {
                    let src = unsafe { arena.get_str(*sym).unwrap_unchecked() };
                    hasher.hash_one(src)
                }).0
            }
        };

        *k
    }

    pub fn resolve(&self, sym: Symbol) -> Option<&str> {
        self.arena.get_str(sym)
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
        let mut interner: Interner<DefaultHashBuilder> = Interner::new();

        let a = interner.get_or_intern("hello");
        let b = interner.get_or_intern("world");
        let c = interner.get_or_intern("hello");

        let mut map = HashMap::new();
        map.insert(a, "hello");
        map.insert(b, "world");

        assert_eq!(a,c);
        assert_ne!(a,b);
        assert_ne!(b,c);

        let hello = * map.get(&c).unwrap();
        assert_eq!(hello, "hello");
    }
}
