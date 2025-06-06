use core::str::FromStr;
use std::{
    fmt::{Debug, Display},
    sync::RwLock,
};

use interner::StringInterner;

#[derive(Clone, Copy, Hash, Eq, PartialEq)]
#[repr(transparent)]
pub struct Symbol(interner::Symbol<str>);

impl Symbol {
    pub fn try_borrow<R>(&self, f: impl FnOnce(&str) -> R) -> R {
        with_symbol(*self, f)
    }

    #[inline]
    pub fn new(s: &str) -> Self {
        with_session_interner(|i| i.get_or_intern(s))
    }
}

impl FromStr for Symbol {
    type Err = core::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::new(s))
    }
}

impl PartialEq<&str> for Symbol {
    /// Attemps to resolve the symbol, and compares it
    /// with the given string
    ///
    /// NOTE: If the symbol doesn't exist in the session
    /// storage, it returns false.
    fn eq(&self, other: &&str) -> bool { symbol_equals(*self, other) }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        try_with_symbol(*self, |sym| {
            match sym {
                Some(s) => write!(f, "{s}"),
                None => write!(f, "{:?}", self.0),
            }
        })
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        with_symbol(*self, |s| write!(f, "{s}"))
    }
}

pub struct Interner(RwLock<StringInterner>);

impl Interner {
    #[inline]
    pub fn get_or_intern(&self, src: &str) -> Symbol {
        Symbol(self.0.write().unwrap().get_or_intern(src))
    }

    #[inline]
    pub fn resolve<R>(&self, sym: Symbol, f: impl FnOnce(Option<&str>) -> R) -> R {
        f(self.0.read().unwrap().resolve(sym.0))
    }

    pub fn resolve_unchecked<R>(&self, sym: Symbol, f: impl FnOnce(&str) -> R) -> R {
        let i = self.0.read().unwrap();
        let s = i.resolve(sym.0).unwrap_or_else(|| {
            /* It's VERY unlikely that we ask the interner for a
             * symbol it hasn't generated.  */
            cold();
            panic!("Attemp to get unexisting symbol: {sym:?}")
        });
        f(s)
    }
}

pub struct Session {
    pub string_interner: Interner,
}

impl Session {
    fn new() -> Self {
        Self {
            string_interner: Interner(RwLock::new(interner::Interner::new())),
        }
    }
}

thread_local! {
    static SESSION: Session = Session::new();
}

#[inline]
pub fn with_session<R>(f: impl FnOnce(&Session) -> R) -> R { SESSION.with(|sess| f(sess)) }

#[inline]
pub fn with_session_interner<R>(f: impl FnOnce(&Interner) -> R) -> R {
    with_session(|sess| f(&sess.string_interner))
}

#[inline]
pub fn try_with_symbol<R>(sym: Symbol, f: impl FnOnce(Option<&str>) -> R) -> R {
    with_session_interner(|i| i.resolve(sym, f))
}

#[inline]
pub fn with_symbol<R>(sym: Symbol, f: impl FnOnce(&str) -> R) -> R {
    with_session_interner(|i| i.resolve_unchecked(sym, f))
}

#[inline]
pub fn symbol_into_owned(sym: Symbol) -> String {
    with_session_interner(|i| i.resolve_unchecked(sym, str::to_string))
}

#[inline]
pub fn symbol_equals(sym: Symbol, o: &str) -> bool { with_symbol(sym, |s| s == o) }

#[inline]
pub fn intern_str(src: &str) -> Symbol { with_session_interner(|i| i.get_or_intern(src)) }

#[inline]
#[cold]
fn cold() {}
