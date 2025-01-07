use std::fmt::Debug;
use std::sync::RwLock;

#[derive(Clone,Copy,Hash,Eq,PartialEq)]
#[repr(transparent)]
pub struct Symbol(interner::Symbol);

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        try_with_symbol(*self, |sym| {
            match sym {
                Some(s) => write!(f, "{s}"),
                None => write!(f, "{:?}", self.0)
            }
        })
    }
}

pub struct Interner(RwLock<interner::Interner>);

impl Interner {
    pub fn get_or_intern(&self, src: &str) -> Symbol {
        Symbol(self.0.write().unwrap().get_or_intern(src))
    }

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
    pub string_interner: Interner
}

impl Session {
    fn new() -> Self {
        Self {
            string_interner: Interner(RwLock::new(interner::Interner::new()))
        }
    }
}

thread_local! {
    static SESSION: Session = Session::new();
}

pub fn with_session<R>(f: impl FnOnce(&Session) -> R) -> R {
    SESSION.with(|sess| f(sess))
}


pub fn with_session_interner<R>(f: impl FnOnce(&Interner) -> R) -> R {
    with_session(|sess| {
        f(&sess.string_interner)
    })
}

pub fn try_with_symbol<R>(sym: Symbol, f: impl FnOnce(Option<&str>) -> R) -> R {
    with_session_interner(|i| {
        i.resolve(sym,f)
    })
}

pub fn with_symbol<R>(sym: Symbol, f: impl FnOnce(&str) -> R) -> R {
    with_session_interner(|i| {
        i.resolve_unchecked(sym, f)
    })
}

#[inline(always)]
#[cold]
fn cold() {}
