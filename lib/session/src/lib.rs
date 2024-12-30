use std::fmt::Debug;
use std::sync::RwLock;

#[derive(Clone,Copy,Hash,Eq,PartialEq)]
#[repr(transparent)]
pub struct Symbol(interner::Symbol);

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match get_symbol_str(*self) {
            Some(s) => write!(f, "{s}"),
            None => write!(f, "{:?}", self.0)
        }
    }
}

pub struct Interner(RwLock<interner::Interner>);

impl Interner {
    pub fn get_or_intern(&self, src: &str) -> Symbol {
        Symbol(self.0.write().unwrap().get_or_intern(src))
    }

    pub fn get_str(&self, sym: Symbol) -> Option<&'static str> {
        self.0.read().unwrap().get_str(sym.0)
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

pub fn get_symbol_str<'a>(sym: Symbol) -> Option<&'a str> {
    with_session_interner(|i| {
        i.get_str(sym)
    })
}