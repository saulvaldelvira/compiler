//! String interner
//!
//! Interning strings gives us two benefits.
//!
//! 1) Less memory usage, since we don't need to over-allocate memory.
//!    If we intern "abcd" 1000 times, we will only need 4 bytes.
//!    Compared with using 1000 [String]s, which would cause 1000 allocations
//!
//! 2) Faster comparison.
//!    Instead of comparing string slices, which takes a lot of time, comparing
//!    symbols (which are represented as an usize) is much more faster.
//!    Since all calls to [`Symbol::new`] with the same string will resolve to the
//!    same [Symbol], we can just compare the symbols.
//!
//! # Example
//! ```
//! use interner::Symbol;
//!
//! let str1 = Symbol::new("abcdef");
//! let same = Symbol::new("abcdef");
//! assert_eq!(str1, same);
//!
//! str1.borrow(|s| assert_eq!(s, "abcdef"));
//! ```

use core::fmt::{Display, Debug};
use core::str::FromStr;
use std::sync::{LazyLock, RwLock};

use interns::StringInterner;

#[derive(Clone, Copy, Hash, Eq, PartialEq)]
#[repr(transparent)]
/// Identifies an interned string.
pub struct Symbol(interns::Symbol<str>);

#[macro_export]
macro_rules! symbols_identified {
    (@rec $idx:expr; $name:ident : $val:literal $(, $($t:tt)+ )?) => {
        pub const $name : Symbol = BUILDER.symbol_at($idx);

        $(symbols_identified!(@rec $idx + 1; $($t)+); )?
    };
    (@rec_impl $idx:expr; $name:ident : $val:literal $(, $($t:tt)+ )?) => {

        #[doc = concat!(
            "Keyword \"", $val , "\""
        )]
        pub const $name : Self = Self(symbols::$name);

        $(symbols_identified!(@rec_impl $idx + 1; $($t)+); )?
    };
    ($($name:ident : $val:literal),* $(,)?) => {
        mod symbols {
            use interns::backend::string::StringInternerBuilder;
            type Symbol = interns::Symbol::<str>;
            pub const COUNT: usize = 0 $( + { let _ = $val; 1 } )*;
            pub const BUILDER: StringInternerBuilder<COUNT> = StringInternerBuilder::with_const_symbols([
                $($val,)*
            ]);
            symbols_identified!(@rec 0; $($name: $val),*);
        }

        /// Pre-defined [Symbol]s for keywords
        impl Symbol {
            symbols_identified!(@rec_impl 0; $($name : $val),*);
        }
    };
}

symbols_identified!{
    KWSELF: "self",
    KWSUPER: "super",
}

impl Symbol {

    /// Gets a symbol from the given string.
    ///
    /// # Example
    /// ```
    /// use interner::Symbol;
    ///
    /// let kw_const = Symbol::new("const");
    /// kw_const.borrow(|s| assert_eq!(s, "const"));
    /// ```
    #[inline]
    pub fn new(s: &str) -> Self {
        #[cfg(all(debug_assertions, not(test)))] {
            for i in 0..symbols::COUNT {
                assert!(symbols::BUILDER.string_at(i) != s, "Attemp to intern a symbol '{s}' that is pre-defined");
            }
        }
        GLOBAL_INTERNER.get_or_intern(s)
    }

    /// Borrows this symbol from the global interner, and applies
    /// the given closure to it
    ///
    /// # Example
    /// ```
    /// use interner::Symbol;
    ///
    /// let sym = Symbol::new("my string");
    /// sym.borrow(|s| {
    ///     println!("Symbol {sym:?} resolved to '{s}'");
    /// });
    /// ```
    #[inline]
    pub fn borrow<R>(&self, f: impl FnOnce(&str) -> R) -> R {
        GLOBAL_INTERNER.resolve_unchecked(*self, f)
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
    fn eq(&self, other: &&str) -> bool {
        GLOBAL_INTERNER.resolve_unchecked(*self, |s| s == *other)
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        GLOBAL_INTERNER.resolve(*self, |sym| {
            match sym {
                Some(s) => write!(f, "{s}"),
                None => write!(f, "{:?}", self.0),
            }
        })
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        GLOBAL_INTERNER.resolve_unchecked(*self, |s| {
            write!(f, "{s}")
        })
    }
}

struct Interner(RwLock<StringInterner>);

static GLOBAL_INTERNER: LazyLock<Interner> = LazyLock::new(|| {
    Interner(RwLock::new(symbols::BUILDER.build()))
});

impl Interner {
    #[inline]
    fn get_or_intern(&self, src: &str) -> Symbol {
        Symbol(self.0.write().unwrap().get_or_intern(src))
    }

    #[inline]
    fn resolve<R>(&self, sym: Symbol, f: impl FnOnce(Option<&str>) -> R) -> R {
        f(self.0.read().unwrap().resolve(sym.0))
    }

    fn resolve_unchecked<R>(&self, sym: Symbol, f: impl FnOnce(&str) -> R) -> R {
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

#[inline]
#[cold]
fn cold() {}

#[cfg(test)]
mod test;
