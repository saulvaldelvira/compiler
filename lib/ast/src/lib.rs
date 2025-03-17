//! Abstract Syntax Tree
//!
//! This crate contains the AST for the language.

pub mod expr;
pub mod stmt;

use std::any;
use std::cell::{Ref, RefCell};
use std::fmt::Debug;
use std::ops::Deref;
use std::rc::{Rc, Weak};

pub use expr::Expression;
pub use stmt::Statement;
pub mod types;
pub mod declaration;
pub use declaration::Declaration;
pub mod visitor;
pub use visitor::Visitor;

pub use lexer::Span;

#[derive(Debug)]
pub struct Program {
    pub decls: Box<[Declaration]>,
}

#[derive(Clone,PartialEq)]
pub struct AstDecorated<T>(RefCell<Option<T>>);

impl<T: Debug> Debug for AstDecorated<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(r) = self.0.borrow().deref() {
            write!(f, "{r:?}")
        } else {
            write!(f, "(unset)")
        }
    }
}

impl<T> From<T> for AstDecorated<T> {
    fn from(value: T) -> Self {
        Self(RefCell::new(Some(value)))
    }
}

impl<T> From<Option<T>> for AstDecorated<T> {
    fn from(value: Option<T>) -> Self {
        Self(RefCell::new(value))
    }
}

impl<T> AstDecorated<T> {
    pub fn new() -> Self {
        Self(RefCell::new(None))
    }
    pub fn set(&self, r: T) {
        *self.0.borrow_mut() = Some(r);
    }
    pub fn with<R>(&self, f: impl FnOnce(&T) -> R) -> Option<R> {
        self.0.borrow().deref().as_ref().map(f)
    }
    pub fn is_present(&self) -> bool {
        self.0.borrow().is_some()
    }
}

impl<T> Default for AstDecorated<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone> AstDecorated<T> {
    pub fn cloned(&self) -> Option<T> {
        self.0.borrow().clone()
    }
}

impl<T> AstDecorated<T> {
    pub fn get(&self) -> Option<Ref<'_, T>> {
        let inner = self.0.borrow();
        if inner.is_some() {
            Some(Ref::map(inner, |opt| opt.as_ref().unwrap()))
        } else {
            None
        }
    }

    pub fn unwrap(&self) -> Ref<'_, T> {
        let inner = self.0.borrow();
        Ref::map(inner, |opt| opt.as_ref().unwrap())
    }
}

#[derive(Clone)]
pub struct AstRef<T>(AstDecorated<Weak<T>>);

impl<T> Debug for AstRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tname = any::type_name::<T>();
        let tname = tname.split("::").last().unwrap_or(tname);
        write!(f, "AstRef<{tname}>")?;
        if self.0.is_present() {
            write!(f, "(set)")
        } else {
            write!(f, "(unset)")
        }
    }
}

impl<T> AstRef<T> {
    pub fn empty() -> Self {
        Self(AstDecorated::new())
    }

    pub fn set(&self, value: Weak<T>) {
        self.0.set(value);
    }

    pub fn with<R>(&self, f: impl FnOnce(&T) -> R) -> R {
        f(&self.unwrap())
    }

    pub fn unwrap(&self) -> Rc<T> {
        self.0.unwrap().upgrade().unwrap()
    }
}

impl<T> From<Weak<T>> for AstRef<T> {
    fn from(value: Weak<T>) -> Self {
        Self(AstDecorated::from(value))
    }
}
