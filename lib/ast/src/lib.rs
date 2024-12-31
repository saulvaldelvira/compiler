//! Abstract Syntax Tree
//!
//! This crate contains the AST for the language.

pub mod expr;
pub mod stmt;

use std::cell::RefCell;
use std::fmt::Debug;
use std::ops::Deref;
use std::rc::Rc;

pub use expr::Expression;
pub use stmt::Statement;
pub mod types;
pub mod declaration;
pub use declaration::Declaration;
pub mod visitor;
pub use visitor::Visitor;

#[derive(Debug)]
pub struct Program {
    pub decls: Box<[Declaration]>,
}

pub struct AstRef<T>(RefCell<Option<Rc<T>>>);

impl<T: Debug> Debug for AstRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(r) = self.0.borrow().deref() {
            write!(f, "{r:?}")
        } else {
            Ok(())
        }
    }
}

impl<T> From<Rc<T>> for AstRef<T> {
    fn from(value: Rc<T>) -> Self {
        Self(RefCell::new(Some(value)))
    }
}

impl<T> AstRef<T> {
    pub fn new() -> Self {
        Self(RefCell::new(None))
    }
    pub fn set(&self, r: Rc<T>) {
        *self.0.borrow_mut() = Some(r);
    }
    pub fn with<R>(&self, f: impl FnOnce(&T) -> R) -> Option<R> {
        self.0.borrow().deref().as_ref().map(|r| f(r.deref()))
    }
    pub fn get(&self) -> Option<Rc<T>> {
        self.0.borrow().deref().as_ref().map(Rc::clone)
    }
}

impl<T> Default for AstRef<T> {
    fn default() -> Self {
        Self::new()
    }
}
