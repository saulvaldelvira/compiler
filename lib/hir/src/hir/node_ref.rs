use std::cell::{Ref, RefCell};
use std::fmt::Debug;
use core::ops::Deref;

use crate::hir_id::HirNode;

pub enum NodeRefKind<'hir, T> {
    Resolved(&'hir T),
    Pending,
    Err
}

impl<'hir, T: HirNode<'hir>> Debug for NodeRefKind<'hir, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Resolved(arg0) => write!(f, "{:#?}", arg0.get_hir_id()),
            Self::Pending => write!(f, "Pending"),
            Self::Err => write!(f, "Err"),
        }
    }
}

impl<T: Clone> Clone for NodeRefKind<'_, T> {
    fn clone(&self) -> Self {
        match self {
            Self::Resolved(arg0) => Self::Resolved(arg0),
            Self::Pending => Self::Pending,
            Self::Err => Self::Err,
        }
    }
}

impl<T: Copy> Copy for NodeRefKind<'_, T> {}

pub struct NodeRef<'hir, T> {
    inner: RefCell<NodeRefKind<'hir, T>>,
}

impl<'hir, T: HirNode<'hir>> Debug for NodeRef<'hir, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.inner.borrow().deref())
    }
}

impl<T: Clone> Clone for NodeRef<'_, T> {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<'hir, T> NodeRef<'hir, T> {
    pub fn pending() -> Self {
        Self { inner: RefCell::new(NodeRefKind::Pending) }
    }

    pub fn resolve(&self, val: &'hir T) {
        *self.inner.borrow_mut() = NodeRefKind::Resolved(val);
    }

    pub fn is_resolved(&self) -> bool {
        matches!(&*self.inner.borrow(), NodeRefKind::Resolved(_))
    }

    pub fn state(&self) -> Ref<'_, NodeRefKind<T>> {
        self.inner.borrow()
    }

    pub fn expect_resolved(&self) -> &'hir T {
        self.get().unwrap_or_else(|| {
            unreachable!("Expected NodeRef to be resolved")
        })
    }

    pub fn get(&self) -> Option<&'hir T> {
        match *self.inner.borrow() {
            NodeRefKind::Resolved(res) => Some(res),
            _ => None
        }
    }
}

impl<'hir, T> From<&'hir T> for NodeRef<'hir, T> {
    fn from(value: &'hir T) -> Self {
        Self { inner: RefCell::new(NodeRefKind::Resolved(value)) }
    }
}

impl<'hir, T> From<Option<&'hir T>> for NodeRef<'hir, T> {
    fn from(value: Option<&'hir T>) -> Self {
        let inner = match value {
            Some(val) => NodeRefKind::Resolved(val),
            None => NodeRefKind::Pending,
        };
        Self { inner: RefCell::new(inner) }
    }
}

