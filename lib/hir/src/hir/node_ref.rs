use std::cell::{Ref, RefCell};
use std::fmt::Debug;
use core::ops::Deref;

pub enum NodeRefKind<T> {
    Resolved(T),
    Pending,
    Err
}

impl<T: Debug> Debug for NodeRefKind<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Resolved(arg0) => write!(f, "{:#?}", arg0),
            Self::Pending => write!(f, "Pending"),
            Self::Err => write!(f, "Err"),
        }
    }
}

impl<T: Clone> Clone for NodeRefKind<T> {
    fn clone(&self) -> Self {
        match self {
            Self::Resolved(arg0) => Self::Resolved(arg0.clone()),
            Self::Pending => Self::Pending,
            Self::Err => Self::Err,
        }
    }
}

impl<T: Copy> Copy for NodeRefKind<T> {}

pub struct NodeRef<T> {
    inner: RefCell<NodeRefKind<T>>,
}

impl<T: Debug> Debug for NodeRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.inner.borrow().deref())
    }
}

impl<T: Clone> Clone for NodeRef<T> {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<T> NodeRef<T> {
    pub fn pending() -> Self {
        Self { inner: RefCell::new(NodeRefKind::Pending) }
    }

    pub fn resolve(&self, val: T) {
        *self.inner.borrow_mut() = NodeRefKind::Resolved(val);
    }

    pub fn is_resolved(&self) -> bool {
        matches!(&*self.inner.borrow(), NodeRefKind::Resolved(_))
    }

    pub fn state(&self) -> Ref<'_, NodeRefKind<T>> {
        self.inner.borrow()
    }

    pub fn expect_resolved(&self) -> T
    where
        T: Copy
    {
        self.get().unwrap_or_else(|| {
            unreachable!("Expected NodeRef to be resolved")
        })
    }

    pub fn get(&self) -> Option<T>
    where
        T: Copy
    {
        match *self.inner.borrow() {
            NodeRefKind::Resolved(res) => Some(res),
            _ => None
        }
    }
}

impl<T> From<T> for NodeRef<T> {
    fn from(value: T) -> Self {
        Self { inner: RefCell::new(NodeRefKind::Resolved(value)) }
    }
}

impl<T> From<Option<T>> for NodeRef<T> {
    fn from(value: Option<T>) -> Self {
        let inner = match value {
            Some(val) => NodeRefKind::Resolved(val),
            None => NodeRefKind::Pending,
        };
        Self { inner: RefCell::new(inner) }
    }
}

