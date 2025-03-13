use std::borrow::Cow;
use session::Symbol;

#[derive(Debug,Clone)]
pub struct ErrorType {
    pub msg: Cow<'static,str>,
}

impl PartialEq for ErrorType {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl ErrorType {
    fn size(&self) -> usize { self.msg.len() }
}

#[derive(Debug,Clone,PartialEq)]
pub struct CustomType {
    pub name: Symbol,
}

#[derive(Debug,Clone,PartialEq)]
pub enum TypeKind {
    Int,
    Float,
    Bool,
    Char,
    String,
    Error(ErrorType),
    Custom(CustomType),
    Empty,
}

#[derive(Debug,Clone)]
pub struct Type {
    pub kind: TypeKind,
}

macro_rules! impl_ty {
    ($n:ident, $v:ident) => {
        pub fn $n() -> Self {
            Self {
                kind: TypeKind:: $v,
            }
        }
    };
}

impl Type {
    pub const fn empty() -> Self {
        Self {
            kind: TypeKind::Empty,
        }
    }
    impl_ty!(int, Int);
    impl_ty!(float, Float);
    impl_ty!(string, String);
    impl_ty!(bool, Bool);
    impl_ty!(char, Char);
}

use TypeKind as TK;

fn error(msg: impl Into<Cow<'static,str>>) -> Type {
    Type {
        kind: TypeKind::Error(ErrorType {
            msg: msg.into()
        }),
    }
}

impl Type {
    pub fn size(&self) -> usize {
        match &self.kind {
            TK::Bool => 1,
            TK::String => 8,
            TK::Error(err) => err.size(),
            TK::Empty => 0,
            TypeKind::Int => 4,
            TypeKind::Float => 4,
            TK::Char => 1,
            _ => todo!()
        }
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self.kind, TK::Bool)
    }

    pub fn arithmetic(&self, other: &Type) -> Type {
        if !matches!(self.kind, TK::Int | TK::Float) {
            return error(format!("Can't operate arithmetically with {:#?}", self.kind))
        }
        if self.kind != other.kind {
            return error(format!("Can't operate arithmetically on types {:#?} and {:#?}", self.kind, other.kind))
        }

        self.clone()
    }

    pub fn comparison(&self, other: &Type) -> Type {
        if !matches!(self.kind, TK::Int | TK::Float) {
            return error(format!("Can't compare {:#?}", self.kind))
        }
        if self.kind != other.kind {
            return error(format!("Can't compare types {:#?} and {:#?}", self.kind, other.kind))
        }

        Type::bool()
    }

    pub fn logical(&self, other: &Type) -> Type {
        if !matches!(self.kind, TK::Bool) {
            return error(format!("Can't operate logically on {:#?}", self.kind))
        }
        if !matches!(other.kind, TK::Bool) {
            return error(format!("Can't operate logically on {:#?}", other.kind))
        }

        Type::bool()
    }
}

