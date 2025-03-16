use std::borrow::Cow;
use lexer::Span;
use session::Symbol;

#[derive(Debug,Clone)]
pub struct ErrorType {
    pub msg: Cow<'static,str>,
    pub span: Option<Span>,
}

impl PartialEq for ErrorType {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

#[derive(Debug,Clone,PartialEq)]
pub struct CustomType {
    pub name: Symbol,
}

#[derive(Debug,Clone)]
pub struct ArrayType {
    pub of: Box<Type>,
    pub length: usize,
}

impl PartialEq for ArrayType {
    fn eq(&self, other: &Self) -> bool {
        self.of.kind == other.of.kind && self.length == other.length
    }
}

#[derive(Debug,Clone,PartialEq)]
pub enum TypeKind {
    Int,
    Float,
    Bool,
    Char,
    String,
    Array(ArrayType),
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

use crate::Expression;

fn error(msg: impl Into<Cow<'static,str>>) -> Type {
    Type {
        kind: TypeKind::Error(ErrorType {
            msg: msg.into(),
            span: None
        }),
    }
}

impl Type {
    pub fn is_boolean(&self) -> bool {
        matches!(self.kind, TK::Bool)
    }

    pub fn integral(&self) -> bool {
        matches!(self.kind, TK::Int)
    }

    fn propagate_error(&self, other: &Type) -> Option<Type> {
        if matches!(self.kind, TypeKind::Error(_)) {
            Some(self.clone())
        } else if matches!(other.kind, TypeKind::Error(_)) {
            Some(other.clone())
        } else {
            None
        }
    }

    pub fn square_brackets(&self, other: &Expression) -> Type {
        if let Some(err) = self.propagate_error(&other.ty.unwrap()) { return err };

        let TK::Array(arr) = &self.kind else {
            return error(format!("Can't use square brackets on type {:#?}", self.kind));
        };

        if !other.ty.unwrap().integral() {
            return error(format!("Expected integral index, found {:#?}", self.kind));
        }

        Type::clone(&arr.of)
    }

    pub fn arithmetic(&self, other: &Type) -> Type {
        if let Some(err) = self.propagate_error(other) { return err };

        if !matches!(self.kind, TK::Int | TK::Float) {
            return error(format!("Can't operate arithmetically with {:#?}", self.kind))
        }
        if self.kind != other.kind {
            return error(format!("Can't operate arithmetically on types {:#?} and {:#?}", self.kind, other.kind))
        }

        self.clone()
    }

    pub fn comparison(&self, other: &Type) -> Type {
        if let Some(err) = self.propagate_error(other) { return err };

        if !matches!(self.kind, TK::Int | TK::Float) {
            return error(format!("Can't compare {:#?}", self.kind))
        }
        if self.kind != other.kind {
            return error(format!("Can't compare types {:#?} and {:#?}", self.kind, other.kind))
        }

        Type::bool()
    }

    pub fn logical(&self, other: &Type) -> Type {
        if let Some(err) = self.propagate_error(other) { return err };

        if !matches!(self.kind, TK::Bool) {
            return error(format!("Can't operate logically on {:#?}", self.kind))
        }
        if !matches!(other.kind, TK::Bool) {
            return error(format!("Can't operate logically on {:#?}", other.kind))
        }

        Type::bool()
    }

    pub fn promote_to(&self, other: &Type) -> Type {
        if let Some(err) = self.propagate_error(other) { return err };

        if self.kind != other.kind {
            return error(format!("Can't promote {:#?} to {:#?}", self.kind, other.kind))
        }

        self.clone()
    }
}

