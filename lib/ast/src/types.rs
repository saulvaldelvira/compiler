use std::borrow::Cow;
use std::fmt::Debug;
use span::Span;
use session::Symbol;

#[derive(Debug,Clone)]
pub struct ErrorType {
    pub msg: Cow<'static,str>,
    pub span: Option<Span>,
}

impl ErrorType {
    pub fn new(msg: impl Into<Cow<'static,str>>) -> Self {
        Self { msg: msg.into(), span: None }
    }
}

impl PartialEq for ErrorType {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

#[derive(Clone)]
pub struct ArrayType {
    pub of: Box<Type>,
    pub length: usize,
}

impl PartialEq for ArrayType {
    fn eq(&self, other: &Self) -> bool {
        self.of.kind == other.of.kind && self.length == other.length
    }
}

impl Debug for ArrayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{:#?}; {}]", self.of, self.length)
    }
}

#[derive(Debug,Clone)]
pub struct StructType {
    pub name: Symbol,
    pub decl: AstRef<StructDecl>,
}

impl StructType {
    pub fn new(name: Symbol) -> Self {
        Self {
            name,
            decl: AstRef::empty(),
        }
    }
}

impl PartialEq for StructType {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Clone,PartialEq)]
pub struct RefType {
    pub of: Box<Type>,
}

impl Debug for RefType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "&{:#?}", self.of)
    }
}

#[derive(Clone,PartialEq)]
pub enum TypeKind {
    Int,
    Float,
    Bool,
    Char,
    String,
    Array(ArrayType),
    Struct(StructType),
    Error(ErrorType),
    Ref(RefType),
    Empty,
}

impl Debug for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::Bool => write!(f, "bool"),
            Self::Char => write!(f, "char"),
            Self::String => write!(f, "string"),
            Self::Array(a) => write!(f, "{a:#?}"),
            Self::Struct(s) => write!(f, "{s:#?}"),
            Self::Ref(r) => write!(f, "{r:#?}"),
            Self::Error(err) => write!(f, "{err:#?}"),
            Self::Empty => write!(f, "()"),
        }
    }
}

#[derive(Clone,PartialEq)]
pub struct Type {
    pub kind: TypeKind,
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.kind)
    }
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

use crate::declaration::StructDecl;
use crate::{AstRef, Expression};

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

    pub fn is_primitive(&self) -> bool {
        matches!(self.kind, TK::Int | TK::Bool | TK::Char | TK::Ref(_))
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
        if let Some(err) = self.propagate_error(&other.get_type()) { return err };

        let TK::Array(arr) = &self.kind else {
            return error(format!("Can't use square brackets on type {:#?}", self.kind));
        };

        if !other.get_type().integral() {
            return error(format!("Expected integral index, found {:#?}", self.kind));
        }

        Type::clone(&arr.of)
    }

    pub fn access_field(&self, field: Symbol) -> Type {
        if let TypeKind::Error(_) = self.kind { return self.clone() };

        let TypeKind::Struct(st) = &self.kind else {
            return error("Can't access field of non-struct type");
        };

        for f in &st.decl.unwrap().fields {
            if f.name == field {
                return f.ty.clone();
            }
        }

        error(format!("Struct {:#?} doesn't have a {field:#?} field", st.name))
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

