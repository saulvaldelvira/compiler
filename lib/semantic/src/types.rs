use std::fmt::Display;

use session::Symbol;

use crate::errors::SemanticErrorKind;

/// A TypeId uniquely identifies a type
#[derive(Debug,Hash,PartialEq,Eq,Clone,Copy)]
pub struct TypeId(pub (crate) usize);

#[derive(Debug,Hash,PartialEq,Eq,Clone,Copy)]
pub enum PrimitiveType {
    Int,
    Char,
    Float,
    Bool,
    Empty,
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
            match self {
                PrimitiveType::Int => "int",
                PrimitiveType::Char => "char",
                PrimitiveType::Float => "float",
                PrimitiveType::Bool => "bool",
                PrimitiveType::Empty => "()",
            }
        )
    }
}

impl From<&hir::types::PrimitiveType> for PrimitiveType {
    fn from(value: &hir::types::PrimitiveType) -> Self {
        use hir::types::PrimitiveType as Prim;
        match value {
            Prim::Int => Self::Int,
            Prim::Char => Self::Char,
            Prim::Float => Self::Float,
            Prim::Bool => Self::Bool,
            Prim::Empty => Self::Empty,
        }
    }
}

#[derive(Debug,Clone,Copy,PartialEq,Hash,Eq)]
pub struct Field<'ty> {
    pub name: Symbol,
    pub ty: &'ty Ty<'ty>,
}

#[derive(Debug,Clone,Copy,PartialEq,Hash,Eq)]
pub enum TypeKind<'ty> {
    Primitive(PrimitiveType),
    Ref(&'ty Ty<'ty>),
    Array(&'ty Ty<'ty>, usize),
    Struct { name: Symbol, fields: &'ty [Field<'ty>] },
    Function { params: &'ty [Ty<'ty>], ret_ty: &'ty Ty<'ty> },
}

impl Display for TypeKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeKind::Primitive(pt) => write!(f, "{pt}"),
            TypeKind::Ref(inner) => write!(f, "&{inner}"),
            TypeKind::Array(of, len) => write!(f, "[{of}; {len}]"),
            TypeKind::Struct {name, .. } => write!(f, "{name:#?}"),
            TypeKind::Function { params, ret_ty } => {
                write!(f, "fn(")?;
                let mut first = true;
                for param in *params {
                    if !first {
                        write!(f, ",")?;
                    }
                    first = false;
                    write!(f, "{param}")?;
                }
                write!(f, ") -> {ret_ty}")
            }
        }
    }
}


impl<'ty> TypeKind<'ty> {
    const BOOL: Self = Self::Primitive(PrimitiveType::Bool);

    pub fn can_be_promoted_to(&self, o: &TypeKind<'ty>) -> bool {
        let (Self::Primitive(p1), Self::Primitive(p2)) = (self, o) else {
            return false
        };

        if p1 == p2 { return true };

        use PrimitiveType as P;
        matches!(
            (p1,p2),
            (P::Int, P::Float) |
            (P::Char, P::Int) |
            (P::Bool, P::Int) |
            (P::Char, P::Float) |
            (P::Bool, P::Float)
        )
    }
}

#[derive(Debug,Clone,Copy,PartialEq,Hash,Eq)]
pub struct Ty<'ty> {
    pub kind: TypeKind<'ty>,
    pub id: TypeId,
}

impl Display for Ty<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl<'ty> Ty<'ty> {

    #[inline(always)]
    pub const fn is_empty_type(&self) -> bool {
        matches!(self.kind, TypeKind::Primitive(PrimitiveType::Empty))
    }

    #[inline(always)]
    pub const fn as_function_type(&self) -> Option<(&'ty [Ty<'ty>], &'ty Ty<'ty>)> {
        match self.kind {
            TypeKind::Function { params, ret_ty } => Some((params, ret_ty)),
            _ => None
        }
    }

    pub fn access_field(&'ty self, field_name: &Symbol) -> Result<&'ty Ty<'ty>, SemanticErrorKind> {
        match self.kind {
            TypeKind::Struct { name, fields } => {
                for f in fields {
                    if f.name == *field_name {
                        return Ok(f.ty)
                    }
                }
                Err(SemanticErrorKind::NonExistingField { st: name, field: *field_name })
            },
            _ => Err(SemanticErrorKind::AccessToNonStruct)
        }
    }

    pub fn promote_to(&'ty self, o: &'ty Ty<'ty>) -> Option<&'ty Ty<'ty>> {
        if self.kind.can_be_promoted_to(&o.kind) {
            Some(o)
        } else {
            None
        }
    }

    pub fn arithmetic(&'ty self, o: &'ty Ty<'ty>) -> Option<&'ty Ty<'ty>> {
        if let Some(ty) = self.promote_to(o) {
            Some(ty)
        } else {
            o.promote_to(self)
        }
    }

    pub fn logical(&'ty self, o: &'ty Ty<'ty>, sem: &crate::Semantic<'ty>) -> Option<&'ty Ty<'ty>> {
        if !self.kind.can_be_promoted_to(&TypeKind::BOOL)
        || !o.kind.can_be_promoted_to(&TypeKind::BOOL) {
            None
        } else {
            Some(sem.get_primitive_type(PrimitiveType::Bool))
        }
    }
}
