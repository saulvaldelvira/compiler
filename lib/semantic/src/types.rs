use std::fmt::Display;

use interner::Symbol;

use crate::errors::SemanticErrorKind;

/// A `TypeId` uniquely identifies a type
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct TypeId(pub(crate) usize);

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum PrimitiveType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Char,
    F32,
    F64,
    Bool,
    Empty,
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Char => write!(f, "char"),
            Self::Bool => write!(f, "bool"),
            Self::Empty => write!(f, "()"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
        }
    }
}

impl From<&hir::types::PrimitiveType> for PrimitiveType {
    fn from(value: &hir::types::PrimitiveType) -> Self {
        use hir::types::PrimitiveType as Prim;
        match value {
            Prim::Char => Self::Char,
            Prim::Bool => Self::Bool,
            Prim::Empty => Self::Empty,
            Prim::I8 => Self::I8,
            Prim::I16 => Self::I16,
            Prim::I32 => Self::I32,
            Prim::I64 => Self::I64,
            Prim::U8 => Self::U8,
            Prim::U16 => Self::U16,
            Prim::U32 => Self::U32,
            Prim::U64 => Self::U64,
            Prim::F32 => Self::F32,
            Prim::F64 => Self::F64,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct Field<'ty> {
    pub name: Symbol,
    pub ty: &'ty Ty<'ty>,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum TypeKind<'ty> {
    Primitive(PrimitiveType),
    Ref(&'ty Ty<'ty>),
    Array(&'ty Ty<'ty>, u32),
    Struct {
        name: Symbol,
        fields: &'ty [Field<'ty>],
    },
    Function {
        is_variadic: bool,
        params: &'ty [&'ty Ty<'ty>],
        ret_ty: &'ty Ty<'ty>,
    },
}

impl Display for TypeKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeKind::Primitive(pt) => write!(f, "{pt}"),
            TypeKind::Ref(inner) => write!(f, "&{inner}"),
            TypeKind::Array(of, len) => write!(f, "[{of}; {len}]"),
            TypeKind::Struct { name, .. } => write!(f, "{name:#?}"),
            TypeKind::Function { is_variadic, params, ret_ty } => {
                write!(f, "fn(")?;
                let mut first = true;
                for param in *params {
                    if !first {
                        write!(f, ",")?;
                    }
                    first = false;
                    write!(f, "{param}")?;
                }
                if *is_variadic {
                    write!(f, ", ...")?;
                }
                write!(f, ") -> {ret_ty}")
            }
        }
    }
}

impl<'ty> TypeKind<'ty> {
    const BOOL: Self = Self::Primitive(PrimitiveType::Bool);

    #[inline]
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            TypeKind::Primitive(
                PrimitiveType::I8 | PrimitiveType::I16 | PrimitiveType::I32 | PrimitiveType::I64 |
                PrimitiveType::U8 | PrimitiveType::U16 | PrimitiveType::U32 | PrimitiveType::U64 |
                PrimitiveType::F32 | PrimitiveType::F64
            )
        )
    }

    pub fn can_be_promoted_to(&self, o: &TypeKind<'ty>) -> bool {
        if let (Self::Ref(r1), Self::Ref(r2)) = (self, o) {
            return r1.kind == r2.kind;
        }

        let (Self::Primitive(p1), Self::Primitive(p2)) = (self, o) else {
            return false;
        };

        p1 == p2
    }

    pub fn can_cast(&self, o: &TypeKind<'ty>) -> bool {
        matches!((self, o), (Self::Primitive(_) | Self::Ref(_), Self::Primitive(_) | Self::Ref(_)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
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
    #[inline]
    pub const fn is_empty_type(&self) -> bool {
        matches!(self.kind, TypeKind::Primitive(PrimitiveType::Empty))
    }

    #[inline]
    pub const fn is_boolean(&self) -> bool {
        matches!(self.kind, TypeKind::Primitive(PrimitiveType::Bool))
    }

    #[inline]
    pub const fn is_integer(&self) -> bool {
        matches!(
            self.kind,
            TypeKind::Primitive(
                PrimitiveType::I8 | PrimitiveType::I16 | PrimitiveType::I32 | PrimitiveType::I64 |
                PrimitiveType::U8 | PrimitiveType::U16 | PrimitiveType::U32 | PrimitiveType::U64
            )
        )
    }

    #[inline]
    pub const fn is_signed(&self) -> bool {
        matches!(self.kind, TypeKind::Primitive(PrimitiveType::I8 | PrimitiveType::I16 | PrimitiveType::I32 | PrimitiveType::I64))
    }

    #[inline]
    pub const fn as_function_type(&self) -> Option<(bool, &'ty [&'ty Ty<'ty>], &'ty Ty<'ty>)> {
        match self.kind {
            TypeKind::Function { is_variadic, params, ret_ty } => Some((is_variadic, params, ret_ty)),
            _ => None,
        }
    }

    #[inline]
    pub const fn as_struct_type(&self) -> Option<(Symbol, &'ty [Field<'ty>])> {
        match self.kind {
            TypeKind::Struct { name, fields } => Some((name, fields)),
            _ => None,
        }
    }

    #[inline]
    pub const fn is_pointer(&self) -> bool {
        matches!(self.kind, TypeKind::Ref(_))
    }

    pub fn field_index_of(&self, name: Symbol) -> Option<usize> {
        for (i, field) in self.as_struct_type()?.1.iter().enumerate() {
            if field.name == name {
                return Some(i)
            }
        }
        None
    }

    pub fn access_field(&'ty self, field_name: &Symbol) -> Result<&'ty Ty<'ty>, SemanticErrorKind> {
        match self.kind {
            TypeKind::Struct { name, fields } => {
                for f in fields {
                    if f.name == *field_name {
                        return Ok(f.ty);
                    }
                }
                Err(SemanticErrorKind::NonExistingField {
                    st: name,
                    field: *field_name,
                })
            }
            _ => Err(SemanticErrorKind::AccessToNonStruct),
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
            || !o.kind.can_be_promoted_to(&TypeKind::BOOL)
        {
            None
        } else {
            Some(sem.get_or_intern_type(TypeKind::Primitive(PrimitiveType::Bool)))
        }
    }

    pub fn comparison(
        &'ty self,
        o: &'ty Ty<'ty>,
        sem: &crate::Semantic<'ty>,
    ) -> Option<&'ty Ty<'ty>> {
        if self.kind.is_numeric() && o.kind.is_numeric() {
            Some(sem.get_or_intern_type(TypeKind::Primitive(PrimitiveType::Bool)))
        } else {
            None
        }
    }
}
