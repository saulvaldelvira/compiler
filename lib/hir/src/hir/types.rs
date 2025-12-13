use core::fmt;
use std::hash::Hash;

use super::Path;
use crate::{HirId, hir_id::HirNode, node_map::HirNodeKind};

#[derive(Hash, PartialEq, Eq, Clone)]
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
}

impl fmt::Debug for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Char => write!(f, "char"),
            Self::Bool => write!(f, "bool"),
            PrimitiveType::I8 => write!(f, "i8"),
            PrimitiveType::I16 => write!(f, "i16"),
            PrimitiveType::I32 => write!(f, "i32"),
            PrimitiveType::I64 => write!(f, "i64"),
            PrimitiveType::U8 => write!(f, "u8"),
            PrimitiveType::U16 => write!(f, "u16"),
            PrimitiveType::U32 => write!(f, "u32"),
            PrimitiveType::U64 => write!(f, "u64"),
            PrimitiveType::F32 => write!(f, "f32"),
            PrimitiveType::F64 => write!(f, "f64"),
        }
    }
}

#[derive(Clone)]
pub enum TypeKind<'hir> {
    Primitive(PrimitiveType),
    Ref(&'hir Type<'hir>),
    Array(&'hir Type<'hir>, u32),
    Tuple(&'hir [Type<'hir>]),
    Path(Path),
    Function {
        is_variadic: bool,
        params: &'hir [Type<'hir>],
        ret_ty: &'hir Type<'hir>,
    },
}

impl fmt::Debug for TypeKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primitive(arg0) => write!(f, "{arg0:?}"),
            Self::Ref(arg0) => write!(f, "&{arg0:?}"),
            Self::Tuple(tys) => {
                write!(f, "(")?;
                for ty in *tys {
                    write!(f, "{ty:?}")?;
                }
                write!(f, ")")
            }
            Self::Array(arg0, arg1) => write!(f, "[{arg0:?}; {arg1}]"),
            Self::Path(arg0) => {
                let mut first = true;
                for seg in &arg0.segments {
                    if !first {
                        write!(f, "::")?;
                    }
                    first = false;
                    write!(f, "{}", seg.ident.sym)?;
                }
                Ok(())
            }
            Self::Function { is_variadic, params, ret_ty } => {
                write!(f, "fn (")?;
                let mut first = true;
                for p in *params {
                    if !first {
                        write!(f, ",")?;
                    }
                    first = false;
                    write!(f, "{p:?}")?;
                }
                if *is_variadic {
                    write!(f, ", ...")?;
                }
                write!(f, ") -> {ret_ty:?}")
            }
        }
    }
}

impl Hash for TypeKind<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            TypeKind::Path(s) => s.last_segment().ident.sym.hash(state),
            _ => core::mem::discriminant(self).hash(state),
        }
    }
}

impl PartialEq for TypeKind<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Primitive(l0), Self::Primitive(r0)) => l0 == r0,
            (Self::Ref(l0), Self::Ref(r0)) => l0 == r0,
            (Self::Array(l0, l1), Self::Array(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Path(l0), Self::Path(r0)) => {
                l0.last_segment().ident.sym == r0.last_segment().ident.sym
            }
            (
                Self::Function {
                    is_variadic: l_isvariadic,
                    params: l_params,
                    ret_ty: l_ret_ty,
                },
                Self::Function {
                    is_variadic: r_isvariadic,
                    params: r_params,
                    ret_ty: r_ret_ty,
                },
            ) => l_params == r_params && l_ret_ty == r_ret_ty && l_isvariadic == r_isvariadic,
            _ => false,
        }
    }
}

impl Eq for TypeKind<'_> {}

#[derive(Clone)]
pub struct Type<'hir> {
    pub id: HirId,
    pub kind: TypeKind<'hir>,
}

impl fmt::Debug for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

macro_rules! const_variants {
    ( $v:ident ) => {
            pub const $v: Self = Self {
                kind: TypeKind::Primitive(PrimitiveType::$v),
                id: HirId::DUMMY,
            };
    };
    ( = $name:ident : $v:ident) => {
            pub const $name: Self = Self {
                kind: TypeKind::Primitive(PrimitiveType::$v),
                id: HirId::DUMMY,
            };
    };
    ($($( = $name:ident :)?  $v:ident),* $(,)?) => {
        $(
            const_variants!( $( = $name :)? $v);
        )*
    };
}

impl Type<'_> {
    const_variants!(I8, I16, I32, I64, U8, U16, U32, U64, F32, F64,
        = CHAR: Char,
        = BOOL: Bool,
    );

    pub fn is_empty(&self) -> bool {
        matches!(self.kind, TypeKind::Tuple([]))
    }

    pub const fn empty() -> &'static Self {
        &Self {
            id: HirId::DUMMY,
            kind: TypeKind::Tuple(&[]),
        }
    }
}

impl Hash for Type<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.kind.hash(state); }
}

impl PartialEq for Type<'_> {
    fn eq(&self, other: &Self) -> bool { self.kind == other.kind }
}

impl Eq for Type<'_> {}

impl<'hir> Type<'hir> {
    pub fn new(kind: TypeKind<'hir>) -> Self {
        Self {
            kind,
            id: HirId::DUMMY,
        }
    }
}

impl<'hir> HirNode<'hir> for Type<'hir> {
    fn get_hir_id(&self) -> HirId { self.id }

    fn get_hir_node_kind(&'hir self) -> HirNodeKind<'hir> { HirNodeKind::Ty(self) }

    fn set_hir_id(&mut self, id: HirId) { self.id = id; }
}
