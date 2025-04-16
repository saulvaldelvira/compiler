use core::fmt;
use std::hash::Hash;

use crate::hir_id::HirNode;
use crate::node_map::HirNodeKind;
use crate::HirId;

use super::Path;

#[derive(Hash,PartialEq,Eq,Clone)]
pub enum PrimitiveType {
    Int,
    Char,
    Float,
    Bool,
    Empty,
}

impl fmt::Debug for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Char => write!(f, "char"),
            Self::Float => write!(f, "float"),
            Self::Bool => write!(f, "bool"),
            Self::Empty => write!(f, "()"),
        }
    }
}

#[derive(Clone)]
pub enum TypeKind<'hir> {
    Primitive(PrimitiveType),
    Ref(&'hir Type<'hir>),
    Array(&'hir Type<'hir>, usize),
    Path(Path),
    Function { params: &'hir [Type<'hir>], ret_ty: &'hir Type<'hir> }
}

impl fmt::Debug for TypeKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primitive(arg0) => write!(f, "{arg0:?}"),
            Self::Ref(arg0) => write!(f, "&{arg0:?}"),
            Self::Array(arg0, arg1) => write!(f, "[{arg0:?}; {arg1}]"),
            Self::Path(arg0) => write!(f, "struct {}", arg0.segments.last().unwrap().ident.sym),
            Self::Function { params, ret_ty } => {
                write!(f, "fn (")?;
                let mut first = true;
                for p in *params {
                    if !first {
                        write!(f, ",")?;
                    }
                    first = false;
                        write!(f, "{p:?}")?;
                }
                write!(f, ") -> {ret_ty:?}")
            }
        }
    }
}

impl Hash for TypeKind<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            TypeKind::Path(s) => s.segments.last().unwrap().ident.sym.hash(state),
            _ => core::mem::discriminant(self).hash(state)
        }
    }
}

impl PartialEq for TypeKind<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Primitive(l0), Self::Primitive(r0)) => l0 == r0,
            (Self::Ref(l0), Self::Ref(r0)) => l0 == r0,
            (Self::Array(l0, l1), Self::Array(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Path(l0), Self::Path(r0)) => l0.segments.last().unwrap().ident.sym
                                                    == r0.segments.last().unwrap().ident.sym,
            (Self::Function { params: l_params, ret_ty: l_ret_ty }, Self::Function { params: r_params, ret_ty: r_ret_ty }) => l_params == r_params && l_ret_ty == r_ret_ty,
            _ => false,
        }
    }
}

impl Eq for TypeKind<'_> { }

#[derive(Clone)]
pub struct Type<'hir> {
    pub id: HirId,
    pub kind: TypeKind<'hir>
}

impl fmt::Debug for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

impl<'ty> Type<'ty> {
    const INT: Self = Self { kind: TypeKind::Primitive(PrimitiveType::Int), id: HirId::DUMMY };
    const FLOAT: Self = Self { kind: TypeKind::Primitive(PrimitiveType::Float), id: HirId::DUMMY };
    const CHAR: Self = Self { kind: TypeKind::Primitive(PrimitiveType::Char), id: HirId::DUMMY };
    const BOOL: Self = Self { kind: TypeKind::Primitive(PrimitiveType::Bool), id: HirId::DUMMY };
    const EMPTY: Self = Self { kind: TypeKind::Primitive(PrimitiveType::Empty), id: HirId::DUMMY };

    pub const fn int() -> &'ty Self { &Self::INT }
    pub const fn float() -> &'ty Self { &Self::FLOAT }
    pub const fn char() -> &'ty Self { &Self::CHAR }
    pub const fn bool() -> &'ty Self { &Self::BOOL }
    pub const fn empty() -> &'ty Self { &Self::EMPTY }

    pub fn primitive_array() -> &'ty [Self] {
        &[
            Self::INT,
            Self::FLOAT,
            Self::CHAR,
            Self::BOOL,
            Self::EMPTY,
        ]
    }
}

impl Hash for Type<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

impl PartialEq for Type<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for Type<'_> { }

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

    fn get_hir_node_kind(&'hir self) -> HirNodeKind<'hir> {
        HirNodeKind::Ty(self)
    }

    fn set_hir_id(&mut self, id: HirId) {
        self.id = id;
    }
}
