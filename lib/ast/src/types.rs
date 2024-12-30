use std::borrow::Cow;
use session::Symbol;

#[derive(Debug,Clone)]
pub struct StringType;
impl StringType {
    const SIZE: usize = 8;
}

#[derive(Debug,Clone)]
pub struct ErrorType {
    msg: Cow<'static,str>,
}
impl ErrorType {
    fn size(&self) -> usize { self.msg.len() }
}

#[derive(Debug,Clone)]
pub struct CustomType {
    pub name: Symbol,
}

#[derive(Debug,Clone)]
pub enum TypeKind {
    Int,
    Float,
    Bool,
    Char,
    String(StringType),
    Error(ErrorType),
    Custom(CustomType),
    Empty,
}

#[derive(Debug,Clone)]
pub struct Type {
    pub kind: TypeKind,
}

use TypeKind as TK;

impl Type {
    pub fn size(&self) -> usize {
        match &self.kind {
            TK::Bool => 1,
            TK::String(_) => StringType::SIZE,
            TK::Error(err) => err.size(),
            TK::Empty => 0,
            TypeKind::Int => 4,
            TypeKind::Float => 4,
            TK::Char => 1,
            _ => todo!()
        }
    }
    pub fn arithmetic(&self, other: Type) -> Type {
        match &self.kind {
            TK::Bool => {
                match &other.kind {
                    TK::String(_) => Type { kind: TK::Error(ErrorType{ msg: "Can't operate arithmetically with strings".into() }) },
                    _ => other.clone(),
                }
            },
            TK::String(_) => Type { kind: TK::Error(ErrorType{ msg: "Can't operate arithmetically with strings".into() }) },
            TK::Empty => Type { kind: TK::Error(ErrorType{ msg: "Can't operate arithmetically with the empty type".into() }) },
            TK::Error { .. } => self.clone(),
            TypeKind::Int => match other.kind {
                TK::Float => Type { kind: TypeKind::Float },
                TK::Int => Type { kind: TypeKind::Int },
                _ => Type { kind: TK::Error(ErrorType{ msg: format!("Can't operate arithmetically with {:?}", other.kind).into() }) },
            }
            TypeKind::Float => match other.kind {
                TK::Int | TK::Float => Type { kind: TypeKind::Float },
                _ => Type { kind: TK::Error(ErrorType{ msg: format!("Can't operate arithmetically with {:?}", other.kind).into() }) },
            }
            TK::Char => Type { kind: TK::Error(ErrorType{ msg: format!("Can't operate arithmetically with chars").into() }) },
            _ => todo!()
        }
    }
}

