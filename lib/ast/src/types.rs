use std::borrow::Cow;

#[derive(Debug,Clone)]
pub struct NumberType;

impl NumberType {
    const SIZE: usize = 8;
}

#[derive(Debug,Clone)]
pub struct BoolType;

impl BoolType {
    const SIZE: usize = 8;
}

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
pub enum TypeKind {
    Number(NumberType),
    Bool(BoolType),
    String(StringType),
    Error(ErrorType)
}

#[derive(Debug,Clone)]
pub struct Type {
    pub kind: TypeKind,
}

use TypeKind as TK;

impl Type {
    pub fn size(&self) -> usize {
        match &self.kind {
            TK::Number(_) => NumberType::SIZE,
            TK::Bool(_) => BoolType::SIZE,
            TK::String(_) => StringType::SIZE,
            TK::Error(err) => err.size(),
        }
    }
    pub fn arithmetic(&self, other: Type) -> Type {
        match &self.kind {
            TK::Number(_) => todo!(),
            TK::Bool(_) => {
                match &other.kind {
                    TK::String(_) => Type { kind: TK::Error(ErrorType{ msg: "Can't operate arithmetically with strings".into() }) },
                    _ => other.clone(),
                }
            },
            TK::String(_) => Type { kind: TK::Error(ErrorType{ msg: "Can't operate arithmetically with strings".into() }) },
            TK::Error { .. } => self.clone(),
        }
    }
}

