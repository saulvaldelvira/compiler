use std::borrow::Cow;

use builders::IntoEnum;
use lexer::{spanned, Spanned};

use crate::AST;

#[spanned]
#[derive(Debug,IntoEnum,Clone)]
#[into_enum(enum_name = Type, field = Number)]
pub struct NumberType;
impl NumberType {
    const SIZE: usize = 8;
}

#[spanned]
#[derive(Debug,IntoEnum,Clone)]
#[into_enum(enum_name = Type, field = Bool)]
pub struct BoolType;
impl BoolType {
    const SIZE: usize = 8;
}

#[spanned]
#[derive(Debug,IntoEnum,Clone)]
#[into_enum(enum_name = Type, field = String)]
pub struct StringType;
impl StringType {
    const SIZE: usize = 8;
}

#[spanned]
#[derive(Debug,IntoEnum,Clone)]
#[into_enum(enum_name = Type, field = Error)]
pub struct ErrorType {
    msg: Cow<'static,str>,
}
impl ErrorType {
    fn size(&self) -> usize { self.msg.len() }
}

#[derive(Clone,IntoEnum,Spanned)]
#[into_enum(enum_name = AST)]
pub enum Type {
    Number(NumberType),
    Bool(BoolType),
    String(StringType),
    Error(ErrorType)
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Number(_) => NumberType::SIZE,
            Type::Bool(_) => BoolType::SIZE,
            Type::String(_) => StringType::SIZE,
            Type::Error(err) => err.size(),
        }
    }
    pub fn arithmetic(&self, other: Type) -> Type {
        match self {
            Type::Number(_) => todo!(),
            Type::Bool(_) => {
                match other {
                    Type::String(_) => ErrorType::new("Can't operate arithmetically with strings").into(),
                    _ => other.clone(),
                }
            },
            Type::String(_) => ErrorType::new("Can't operate arithmetically with strings").into(),
            Type::Error { .. } => self.clone(),
        }
    }
}

