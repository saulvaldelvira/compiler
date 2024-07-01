use std::borrow::Cow;

use builders::IntoEnum;

use crate::AST;

#[derive(Clone,IntoEnum)]
#[into_enum(enum_name = AST)]
pub enum Type {
    Number,
    Bool,
    String,
    Error { msg: Cow<'static,str> }
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Number => 8,
            Type::Bool => 1,
            Type::String => 4,
            Type::Error{ msg } => msg.len(),
        }
    }
    pub fn arithmetic(&self, other: Type) -> Type {
        match self {
            Type::Number => todo!(),
            Type::Bool => {
                match other {
                    Type::Number => Type::Number,
                    Type::Bool => Type::Number,
                    Type::String => Type::Error { msg: "Can't operate arithmetically with strings".into() },
                    Type::Error { .. } => other.clone(),
                }
            },
            Type::String => Type::Error { msg: "Can't operate arithmetically with strings".into() },
            Type::Error { .. } => self.clone(),
        }
    }
}

