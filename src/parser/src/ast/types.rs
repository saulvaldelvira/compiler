use std::borrow::Cow;

pub enum TypeEnum {
    Number, Bool, String, Error
}

pub trait Type {
    fn size(&self) -> usize;
    fn arithmetic(&self, other: Box<dyn Type>) -> Box<dyn Type>;
    fn get_enum(&self) -> TypeEnum;
}

pub struct NumberType;
impl Type for NumberType {
    fn size(&self) -> usize { 8 }
    fn arithmetic(&self, other: Box<dyn Type>) -> Box<dyn Type> {
        match other.get_enum() {
            TypeEnum::Number | TypeEnum::Bool => Box::new(NumberType),
            _ => Box::new(ErrorType { msg: "Can't operate with non-numbers".into() })
        }
    }
    fn get_enum(&self) -> TypeEnum {
        TypeEnum::Number
    }
}

pub struct BoolType;
impl Type for BoolType {
    fn size(&self) -> usize { 1 }
    fn arithmetic(&self, other: Box<dyn Type>) -> Box<dyn Type> {
        other
    }
    fn get_enum(&self) -> TypeEnum {
        TypeEnum::Bool
    }
}

pub struct StringType;

impl Type for StringType {
    fn size(&self) -> usize { 4 }

    fn arithmetic(&self, other: Box<dyn Type>) -> Box<dyn Type> {
        match other.get_enum() {
            TypeEnum::String => Box::new(StringType),
            _ => Box::new(ErrorType { msg: "Can't operate with strings".into() })
        }
    }
    fn get_enum(&self) -> TypeEnum {
        TypeEnum::String
    }
}

#[derive(Clone)]
pub struct ErrorType {
    msg: Cow<'static,str>
}
impl Type for ErrorType {
    fn size(&self) -> usize { self.msg.len() }

    fn arithmetic(&self, _other: Box<dyn Type>) -> Box<dyn Type> {
        Box::new(self.clone())
    }
    fn get_enum(&self) -> TypeEnum { TypeEnum::Error }
}
