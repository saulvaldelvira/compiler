use std::borrow::Cow;
use lexer::Span;
use session::Symbol;

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
    String,
    Error(ErrorType),
    Custom(CustomType),
    Empty,
}

#[derive(Debug,Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

macro_rules! impl_ty {
    ($n:ident, $v:ident) => {
        pub fn $n() -> Self {
            Self {
                kind: TypeKind:: $v,
                span: Span::default(),
            }
        }
    };
}

impl Type {
    pub const fn empty_implicit() -> Self {
        Self {
            kind: TypeKind::Empty,
            span: Span::new()
        }
    }
    impl_ty!(int, Int);
    impl_ty!(float, Float);
    impl_ty!(string, String);
    impl_ty!(bool, Bool);
    impl_ty!(char, Char);
}

use TypeKind as TK;

fn error(msg: impl Into<Cow<'static,str>>) -> Type {
    Type {
        kind: TypeKind::Error(ErrorType {
            msg: msg.into()
        }),
        span: Span::default()
    }
}

impl Type {
    pub fn size(&self) -> usize {
        match &self.kind {
            TK::Bool => 1,
            TK::String => 8,
            TK::Error(err) => err.size(),
            TK::Empty => 0,
            TypeKind::Int => 4,
            TypeKind::Float => 4,
            TK::Char => 1,
            _ => todo!()
        }
    }
    pub fn arithmetic(&self, other: Type) -> Type {
        let span = self.span;
        match &self.kind {
            TK::Bool => {
                match &other.kind {
                    TK::String => Type { kind: TK::Error(ErrorType{ msg: "Can't operate arithmetically with strings".into() }), span },
                    _ => other.clone(),
                }
            },
            TK::String => Type { kind: TK::Error(ErrorType{ msg: "Can't operate arithmetically with strings".into() }), span },
            TK::Empty => Type { kind: TK::Error(ErrorType{ msg: "Can't operate arithmetically with the empty type".into() }), span},
            TK::Error { .. } => self.clone(),
            TypeKind::Int => match other.kind {
                TK::Float => Type { kind: TypeKind::Float, span},
                TK::Int => Type { kind: TypeKind::Int, span},
                _ => Type { kind: TK::Error(ErrorType{ msg: format!("Can't operate arithmetically with {:?}", other.kind).into() }), span},
            }
            TypeKind::Float => match other.kind {
                TK::Int | TK::Float => Type { kind: TypeKind::Float, span },
                _ => error(format!("Can't operate arithmetically with {:?}", other.kind))
            }
            TK::Char => Type { kind: TK::Error(ErrorType{ msg: "Can't operate arithmetically with chars".into() }), span},
            _ => todo!()
        }
    }
}

