use std::borrow::Cow;
use std::fmt;
use lexer::Span;
use session::{with_session_interner, with_symbol, Symbol};

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

impl PartialEq for TypeKind {
    fn eq(&self, other: &Self) -> bool {
        core::mem::discriminant(self) == core::mem::discriminant(other)
    }
}

impl TypeKind {
    fn repr(&self) -> Cow<'static,str> {
        match self {
            TypeKind::Int => "int".into(),
            TypeKind::Float => "float".into(),
            TypeKind::Bool => "bool".into(),
            TypeKind::Char => "char".into(),
            TypeKind::String => "string".into(),
            TypeKind::Error(_) => "error".into(),
            TypeKind::Custom(ct) => with_symbol(ct.name, str::to_string).into(),
            TypeKind::Empty => "()".into(),
        }
    }
}

impl From<&str> for TypeKind {
    fn from(value: &str) -> Self {
        match value {
            "int" => TypeKind::Int,
            "float" => TypeKind::Float,
            "bool" => TypeKind::Bool,
            "char" => TypeKind::Char,
            "string" => TypeKind::String,
            "()" => TypeKind::Empty,
            n => {
                let s = with_session_interner(|i| i.get_or_intern(n));
                TypeKind::Custom(CustomType {
                        name: s
                })
            }
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind.repr())
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}


#[derive(Debug,Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

macro_rules! t {
    ($n:ident, $v:ident) => {
        fn $n() -> Self {
            Self {
                kind: TypeKind::$v,
                span: Span::default()
            }
        }
    };
}

impl Type {
    pub fn empty_implicit() -> Self {
        Self {
            kind: TypeKind::Empty,
            span: Span::default()
        }
    }

    t!(int, Int);
    t!(float, Float);
    t!(char, Char);
    t!(bool, Bool);
    t!(string, String);
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

macro_rules! err {
    ($($t:tt)*) => {
        Type {
            kind: TK::Error(ErrorType {
                msg: format!($($t)*).into(),
            }),
            span: Span::default()
        }
    };
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

    pub fn promote_to(&self, to: &Type) -> Type {
        match &self.kind {
            TypeKind::Int => promote_int(to),
            TypeKind::Float => promote_float(to),
            TypeKind::Bool => promote_bool(to),
            TypeKind::Char => promote_char(to),
            TypeKind::String => promote_string(to),
            TypeKind::Error(_) => self.clone(),
            TypeKind::Custom(_) => todo!(),
            TypeKind::Empty => err!("Can't promote empty type")
        }
    }
}

fn promote_int(to: &Type) -> Type {
    match &to.kind {
        TypeKind::Int => Type::int(),
        TypeKind::Float => Type::float(),
        _ => err!("Can't promote int to {}", to.kind.repr()),
    }
}

fn promote_float(to: &Type) -> Type {
    match &to.kind {
        TypeKind::Float => Type::float(),
        _ => err!("Can't promote int to {}", to.kind.repr()),
    }
}

fn promote_char(to: &Type) -> Type {
    match &to.kind {
        TypeKind::Int => Type::int(),
        TypeKind::Float => Type::float(),
        TypeKind::Char => Type::char(),
        _ => err!("Can't promote int to {}", to.kind.repr()),
    }
}

fn promote_bool(to: &Type) -> Type {
    match &to.kind {
        TypeKind::Int => Type::int(),
        TypeKind::Float => Type::float(),
        TypeKind::Char => Type::char(),
        TypeKind::Bool => Type::bool(),
        _ => err!("Can't promote int to {}", to.kind.repr()),
    }
}

fn promote_string(to: &Type) -> Type {
    match &to.kind {
        TypeKind::String => Type::string(),
        _ => err!("Can't promote int to {}", to.kind.repr()),
    }
}
