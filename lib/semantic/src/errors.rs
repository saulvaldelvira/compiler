use hir::Ident;
use session::Symbol;
use span::Span;

pub enum SemanticErrorKind {
    LValue,
    AccessToNonStruct,
    NonExistingField { st: Symbol, field: Symbol },
    IndexToNonArray,
    NonIntegerIndex,
    MissingField(Ident),
    CantPromote(String,String),
    Arithmetic(String,String),
    Logical(String,String),
    Compare(String,String),
    CallToNonFunction,
    MismatchedArgsNum { expected: usize, received: usize },
    FunctionNeedsReturn(Symbol),
    MistmatchedReturn { expected: String, got: String },
    InvalidCast { from: String, to: String },
    NonPrimitiveCast(String),
    DereferenceNonRef(String),
}

pub struct SemanticError {
    pub kind: SemanticErrorKind,
    pub span: Span,
}

impl error_manager::Error for SemanticError {
    fn get_span(&self) -> Span { self.span }

    fn write_msg(&self, out: &mut dyn core::fmt::Write) -> core::fmt::Result {
        match &self.kind {
            SemanticErrorKind::LValue => write!(out, "Attempt to assign to non-lvalue"),
            SemanticErrorKind::AccessToNonStruct => write!(out, "Attempt to perform field access on a non-struct type"),
            SemanticErrorKind::MissingField(ident) => write!(out, "Unexisting field with name '{:#?}'", ident.sym),
            SemanticErrorKind::IndexToNonArray => write!(out, "Attempt to index a non-array type"),
            SemanticErrorKind::NonIntegerIndex => write!(out, "Attempt to index an array with a non-integer expression"),
            SemanticErrorKind::CantPromote(l, r) => write!(out, "Can't promote {l} to {r}"),
            SemanticErrorKind::NonExistingField { st, field } => write!(out, "Struct {st:?} doesn't have a field named {field:?}"),
            SemanticErrorKind::Arithmetic(l, r) => write!(out, "Can't operate arithmetically between {l} and {r}"),
            SemanticErrorKind::Logical(l, r) => write!(out, "Can't operate logically between {l} and {r}"),
            SemanticErrorKind::Compare(l, r) => write!(out, "Can't compare {l} and {r}"),
            SemanticErrorKind::CallToNonFunction => write!(out, "Call to non-function type"),
            SemanticErrorKind::MismatchedArgsNum { expected, received } => write!(out, "Mismatched number of arguments on function call. Expected {expected} received {received}"),
            SemanticErrorKind::FunctionNeedsReturn(name) => write!(out, "Some branches of '{name:#?}' won't return"),
            SemanticErrorKind::MistmatchedReturn { expected, got } => write!(out, "Mistmatched retun type. Expected {expected}, got {got}"),
            SemanticErrorKind::InvalidCast { from, to } => write!(out, "Invalid cast from {from} to {to}"),
            SemanticErrorKind::NonPrimitiveCast(t) => write!(out, "Can't perform cast on non primitive type {t}"),
            SemanticErrorKind::DereferenceNonRef(t) => write!(out, "Attempt to dereference non-ref type {t}"),
        }
    }
}
