use hir::Ident;
use interner::Symbol;
use span::Span;

pub enum SemanticErrorKind {
    LValue,
    AccessToNonStruct(String),
    NonExistingField { st: Symbol, field: Symbol },
    IndexToNonArray,
    NonIntegerIndex,
    MissingField(Ident),
    CantPromote(String, String),
    MismatchedIfTypes(String, String),
    NonEmptyThenWithoutElse(String),
    Arithmetic(String, String),
    Logical(String, String),
    Compare(String, String),
    CallToNonFunction,
    MismatchedArgsNum { expected: usize, received: usize },
    FunctionNeedsReturn(Symbol),
    MistmatchedReturn { expected: String, got: String },
    InvalidCast { from: String, to: String },
    NonPrimitiveCast(String),
    DereferenceNonRef(String),
    NonBooleanCondition(&'static str),
    TupleAccessOnNonTuple(String),
    InvalidIndexForTuple(u16, u16),
    MismatchedArrayTypes(String, String),
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
            SemanticErrorKind::AccessToNonStruct(ty) => {
                write!(out, "Attempt to perform field access on a non-struct type \"{ty}\"")
            }
            SemanticErrorKind::MissingField(ident) => {
                write!(out, "Unexisting field with name '{:#?}'", ident.sym)
            }
            SemanticErrorKind::IndexToNonArray => write!(out, "Attempt to index a non-array type"),
            SemanticErrorKind::NonIntegerIndex => {
                write!(
                    out,
                    "Attempt to index an array with a non-integer expression"
                )
            }
            SemanticErrorKind::CantPromote(l, r) => write!(out, "Can't promote {l} to {r}"),
            SemanticErrorKind::NonExistingField { st, field } => {
                write!(out, "Struct {st:?} doesn't have a field named {field:?}")
            }
            SemanticErrorKind::Arithmetic(l, r) => {
                write!(out, "Can't operate arithmetically between {l} and {r}")
            }
            SemanticErrorKind::Logical(l, r) => {
                write!(out, "Can't operate logically between {l} and {r}")
            }
            SemanticErrorKind::Compare(l, r) => write!(out, "Can't compare {l} and {r}"),
            SemanticErrorKind::CallToNonFunction => write!(out, "Call to non-function type"),
            SemanticErrorKind::MismatchedArgsNum { expected, received } => {
                write!(
                    out,
                    "Mismatched number of arguments on function call. Expected {expected} received {received}"
                )
            }
            SemanticErrorKind::FunctionNeedsReturn(name) => {
                write!(out, "Some branches of '{name:#?}' won't return")
            }
            SemanticErrorKind::MistmatchedReturn { expected, got } => {
                write!(
                    out,
                    "Mistmatched retun type. Expected {expected}, got {got}"
                )
            }
            SemanticErrorKind::InvalidCast { from, to } => {
                write!(out, "Invalid cast from {from} to {to}")
            }
            SemanticErrorKind::NonPrimitiveCast(t) => {
                write!(out, "Can't perform cast on non primitive type {t}")
            }
            SemanticErrorKind::DereferenceNonRef(t) => {
                write!(out, "Attempt to dereference non-ref type {t}")
            }
            SemanticErrorKind::NonBooleanCondition(name) => {
                write!(out, "Non boolean condition on {name} statement")
            }
            SemanticErrorKind::MismatchedIfTypes(ift, iff) => {
                write!(out, "If expression has mismatched types on it's true and false branches ({ift} and {iff})")
            }
            SemanticErrorKind::NonEmptyThenWithoutElse(ty) => {
                writeln!(out, "Non empty type on then block ({ty}) without an else")
            }
            SemanticErrorKind::TupleAccessOnNonTuple(ty) => {
                writeln!(out, "Can't permform tuple access on non-tuple type \"{ty}\"")
            }
            SemanticErrorKind::InvalidIndexForTuple(index, len) => {
                write!(out, "Index {index} invalid for tuple of length {len}")
            }
            SemanticErrorKind::MismatchedArrayTypes(expected, found) => {
                write!(out, "Unexpected type on array literal. Expected {expected}, found {found}")
            }
        }
    }
}

pub enum SemanticWarningKind {
    UselessExpressionAsStmt,
}

pub struct SemanticWarning {
    pub kind: SemanticWarningKind,
    pub span: Span,
}

impl error_manager::Error for SemanticWarning {
    fn get_span(&self) -> Span { self.span }

    fn write_msg(&self, out: &mut dyn core::fmt::Write) -> core::fmt::Result {
        match &self.kind {
            SemanticWarningKind::UselessExpressionAsStmt => {
                write!(out, "This expression has no side-effects and can be elided")
            }
        }
    }
}
