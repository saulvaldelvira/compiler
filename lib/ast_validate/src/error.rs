use std::io;

use span::Span;

pub enum ErrorKind {
    UnnecesaryParenthesis,
    NonExternVariadic,
}

pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

impl error_manager::Error for Error {
    fn get_span(&self) -> Span { self.span }

    fn write_msg(&self, out: &mut dyn io::Write) -> io::Result<()> {
        match self.kind {
            ErrorKind::UnnecesaryParenthesis => write!(out, "Unnecesary paranthesis"),
            ErrorKind::NonExternVariadic => write!(out, "Only \"extern\" functions are allowed to be variadic"),
        }
    }
}
