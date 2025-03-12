use core::fmt;

use lexer::Span;

struct Error {
    msg: String,
    span: Option<Span>,
}

impl Error {
    pub fn new(msg: impl Into<String>, span: impl Into<Option<Span>>) -> Self {
        Self {
            msg: msg.into(),
            span: span.into()
        }
    }

    pub fn print(&self, out: &mut dyn fmt::Write) -> fmt::Result {
        if let Some(s) = self.span {
            write!(out, "{s}: ")?;
        }
        writeln!(out, "{}", self.msg)
    }
}

pub struct ErrorManager {
    errors: Vec<Error>,
}

impl ErrorManager {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn error(&mut self, msg: impl Into<String>, span: impl Into<Option<Span>>) {
       self.errors.push(Error::new(msg, span));
    }

    pub fn n_errors(&self) -> usize { self.errors.len() }

    pub fn print_errors(&self, out: &mut dyn fmt::Write) -> fmt::Result {
        for err in &self.errors {
            err.print(out)?;
        }
        Ok(())
    }
}

impl Default for ErrorManager {
    fn default() -> Self {
        Self::new()
    }
}
