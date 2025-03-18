use core::fmt;
use std::borrow::Cow;
use std::io;

use span::{Span,FilePosition};

pub struct Error {
    msg: Cow<'static, str>,
    span: Option<Span>,
}

impl Error {
    pub fn new(msg: impl Into<Cow<'static,str>>, span: impl Into<Option<Span>>) -> Self {
        Self {
            msg: msg.into(),
            span: span.into()
        }
    }

    pub fn print(&self, src: &str, out: &mut dyn fmt::Write) -> fmt::Result {
        if let Some(s) = self.span {
            let FilePosition { start_line, start_col, .. } = s.file_position(src);
            write!(out, "[{start_line}:{start_col}]: ")?;
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

    pub fn error(&mut self, msg: impl Into<Cow<'static, str>>, span: impl Into<Option<Span>>) {
       self.errors.push(Error::new(msg, span));
    }

    pub fn n_errors(&self) -> usize { self.errors.len() }

    pub fn errors(&self) -> &[Error] { &self.errors }

    pub fn print_errors(&self, src: &str, out: &mut dyn io::Write) -> fmt::Result {
        let mut buf = String::new();
        for err in &self.errors {
            err.print(src, &mut buf)?;
            out.write_all(buf.as_bytes()).unwrap();
            buf.clear();
        }
        Ok(())
    }
}

impl Default for ErrorManager {
    fn default() -> Self {
        Self::new()
    }
}
