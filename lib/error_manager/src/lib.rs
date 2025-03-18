use core::fmt;
use std::borrow::Cow;
use std::io;
use std::ops::Deref;

use span::{Span,FilePosition};

pub trait Error {
    fn get_span(&self) -> Span;
    fn write_msg(&self, out: &mut dyn fmt::Write) -> fmt::Result;
}

pub struct StringError {
    msg: Cow<'static, str>,
    span: Span,
}

impl Error for StringError {
    fn write_msg(&self, out: &mut dyn fmt::Write) -> fmt::Result {
        write!(out, "{}", self.msg)
    }

    fn get_span(&self) -> Span {
        self.span
    }
}

pub struct ErrorManager {
    errors: Vec<Box<dyn Error>>,
    warnings: Vec<Box<dyn Error>>,
}

fn print_error(err: &dyn Error, src: &str, out: &mut dyn fmt::Write) -> fmt::Result {
    let FilePosition { start_line, start_col, .. } = err.get_span().file_position(src);
    write!(out, "[{start_line}:{start_col}]: ")?;
    err.write_msg(out)?;
    writeln!(out)
}

impl ErrorManager {
    pub fn new() -> Self {
        Self { errors: Vec::new(), warnings: Vec::new() }
    }

    pub fn emit_error(&mut self, err: impl Error + 'static) {
        self.errors.push(Box::new(err));
    }

    pub fn error(&mut self, msg: impl Into<Cow<'static, str>>, span: impl Into<Option<Span>>) {
       self.errors.push(Box::new(StringError { msg: msg.into(), span: span.into().unwrap() }));
    }

    pub fn warning(&mut self, msg: impl Into<Cow<'static, str>>, span: impl Into<Option<Span>>) {
       self.warnings.push(Box::new(StringError { msg: msg.into(), span: span.into().unwrap() }));
    }

    pub fn n_errors(&self) -> usize { self.errors.len() }

    pub fn print_errors(&self, src: &str, out: &mut dyn io::Write) -> fmt::Result {
        let mut buf = String::new();
        for err in &self.errors {
            print_error(err.deref(), src, &mut buf)?;
            out.write_all(buf.as_bytes()).unwrap();
            buf.clear();
        }
        Ok(())
    }

    pub fn n_warnings(&self) -> usize { self.warnings.len() }

    pub fn print_warnings(&self, src: &str, out: &mut dyn io::Write) -> fmt::Result {
        let mut buf = String::new();
        for err in &self.warnings {
            print_error(err.deref(), src, &mut buf)?;
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
