use core::fmt;
use std::borrow::Cow;
use std::io;

pub type Result<T> = std::result::Result<T,Error>;

#[derive(Debug,Clone)]
pub struct Error {
    msg: Cow<'static, str>,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl From<&'static str> for Error {
    fn from(value: &'static str) -> Self {
        Self {
            msg: value.into()
        }
    }
}

impl From<String> for Error {
    fn from(value: String) -> Self {
        Self {
            msg: value.into()
        }
    }
}

impl From<fmt::Error> for Error {
    fn from(value: fmt::Error) -> Self {
        Self {
            msg: value.to_string().into()
        }
    }
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self {
            msg: value.to_string().into()
        }
    }
}
