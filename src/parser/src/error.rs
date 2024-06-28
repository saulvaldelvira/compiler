use std::{borrow::Cow, error::Error, fmt::{Debug, Display}};

pub struct ParseError {
    msg: Cow<'static,str>,
}

impl ParseError {
    #[inline]
    pub fn from_str(msg: &'static str) -> Self {
        Self { msg: msg.into() }
    }
    #[inline]
    pub fn from_string(msg: String) -> Self {
        Self { msg: msg.into() }
    }
    #[inline]
    pub fn err<T>(self) -> Result<T,Self> {
        Err(self)
    }
    #[inline]
    pub fn get_message(&self) -> &str { &self.msg }
}

impl From<&'static str> for ParseError {
    fn from(value: &'static str) -> Self {
        ParseError::from_str(value)
    }
}

impl From<String> for ParseError {
    fn from(value: String) -> Self {
        ParseError::from_string(value)
    }
}

impl Debug for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       write!(f, "{}", self.get_message())
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       write!(f, "{}", self.get_message())
    }
}

impl Error for ParseError { }
