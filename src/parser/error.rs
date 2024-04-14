use std::{error::Error, fmt::{Debug, Display}};

pub enum ParseError {
    Str(&'static str),
    String(String),
}

impl ParseError {
    pub fn from_str(msg: &'static str) -> Self {
        Self::Str(msg)
    }
    pub fn from_string(msg: String) -> Self {
        Self::String(msg)
    }
    pub fn err<T>(self) -> Result<T,Self> {
        Err(self)
    }
    pub fn get_message(&self) -> &str {
        match &self {
            Self::Str(msg) => msg,
            Self::String(msg) => &msg,
        }
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
