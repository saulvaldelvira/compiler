use std::{error::Error, fmt::{Debug, Display}};

pub enum ParseError {
    Str(&'static str),
    String(String),
}

impl ParseError {
    pub fn from_str<T>(msg: &'static str) -> Result<T,Self> {
        Err(Self::Str(msg))
    }
    pub fn from_string<T>(msg: String) -> Result<T,Self> {
        Err(Self::String(msg))
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
