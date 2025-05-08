use std::borrow::Cow;

use lexer::token::TokenKind;
use span::Span;

pub enum ParseErrorKind {
    ExpectedToken {
        tokens: Cow<'static, [TokenKind]>,
        found: TokenKind,
    },
    ExpectedNode(&'static str),
    ExpectedConstruct {
        expected: &'static str,
        found: String,
    },
    InvalidBinaryOp(TokenKind),
    InvalidUnaryOp(TokenKind),
    InvalidEscape(String),
    CantPeek,
    NoPreviousToken,
    LexemParseError,
}

pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

impl error_manager::Error for ParseError {
    fn get_span(&self) -> Span { self.span }

    fn write_msg(&self, out: &mut dyn core::fmt::Write) -> core::fmt::Result {
        match &self.kind {
            ParseErrorKind::ExpectedToken { tokens, found } => {
                write!(out, "Expected {tokens:?}, found {found}")
            }
            ParseErrorKind::ExpectedNode(name) => write!(out, "Expected {name}"),
            ParseErrorKind::ExpectedConstruct { expected, found } => {
                write!(out, "Expected {expected}, found '{found}'")
            }
            ParseErrorKind::InvalidBinaryOp(op) => write!(out, "Invalid binary operand '{op}'"),
            ParseErrorKind::InvalidUnaryOp(op) => write!(out, "Invalid unary operand '{op}'"),
            ParseErrorKind::InvalidEscape(lit) => write!(out, "Invalid escape '{lit}'"),
            ParseErrorKind::CantPeek => write!(out, "Can't peek"),
            ParseErrorKind::NoPreviousToken => write!(out, "No previous token"),
            ParseErrorKind::LexemParseError => write!(out, "Error parsing lexem"),
        }
    }
}
