use std::borrow::Cow;
use std::io;
use std::path::PathBuf;

use lexer::token::TokenKind;
use span::Span;

#[derive(Debug)]
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
    ExternFnDefined,
    InvalidBinaryOp(TokenKind),
    InvalidUnaryOp(TokenKind),
    InvalidEscape(String),
    CantPeek,
    NoPreviousToken,
    LexemParseError,
    Use,
    MissingSemmicolon,
    UseTypeUnnamed,
    ReadFile(PathBuf, io::Error),
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
            ParseErrorKind::ExternFnDefined => write!(out, "Expected semicollon after \"extern\" function declaration"),
            ParseErrorKind::Use => write!(out, "\"use\" item must be a path or a type"),
            ParseErrorKind::UseTypeUnnamed => write!(out, "\"use <type>\" must alias to a new name"),
            ParseErrorKind::ReadFile(path, err) => write!(out, "Error reading \"{}\": {err}", path.display()),
            ParseErrorKind::MissingSemmicolon => write!(out, "Missing semmicolon on expression"),
        }
    }
}
