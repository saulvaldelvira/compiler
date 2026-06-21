use std::io;

use span::Span;

pub enum LexerErrorKind {
    UnexpectedCharacter(char),
    ExpectedClosingTickOnCharLiteral,
    UnterminatedComment,
    UnterminatedString,
    InvalidEscape(char),
}

pub struct LexerError {
    pub kind: LexerErrorKind,
    pub span: Span,
}

impl error_manager::Error for LexerError {
    fn get_span(&self) -> Span { self.span }

    fn write_msg(&self, out: &mut dyn io::Write) -> io::Result<()> {
        match self.kind {
            LexerErrorKind::UnexpectedCharacter(c) => {
                write!(out, "Encountered unexpected character '{c}'")
            }
            LexerErrorKind::ExpectedClosingTickOnCharLiteral => {
                write!(out, "Expected closing ', on char literal")
            }
            LexerErrorKind::UnterminatedComment => write!(out, "Unterminated comment"),
            LexerErrorKind::UnterminatedString => write!(out, "Unterminated string"),
            LexerErrorKind::InvalidEscape(c) => write!(out, "Invalid escape '\\{c}'"),
        }
    }
}
