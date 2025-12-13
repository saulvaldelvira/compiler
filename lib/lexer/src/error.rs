use span::Span;

pub enum LexerErrorKind {
    FloatLitWithoutFloatingPart,
    UnexpectedCharacter(char),
    ExpectedClosingTickOnCharLiteral,
    UnterminatedComment,
    UnterminatedString,
}

pub struct LexerError {
    pub kind: LexerErrorKind,
    pub span: Span,
}

impl error_manager::Error for LexerError {
    fn get_span(&self) -> Span { self.span }

    fn write_msg(&self, out: &mut dyn core::fmt::Write) -> core::fmt::Result {
        match self.kind {
            LexerErrorKind::FloatLitWithoutFloatingPart => {
                write!(out, "Float literal must have a floating part")
            }
            LexerErrorKind::UnexpectedCharacter(c) => {
                write!(out, "Encountered unexpected character '{c}'")
            }
            LexerErrorKind::ExpectedClosingTickOnCharLiteral => {
                write!(out, "Expected closing ', on char literal")
            }
            LexerErrorKind::UnterminatedComment => write!(out, "Unterminated comment"),
            LexerErrorKind::UnterminatedString => write!(out, "Unterminated string"),
        }
    }
}
