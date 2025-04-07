use span::Span;

pub enum WarningKind {
    UnnecesaryParenthesis
}

pub struct Warning {
    pub kind: WarningKind,
    pub span: Span,
}

impl error_manager::Error for Warning {
    fn get_span(&self) -> Span { self.span }

    fn write_msg(&self, out: &mut dyn core::fmt::Write) -> core::fmt::Result {
        match self.kind {
            WarningKind::UnnecesaryParenthesis => write!(out, "Unnecesary paranthesis"),
        }
    }
}
