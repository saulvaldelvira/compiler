use lexer::Span;

pub struct Cursor {
    spans: Vec<Span>,
    current: Span,
}

impl Cursor {
    pub fn new() -> Self {
        Self {
            spans: Vec::new(),
            current: Span::default()
        }
    }
    pub fn push(&mut self, span: Span) {
        self.spans.push(self.current);
        self.current = span;
    }
    pub fn pop(&mut self, end: Span) -> Span {
        let span = self.current.join(&end);
        self.current = self.spans.pop().unwrap_or(end);
        span
    }
}
