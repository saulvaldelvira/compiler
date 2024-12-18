use std::fmt;

#[derive(Clone,Copy,Debug)]
pub struct Span {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

impl Span {
    pub fn join(&self, other: &Span) -> Span {
        Span {
            start_line: self.start_line,
            start_col: self.start_col,
            end_line: other.end_line,
            end_col: other.end_col
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Span { start_line: 1, start_col: 1, end_line: 1, end_col: 1 }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}:{}] [{}:{}]", self.start_line, self.start_col, self.end_line, self.end_col)
    }
}
