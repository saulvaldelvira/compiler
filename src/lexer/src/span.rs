use std::{fmt, str};

#[derive(Clone,Copy,Debug)]
pub struct Span {
    pub offset: usize,
    pub len: usize,
}

impl Span {
    pub fn join(&self, other: &Span) -> Span {
        Span {
            offset: self.offset,
            len: (other.offset + other.len) - self.offset,
        }
    }
    pub fn slice<'a>(&self, src: &'a str) -> &'a str {
        &src[self.offset..self.offset + self.len]
    }
    /// Gets the line and column of this span in the
    /// given string slice
    pub fn line_col(&self, src: &str) -> (usize,usize) {
        let mut nlines = 1;
        let mut ncols = 1;
        for c in src[..self.offset + self.len].chars() {
            if c == '\n' {
                ncols = 0;
                nlines += 1;
            }
            ncols += 1;
        }
        (nlines,ncols)
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}:{}]", self.offset, self.offset + self.len)
    }
}
