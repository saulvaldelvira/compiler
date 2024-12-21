use std::{fmt, str};

#[derive(Clone,Copy,Debug)]
pub struct Span {
    pub offset: usize,
    pub len: usize,
}

pub struct FilePosition {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
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
    pub fn file_position(&self, src: &str) -> FilePosition {
        let mut fpos = FilePosition {
            start_line: 0,
            start_col: 0,
            end_line: 0,
            end_col: 0,
        };

        for c in src[..self.offset].chars() {
            if c == '\n' {
                fpos.start_col = 0;
                fpos.start_line += 1;
            }
            fpos.start_col += 1;
        }

        fpos.end_line = fpos.start_line;
        fpos.end_col = fpos.start_col;

        for c in src[self.offset..self.offset + self.len].chars() {
            if c == '\n' {
                fpos.end_col = 0;
                fpos.end_line += 1;
            }
            fpos.end_col += 1;
        }

        fpos
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}:{}]", self.offset, self.offset + self.len)
    }
}
