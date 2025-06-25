//! Utilities to represent spans inside a file

use std::{fmt, fmt::Debug, ops::Deref, str};

pub mod source;
pub use source::{FileName, Source};

/// Represents a span in a buffer, bounded by an offset and a len
#[derive(Clone, Copy, Default, PartialEq)]
pub struct Span {
    /// Offset of the span inside the buffer
    pub offset: u32,
    /// Length of the span
    pub len: u32,
    /// File id
    pub fileid: u32,
}

impl Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Span { offset, len, fileid } = self;
        write!(f, "Span {{ {offset}, {len}, {fileid} }}")
    }
}

/// Represents a [`Span`] in a file, bounded by
/// it's start line and col, plus it's end line and col
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct FilePosition {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

impl fmt::Display for FilePosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let FilePosition {
            start_line,
            start_col,
            ..
        } = self;
        write!(f, "[{start_line}:{start_col}]")
    }
}

impl Span {

    pub const fn new(offset: usize, len: usize, fileid: u32) -> Self {
        assert!(offset + len < u32::MAX as usize);

        #[allow(clippy::cast_possible_truncation)]
        Self {
            offset: offset as u32,
            len: len as u32,
            fileid,
        }
    }

    pub const fn dummy() -> Span { Span { offset: 0, len: 0, fileid: u32::MAX } }
    /// Joins two spans together.
    /// Returns the smallest Span that covers both.
    #[must_use]
    pub const fn join(&self, other: &Span) -> Span {
        assert!(self.fileid == other.fileid);
        let (left, right) = if self.offset < other.offset {
            (self, other)
        } else {
            (other, self)
        };
        Span {
            offset: left.offset,
            len: right.end_offset() - left.offset,
            fileid: self.fileid,
        }
    }
    /// Slices the given string with this span
    #[must_use]
    #[inline]
    pub fn slice<'a>(&self, src: &'a str) -> &'a str {
        let start = self.offset as usize;
        let end = start + self.len as usize;
        &src[start..end]
    }
    /// Gets the [file position] of this span in the given string slice
    ///
    /// [file position]: FilePosition
    #[must_use]
    pub fn file_position(&self, src: &str) -> FilePosition {
        let mut fpos = FilePosition {
            start_line: 1,
            start_col: 0,
            end_line: 0,
            end_col: 0,
        };

        for c in src[..self.offset as usize].chars() {
            if c == '\n' {
                fpos.start_col = 0;
                fpos.start_line += 1;
            }
            fpos.start_col += 1;
        }

        fpos.end_line = fpos.start_line;
        fpos.end_col = fpos.start_col;

        let start = self.offset as usize;
        let end = start + self.len as usize;
        for c in src[start..end].chars() {
            if c == '\n' {
                fpos.end_col = 0;
                fpos.end_line += 1;
            }
            fpos.end_col += 1;
        }

        fpos
    }
    /// Returns the end offset of the span. This is, the
    /// offset of the span plus it's length
    #[must_use]
    #[inline]
    pub const fn end_offset(&self) -> u32 { self.offset + self.len }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}:{}]", self.offset, self.offset + self.len)
    }
}

#[derive(Debug)]
pub struct Spanned<T> {
    pub val: T,
    pub span: Span,
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target { &self.val }
}

impl<T: Clone> Clone for Spanned<T> {
    fn clone(&self) -> Self {
        Self {
            val: self.val.clone(),
            span: self.span,
        }
    }
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool { self.val == other.val }
}
