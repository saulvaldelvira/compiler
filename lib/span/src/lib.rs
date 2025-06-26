//! Utilities to represent spans inside a file

use std::{fmt, fmt::Debug, ops::Deref, str};

pub mod source;
#[doc(hidden)]
pub use source::SourceMap;

/// Represents a span in the [`SourceMap`], bounded by an offset and a len
///
/// # About offset
/// The offset of this Span is an absolute offset inside the [`SourceMap`]
/// It doesn't represent the offset relative to the file it was created from.
/// This is done to avoid havind an additional field to identify the source file.
///
/// To get the base offset of a [`Span`] you can use the
/// [`SourceMap::get_base_offset_of_span`] function
#[derive(Clone, Copy, Default, PartialEq)]
pub struct Span {
    /// Offset of the span
    pub offset: usize,
    /// Length of the span
    pub len: usize
}

impl Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Span { offset, len } = self;
        write!(f, "Span {{ {offset}, {len} }}")
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

    pub const fn dummy() -> Span { Span { offset: 0, len: 0 } }

    /// Joins two spans together.
    /// Returns the smallest Span that covers both.
    #[must_use]
    pub const fn join(&self, other: &Span) -> Span {
        let (left, right) = if self.offset < other.offset {
            (self, other)
        } else {
            (other, self)
        };
        Span {
            offset: left.offset,
            len: right.end_offset() - left.offset,
        }
    }
    /// Slices the given string with this span
    ///
    /// `base_offset` is the base offset of the corresponding
    /// [`SourceFile`](source::SourceFile) for this `Span`
    ///
    /// You can also use the [`SourceMap::slice`] function
    ///
    /// See: [about offset](Self#about-offset)
    ///
    /// # Examples
    ///
    /// Simple example with a single source file
    /// ```
    /// use span::Span;
    ///
    /// let src = "abcdefg";
    /// let span = Span {
    ///     offset: 1,
    ///     len: 5,
    /// };
    /// let slice = span.slice(0, src);
    /// assert_eq!("bcdef", slice);
    /// ```
    ///
    /// Example with multiple source files
    /// ```
    /// use span::{Span, source::SourceMap};
    ///
    /// let src1 = "abcdefghi";
    /// let src2 = "abcdefhij";
    ///
    /// let mut source = SourceMap::default();
    /// let offset1 = source.add_file_annon(src1.into()).offset();
    /// let offset2 = source.add_file_annon(src2.into()).offset();
    ///
    /// let span1 = Span {
    ///     offset: offset1 + 3,
    ///     len: 4,
    /// };
    ///
    /// let span2 = Span {
    ///     offset: offset2 + 4,
    ///     len: 2,
    /// };
    ///
    /// let slice1 = span1.slice(offset1, src1);
    /// let slice2 = span2.slice(offset2, src1);
    ///
    /// assert_eq!(slice1, "defg");
    /// assert_eq!(slice2, "ef");
    ///
    /// ```
    #[must_use]
    #[inline]
    pub fn slice<'a>(&self, base_offset: usize, src: &'a str) -> &'a str {
        let start = self.offset - base_offset;
        let end = start + self.len;
        &src[start..end]
    }
    /// Gets the [file position](FilePosition) of this span in the given string slice
    ///
    /// `base_offset` is the base offset of the corresponding
    /// [`SourceFile`](source::SourceFile) for this `Span`
    ///
    /// You can also use the [`SourceMap::file_position`] function
    ///
    /// See: [about offset](Self#about-offset)
    #[must_use]
    pub fn file_position(&self, base_offset: usize, src: &str) -> FilePosition {
        let mut fpos = FilePosition {
            start_line: 1,
            start_col: 0,
            end_line: 0,
            end_col: 0,
        };

        let start = self.offset - base_offset;

        for c in src[..start].chars() {
            if c == '\n' {
                fpos.start_col = 0;
                fpos.start_line += 1;
            }
            fpos.start_col += 1;
        }

        fpos.end_line = fpos.start_line;
        fpos.end_col = fpos.start_col;

        let end = start + self.len;
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
    pub const fn end_offset(&self) -> usize { self.offset + self.len }
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
