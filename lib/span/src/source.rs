//! Source file representation

use core::marker::PhantomData;
use std::{fs, io};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::{FilePosition, Span};

/// The filename of a [`SourceFile`]
///
/// It can either be a filesystem path, or an anonymous source (used on testing)
#[derive(Clone)]
pub enum FileName {
    Path(PathBuf),
    Stdin,
    Annon,
}

impl<T: Into<PathBuf>> From<T> for FileName {
    fn from(value: T) -> Self {
        FileName::Path(value.into())
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Default)]
pub struct FileId(usize);

impl FileId {
    pub fn from_offset(off: usize) -> Self { Self(off) }
}

/// A source file for the compiler
#[derive(Clone)]
pub struct SourceFile {
    pub fname: FileName,
    pub contents: Rc<str>,
    pub offset: usize,
    /* Private field to avoid external modules
     * from building their own SourceFiles */
    _marker: PhantomData<()>,
}

impl SourceFile {

    /// Gets a string slice with the filename of this `SourceFile`
    ///
    /// This method returns an `Option` because the [`FileName::Path`] variant
    /// stores a [`PathBuf`], which can contain non-utf8 filenames.
    ///
    /// It the conversion [to string](std::path::Path::to_str) fails, returns None
    pub fn filename(&self) -> Option<&str> {
        match &self.fname {
            FileName::Path(path) => path.to_str(),
            FileName::Stdin => Some("/dev/stdin"),
            FileName::Annon => Some("<annon>"),
        }
    }

    /// Returns the length of this `SourceFile`
    ///
    /// This is the same as `self.contents.len()`
    pub fn len(&self) -> usize { self.contents.len() }

    /// Returns true if this `SourceFile` is empty
    ///
    /// This is the same as `self.contents.is_empty()`
    pub fn is_empty(&self) -> bool { self.contents.is_empty() }

    /// Returns a tuple with the contents and offset of this file
    ///
    /// # Example
    /// ```
    /// use span::source::{SourceMap, SourceFile};
    ///
    /// let mut sm = SourceMap::new();
    ///
    /// let input1 = "Hello world!";
    /// let (contents, offset) = sm.add_file_annon(input1.into()).into_parts();
    /// assert_eq!(&*contents, input1);
    /// assert_eq!(offset, 0);
    ///
    /// let input2 = "Second file";
    /// let (contents, offset) = sm.add_file_annon(input2.into()).into_parts();
    /// assert_eq!(&*contents, input2);
    /// assert_eq!(offset, input1.len());
    /// ```
    pub fn into_parts(&self) -> (Rc<str>, usize) {
        (Rc::clone(&self.contents), self.offset)
    }

    /// Returns the absolute offset of this file inside it's [`SourceMap`]
    ///
    /// # Example
    /// ```
    /// use span::source::{SourceMap, SourceFile};
    ///
    /// let mut sm = SourceMap::new();
    ///
    /// let input1 = "Hello world!";
    /// let offset1 = sm.add_file_annon(input1.into()).offset();
    /// assert_eq!(offset1, 0);
    ///
    /// let input2 = "Second file";
    /// let offset2 = sm.add_file_annon(input2.into()).offset();
    /// assert_eq!(offset2, input1.len());
    /// ```
    pub const fn offset(&self) -> usize { self.offset }

    pub fn contains_span(&self, span: &Span) -> bool {
        span.offset >= self.offset &&
        span.offset + span.len <= self.offset + self.len()
    }

    /// Slices the given span
    ///
    /// **NOTE**: `span` must be contained in this `SourceFile`
    #[must_use]
    #[inline]
    pub fn slice(&self, span: &Span) -> &str {
        debug_assert!(self.contains_span(span));
        span.slice(self.offset, &self.contents)
    }

    /// Returns the file position of this span inside `self`
    ///
    /// **NOTE**: `span` must be contained in this `SourceFile`
    #[must_use]
    #[inline]
    pub fn file_position(&self, span: &Span) -> FilePosition {
        span.file_position(self.offset, &self.contents)
    }

    pub const fn id(&self) -> FileId { FileId(self.offset) }

    pub fn path(&self) -> Option<&Path> {
        match &self.fname {
            FileName::Path(pbuf) => Some(pbuf),
            FileName::Stdin |
            FileName::Annon => None,
        }
    }
}

/// A storage for source files.
///
/// # Example
/// ```
/// use span::{Span, source::{FileName, SourceMap, SourceFile}};
///
/// fn process_file(f: &SourceFile) -> Span {
///     Span {
///         offset: 2 + f.offset(),
///         len: 3,
///     }
/// }
///
/// let mut source = SourceMap::default();
///
/// let f = source.add_file(FileName::Annon, "ABCDEFG".into());
/// let span1 = process_file(f);
///
/// let f = source.add_file(FileName::Annon, "DEFGHIJK".into());
/// let span2 = process_file(f);
///
/// let slice1 = source.slice(&span1);
/// let slice2 = source.slice(&span2);
///
/// assert_eq!(slice1, Some("CDE"));
/// assert_eq!(slice2, Some("FGH"));
/// ```
#[derive(Default)]
pub struct SourceMap {
    files: Vec<SourceFile>,
}

impl SourceMap {
    /// Creates a new empty `SourceMap`
    pub const fn new() -> Self {
        Self { files: Vec::new() }
    }

    /// Adds a new [`SourceFile`] to the `SourceMap`
    ///
    /// Returns a reference to the newly created file
    pub fn add_file(&mut self, fname: FileName, contents: Rc<str>) -> &SourceFile {
        #[allow(clippy::cast_possible_truncation)]
        let offset = match self.files.last() {
            Some(file) => file.offset + file.len(),
            None => 0
        };
        let file = SourceFile { fname, contents, offset, _marker: PhantomData };
        self.files.push(file);
        self.files.last().unwrap()
    }

    /// Reads the given `path` and creates a [`SourceFile`] with it's contents
    pub fn add_file_fs(&mut self, path: PathBuf) -> io::Result<&SourceFile> {
        let text = fs::read_to_string(&path)?;
        Ok(self.add_file(FileName::Path(path), text.into()))
    }

    /// Adds a new annonymous [`SourceFile`] (without a filename) to this `SourceMap`
    #[inline]
    pub fn add_file_annon(&mut self, contents: Rc<str>) -> &SourceFile {
        self.add_file(FileName::Annon, contents)
    }

    /// Gets the corresponding [`SourceFile`] for the given `span`
    ///
    /// # Example
    /// ```
    /// use span::{Span, source::{SourceMap, FileName}};
    ///
    /// let mut sm = SourceMap::new();
    /// let (input1, offset1) =
    ///     sm.add_file(
    ///         FileName::from("file1.txt"),
    ///         "Hello world!".into()
    ///     )
    ///     .into_parts();
    /// let (input2, offset2) =
    ///     sm.add_file(
    ///         FileName::from("file2.txt"),
    ///         "Another file :)".into()
    ///     )
    ///     .into_parts();
    ///
    /// let span1 = Span {
    ///     offset: 3 + offset1,
    ///     len: 3,
    /// };
    ///
    /// let span2 = Span {
    ///     offset: 1 + offset2,
    ///     len: 4,
    /// };
    ///
    /// let file1 = sm.get_file_of_span(&span1).unwrap();
    /// assert_eq!(file1.filename().unwrap(), "file1.txt");
    ///
    /// let file2 = sm.get_file_of_span(&span2).unwrap();
    /// assert_eq!(file2.filename().unwrap(), "file2.txt");
    /// ```
    pub fn get_file_of_span(&self, span: &Span) -> Option<&SourceFile> {
        self.files.iter().find(|file| file.contains_span(span))
    }

    /// Returns the file for a given offset
    pub fn get_file_for_offset(&self, offset: usize) -> Option<&SourceFile> {
        self.files.iter().find(|file| file.offset == offset)
    }

    pub fn get_file_for_id(&self, id: &FileId) -> Option<&SourceFile> {
        self.get_file_for_offset(id.0)
    }

    /// Returns base offset for a [`Span`]. This is: the offset of the
    /// file it belongs to.
    ///
    /// If no file is found, returns 0
    pub fn get_base_offset_of_span(&self, span: &Span) -> Option<usize> {
        self.get_file_of_span(span).map(|f| f.offset)
    }

    /// Slices the given [`Span`]
    ///
    /// This function will compute the [`SourceFile`] for the span
    /// and slice it
    pub fn slice(&self, span: &Span) -> Option<&str> {
        self.get_file_of_span(span).map(|file| file.slice(span))
    }

    /// Returns the [`FilePosition`] for this [`Span`]
    ///
    /// This function will compute the [`SourceFile`] for the span
    /// and slice it
    pub fn file_position(&self, span: &Span) -> Option<FilePosition> {
        self.get_file_of_span(span).map(|file| file.file_position(span))
    }

    pub fn files(&self) -> &[SourceFile] { &self.files }

    /// Returns true if this is a valid span, this is, if it finds
    /// a matching [`SourceFile`] for it
    pub fn is_valid(&self, span: &Span) -> bool {
        self.get_file_of_span(span).is_some()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn spans() {
         let mut sm = SourceMap::new();
         let (_, offset1) =
             sm.add_file(
                 FileName::from("file1.txt"),
                 "Hello world!".into()
             )
             .into_parts();
         let (_, offset2) =
             sm.add_file(
                 FileName::from("file2.txt"),
                 "Another file :)".into()
             )
             .into_parts();

         let span1 = Span {
             offset: 3 + offset1,
             len: 3,
         };

         let slice1 = sm.slice(&span1);
         let pos1 = sm.file_position(&span1);
         assert_eq!(slice1, Some("lo "));
         assert_eq!(pos1, Some(FilePosition {
             start_line: 1,
             start_col: 3,
             end_line: 1,
             end_col: 6,
         }));

         let span2 = Span {
             offset: 1 + offset2,
             len: 4,
         };

         let slice2 = sm.slice(&span2);
         let pos2 = sm.file_position(&span2);
         assert_eq!(slice2, Some("noth"));
         assert_eq!(pos2, Some(FilePosition {
             start_line: 1,
             start_col: 1,
             end_line: 1,
             end_col: 5,
         }));

         let bad_span1 = Span {
             offset: 9999,
             len: 4,
         };

         let bad_span2 = Span {
             offset: 0,
             len: 182,
         };

         assert!(sm.slice(&bad_span1).is_none());
         assert!(sm.file_position(&bad_span1).is_none());
         assert!(sm.slice(&bad_span2).is_none());
         assert!(sm.file_position(&bad_span2).is_none());
    }
}
