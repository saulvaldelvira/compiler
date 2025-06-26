use core::marker::PhantomData;
use std::path::PathBuf;
use std::rc::Rc;

use crate::{FilePosition, Span};

pub enum FileName {
    Path(PathBuf),
    Stdin,
    Anon,
}

impl<T: Into<PathBuf>> From<T> for FileName {
    fn from(value: T) -> Self {
        FileName::Path(value.into())
    }
}

pub struct SourceFile {
    pub fname: FileName,
    pub contents: Rc<str>,
    pub offset: usize,
    _marker: PhantomData<()>,
}

impl SourceFile {
    pub fn filename(&self) -> Option<&str> {
        match &self.fname {
            FileName::Path(path) => path.to_str(),
            FileName::Stdin => Some("/dev/stdin"),
            FileName::Anon => Some("<anon>"),
        }
    }

    pub fn len(&self) -> usize { self.contents.len() }
    pub fn is_empty(&self) -> bool { self.len() == 0 }

    pub fn into_parts(&self) -> (Rc<str>, usize) {
        (Rc::clone(&self.contents), self.offset)
    }

    #[must_use]
    #[inline]
    pub fn slice(&self, span: Span) -> &str {
        span.slice(self.offset, &self.contents)
    }

    #[must_use]
    pub fn file_position(&self, span: Span) -> FilePosition {
        span.file_position(self.offset, &self.contents)
    }
}

#[derive(Default)]
pub struct SourceMap {
    files: Vec<SourceFile>,
}

impl SourceMap {
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

    #[inline]
    pub fn add_file_anon(&mut self, contents: Rc<str>) -> &SourceFile {
        self.add_file(FileName::Anon, contents)
    }

    #[inline]
    pub fn get(&self, id: u32) -> Option<&SourceFile> {
        self.files.get(id as usize)
    }

    pub fn get_file_of_span(&self, span: Span) -> Option<&SourceFile> {
        self.files.iter().find(|file| {
            span.offset >= file.offset
            && (span.offset + span.len) <= (file.offset + file.len())
        })
    }

    pub fn get_file_for_offset(&self, offset: usize) -> Option<&SourceFile> {
        self.files.iter().find(|file| file.offset == offset)
    }

    pub fn slice(&self, span: Span) -> &str {
        self.get_file_of_span(span)
            .unwrap()
            .slice(span)
    }

    pub fn file_position(&self, span: Span) -> FilePosition {
        self.get_file_of_span(span)
            .unwrap()
            .file_position(span)
    }
}

