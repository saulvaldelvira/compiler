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
    pub id: u32,
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

    pub fn into_parts(&self) -> (Rc<str>, u32) {
        (Rc::clone(&self.contents), self.id)
    }

}

#[derive(Default)]
pub struct SourceMap {
    files: Vec<SourceFile>,
}

impl SourceMap {
    pub fn add_file(&mut self, fname: FileName, contents: Rc<str>) -> &SourceFile {
        #[allow(clippy::cast_possible_truncation)]
        let id = self.files.len() as u32;
        let file = SourceFile { fname, contents, id, _marker: PhantomData };
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

    pub fn slice(&self, span: Span) -> &str {
        let src = &self.get(span.fileid)
            .unwrap()
            .contents;
        span.slice(src)
    }

    pub fn file_position(&self, span: Span) -> FilePosition {
        self.get(span.fileid)
            .map(|file| span.file_position(&file.contents))
            .unwrap_or_default()
    }
}

