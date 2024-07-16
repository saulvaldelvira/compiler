use std::{fmt::Display, ops::{Deref, DerefMut}};

use builders::{Constructor, Getters, Setters};

#[derive(Constructor,Getters,Setters,Clone,Copy,Debug)]
#[getters(prefix = "")]
pub struct Span {
    pub (super) start_line: usize,
    pub (super) start_col: usize,
    pub (super) end_line: usize,
    pub (super) end_col: usize,
}

impl Span {
    pub fn join(&self, other: &Span) -> Span {
        Span::new(self.start_line, self.start_col, other.end_line, other.end_col)
    }
}

pub trait Spannable {
    fn set_span(&mut self, span: Span);
    fn get_span(&self) -> Option<Span>;
}

impl<T: Spannable> Spannable for Box<T> {
    fn set_span(&mut self, span: Span) {
        self.deref_mut().set_span(span)
    }

    fn get_span(&self) -> Option<Span> {
        self.deref().get_span()
    }
}

impl Default for Span {
    fn default() -> Self {
        Span { start_line: 1, start_col: 1, end_line: 1, end_col: 1 }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}:{}] [{}:{}]", self.start_line, self.start_col, self.end_line, self.end_col)
    }
}
