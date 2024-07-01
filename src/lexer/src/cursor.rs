use std::str::Chars;

use crate::Span;

pub struct Cursor<'a> {
    chars: Chars<'a>,
    span: Span,
    start: usize,
    start_chars: Chars<'a>,
    current: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            chars: text.chars(),
            span: Span::default(),
            start: 0,
            start_chars: text.chars(),
            current: 0,
        }
    }
    pub fn step(&mut self) {
        self.start = self.current;
        self.span.start_line = self.span.end_line;
        self.span.start_col = self.span.end_col;
        self.start_chars = self.chars.clone();
    }
    pub fn is_finished(&self) -> bool {
        self.chars.as_str().is_empty()
    }
    pub fn current_lexem(&self) -> &str {
        let n = self.current - self.start;
        let n = self.start_chars.clone().take(n).map(|c| c.len_utf8()).sum();
        &self.start_chars.as_str()[0..n]
    }
    pub fn get_span(&self) -> Span {
        let mut span = self.span;
        span.end_col -= 1;
        span
    }
    pub fn line(&self) -> usize { self.span.end_line }
    pub fn col(&self) -> usize { self.span.end_col }
    pub fn advance(&mut self) -> char {
        let c = self.chars.next().unwrap_or('\0');
        self.current += 1;
        self.span.end_col += 1;
        if c == '\n' {
            self.span.end_line += 1;
            self.span.end_col = 1;
        }
        c
    }
    pub fn advance_while<F>(&mut self, f: F) -> bool
    where
        F: Fn(&char) -> bool
    {
        while f(&self.peek()) {
            self.advance();
            if self.is_finished() { return false; }
        }
        true
    }
    pub fn peek(&self) -> char {
        self.chars
            .clone()
            .next()
            .unwrap_or('\0')
    }
    pub fn peek_next(&self) -> char {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().unwrap_or('\0')
    }
    pub fn match_next(&mut self, c: char) -> bool {
        if self.peek() == c {
            self.advance();
            return true;
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::Cursor;

    #[test]
    fn test() {
        let text = "Hello world!";
        let mut cursor = Cursor::new(text);
        for c in text.chars() {
            assert!(!cursor.is_finished());
            let next = cursor.advance();
            assert_eq!(next, c);
        }
        assert!(cursor.is_finished());
    }
}
