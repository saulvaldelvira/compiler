use std::str::CharIndices;

use span::Span;

pub struct Cursor<'lex> {
    chars: CharIndices<'lex>,
    start_chars: CharIndices<'lex>,

    line: usize,
    col: usize,
}

impl<'lex> Cursor<'lex> {
    pub fn new(text: &'lex str) -> Self {
        Self {
            chars: text.char_indices(),
            start_chars: text.char_indices(),
            line: 0,
            col: 0,
        }
    }
    pub fn step(&mut self) {
        self.start_chars = self.chars.clone();
    }
    pub fn is_finished(&self) -> bool {
        self.chars.as_str().is_empty()
    }
    pub fn current_offset(&self) -> usize {
        self.start_chars.offset()
    }
    pub fn current_len(&self) -> usize {
        self.chars.offset() - self.start_chars.offset()
    }
    pub fn current_lexem(&self) -> &str {
        let n = self.chars.offset() - self.start_chars.offset();
        &self.start_chars.as_str()[..n]
    }
    pub fn current_span(&self) -> Span {
        Span {
            offset: self.start_chars.offset(),
            len: self.chars.offset() - self.start_chars.offset()
        }
    }
    pub fn line(&self) -> usize { self.line }
    pub fn col(&self) -> usize { self.col }
    pub fn advance(&mut self) -> char {
        let c = self.chars.next().map(|(_,c)| c).unwrap_or('\0');
        self.col += 1;
        if c == '\n' {
            self.line += 1;
            self.col = 1;
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
            .map(|(_,c)| c)
            .unwrap_or('\0')
    }
    pub fn peek_next(&self) -> char {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next()
            .map(|(_,c)| c)
            .unwrap_or('\0')
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
