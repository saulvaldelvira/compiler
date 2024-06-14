use std::str::Chars;

pub struct Cursor<'a> {
    chars: Chars<'a>,
    line: u32,
    start: usize,
    start_chars: Chars<'a>,
    current: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            chars: text.chars(),
            line: 0,
            start: 0,
            start_chars: text.chars(),
            current: 0,
        }
    }
    pub fn step(&mut self) {
        self.start = self.current;
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
    pub fn start(&self) -> usize { self.start }
    pub fn current(&self) -> usize { self.current }
    pub fn line(&self) -> u32 { self.line }
    pub fn new_line(&mut self) { self.line += 1; }
    pub fn advance(&mut self) -> char {
        let c = self.chars.next().unwrap_or('\0');
        self.current += 1;
        c
    }
    pub fn advance_while<F>(&mut self, f: F) -> bool
    where
        F: Fn(&char) -> bool
    {
        while f(&self.peek()) {
            if self.advance() == '\n' {
                self.line += 1;
            }
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
