use std::char;
use std::str::Chars;

pub fn unescape_char(c: char) -> Option<char> {
    let c = match c {
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        '\\' => '\\',
        '\'' => '\'',
        '"' => '\"',
        _ => return None
    };
    Some(c)
}

pub struct Unescaped<'src> {
    chars: Chars<'src>,
}

impl Iterator for Unescaped<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.chars.next()?;
        if c != '\\' {
            Some(c)
        } else {
            unescape_char(self.chars.next()?)
        }
    }
}

impl<'src> From<&'src str> for Unescaped<'src> {
    fn from(value: &'src str) -> Self {
        Self {
            chars: value.chars()
        }
    }
}

