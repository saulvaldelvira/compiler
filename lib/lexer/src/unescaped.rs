//! Utilities for unescaping strings

use std::char;
use std::str::Chars;

/// Escapes the given char.
///
/// # Example
/// ```
/// use lexer::unescaped::escape_char;
/// assert_eq!(Some('\n'), escape_char('n'));
/// /* \g is NOT a valid escape character */
/// assert_eq!(None, escape_char('g'));
/// ```
pub const fn escape_char(c: char) -> Option<char> {
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

/// Iterator that takes a string and outputs it's characters,
/// unescaping them if necessary.
///
/// This means, if the string contains the characters '\\' 'n',
/// this iterator would return a single character '\n'
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
            escape_char(self.chars.next()?)
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

