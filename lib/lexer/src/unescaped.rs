//! Utilities for unescaping strings

use std::{char, str::Chars};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum EscapeError {
    InvalidEscape(char),
    MissingCharAfterSlash,
}

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
        '0' => '\0',
        _ => return None,
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
    type Item = Result<char, EscapeError>;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.chars.next()?;
        if c == '\\' {
            let Some(c) = self.chars.next() else {
                return Some(Err(EscapeError::MissingCharAfterSlash));
            };
            Some(escape_char(c).ok_or(EscapeError::InvalidEscape(c)))
        } else {
            Some(Ok(c))
        }
    }
}

impl<'src> From<&'src str> for Unescaped<'src> {
    fn from(value: &'src str) -> Self {
        Self {
            chars: value.chars(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::unescaped::{EscapeError, Unescaped};

    #[test]
    fn test() {
        let res: String = Unescaped::from("abc\\n").map(Result::unwrap).collect();
        assert_eq!(res, "abc\n");

        let err = Unescaped::from("abc\\?").skip_while(|s| s.is_ok()).next().unwrap();
        assert_eq!(err, Err(EscapeError::InvalidEscape('?')));

        let err = Unescaped::from("abc\\").skip_while(|s| s.is_ok()).next().unwrap();
        assert_eq!(err, Err(EscapeError::MissingCharAfterSlash));
    }
}