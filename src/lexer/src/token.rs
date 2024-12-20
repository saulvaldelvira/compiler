use std::fmt;

use crate::Span;

#[derive(Clone,Copy,Debug,PartialEq)]
pub enum TokenKind {
    /* Single-character tokens. */
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
    Colon, Question,
    /* One or two character tokens. */
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
    /* literals. */
    Identifier, String, Number,
    /* types */
    Int, Char, Float,
    /* keywords. */
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, Const, While,
    Break, Continue
}

#[derive(Debug,Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
