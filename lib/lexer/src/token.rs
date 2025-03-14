//! Token model

use std::fmt;

use crate::Span;

/// Models all the types of token
#[derive(Clone,Copy,Debug,PartialEq)]
pub enum TokenKind {
    /// "("
    LeftParen,

    /// ")"
    RightParen,

    /// "{"
    LeftBrace,

    /// "}"
    RightBrace,

    /// ",
    /// "
    Comma,

    /// "."
    Dot,

    /// "-"
    Minus,

    /// "+"
    Plus,

    /// ";"
    Semicolon,

    /// ":"
    Colon,

    /// "/"
    Slash,

    /// "*"
    Star,

    /// "?"
    Question,

    /// "!"
    Bang,

    /// "!="
    BangEqual,

    /// "="
    Equal,

    /// "=="
    EqualEqual,

    /// "\>"
    Greater,

    /// "\>="
    GreaterEqual,

    /// "<"
    Less,

    /// "->"
    Arrow,

    /// "<="
    LessEqual,

    /// [a-zA-Z_][a-zA-Z_0-9]*
    Identifier,

    /// "(.|\\\\.)*?"
    String,

    /// [0-9]+
    IntLiteral,

    /// [0-9]+\.[0-9]+
    FloatLiteral,

    /// '\\?.'
    CharLiteral,

    /// "int"
    Int,

    /// "char"
    Char,

    /// "float"
    Float,

    /// "bool"
    Bool,

    /// "class"
    Class,

    /// "else"
    Else,

    /// "false"
    False,

    /// "fn"
    Fn,

    /// "for"
    For,

    /// "if"
    If,

    /// "print"
    Print,

    /// "read"
    Read,

    /// "return"
    Return,

    /// "super"
    Super,

    /// "this"
    This,

    /// "true"
    True,

    /// "let"
    Let,

    /// "const"
    Const,

    /// "while"
    While,

    /// "break"
    Break,

    /// "continue"
    Continue,

    /// "&"
    BitWiseAnd,

    /// "|"
    BitWiseOr,

    /// "&&"
    And,

    /// "||"
    Or,

    /// '%'
    Mod,
}

/// A token consist on a discriminator [TokenKind] and a [Span]
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
