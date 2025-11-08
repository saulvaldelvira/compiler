//! Token model

use std::fmt;

use span::Span;

/// Models all the types of token
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
    /// "("
    LeftParen,

    /// ")"
    RightParen,

    /// "{"
    LeftBrace,

    /// "}"
    RightBrace,

    /// "["
    LeftBracket,

    /// "]"
    RightBracket,

    /// ","
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

    /// "::"
    DoubleColon,

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

    // Int types
    I8, I16, I32, I64,

    // Uint types
    U8, U16, U32, U64,

    // Float types
    F32, F64,

    /// "char"
    Char,

    /// "bool"
    Bool,

    /// "struct"
    Struct,

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

    /// "use"
    Use,

    /// "as"
    As,

    /// "mod"
    Mod,

    /// "&"
    Ampersand,

    /// "|"
    VerticalPipe,

    /// "&&"
    And,

    /// "||"
    Or,

    /// '%'
    Modulo,

    /// "extern"
    Extern,

    /// ...
    ThreeDot,
}

/// A token consist on a discriminator [`TokenKind`] and a [Span]
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{self:?}") }
}
