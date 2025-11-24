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

    /// [a-zA-Z_]\[a-zA-Z_0-9\]*
    Identifier,

    /// "(.|\\\\.)*?"
    String,

    /// [0-9]+
    IntLiteral,

    /// [0-9]+\.[0-9]+
    FloatLiteral,

    /// '\\?.'
    CharLiteral,

    /* ==== Primitive Types ==== */
    /// "i8"
    I8,
    /// "i16"
    I16,
    /// "i32"
    I32,
    /// "i64"
    I64,
    /// "u8"
    U8,
    /// "u16"
    U16,
    /// "u32"
    U32,
    /// "u64"
    U64,
    /// "f32"
    F32,
    /// "f64"
    F64,
    /// "char"
    Char,
    /// "bool"
    Bool,
    /* ========================= */

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

    /// "return"
    Return,

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

    /// "super"
    Super,

    /// "self"
    Slf,

    /// "..."
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
