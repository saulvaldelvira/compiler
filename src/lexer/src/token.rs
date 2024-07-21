use std::fmt;
use crate::span::Span;

#[derive(Clone,Copy,Debug,PartialEq)]
pub enum TokenType {
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

#[derive(Debug)]
pub struct Token {
    lexem: Option<String>,
    token_type: TokenType,
    span: Span,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Token {
    pub fn new(lexem: &str, token_type: TokenType, span: Span) -> Self {
        let lexem = Some(lexem.to_owned());
        Self{ lexem, token_type, span }
    }
    pub fn get_type(&self) -> TokenType { self.token_type }
    pub fn get_lexem(&self) -> &str {
        match &self.lexem  {
            Some(l) => l,
            None => "",
        }
    }
    pub fn take(&mut self) -> Self {
        Self{
            span: self.span,
            token_type: self.token_type,
            lexem: Some(self.take_lexem()),
        }
    }
    pub fn take_lexem(&mut self) -> String {
        self.lexem.take()
            .expect("Cannot take lexem of the token. Lexem is None.")
    }
    pub fn span(&self) -> Span { self.span }
    pub fn print(&self) {
        print!("[{}:{}] {}",self.span.start_line(), self.span.start_col(), self.token_type);
        if let Some(l) = &self.lexem {
            print!(" '{}'", l);
        }
        println!();
    }
}
