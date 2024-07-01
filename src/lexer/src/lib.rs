mod cursor;
mod span;
pub use span::{Span,Spannable};

use std::collections::HashMap;

pub use macros::Spanned;
pub use macros::spanned;

use cursor::Cursor;

use delay_init::delay;

pub mod token;
use token::{Token,TokenType};

pub struct Lexer<'a> {
    c: Cursor<'a>,
    n_errors: u32,
}

delay! {
    static KEYWORDS : HashMap<&str,TokenType> = {
        let mut map = HashMap::new();
        map.insert("and",TokenType::And);
        map.insert("class",TokenType::Class);
        map.insert("else",TokenType::Else);
        map.insert("false",TokenType::False);
        map.insert("fun",TokenType::Fun);
        map.insert("for",TokenType::For);
        map.insert("if",TokenType::If);
        map.insert("nil",TokenType::Nil);
        map.insert("or",TokenType::Or);
        map.insert("print",TokenType::Print);
        map.insert("int",TokenType::Int);
        map.insert("char",TokenType::Char);
        map.insert("float",TokenType::Float);
        map.insert("return",TokenType::Return);
        map.insert("super",TokenType::Super);
        map.insert("this",TokenType::This);
        map.insert("true",TokenType::True);
        map.insert("var",TokenType::Var);
        map.insert("while",TokenType::While);
        map
    };
 }

impl<'a> Lexer<'a> {
    /* PUBLIC */
    pub fn new() -> Self {
        Self { c: Cursor::new(""), n_errors:0 }
    }
    pub fn tokenize(&mut self, text: &'a str) -> Vec<Token> {
        self.n_errors= 0;
        self.c = Cursor::new(text);
        let mut tokens:Vec<Token> = Vec::new();
        while !self.c.is_finished() {
            self.c.step();
            if let Some(t) = self.scan_token() {
                tokens.push(t);
            }
        }
        tokens
    }
    pub fn has_errors(&self) -> bool { self.n_errors > 0 }
    pub fn n_errors(&self) -> u32 { self.n_errors }
    /* PRIVATE */
    fn add_token(&self, token_type: TokenType) -> Option<Token> {
        Some(Token::new(
                self.c.current_lexem(),
                token_type, self.c.get_span()))
    }
    fn scan_token(&mut self) -> Option<Token> {
        match self.c.advance() {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            ':' => self.add_token(TokenType::Colon),
            '?' => self.add_token(TokenType::Question),
            '*' => self.add_token(TokenType::Star),
            '!' =>
                if self.c.match_next('=') {
                    self.add_token(TokenType::BangEqual)
                }else {
                    self.add_token(TokenType::Bang)
                },
            '=' =>
                if self.c.match_next('=') {
                    self.add_token(TokenType::EqualEqual)
                }else {
                    self.add_token(TokenType::Equal)
                },
            '<' =>
                if self.c.match_next('=') {
                    self.add_token(TokenType::LessEqual)
                }else {
                    self.add_token(TokenType::Less)
                },
            '>' =>
                if self.c.match_next('=') {
                    self.add_token(TokenType::GreaterEqual)
                }else {
                    self.add_token(TokenType::Greater)
                },
            '/' =>
                if self.c.match_next('/') {
                    self.comment()
                } else if self.c.match_next('*') {
                    self.ml_comment()
                } else {
                    self.add_token(TokenType::Slash)
                },
            '"' => self.string(),
            ' ' | '\n' | '\r' | '\t' => None , // Ignore whitespace.
            c =>
                if c.is_ascii_digit() {
                    self.number()
                } else if c.is_ascii_alphabetic() {
                    self.identifier()
                } else{
                    let mut msg = "Unexpected character [".to_string();
                    msg += &c.to_string();
                    msg += "]";
                    self.error(&msg);
                    None
                }
        }
    }
    fn comment(&mut self) -> Option<Token> {
        self.c.advance_while(|c| *c != '\n');
        None
    }
    fn ml_comment(&mut self) -> Option<Token> {
        while self.c.advance() != '*' || self.c.peek() != '/' {
            if self.c.is_finished() {
                self.error("Non terminated comment block.");
            }
        }
        self.c.advance(); /* Consume the / */
        None
    }
    fn string(&mut self) -> Option<Token> {
        let was_eof = !self.c.advance_while(|c| *c != '"');
        if was_eof {
            self.error("Unterminated string");
            return None;
        }
        self.c.advance();
        self.add_token(TokenType::String)
    }
    fn number(&mut self) -> Option<Token> {
        self.c.advance_while(char::is_ascii_digit);
        if self.c.peek() == '.' && self.c.peek_next().is_ascii_digit() {
            self.c.advance();
            self.c.advance_while(char::is_ascii_digit);
        }
        self.add_token(TokenType::Number)
    }
    fn identifier(&mut self) -> Option<Token> {
        self.c.advance_while(|c| c.is_ascii_alphanumeric() || *c == '_');
        let lexem = self.c.current_lexem();
        let token_type = KEYWORDS.get(lexem).cloned().unwrap_or(TokenType::Identifier);
        self.add_token(token_type)
    }
    fn error(&mut self, msg: &str) {
        eprintln!("[{}:{}] ERROR: {}", self.c.line(), self.c.col(), msg);
        self.n_errors += 1;
    }
}
