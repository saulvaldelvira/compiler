pub mod token;
use std::usize;

use crate::lexer::token::{Token,TokenType};

pub struct Lexer<'a> {
    text: &'a str,
    line: u32,
    start: usize,
    current: usize,
    n_errors: u32,
}

fn match_keyword(lexem: &str) -> Option<TokenType> {
    match lexem {
        "and" => Some(TokenType::And),
        "class" => Some(TokenType::Class),
        "else" => Some(TokenType::Else),
        "false" => Some(TokenType::False),
        "fun" => Some(TokenType::Fun),
        "for" => Some(TokenType::For),
        "if" => Some(TokenType::If),
        "nil" => Some(TokenType::Nil),
        "or" => Some(TokenType::Or),
        "print" => Some(TokenType::Print),
        "int" => Some(TokenType::Int),
        "char" => Some(TokenType::Char),
        "float" => Some(TokenType::Float),
        "return" => Some(TokenType::Return),
        "super" => Some(TokenType::Super),
        "this" => Some(TokenType::This),
        "true" => Some(TokenType::True),
        "var" => Some(TokenType::Var),
        "while" => Some(TokenType::While),
        _ => None
    }
}

impl<'a> Lexer<'a> {
    /* PUBLIC */
    pub fn new() -> Self {
        Self { text:"", line:0, start:0, current:0, n_errors:0 }
    }
    pub fn tokenize(&mut self, text: &'a str) -> Vec<Token> {
        self.text = text;
        self.line = 0;
        self.current = 0;
        self.n_errors= 0;
        let mut tokens:Vec<Token> = Vec::new();
        while !self.is_finished() {
            self.start = self.current;
            if let Some(t) = self.scan_token() {
                tokens.push(t);
            }
        }
        tokens
    }
    pub fn has_errors(&self) -> bool { self.n_errors > 0 }
    pub fn n_errors(&self) -> u32 { self.n_errors }
    /* PRIVATE */
    fn is_finished(&self) -> bool {
        self.current >= self.text.len()
    }
    fn add_token(&self, token_type: TokenType) -> Option<Token> {
        Some(Token::new(
                &self.text[self.start..self.current],
                token_type, self.start, self.current))
    }
    fn scan_token(&mut self) -> Option<Token> {
        match self.advance() {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '!' =>
                if self.match_next('=') {
                    self.add_token(TokenType::BangEqual)
                }else {
                    self.add_token(TokenType::Bang)
                },
            '=' =>
                if self.match_next('=') {
                    self.add_token(TokenType::EqualEqual)
                }else {
                    self.add_token(TokenType::Equal)
                },
            '<' =>
                if self.match_next('=') {
                    self.add_token(TokenType::LessEqual)
                }else {
                    self.add_token(TokenType::Less)
                },
            '>' =>
                if self.match_next('=') {
                    self.add_token(TokenType::GreaterEqual)
                }else {
                    self.add_token(TokenType::Greater)
                },
            '/' =>
                if self.match_next('/') {
                    self.comment()
                } else if self.match_next('*') {
                    self.ml_comment()
                } else {
                    self.add_token(TokenType::Slash)
                },
            '"' => self.string(),
            ' ' | '\r' | '\t' => None , // Ignore whitespace.
            '\n' => {
                self.line += 1;
                None
            },
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
    fn match_next(&mut self, c: char) -> bool {
        if self.peek() == c {
            self.advance();
            return true;
        }
        false
    }
    fn advance(&mut self) -> char {
        let c = self.peek();
        self.current += 1;
        c
    }
    fn advance_while<F>(&mut self, f: F) -> bool
    where
        F: Fn(&char) -> bool
    {
        while f(&self.peek()) {
            self.advance();
            if self.peek() == '\n' {
                self.line += 1;
            }
            if self.is_finished() { return false; }
        }
        true
    }
    fn peek(&self) -> char {
        self.peek_nth(0)
    }
    fn peek_next(&self) -> char {
        self.peek_nth(1)
    }
    fn peek_nth(&self, n: usize) -> char {
        self.text
            .get(self.current + n..)
            .unwrap_or("\0")
            .chars().next().unwrap_or('\0')
    }
    fn comment(&mut self) -> Option<Token> {
        self.advance_while(|c| *c != '\n');
        None
    }
    fn ml_comment(&mut self) -> Option<Token> {
        while self.advance() != '*' || self.peek() != '/' {
            if self.is_finished() {
                self.error("Non terminated comment block.");
            }
            if self.peek() == '\n' {
                self.line += 1;
            }
        }
        self.advance(); /* Consume the / */
        None
    }
    fn string(&mut self) -> Option<Token> {
        let was_eof = !self.advance_while(|c| *c != '"');
        if was_eof {
            self.error("Unterminated string");
            return None;
        }
        self.advance();
        self.add_token(TokenType::String)
    }
    fn number(&mut self) -> Option<Token> {
        self.advance_while(char::is_ascii_digit);
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();
            self.advance_while(char::is_ascii_digit);
        }
        self.add_token(TokenType::Number)
    }
    fn identifier(&mut self) -> Option<Token> {
        self.advance_while(|c| c.is_alphanumeric());
        let lexem = &self.text[self.start..self.current];
        let token_type = match_keyword(lexem).unwrap_or(TokenType::Identifier);
        self.add_token(token_type)
    }
    fn error(&mut self, msg: &str) {
        println!("Lexer: ERROR [{}]: {}", self.line, msg);
        self.n_errors += 1;
    }
}
