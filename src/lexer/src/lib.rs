pub mod token;
use std::str::Chars;
use crate::token::{Token,TokenType};

pub struct Lexer<'a> {
    chars: Chars<'a>,
    line: u32,
    start: usize,
    start_chars: Chars<'a>,
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
        Self { chars:"".chars(), start_chars: "".chars(), line:0, start:0, current:0, n_errors:0 }
    }
    pub fn tokenize(&mut self, text: &'a str) -> Vec<Token> {
        self.chars = text.chars();
        self.start_chars = text.chars();
        self.line = 0;
        self.current = 0;
        self.n_errors= 0;
        let mut tokens:Vec<Token> = Vec::new();
        while !self.is_finished() {
            self.start = self.current;
            self.start_chars = self.chars.clone();
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
        self.chars.as_str().is_empty()
    }
    fn current_lexem(&self) -> &str {
        let n = self.current - self.start;
        let n = self.start_chars.clone().take(n).map(|c| c.len_utf8()).sum();
        &self.start_chars.as_str()[0..n]
    }
    fn add_token(&self, token_type: TokenType) -> Option<Token> {
        Some(Token::new(
                self.current_lexem(),
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
            ':' => self.add_token(TokenType::Colon),
            '?' => self.add_token(TokenType::Question),
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
        let c = self.chars.next().unwrap_or('\0');
        self.current += 1;
        c
    }
    fn advance_while<F>(&mut self, f: F) -> bool
    where
        F: Fn(&char) -> bool
    {
        while f(&self.peek()) {
            if self.advance() == '\n' {
                self.line += 1;
            }
            if self.is_finished() { return false; }
        }
        true
    }
    fn peek(&self) -> char {
        self.chars
            .clone()
            .next()
            .unwrap_or('\0')
    }
    fn peek_next(&self) -> char {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().unwrap_or('\0')
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
        self.advance_while(|c| c.is_ascii_alphanumeric() || *c == '_');
        let lexem = self.current_lexem();
        let token_type = match_keyword(lexem).unwrap_or(TokenType::Identifier);
        self.add_token(token_type)
    }
    fn error(&mut self, msg: &str) {
        println!("Lexer: ERROR [{}]: {}", self.line, msg);
        self.n_errors += 1;
    }
}
