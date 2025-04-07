mod cursor;
mod error;
pub mod unescaped;

use std::collections::HashMap;

pub use cursor::Cursor;

use delay_init::delay;

pub mod token;
use error::{LexerError, LexerErrorKind};
use error_manager::ErrorManager;
use token::{Token,TokenKind};

pub struct TokenStream<'lex, 'src>{
    lexer: Lexer<'lex, 'src>,
    prev: Option<Token>,
    next: Option<Token>,
}

impl TokenStream<'_,'_> {
    pub fn is_finished(&self) -> bool { self.lexer.c.is_finished() }

    pub fn previous(&self) -> Option<&Token> {
        self.prev.as_ref()
    }

    pub fn peek(&self) -> Option<&Token> {
        self.next.as_ref()
    }
}

pub struct Lexer<'lex, 'src> {
    c: Cursor<'src>,
    em: &'lex mut ErrorManager,
}

delay! {
    static KEYWORDS : HashMap<&str,TokenKind> = {
        let mut map = HashMap::new();
        map.insert("struct",TokenKind::Struct);
        map.insert("else",TokenKind::Else);
        map.insert("false",TokenKind::False);
        map.insert("fn",TokenKind::Fn);
        map.insert("for",TokenKind::For);
        map.insert("if",TokenKind::If);
        map.insert("print",TokenKind::Print);
        map.insert("read",TokenKind::Read);
        map.insert("int",TokenKind::Int);
        map.insert("char",TokenKind::Char);
        map.insert("float",TokenKind::Float);
        map.insert("bool",TokenKind::Bool);
        map.insert("return",TokenKind::Return);
        map.insert("super",TokenKind::Super);
        map.insert("this",TokenKind::This);
        map.insert("true",TokenKind::True);
        map.insert("let",TokenKind::Let);
        map.insert("as",TokenKind::As);
        map.insert("const",TokenKind::Const);
        map.insert("while",TokenKind::While);
        map.insert("break",TokenKind::Break);
        map.insert("continue",TokenKind::Continue);
        map
    };
 }

impl<'lex, 'src> Iterator for TokenStream<'lex, 'src> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.next.take();
        self.prev = ret.clone();
        self.next = self.lexer.next_token();

        ret
    }
}

impl<'lex, 'src> IntoIterator for Lexer<'lex, 'src> {
    type Item = Token;

    type IntoIter = TokenStream<'lex, 'src>;

    fn into_iter(self) -> Self::IntoIter {
        self.into_token_stream()
    }
}

impl<'lex, 'src> Lexer<'lex, 'src> {
    pub fn new(text: &'src str, em: &'lex mut ErrorManager) -> Self {
        Self { c: Cursor::new(text), em }
    }

    pub fn into_token_stream(mut self) -> TokenStream<'lex, 'src> {
        let next = self.next_token();
        TokenStream {
            lexer: self,
            prev: None,
            next
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        if self.c.is_finished() {
            return None
        }
        self.c.step();
        if let Some(t) = self.scan_token() {
            Some(t)
        } else {
            self.next_token()
        }
    }
    /* PRIVATE */
    fn add_token(&self, kind: TokenKind) -> Option<Token> {
        Some(Token {
            kind,
            span: self.c.current_span()
        })
    }
    fn scan_token(&mut self) -> Option<Token> {
        match self.c.advance() {
            '(' => self.add_token(TokenKind::LeftParen),
            ')' => self.add_token(TokenKind::RightParen),
            '{' => self.add_token(TokenKind::LeftBrace),
            '}' => self.add_token(TokenKind::RightBrace),
            '[' => self.add_token(TokenKind::LeftBracket),
            ']' => self.add_token(TokenKind::RightBracket),
            ',' => self.add_token(TokenKind::Comma),
            '.' => {
                if self.c.peek().is_numeric() {
                    self.error(LexerErrorKind::FloatLitWithoutIntegralPart);
                    None
                } else {
                    self.add_token(TokenKind::Dot)
                }
            },
            '-' => {
                if self.c.match_next('>') {
                    self.add_token(TokenKind::Arrow)
                } else {
                    self.add_token(TokenKind::Minus)
                }
            },
            '+' => self.add_token(TokenKind::Plus),
            ';' => self.add_token(TokenKind::Semicolon),
            ':' => self.add_token(TokenKind::Colon),
            '?' => self.add_token(TokenKind::Question),
            '*' => self.add_token(TokenKind::Star),
            '\'' => self.char_literal(),
            '!' =>
                if self.c.match_next('=') {
                    self.add_token(TokenKind::BangEqual)
                }else {
                    self.add_token(TokenKind::Bang)
                },
            '=' =>
                if self.c.match_next('=') {
                    self.add_token(TokenKind::EqualEqual)
                }else {
                    self.add_token(TokenKind::Equal)
                },
            '<' =>
                if self.c.match_next('=') {
                    self.add_token(TokenKind::LessEqual)
                }else {
                    self.add_token(TokenKind::Less)
                },
            '>' =>
                if self.c.match_next('=') {
                    self.add_token(TokenKind::GreaterEqual)
                }else {
                    self.add_token(TokenKind::Greater)
                },
            '/' =>
                if self.c.match_next('/') {
                    self.comment()
                } else if self.c.match_next('*') {
                    self.ml_comment()
                } else {
                    self.add_token(TokenKind::Slash)
                },
            '&' => {
                if self.c.match_next('&') {
                    self.add_token(TokenKind::And)
                } else {
                    self.add_token(TokenKind::Ampersand)
                }
            },
            '|' => {
                if self.c.match_next('|') {
                    self.add_token(TokenKind::Or)
                } else {
                    self.add_token(TokenKind::VerticalPipe)
                }
            },
            '%' => self.add_token(TokenKind::Mod),
            '"' => self.string(),
            ' ' | '\n' | '\r' | '\t' => None , // Ignore whitespace.
            c =>
                if c.is_numeric() {
                    self.number()
                } else if c.is_alphabetic() || c == '_' {
                    self.identifier()
                } else{
                    self.error(LexerErrorKind::UnexpectedCharacter(c));
                    None
                }
        }
    }
    fn char_literal(&mut self) -> Option<Token> {
        if self.c.advance() == '\\' {
            self.c.advance();
        };
        if !self.c.match_next('\'') {
            self.error(LexerErrorKind::ExpectedClosingTickOnCharLiteral);
        }
        self.add_token(TokenKind::CharLiteral)
    }
    fn comment(&mut self) -> Option<Token> {
        self.c.advance_while(|c| *c != '\n');
        None
    }
    fn ml_comment(&mut self) -> Option<Token> {
        while self.c.advance() != '*' || self.c.peek() != '/' {
            if self.c.is_finished() {
                self.error(LexerErrorKind::UnterminatedComment);
            }
        }
        self.c.advance(); /* Consume the / */
        None
    }
    fn string(&mut self) -> Option<Token> {
        while self.c.advance() != '"' {
            if self.c.is_finished() {
                self.error(LexerErrorKind::UnterminatedString);
                return None;
            }
            if self.c.peek() == '\\' {
                self.c.advance();
                self.c.advance();
            }
        }
        self.add_token(TokenKind::String)
    }
    fn floating(&mut self) -> Option<Token> {
        self.c.advance(); /* Consume the . */
        if !self.c.peek().is_numeric() {
            self.error(LexerErrorKind::FloatLitWithoutFloatingPart);
            None
        } else {
            self.c.advance_while(char::is_ascii_digit);
            self.add_token(TokenKind::FloatLiteral)
        }
    }
    fn number(&mut self) -> Option<Token> {
        self.c.advance_while(|n| n.is_numeric());
        if self.c.peek() == '.' {
            self.floating()
        } else {
            self.add_token(TokenKind::IntLiteral)
        }
    }
    fn identifier(&mut self) -> Option<Token> {
        self.c.advance_while(|c| c.is_alphanumeric() || *c == '_');
        let lexem = self.c.current_lexem();
        let token_type = KEYWORDS.get(lexem).cloned().unwrap_or(TokenKind::Identifier);
        self.add_token(token_type)
    }
    fn error(&mut self, kind: LexerErrorKind) {
        self.em.emit_error(LexerError {
            kind,
            span: self.c.current_span()
        });
    }
}
