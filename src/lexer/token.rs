use std::fmt;

#[derive(Clone, Copy, Debug)]
pub enum TokenType {
    /* Single-character tokens. */
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
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
    Print, Return, Super, This, True, Var, While,
}

pub struct Token {
    lexem: String,
    token_type: TokenType,
    start: usize,
    end: usize,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Token {
    pub fn new(lexem: &str, token_type: TokenType, start: usize, end: usize) -> Self {
        let lexem = lexem.to_owned();
        Self{ lexem, token_type, start, end }
    }
    pub fn get_type(&self) -> TokenType { self.token_type }
    pub fn get_lexem(&self) -> &str { &self.lexem }
    pub fn print(&self) {
        println!("[{},{}] {} '{}'",self.start, self.end, self.token_type, self.lexem);
    }
}
