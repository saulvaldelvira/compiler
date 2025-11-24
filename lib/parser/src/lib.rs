pub mod error;

mod item;
mod expr;
mod stmt;
mod ty;

use core::cell::RefCell;
use std::{borrow::Cow, str::FromStr};

use ast::{Block, Module, Parenthesized, Path};
use error::ParseErrorKind;
use error_manager::ErrorManager;
use lexer::Lexer;
use lexer::{
    token::{Token, TokenKind},
    TokenStream,
};
use interner::Symbol;
use span::source::FileId;
use span::{Span, Spanned, source::SourceMap};

use self::error::ParseError;

type Result<T> = std::result::Result<T, ParseErrorKind>;

/// Parsers the given program and produces an AST.
///
/// # Arguments
/// - src: The source file
/// - `base_offset`: Offset of this file inside the [`SourceMap`]
/// - `src_map`: The [`SourceMap`] for this compilation
/// - em: An [`ErrorManager`], where all the errors will be sent
pub fn parse(
    src: &str,
    base_offset: usize,
    src_map: &RefCell<SourceMap>,
    em: &mut ErrorManager,
) -> Option<Module> {
    let stream = Lexer::new(src, base_offset, em).into_token_stream();
    let mut parse_em = ErrorManager::new();
    let ast = Parser { stream, src, base_offset, em: &mut parse_em, src_map }.parse();
    em.merge(&mut parse_em);
    ast
}

struct Parser<'sess, 'src> {
    stream: TokenStream<'sess, 'src>,
    src: &'src str,
    base_offset: usize,
    em: &'sess mut ErrorManager,
    src_map: &'sess RefCell<SourceMap>,
}

impl<'sess, 'src> Parser<'sess, 'src> {
    fn parse(mut self) -> Option<Module> {
        let mut decls = Vec::new();
        while !self.is_finished() {
            match self.item() {
                Ok(stmt) => decls.push(stmt),
                Err(e) => {
                    self.error(e);
                    self.synchronize_with(&[TokenKind::Let, TokenKind::Const,
                        TokenKind::Fn, TokenKind::Mod, TokenKind::Semicolon]);
                }
            }
        }

        if self.em.has_errors() {
            return None;
        }

        let mut span = decls.first().map(|s| s.span).unwrap_or_default();
        if let Some(l) = decls.last() {
            span = span.join(&l.span);
        }
        let name = Symbol::new("root");
        let name = Spanned { span: Span::dummy(), val: name };

        let m = Module {
            body: ast::ModuleBody::Slf(decls.into_boxed_slice(), FileId::from_offset(self.base_offset)),
            name,
            span,
        };
        Some(m)
    }

    fn consume_ident_spanned(&mut self) -> Result<Spanned<Symbol>> {
        let span = self.consume(TokenKind::Identifier)?.span;
        let sym = Symbol::new(span.slice(self.base_offset, self.src));
        Ok(Spanned { val: sym, span })
    }

    fn expected_err<T>(&self, tokens: &'static [TokenKind]) -> Result<T> {
        let found = self.peek()?.kind;
        Err(ParseErrorKind::ExpectedToken {
            tokens: tokens.into(),
            found,
        })
    }

    fn parenthesized<T>(
        &mut self,
        f: impl for<'a> FnOnce(&'a mut Parser<'sess, 'src>) -> Result<T>,
    ) -> Result<Parenthesized<T>> {
        let op = self.consume(TokenKind::LeftParen)?.span;
        let val = f(self)?;
        let cp = self.consume(TokenKind::RightParen)?.span;
        Ok(Parenthesized {
            open_paren: op,
            val,
            close_paren: cp,
        })
    }

    fn try_block<T, F>(&mut self, f: F) -> Option<Result<Block<T>>>
    where
        F: FnMut(&mut Self) -> Result<T>
    {
        if self.check(TokenKind::LeftBrace) {
            Some(self.block(f))
        } else {
            None
        }
    }
    fn block_inner<T, F>(&mut self, mut f: F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>
    {
        let mut stmts = Vec::new();
        while !self.is_finished()  && !self.check(TokenKind::RightBrace) {
            let val = f(self)?;
            stmts.push(val);
        }
        Ok(stmts)
    }
    fn block<T, F>(&mut self, f: F) -> Result<Block<T>>
    where
        F: FnMut(&mut Self) -> Result<T>
    {
        let open = self.consume(TokenKind::LeftBrace)?.span;
        let stmts = self.block_inner(f)?;
        let close = self.consume(TokenKind::RightBrace)?.span;
        Ok(Block {
            open_brace: open,
            close_brace: close,
            val: stmts.into_boxed_slice(),
        })
    }
    fn try_path(&mut self, can_wildard: bool) -> Option<(Path, Option<(Span, Span)>)> {
        if self.peek().is_ok_and(|token| matches!(token.kind, TokenKind::Identifier | TokenKind::Super | TokenKind::Slf | TokenKind::DoubleColon)) {
            self.path(can_wildard).ok()
        } else {
            None
        }
    }
    fn path(&mut self, can_wildard: bool) -> Result<(Path, Option<(Span, Span)>)> {
        let mut from_root: Option<Span> = None;
        let mut path = vec![if self.match_type(TokenKind::Super) {
            Spanned {
                val: Symbol::new("super"),
                span: self.previous_span().unwrap(),
            }
        }
        else if self.match_type(TokenKind::Slf) {
            Spanned {
                val: Symbol::new("self"),
                span: self.previous_span().unwrap(),
            }
        }
        else {
            if self.check(TokenKind::DoubleColon) {
                from_root = Some(self.consume(TokenKind::DoubleColon)?.span);
            }
            /* For cases like ::self::... */
            if self.match_type(TokenKind::Slf) {
                Spanned {
                    val: Symbol::new("self"),
                    span: self.previous_span().unwrap(),
                }
            } else {
                self.consume_ident_spanned()?
            }
        }];

        let mut dc = None;
        let mut star = None;

        while self.match_type(TokenKind::DoubleColon) {
            if can_wildard && self.check(TokenKind::Star) {
                dc = Some(self.previous_span().unwrap());
                star = Some(self.consume(TokenKind::Star)?.span);
            } else {
                path.push(self.consume_ident_spanned()?);
            }
        }

        let segments = path.into_boxed_slice();
        let mut span = if let Some(f) = from_root { f } else {
            segments.first().unwrap().span
        };
        if let Some(l) = segments.last() {
            span = span.join(&l.span);
        }

        let wc = if dc.is_some() {
            Some((dc.unwrap(), star.unwrap()))
        } else {
            None
        };

        Ok((Path { start_collon: from_root, span, segments}, wc))
    }
    fn owned_lexem(&mut self, span: Span) -> Symbol {
        let slice = span.slice(self.base_offset, self.src);
        Symbol::new(slice)
    }
    fn consume(&mut self, t: TokenKind) -> Result<&Token> {
        if self.check(t) {
            return self.advance();
        }
        Err(ParseErrorKind::ExpectedToken {
            tokens: Cow::from(&[t]).into_owned().into(),
            found: self.peek()?.kind,
        })
    }
    fn match_type(&mut self, t: TokenKind) -> bool {
        if self.check(t) {
            self.advance().unwrap_or_else(|_| unreachable!());
            return true;
        }
        false
    }
    fn match_types(&mut self, types: &[TokenKind]) -> bool {
        types.iter().any(|t| self.match_type(*t))
    }
    fn check_types(&mut self, types: &[TokenKind]) -> bool { types.iter().any(|t| self.check(*t)) }
    fn check(&mut self, t: TokenKind) -> bool {
        self.peek().is_ok_and(|token| token.kind == t)
    }
    #[inline]
    fn bump(&mut self) { self.stream.next(); }
    fn advance(&mut self) -> Result<&Token> {
        self.bump();
        self.previous()
    }
    #[inline]
    pub fn is_finished(&self) -> bool { self.stream.is_finished() }
    #[inline]
    fn peek(&self) -> Result<&Token> { self.stream.peek().ok_or(ParseErrorKind::CantPeek) }
    #[inline]
    fn previous(&self) -> Result<&Token> {
        self.stream
            .previous()
            .ok_or(ParseErrorKind::NoPreviousToken)
    }
    #[inline]
    fn previous_span(&self) -> Result<Span> { self.previous().map(|s| s.span) }
    fn previous_parse<T: FromStr>(&self) -> Result<T> {
        let span = self.previous()?.span;
        span.slice(self.base_offset, self.src)
            .parse::<T>()
            .map_err(|_| ParseErrorKind::LexemParseError)
    }
    fn previous_lexem(&mut self) -> Result<Symbol> { Ok(self.owned_lexem(self.previous()?.span)) }

    fn synchronize_with(&mut self, safe: &[TokenKind]) -> bool {
        self.bump();
        while !self.is_finished() {
            if safe.contains(&self.peek().unwrap_or_else(|_| unreachable!()).kind) {
                return true;
            }
            self.bump();
        }
        false
    }
    fn error(&mut self, kind: ParseErrorKind) {
        let tok = if self.is_finished() {
            self.previous()
        } else {
            self.peek()
        }
        .unwrap_or_else(|_| unreachable!());

        self.em.emit_error(ParseError {
            kind,
            span: tok.span,
        });
    }
}
