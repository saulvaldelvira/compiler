pub mod error;

mod decl;
mod expr;
mod stmt;
mod ty;

use core::str;
use std::{borrow::Cow, str::FromStr};

use ast::{expr::ExpressionKind, Block, Expression, Module, Parenthesized, Statement};
use error::ParseErrorKind;
use error_manager::ErrorManager;
use lexer::{
    token::{Token, TokenKind},
    TokenStream,
};
use session::{with_session, with_session_interner, Symbol};
use span::{Span, Spanned};

use self::error::ParseError;

type Result<T> = std::result::Result<T, ParseErrorKind>;

/// Parsers the given [TokenStream] and produces an AST.
///
/// # Arguments
/// - stream: The token stream
/// - src: The source file
/// - em: An [ErrorManager], where all the errors will be sent
pub fn parse<'src>(
    stream: TokenStream<'_, 'src>,
    src: &'src str,
    em: &mut ErrorManager,
) -> Option<Module> {
    Parser { stream, src, em }.parse()
}

struct Parser<'sess, 'src> {
    stream: TokenStream<'sess, 'src>,
    src: &'src str,
    em: &'sess mut ErrorManager,
}

impl<'sess, 'src> Parser<'sess, 'src> {
    fn parse(mut self) -> Option<Module> {
        let mut decls = Vec::new();
        while !self.is_finished() {
            match self.mod_item() {
                Ok(stmt) => decls.push(stmt),
                Err(e) => {
                    self.error(e);
                    self.synchronize_with(&[TokenKind::Let, TokenKind::Const, TokenKind::Fn]);
                }
            }
        }

        if self.em.has_errors() {
            return None;
        }

        let span = Span::new();
        let name = with_session_interner(|i| i.get_or_intern("ROOT"));
        let name = Spanned { span, val: name };

        let m = Module {
            elems: decls.into_boxed_slice(),
            name,
            span,
        };
        Some(m)
    }

    fn consume_ident_spanned(&mut self) -> Result<Spanned<Symbol>> {
        let span = self.consume(TokenKind::Identifier)?.span;
        Ok(with_session_interner(|i| {
            let sym = i.get_or_intern(span.slice(self.src));
            Spanned { val: sym, span }
        }))
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

    fn try_block(&mut self) -> Option<Result<Block<Statement>>> {
        if self.check(TokenKind::LeftBrace) {
            Some(self.block())
        } else {
            None
        }
    }
    fn block_inner(&mut self) -> Result<Vec<Statement>> {
        let mut stmts = Vec::new();
        while !self.check(TokenKind::RightBrace) && !self.is_finished() {
            let stmt = self.statement()?;
            stmts.push(stmt);
        }
        Ok(stmts)
    }
    fn block(&mut self) -> Result<Block<Statement>> {
        let open = self.consume(TokenKind::LeftBrace)?.span;
        let stmts = self.block_inner()?;
        let close = self.consume(TokenKind::RightBrace)?.span;
        Ok(Block {
            open_brace: open,
            close_brace: close,
            val: stmts.into_boxed_slice(),
        })
    }
    fn path(&mut self) -> Result<Expression> {
        let mut path = vec![self.consume_ident_spanned()?];

        while self.match_type(TokenKind::DoubleColon) {
            path.push(self.consume_ident_spanned()?);
        }

        let span = path.first().unwrap().span.join(&path.last().unwrap().span);
        Ok(Expression {
            kind: ExpressionKind::Path(path.into_boxed_slice()),
            span,
        })
    }
    fn owned_lexem(&mut self, span: Span) -> Symbol {
        let slice = span.slice(self.src);
        with_session(|sess| sess.string_interner.get_or_intern(slice))
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
        if self.is_finished() {
            return false;
        }
        match self.peek() {
            Ok(token) => token.kind == t,
            Err(_) => {
                unreachable!(
                    "Parser::is_finished returned false, \
                              so peek can't fail"
                )
            }
        }
    }
    #[inline(always)]
    fn bump(&mut self) { self.stream.next(); }
    fn advance(&mut self) -> Result<&Token> {
        self.bump();
        self.previous()
    }
    #[inline(always)]
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
        self.previous()?
            .span
            .slice(self.src)
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
