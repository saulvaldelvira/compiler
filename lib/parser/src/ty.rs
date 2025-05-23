use ast::{
    expr::ExpressionKind,
    types::{Type, TypeKind},
};
use lexer::token::TokenKind;

use crate::{error::ParseErrorKind, Parser, Result};

impl Parser<'_, '_> {
    fn array_type(&mut self) -> Result<Type> {
        let ob = self.previous_span()?;
        let ty = Box::new(self.ty()?);
        let sc = self.consume(TokenKind::Semicolon)?.span;
        self.consume(TokenKind::IntLiteral)?;
        let length = self.previous_parse::<u32>()?;
        let cb = self.consume(TokenKind::RightBracket)?.span;
        let span = ob.join(&cb);
        Ok(Type {
            kind: TypeKind::Array {
                open_brace: ob,
                ty,
                semicollon: sc,
                length,
                close_brace: cb,
            },
            span,
        })
    }
    pub(super) fn ty(&mut self) -> Result<Type> {
        macro_rules! ty {
            ($tk:expr) => {{
                let span = self.previous_span()?;
                Ok(Type {
                    kind: $tk(span),
                    span,
                })
            }};
        }
        if self.match_type(TokenKind::Int) {
            ty!(TypeKind::Int)
        } else if self.match_type(TokenKind::Float) {
            ty!(TypeKind::Float)
        } else if self.match_type(TokenKind::Char) {
            ty!(TypeKind::Char)
        } else if self.match_type(TokenKind::Bool) {
            ty!(TypeKind::Bool)
        } else if self.check(TokenKind::Identifier) {
            let p = self.path()?;
            let span = p.span;
            let ExpressionKind::Path(path) = p.kind else {
                unreachable!()
            };
            Ok(Type {
                kind: TypeKind::Path(path),
                span,
            })
        } else if self.match_type(TokenKind::LeftBracket) {
            self.array_type()
        } else if self.match_type(TokenKind::Ampersand) {
            let aspan = self.previous_span()?;
            let inner = self.ty()?;
            let span = aspan.join(&inner.span);
            Ok(Type {
                kind: TypeKind::Ref {
                    ampersand: aspan,
                    of: Box::new(inner),
                },
                span,
            })
        } else if self.match_type(TokenKind::And) {
            let aspan = self.previous_span()?;
            let inner = self.ty()?;
            let span = aspan.join(&inner.span);
            let t = Type {
                kind: TypeKind::Ref {
                    ampersand: aspan,
                    of: Box::new(Type {
                        kind: TypeKind::Ref {
                            ampersand: aspan,
                            of: Box::new(inner),
                        },
                        span,
                    }),
                },
                span,
            };
            Ok(t)
        } else {
            Err(ParseErrorKind::ExpectedNode("type"))
        }
    }
}
