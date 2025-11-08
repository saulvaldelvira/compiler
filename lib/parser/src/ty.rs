use ast::{
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
        macro_rules! primitives {
            ($first:ident $(,)? $($n:ident),* $(,)?) => {
                if self.match_type(TokenKind::$first) {
                    return ty!(TypeKind::$first)
                } $(
                    else if self.match_type(TokenKind::$n) {
                        return ty!(TypeKind::$n)
                    }
                )*
            };
        }
        primitives!(I8, I16, I32, I64, U8, U16, U32, U64, F32, F64, Char, Bool);

        if self.check(TokenKind::Identifier) {
            let p = self.path()?;
            let span = p.span;
            Ok(Type {
                kind: TypeKind::Path(p),
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
