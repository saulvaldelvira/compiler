use ast::{
    declaration::{DeclarationKind, Field, Param, VariableConstness},
    Block, Declaration, ModItem, Module,
};
use lexer::token::TokenKind;

use crate::{error::ParseErrorKind, Parser, Result};

impl Parser<'_, '_> {
    pub(super) fn try_module(&mut self) -> Option<Result<Module>> {
        if self.check(TokenKind::Mod) {
            Some(self.module())
        } else {
            None
        }
    }

    pub(super) fn mod_item(&mut self) -> Result<ModItem> {
        self.try_mod_item()
            .ok_or(ParseErrorKind::ExpectedNode("module"))?
    }

    fn try_mod_item(&mut self) -> Option<Result<ModItem>> {
        if let Some(decl) = self.try_declaration() {
            Some(decl.map(|d| ModItem::Decl(Box::new(d))))
        } else {
            self.try_module().map(|m| m.map(ModItem::Mod))
        }
    }

    fn module(&mut self) -> Result<Module> {
        let kw_mod = self.consume(TokenKind::Mod)?.span;
        let name = self.consume_ident_spanned()?;

        let open_brace = self.consume(TokenKind::LeftBrace)?.span;

        let mut decls = Vec::new();

        while let Some(decl) = self.try_mod_item() {
            decls.push(decl?);
        }

        let close_brace = self.consume(TokenKind::RightBrace)?.span;

        let span = kw_mod.join(&close_brace);

        let decls = Block {
            open_brace,
            val: decls.into(),
            close_brace,
        };

        Ok(Module {
            elems: decls.val,
            name,
            span,
        })
    }

    pub(super) fn try_declaration(&mut self) -> Option<Result<Declaration>> {
        if let Some(vdecl) = self.try_var_decl() {
            Some(vdecl)
        } else if let Some(func) = self.try_function() {
            Some(func)
        } else {
            self.try_struct()
        }
    }

    fn try_function(&mut self) -> Option<Result<Declaration>> {
        if self.match_type(TokenKind::Fn) {
            Some(self.function())
        } else {
            None
        }
    }
    fn try_struct(&mut self) -> Option<Result<Declaration>> {
        if self.match_type(TokenKind::Struct) {
            Some(self.struct_decl())
        } else {
            None
        }
    }
    fn struct_field(&mut self) -> Result<Field> {
        let name = self.consume_ident_spanned()?;
        self.consume(TokenKind::Colon)?;
        let ty = self.ty()?;
        let span = name.span.join(&ty.span);
        Ok(Field { ty, name, span })
    }
    fn struct_decl(&mut self) -> Result<Declaration> {
        let kw_struct = self.previous_span()?;
        let name = self.consume_ident_spanned()?;
        let lb = self.consume(TokenKind::LeftBrace)?.span;

        let mut fields = Vec::new();
        loop {
            if self.check(TokenKind::RightBrace) {
                break;
            };

            let field = self.struct_field()?;
            fields.push(field);

            if self.match_type(TokenKind::Comma) {
                continue;
            } else {
                break;
            }
        }

        let end_span = self.consume(TokenKind::RightBrace)?.span;

        let fields = Block {
            val: fields.into_boxed_slice(),
            open_brace: lb,
            close_brace: end_span,
        };

        let span = kw_struct.join(&fields.close_brace);

        Ok(Declaration {
            kind: DeclarationKind::Struct {
                kw_struct,
                name,
                fields,
            },
            span,
        })
    }
    fn param(&mut self) -> Result<Param> {
        let name = self.consume_ident_spanned()?;
        self.consume(TokenKind::Colon)?;
        let ty = self.ty()?;
        let span = name.span.join(&ty.span);
        Ok(Param { span, ty, name })
    }
    fn function(&mut self) -> Result<Declaration> {
        let kw_fn = self.previous_span()?;

        let name = self.consume_ident_spanned()?;

        self.consume(TokenKind::LeftParen)?;

        let mut params = Vec::new();

        let mut first = true;
        while !self.match_type(TokenKind::RightParen) {
            if !first {
                self.consume(TokenKind::Comma)?;
            }
            first = false;

            let p = self.param()?;
            params.push(p);
        }

        let return_type = if self.match_type(TokenKind::Arrow) {
            Some(self.ty()?)
        } else {
            None
        };

        let body = self.block()?;

        let span = kw_fn.join(&body.close_brace);

        Ok(Declaration {
            kind: DeclarationKind::Function {
                kw_fn,
                name,
                params: params.into_boxed_slice(),
                return_type,
                body,
            },
            span,
        })
    }
    pub(super) fn try_var_decl(&mut self) -> Option<Result<Declaration>> {
        if self.check_types(&[TokenKind::Let, TokenKind::Const]) {
            Some(self.var_decl())
        } else {
            None
        }
    }
    pub(super) fn var_decl(&mut self) -> Result<Declaration> {
        let constness = if self.match_type(TokenKind::Let) {
            VariableConstness::Let(self.previous_span()?)
        } else if self.match_type(TokenKind::Const) {
            VariableConstness::Const(self.previous_span()?)
        } else {
            return self.expected_err(&[TokenKind::Let, TokenKind::Const]);
        };
        let prev = self.previous()?;
        let mut span = prev.span;

        let name = self.consume_ident_spanned()?;

        let ty = if self.match_type(TokenKind::Colon) {
            let ty = self.ty()?;
            span = span.join(&self.previous_span()?);
            Some(ty)
        } else {
            None
        };

        let mut init = None;
        if self.match_type(TokenKind::Equal) {
            init = Some(self.expression()?);
        }

        let semicolon = self.consume(TokenKind::Semicolon)?.span;
        let span = span.join(&semicolon);

        let decl = Declaration {
            kind: DeclarationKind::Variable {
                constness,
                name,
                ty,
                init,
                semicolon,
            },
            span,
        };
        Ok(decl)
    }
}
