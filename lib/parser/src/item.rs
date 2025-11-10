use std::fs;
use std::path::PathBuf;

use ast::{Item, ItemKind, ModuleBody, Statement, Symbol, UseTarget};
use ast::{
    item::{Field, Param, VariableConstness},
    Block, Module,
};
use error_manager::ErrorManager;
use lexer::token::TokenKind;
use span::source::{FileId, FileName};
use span::Span;

use crate::{error::ParseErrorKind, Parser, Result};

impl Parser<'_, '_> {
    pub(super) fn try_module(&mut self) -> Option<Result<Item>> {
        if self.check(TokenKind::Mod) {
            Some(self.module())
        } else {
            None
        }
    }

    pub(super) fn item(&mut self) -> Result<Item> {
        self.try_item()
            .ok_or(ParseErrorKind::ExpectedNode("item"))?
    }

    pub(crate) fn try_item(&mut self) -> Option<Result<Item>> {
        if let Some(vdecl) = self.try_var_decl() {
            Some(vdecl)
        } else if let Some(func) = self.try_function() {
            Some(func)
        }
        else if let Some(module) = self.try_module() {
            Some(module)
        }
        else if let Some(as_item) = self.try_as() {
            Some(as_item)
        }
        else {
            self.try_struct()
        }
    }

    pub(crate) fn try_as(&mut self) -> Option<Result<Item>> {
        if self.match_type(TokenKind::Use) {
            Some(self.parse_as())
        } else { None }
    }

    fn parse_as(&mut self) -> Result<Item> {
        let kw_use = self.previous_span()?;
        let src = if let Some(path) = self.try_path() {
            UseTarget::Path(path)
        } else {
            let ty = self.ty().map_err(|_| ParseErrorKind::Use)?;
            UseTarget::Type(ty)
        };

        let mut kw_as = None;
        let mut as_name = None;
        let mut span = kw_use;

        if self.match_type(TokenKind::As) {
            kw_as = Some(self.previous_span()?);
            as_name = Some(self.consume_ident_spanned()?);
        } else if matches!(src, UseTarget::Type(_)) {
            return Err(ParseErrorKind::UseTypeUnnamed)
        }
        let semicolon = self.consume(TokenKind::Semicolon)?.span;
        span = span.join(&semicolon);

        Ok(Item {
            kind: ItemKind::Use { kw_use, src, kw_as, as_name, semicolon },
            span
        })
    }

    fn parse_extern_mod(&mut self, name: Symbol) -> Result<(Box<[Item]>, FileId)> {
        let mut new_path = {
            let src_map = self.src_map.borrow();
            let fname = src_map.get_file_for_offset(self.base_offset).unwrap().filename().unwrap();

            let path: &std::path::Path = fname.as_ref();
            let parent = path.parent().unwrap();
            PathBuf::from(parent)
        };

        let mut child = name.to_string();
        child.push_str(".txt");
        new_path.push(child);

        let contents = fs::read_to_string(&new_path).map_err(|err| {
            ParseErrorKind::ReadFile(new_path.clone(), err)
            /* eprintln!("Error reading {}: {err}", new_path.display()); */
            /* std::process::exit(1); */
        })?;

        let (source, id) = self.src_map.borrow_mut()
            .add_file(FileName::Path(new_path), contents.into())
            .into_parts();

        let mut em_parse = ErrorManager::new();

        let ast = crate::parse(&source, id, self.src_map, &mut em_parse).unwrap();

        self.em.merge(&mut em_parse);

        let ModuleBody::Slf(items, id) = ast.body else { unreachable!() };
        Ok((items, id))
    }

    pub(super) fn module(&mut self) -> Result<Item> {
        let kw_mod = self.consume(TokenKind::Mod)?.span;
        let name = self.consume_ident_spanned()?;

        let (body, end_span) = if let Some(block) = self.try_block(Self::item) {
            let block = block?;
            let span = block.close_brace;
            (ModuleBody::Inline(block), span)
        } else {
            let (items, id) = self.parse_extern_mod(*name)?;
            let semicolon = self.consume(TokenKind::Semicolon)?.span;
            (ModuleBody::Extern { semicolon, items, id }, semicolon)
        };

        let span = kw_mod.join(&end_span);

        Ok(Item {
            kind: ItemKind::Mod(Module { body, name, span }),
            span,
        })
    }

    fn try_function(&mut self) -> Option<Result<Item>> {
        let extern_span = self.match_type(TokenKind::Extern).then(|| self.previous_span().unwrap());
        if self.match_type(TokenKind::Fn) {
            Some(self.function(extern_span))
        } else {
            None
        }
    }
    pub(crate) fn try_struct(&mut self) -> Option<Result<Item>> {
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
    fn struct_decl(&mut self) -> Result<Item> {
        let kw_struct = self.previous_span()?;
        let name = self.consume_ident_spanned()?;
        let lb = self.consume(TokenKind::LeftBrace)?.span;

        let mut fields = Vec::new();
        loop {
            if self.check(TokenKind::RightBrace) {
                break;
            }

            let field = self.struct_field()?;
            fields.push(field);

            if self.match_type(TokenKind::Comma) {
                continue;
            }
            break;
        }

        let end_span = self.consume(TokenKind::RightBrace)?.span;

        let fields = Block {
            val: fields.into_boxed_slice(),
            open_brace: lb,
            close_brace: end_span,
        };

        let span = kw_struct.join(&fields.close_brace);

        Ok(Item {
            kind: ItemKind::Struct {
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
    fn function(&mut self, extern_span: Option<Span>) -> Result<Item> {
        let kw_fn = self.previous_span()?;

        let name = self.consume_ident_spanned()?;

        self.consume(TokenKind::LeftParen)?;

        let mut params = Vec::new();
        let mut variadic_span = None;

        let mut first = true;
        while !self.match_type(TokenKind::RightParen) {
            if !first {
                self.consume(TokenKind::Comma)?;
            }
            first = false;

            if self.match_type(TokenKind::ThreeDot) {
               variadic_span = Some(self.previous_span()?);
               self.consume(TokenKind::RightParen)?;
               break;
            }

            let p = self.param()?;
            params.push(p);
        }

        let return_type = if self.match_type(TokenKind::Arrow) {
            Some(self.ty()?)
        } else {
            None
        };

        let mut semicolon = None;
        let mut body = None;
        let span;

        if let Some(ext) = extern_span {
            let scspan = match self.consume(TokenKind::Semicolon) {
                Ok(sc) => sc.span,
                Err(er) => {
                    if self.peek().is_ok_and(|t| t.kind == TokenKind::LeftBrace) {
                        return Err(ParseErrorKind::ExternFnDefined)
                    }
                    return Err(er)
                }
            };
            span = ext.join(&scspan);
            semicolon = Some(scspan);
        } else {
            fn _stmt(slf: &mut Parser<'_, '_>) -> Result<Statement> {
                match slf.statement() {
                    Ok(stmt) => Ok(stmt),
                    Err(err) => {
                       slf.error(err);
                       slf.synchronize_with(&[
                           TokenKind::If, TokenKind::Let, TokenKind::Mod,
                           TokenKind::Struct, TokenKind::While, TokenKind::Semicolon,
                           TokenKind::RightBrace,
                       ]);
                       slf.statement()
                    }
                }
            }
            let block = self.block(_stmt)?;
            span = kw_fn.join(&block.close_brace);
            body = Some(block);
        }

        Ok(Item {
            kind: ItemKind::Function {
                kw_extern: extern_span,
                kw_fn,
                name,
                params: params.into_boxed_slice(),
                variadic_span,
                return_type,
                body,
                semicolon
            },
            span,
        })
    }
    pub(super) fn try_var_decl(&mut self) -> Option<Result<Item>> {
        if self.check_types(&[TokenKind::Let, TokenKind::Const]) {
            Some(self.var_decl())
        } else {
            None
        }
    }
    pub(super) fn var_decl(&mut self) -> Result<Item> {
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

        let decl = Item {
            kind: ItemKind::Variable {
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
