use ast::item::{Item, VariableConstness};
use ast::UseTarget;
use hir::{Constness, Ident, UseItem};

use super::AstLowering;
use crate::ident;

impl<'low, 'hir: 'low> AstLowering<'low, 'hir> {
    fn lower_params(
        &mut self,
        params: &[ast::item::Param],
    ) -> &'hir [hir::Param<'hir>] {
        self.sess
            .alloc_iter(params.iter().map(|p| self.lower_param_owned(p)))
    }

    fn lower_param_owned(&mut self, param: &ast::item::Param) -> hir::Param<'hir> {
        let ty = self.lower_type(&param.ty);
        let name = self.lower_pathdef(ident(&param.name));

        hir::item::Param::new(name, ty, param.span)
    }

    pub(super) fn lower_pathdef(&mut self, ident: Ident) -> &'hir hir::PathDef {
        let pd = hir::PathDef::new(ident);
        self.sess.alloc(pd)
    }

    fn lower_fields(
        &mut self,
        fields: &[ast::item::Field],
    ) -> &'hir [hir::item::Field<'hir>] {
        self.sess
            .alloc_iter(fields.iter().map(|f| self.lower_field(f)))
    }

    fn lower_field(&mut self, field: &ast::item::Field) -> hir::item::Field<'hir> {
        let pdef = self.lower_pathdef(ident(&field.name));
        hir::item::Field::new(pdef, self.lower_type(&field.ty), field.span)
    }

    pub(super) fn lower_item(
        &mut self,
        def: &Item,
    ) -> &'hir hir::Item<'hir> {
        self.sess.alloc(self.lower_item_owned(def))
    }

    pub(super) fn lower_items(&mut self, mi: &[ast::Item]) -> &'hir [hir::Item<'hir>] {
        self.sess
            .alloc_iter(mi.iter().map(|mi| self.lower_item_owned(mi)))
    }

    pub(super) fn lower_item_owned(
        &mut self,
        def: &ast::Item,
    ) -> hir::Item<'hir> {
        use ast::item::ItemKind as IK;
        use hir::item::ItemKind as HIK;

        let kind = match &def.kind {
            IK::Variable {
                constness,
                init,
                ty,
                name,
                ..
            } => {
                let constness = match constness {
                    VariableConstness::Const(_) => Constness::Const,
                    VariableConstness::Let(_) => Constness::Default,
                };
                let init = init.as_ref().map(|i| self.lower_expression(i));
                let ty = ty.as_ref().map(|ty| self.lower_type(ty));
                let name = self.lower_pathdef(ident(name));
                HIK::Variable { ty, constness, init, name }
            }
            IK::Function {
                kw_extern,
                name,
                body,
                params,
                variadic_span,
                return_type,
                ..
            } => {
                let body = body.as_ref().map(|body| self.lower_expression(body));
                let params = self.lower_params(params);
                let ret_ty = return_type
                    .as_ref()
                    .map_or(
                        hir::Type::empty(),
                        |rt| self.lower_type(rt));
                let name = self.lower_pathdef(ident(name));
                let func = self.sess.alloc_annon(hir::Function {
                    is_extern: kw_extern.is_some(),
                    is_variadic: variadic_span.is_some(),
                    name,
                    params,
                    body,
                    ret_ty,
                });
                HIK::Function(func)
            }
            IK::Struct { name, fields, .. } => {
                let fields = self.lower_fields(&fields.val);
                let name = self.lower_pathdef(ident(name));
                HIK::Struct { fields, name }
            },
            IK::Mod(m) => {
                let m = self.lower_module(m);
                HIK::Mod(m)
            },
            IK::Use { src, as_name, .. } => {
                match src {
                    UseTarget::Path(path) => {
                        let as_name = as_name.as_ref().map(|as_name| {
                            self.lower_pathdef(ident(as_name))
                        });
                        let u = UseItem::new(Self::lower_path(path), as_name, false);
                        let u = self.sess.alloc_annon(u);
                        HIK::Use(u)
                    },
                    UseTarget::Type(ty) => {
                        let name = ident(as_name.as_ref().expect("The parser won't allow a 'use <type>' with no name"));
                        let name = self.lower_pathdef(name);
                        let ty = self.lower_type(ty);
                        HIK::TypeAlias { ty, name }
                    }
                }
            },
            IK::UseWildCard { src, .. } => {
                let u = UseItem::new(
                    Self::lower_path(src),
                    None,
                    true
                );
                HIK::Use(self.sess.alloc_annon(u))
            }
        };
        hir::Item::new(kind, def.span)
    }

}
