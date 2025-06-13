use ast::item::{Item, VariableConstness};
use hir::{Constness, Ident};

use super::AstLowering;
use crate::ident;

impl<'low, 'hir: 'low> AstLowering<'low, 'hir> {
    fn lower_params(
        &mut self,
        params: &[ast::item::Param],
    ) -> &'hir [hir::Item<'hir>] {
        self.sess
            .alloc_iter(params.iter().map(|p| self.lower_param_owned(p)))
    }

    fn lower_param_owned(&mut self, param: &ast::item::Param) -> hir::Item<'hir> {
        let ty = self.lower_type(&param.ty);
        let name = self.lower_pathdef(ident(&param.name));

        hir::item::Item::new_param(name, ty, param.span)
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

        let name = match &def.kind {
            IK::Variable { name, .. } |
            IK::Function { name, .. } |
            IK::Struct { name, .. } => ident(name),
            IK::Mod(m) => ident(&m.name),
        };

        let name = self.lower_pathdef(name);

        let kind = match &def.kind {
            IK::Variable {
                constness,
                init,
                ty,
                ..
            } => {
                let constness = match constness {
                    VariableConstness::Const(_) => Constness::Const,
                    VariableConstness::Let(_) => Constness::Default,
                };
                let init = init.as_ref().map(|i| self.lower_expression(i));
                let ty = ty.as_ref().map(|ty| self.lower_type(ty));
                HIK::Variable { ty, constness, init, name }
            }
            IK::Function {
                body,
                params,
                return_type,
                ..
            } => {
                let body = self.lower_statements(&body.val);
                let params = self.lower_params(params);
                let ret_ty = return_type
                    .as_ref()
                    .map_or_else(
                        hir::Type::empty,
                        |rt| self.lower_type(rt));
                HIK::Function {
                    name,
                    params,
                    body,
                    ret_ty,
                }
            }
            IK::Struct { fields, .. } => {
                let fields = self.lower_fields(&fields.val);
                HIK::Struct { fields, name }
            },
            IK::Mod(m) => {
                let m = self.lower_module(m);
                HIK::Mod(m)
            }
        };
        hir::Item::new(kind, def.span)
    }

}
