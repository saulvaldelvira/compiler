
use ast::declaration::VariableConstness;
use hir::def::DefinitionKind;
use hir::{Constness, Ident};

use crate::ident;

use super::AstLowering;

impl<'low, 'hir: 'low> AstLowering<'low, 'hir> {

    pub (super) fn lower_definitions(&mut self, expr: &[ast::Declaration]) -> &'hir [hir::Definition<'hir>] {
        self.sess.alloc_iter(
            expr.iter().map(|expr| self.lower_definition_owned(expr))
        )
    }

    fn lower_params(&mut self, params: &[ast::declaration::Param]) -> &'hir [hir::Definition<'hir>] {
        self.sess.alloc_iter(
            params.iter().map(|p| self.lower_param_owned(p))
        )
    }

    fn lower_param_owned(&mut self, param: &ast::declaration::Param) -> hir::Definition<'hir> {
        let ty = self.lower_type(&param.ty);
        let name = self.lower_pathdef(ident(&param.name));

        hir::def::Definition::new(
            DefinitionKind::Variable { constness: Constness::Default, init: None },
            name,
            ty,
            param.span
        )
    }

    fn lower_pathdef(&mut self, ident: Ident) -> &'hir hir::def::PathDef {
        let pd = hir::def::PathDef::new(ident);
        self.sess.alloc(pd)
    }

    fn lower_fields(&mut self, fields: &[ast::declaration::Field]) -> &'hir [hir::def::Field<'hir>] {
        self.sess.alloc_iter(
            fields.iter().map(|f| self.lower_field(f))
        )
    }

    fn lower_field(&mut self, field: &ast::declaration::Field) -> hir::def::Field<'hir> {
        let pdef = self.lower_pathdef(ident(&field.name));
         hir::def::Field::new(pdef, self.lower_type(&field.ty), field.span)
    }

    pub (super) fn lower_definition(&mut self, def: &ast::Declaration) -> &'hir hir::Definition<'hir> {
        self.sess.alloc(self.lower_definition_owned(def))
    }

    fn lower_definition_owned(&mut self, def: &ast::Declaration) -> hir::Definition<'hir> {
        use ast::declaration::DeclarationKind as DK;
        use hir::def::DefinitionKind as HDK;

        let (name,ty) = match &def.kind {
            DK::Variable { name, ty, .. } => {
                let ty = ty.as_ref().map(|t| self.lower_type(t));
                (ident(name), ty)
            },
            DK::Function { name, params, return_type, .. } => {
                let params = params.iter().map(|p| &p.ty);
                let params = self.lower_types(params);

                let ret_ty = return_type.as_ref().map(|t| self.lower_type(t));

                let tk = hir::types::TypeKind::Function { params, ret_ty };
                let ty: &'hir hir::Type<'hir> =
                    self.sess.alloc(hir::Type::new(tk));
                (ident(name),Some(ty))
            },
            DK::Struct { name, .. } => {
                let ty: &'hir hir::Type<'hir> = {
                    let tk = hir::types::TypeKind::Struct(self.lower_path(name));
                    self.sess.alloc(hir::Type::new(tk))
                };
                (ident(name), Some(ty))
            },
        };

        let name = self.lower_pathdef(name);

        let kind = match &def.kind {
            DK::Variable { constness, init, .. } => {
                let constness = match constness {
                    VariableConstness::Const(_) => Constness::Const,
                    VariableConstness::Let(_) => Constness::Default,
                };
                let init = init.as_ref().map(|i| self.lower_expression(i));
                HDK::Variable { constness, init }
            },
            DK::Function { body, params, .. } => {
                let body = self.lower_statements(&body.val);
                let params = self.lower_params(params);
                HDK::Function { params, body }
            },
            DK::Struct { fields, .. } => {
                let fields = self.lower_fields(&fields.val);
                HDK::Struct { fields }

            }
        };
        hir::Definition::new(kind, name, ty, def.span)
    }

}
