use ast::declaration::VariableConstness;
use hir::{Constness, Ident, def::DefinitionKind};

use super::AstLowering;
use crate::ident;

impl<'low, 'hir: 'low> AstLowering<'low, 'hir> {
    fn lower_params(
        &mut self,
        params: &[ast::declaration::Param],
    ) -> &'hir [hir::Definition<'hir>] {
        self.sess
            .alloc_iter(params.iter().map(|p| self.lower_param_owned(p)))
    }

    fn lower_param_owned(&mut self, param: &ast::declaration::Param) -> hir::Definition<'hir> {
        let ty = self.lower_type(&param.ty);
        let name = self.lower_pathdef(ident(&param.name));

        hir::def::Definition::new(
            DefinitionKind::Variable {
                constness: Constness::Default,
                ty: Some(ty),
                init: None,
            },
            name,
            param.span,
        )
    }

    fn lower_pathdef(&mut self, ident: Ident) -> &'hir hir::PathDef {
        let pd = hir::PathDef::new(ident);
        self.sess.alloc(pd)
    }

    fn lower_fields(
        &mut self,
        fields: &[ast::declaration::Field],
    ) -> &'hir [hir::def::Field<'hir>] {
        self.sess
            .alloc_iter(fields.iter().map(|f| self.lower_field(f)))
    }

    fn lower_field(&mut self, field: &ast::declaration::Field) -> hir::def::Field<'hir> {
        let pdef = self.lower_pathdef(ident(&field.name));
        hir::def::Field::new(pdef, self.lower_type(&field.ty), field.span)
    }

    pub(super) fn lower_definition(
        &mut self,
        def: &ast::Declaration,
    ) -> &'hir hir::Definition<'hir> {
        self.sess.alloc(self.lower_definition_owned(def))
    }

    pub(super) fn lower_definition_owned(
        &mut self,
        def: &ast::Declaration,
    ) -> hir::Definition<'hir> {
        use ast::declaration::DeclarationKind as DK;
        use hir::def::DefinitionKind as HDK;

        let name = match &def.kind {
            DK::Variable { name, .. } => ident(name),
            DK::Function { name, .. } => ident(name),
            DK::Struct { name, .. } => ident(name),
        };

        let name = self.lower_pathdef(name);

        let kind = match &def.kind {
            DK::Variable {
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
                HDK::Variable {
                    constness,
                    ty,
                    init,
                }
            }
            DK::Function {
                body,
                params,
                return_type,
                ..
            } => {
                let body = self.lower_statements(&body.val);
                let params = self.lower_params(params);
                let ret_ty = return_type
                    .as_ref()
                    .map(|rt| self.lower_type(rt))
                    .unwrap_or_else(|| hir::Type::empty());
                HDK::Function {
                    params,
                    body,
                    ret_ty,
                }
            }
            DK::Struct { fields, .. } => {
                let fields = self.lower_fields(&fields.val);
                HDK::Struct { fields }
            }
        };
        hir::Definition::new(kind, name, def.span)
    }
}
