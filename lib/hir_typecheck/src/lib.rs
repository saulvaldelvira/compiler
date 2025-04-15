use error_manager::ErrorManager;
use hir::node_map::HirNodeKind;
use hir::visitor::{walk_arithmetic, walk_array_access, walk_assignment, walk_call, walk_cast, walk_comparison, walk_deref, walk_expression, walk_field, walk_for, walk_function_definition, walk_if, walk_logical, walk_ref, walk_return, walk_struct_access, walk_struct_definition, walk_ternary, walk_variable_definition, walk_while, Visitor, VisitorCtx};
use hir::{Definition, Expression, Type};
use semantic::errors::{SemanticError, SemanticErrorKind, SemanticWarning, SemanticWarningKind};
use semantic::rules::expr::{SideEffect, ValidateCast, ValidateComparison, ValidateTernary};
use semantic::rules::stmt::{CheckFunctionReturns, CheckReturnStmt};
use semantic::rules::{
    SemanticRule,
    expr::{ValidateArithmetic, ValidateArrayAccess, ValidateAssignment, ValidateCall, ValidateFieldAccess, ValidateLogical}
};
use semantic::{Semantic, Ty, TypeKind};
use semantic::TypeLowering;

pub fn type_checking(
    sess: &hir::Session<'_>,
    em: &mut ErrorManager,
    semantic: &Semantic<'_>
) {
    let mut tl = TypeLowering::new(semantic);

    let mut tc = TypeChecking {
        semantic,
        em,
        hir: sess,
        lowerer: &mut tl,
        ctx: TypeCheckingCtx::default(),
    };

    tc.visit_module(sess.get_root_program());
}

struct TypeChecking<'tc, 'hir, 'sem> {
    em: &'tc mut ErrorManager,
    hir: &'tc hir::Session<'hir>,
    lowerer: &'tc mut TypeLowering<'tc, 'sem, 'hir>,
    semantic: &'tc Semantic<'sem>,
    ctx: TypeCheckingCtx<'hir>,
}

impl<'tc, 'hir, 'sem> TypeChecking<'tc, 'hir, 'sem> {
    fn check_boolean_condition(&mut self, cond: &'hir Expression<'hir>, name: &'static str) {
        if self.semantic.type_of(&cond.id).is_some_and(|ty| !ty.is_boolean()) {
            self.em.emit_error(SemanticError {
                kind: SemanticErrorKind::NonBooleanCondition(name),
                span: cond.span
            });
        }
    }

    fn lower_type(&mut self, ty: &'hir hir::Type<'hir>) -> &'sem Ty<'sem> {
        use hir::types::TypeKind;

        match &ty.kind {
            TypeKind::Path(path) => {
                let def = path.def().expect_resolved();
                let ty = self.semantic.type_of(&def);
                ty.unwrap()
            },
            _ => self.lowerer.lower_hir_type(ty)
        }
    }
}

#[derive(Default)]
struct TypeCheckingCtx<'hir> {
    funcs: Vec<&'hir Definition<'hir>>,
}

impl<'hir> Visitor<'hir> for TypeChecking<'_,'hir,'_> {
    type Result = ();
    type Ctx = TypeCheckingCtx<'hir>;

    fn get_ctx(&mut self) -> &mut Self::Ctx { &mut self.ctx }

    fn visit_variable(&mut self, base: &'hir Expression<'hir>, path: &'hir hir::Path) {
        use hir::def::DefinitionKind;

        let node = path.def().get().map(|id| self.hir.get_node(&id).unwrap_if_mod_item());
        let Some(HirNodeKind::Def(def)) = node else {
            unreachable!()
        };

        match def.kind {
            DefinitionKind::Variable { .. } |
            DefinitionKind::Function { .. } |
            DefinitionKind::Struct { .. } => {
                let ty = self.semantic.type_id_of(&def.id).unwrap_or_else(|| {
                    todo!("Infer types")
                });
                self.semantic.set_type_of(base.id, ty);
            }
        }
    }

    fn visit_struct_access(
        &mut self,
        expr: &'hir Expression<'hir>,
        st: &'hir Expression<'hir>,
        field: hir::Ident,
    ) -> Self::Result {

        walk_struct_access(self, st, field);

        ValidateFieldAccess {
            st,
            field
        }
        .apply(self.semantic, self.em)
        .inspect(|&id| {
            self.semantic.set_type_of(expr.id, id);
        });
    }

    fn visit_ref(&mut self, base: &'hir Expression<'hir>, r: &'hir Expression<'hir>) -> Self::Result {
       walk_ref(self, r);

       if let Some(ty) = self.semantic.type_of(&r.id) {
           let hir_type = self.lowerer.get_hir_type_from_semantic_id(&ty.id);
           let ty = self.hir.alloc(hir::Type::new(hir::types::TypeKind::Ref(hir_type)));
           let id = self.lowerer.lower_hir_type(ty).id;
           self.semantic.set_type_of(base.id, id);
       }
    }

    fn visit_while(
        &mut self,
        _base: &'hir hir::Statement,
        cond: &'hir Expression<'hir>,
        body: &'hir hir::Statement<'hir>,
    ) -> Self::Result {
        walk_while(self, cond, body);
        self.check_boolean_condition(cond, "while");
    }

    fn visit_for(
        &mut self,
        _base: &'hir hir::Statement,
        init: Option<&'hir Definition<'hir>>,
        cond: Option<&'hir Expression<'hir>>,
        inc: Option<&'hir Expression<'hir>>,
        body: &'hir hir::Statement<'hir>,
    ) -> Self::Result {
        walk_for(self, init, cond, inc, body);
        if let Some(cond) = cond {
            self.check_boolean_condition(cond, "for");
        }
    }

    fn visit_if(
        &mut self,
        _base: &'hir hir::Statement,
        cond: &'hir Expression<'hir>,
        if_true: &'hir hir::Statement<'hir>,
        if_false: Option<&'hir hir::Statement<'hir>>,
    ) -> Self::Result {
        walk_if(self, cond, if_true, if_false);
        self.check_boolean_condition(cond, "if");
    }

    fn visit_deref(&mut self, base: &'hir Expression<'hir>, r: &'hir Expression<'hir>) -> Self::Result {
        walk_deref(self, r);

        if let Some(ty) = self.semantic.type_of(&r.id) {
            match ty.kind {
                TypeKind::Ref(ty) => {
                    self.semantic.set_type_of(base.id, ty.id);
                },
                t => {
                    self.em.emit_error(SemanticError {
                        kind: SemanticErrorKind::DereferenceNonRef(t.to_string()),
                        span: base.span,
                    });
                }
            }
        }
    }

    fn visit_call(
            &mut self,
            expr: &'hir Expression<'hir>,
            callee: &'hir Expression<'hir>,
            args: &'hir [Expression<'hir>]
    ) -> Self::Result {
        walk_call(self, callee, args);

        ValidateCall {
            callee,
            args,
            span: expr.span,
        }
        .apply(self.semantic, self.em)
        .inspect(|&id| {
            self.semantic.set_type_of(expr.id, id);
        });
    }

    fn visit_ternary(&mut self,
        base: &'hir Expression<'hir>,
        cond: &'hir Expression<'hir>,
        if_true: &'hir Expression<'hir>,
        if_false: &'hir Expression<'hir>
    ) -> Self::Result {
        walk_ternary(self, cond, if_true, if_false);

        ValidateTernary {
            cond,
            if_true,
            if_false,
            span: base.span
        }
        .apply(self.semantic, self.em)
        .inspect(|&id| {
            self.semantic.set_type_of(base.id, id);
        });

    }

    fn visit_array_access(
            &mut self,
            expr: &'hir Expression<'hir>,
            array: &'hir Expression<'hir>,
            index: &'hir Expression<'hir>,
    ) -> Self::Result {
        walk_array_access(self, array, index);

        ValidateArrayAccess {
            arr: array,
            index,
        }
        .apply(self.semantic, self.em)
        .inspect(|&id| {
            self.semantic.set_type_of(expr.id, id);
        });
    }

    fn visit_cast(
            &mut self,
            base: &'hir Expression<'hir>,
            expr: &'hir Expression<'hir>,
            to: &'hir Type<'hir>,
    ) -> Self::Result {
        walk_cast(self, expr, to);

        let to = self.lowerer.lower_hir_type(to);

        ValidateCast {
            expr,
            to,
            span: base.span,
        }
        .apply(self.semantic, self.em)
        .inspect(|&id| {
            self.semantic.set_type_of(base.id, id);
        });

    }

    fn visit_literal(&mut self, expr: &'hir Expression<'hir>, lit: &hir::expr::LitValue) -> Self::Result {
        use hir::expr::LitValue;
        let ty = match lit {
            LitValue::Int(_) => Type::int(),
            LitValue::Float(_) => Type::float(),
            LitValue::Bool(_) => Type::bool(),
            LitValue::Char(_) => Type::char(),
            LitValue::Str(_) => todo!(),
        };
        let ty = self.lowerer.lower_hir_type(ty);
        self.semantic.set_type_of(expr.id, ty.id);
    }

    fn visit_arithmetic(
        &mut self,
        base: &'hir Expression<'hir>,
        left: &'hir Expression<'hir>,
        op: &hir::expr::ArithmeticOp,
        right: &'hir Expression<'hir>
    ) -> Self::Result {
        walk_arithmetic(self, left, op, right);

        ValidateArithmetic {
            left,
            right,
            span: base.span
        }
        .apply(self.semantic, self.em)
        .inspect(|&id| {
            self.semantic.set_type_of(base.id, id);
        });
    }

    fn visit_logical(&mut self,
       base: &'hir Expression<'hir>,
        left: &'hir Expression<'hir>,
        op: &hir::expr::LogicalOp,
        right: &'hir Expression<'hir>
    ) -> Self::Result
    {
        walk_logical(self, left, op, right);

        ValidateLogical {
            left,
            right,
            span: base.span,
        }
        .apply(self.semantic, self.em)
        .inspect(|&id| {
            self.semantic.set_type_of(base.id, id);
        });
    }

    fn visit_comparison(
        &mut self,
        base: &'hir Expression<'hir>,
        left: &'hir Expression<'hir>,
        op: &hir::expr::CmpOp,
        right: &'hir Expression<'hir>
    ) -> Self::Result {

        walk_comparison(self, left, op, right);

        ValidateComparison {
            left,
            right,
            span: base.span,
        }
        .apply(self.semantic, self.em)
        .inspect(|&id| {
            self.semantic.set_type_of(base.id, id);
        });
    }

    fn visit_field(&mut self, def: &'hir Definition<'hir>, field: &'hir hir::def::Field<'hir>) -> Self::Result {
       walk_field(self, def, field);
       let ty = self.lowerer.lower_hir_type(field.ty).id;
       self.semantic.set_type_of(field.id, ty);
    }

    fn visit_variable_definition(
        &mut self,
        def: &'hir Definition<'hir>,
        constness: &hir::Constness,
        ty: Option<&'hir Type<'hir>>,
        init: &Option<&'hir Expression<'hir>>,
    ) -> Self::Result {
        walk_variable_definition(self, def, constness, ty, init);
        let ty = ty.expect("TODO: Infer types");
        let ty = self.lower_type(ty);
        self.semantic.set_type_of(def.id, ty.id);
    }

    fn visit_struct_definition(
        &mut self,
        base: &'hir Definition<'hir>,
        fields: &'hir [hir::def::Field<'hir>],
    ) -> Self::Result {
        walk_struct_definition(self, base, fields);

        let name = base.name.ident.sym;
        let fields = self.lowerer.lower_fields(fields);

        let struct_type = semantic::types::TypeKind::Struct { name, fields };
        let id = self.lowerer.lower_semantic_type(struct_type).id;
        self.semantic.set_type_of(base.id, id);
    }

    fn visit_assignment(
            &mut self,
            base: &'hir Expression<'hir>,
            left: &'hir Expression<'hir>,
            right: &'hir Expression<'hir>
    ) -> Self::Result {

        walk_assignment(self, left, right);

        ValidateAssignment {
            left,
            right,
            span: base.span,
        }
        .apply(self.semantic, self.em)
        .inspect(|&id| {
            self.semantic.set_type_of(base.id, id);
        });
    }

    fn visit_function_definition(
        &mut self,
        def: &'hir hir::Definition<'hir>,
        params: &'hir [hir::Definition<'hir>],
        ret_ty: &'hir Type<'hir>,
        body: &'hir [hir::Statement<'hir>]
    ) -> Self::Result {

        {
            let params = params.iter().map(|p| {
                let Some((_, Some(ty), _)) = p.as_variable_def() else {
                    unreachable!()
                };
                ty
            });

            let params = self.lowerer.lower_hir_types_iter(params);
            let ret_ty = self.lowerer.lower_hir_type(ret_ty);

            let func_type = semantic::types::TypeKind::Function { params, ret_ty };
            let ty = self.lowerer.lower_semantic_type(func_type).id;
            self.semantic.set_type_of(def.id, ty);
        }

        walk_function_definition(self, def, params, ret_ty, body);

        CheckFunctionReturns {
            def,
            body,
            span: def.span,
        }
        .apply(self.semantic, self.em);
    }

    fn visit_expression_as_stmt(
        &mut self,
        _base: &'hir hir::Statement<'hir>,
        expr: &'hir Expression<'hir>
    ) -> Self::Result {
        walk_expression(self, expr);

        if !expr.has_side_effect() {
            self.em.emit_warning(SemanticWarning {
                kind: SemanticWarningKind::UselessExpressionAsStmt,
                span: expr.span,
            });
        }

    }

    fn visit_return(&mut self, base: &'hir hir::Statement<'hir>, ret: Option<&'hir Expression<'hir>>) -> Self::Result {
        walk_return(self, ret);

        let definition = self.ctx.funcs.last().unwrap_or_else(|| {
            unreachable!("If we had a return outside a function we would've detected that in the syntactic analysis phase.");
        });

        CheckReturnStmt {
            definition,
            found: ret,
            span: base.span,
        }
        .apply(self.semantic, self.em);
    }
}

impl<'hir> VisitorCtx<'hir> for TypeCheckingCtx<'hir> {

    fn enter_function(&mut self, func: &'hir hir::Definition<'hir>) {
        self.funcs.push(func);
    }

    fn exit_function(&mut self) {
        self.funcs.pop();
    }
}
