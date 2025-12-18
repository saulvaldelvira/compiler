use error_manager::ErrorManager;
use hir::expr::StructAccess;
use hir::stmt::ForStmt;
use hir::visitor::{walk_array_expr, walk_block, walk_tuple_access, walk_type_alias, walk_use, walk_variable_definition};
use hir::{BlockExpr, Item, PathDef};
use hir::{
    Expression, Type,
    visitor::{
        Visitor, VisitorCtx, walk_arithmetic, walk_array_access, walk_assignment, walk_call,
        walk_cast, walk_comparison, walk_deref, walk_expression, walk_field, walk_for,
        walk_function_definition, walk_if, walk_logical, walk_ref, walk_return, walk_struct_access,
        walk_struct_definition, walk_while,
    },
};
use semantic::rules::expr::{ValidateArrayExpr, ValidateIf, ValidateTupleAccess};
use semantic::{
    Semantic, TypeKind, TypeLowering,
    errors::{SemanticError, SemanticErrorKind, SemanticWarning, SemanticWarningKind},
    rules::{
        SemanticRule,
        expr::{
            SideEffect, ValidateArithmetic, ValidateArrayAccess, ValidateAssignment, ValidateCall,
            ValidateCast, ValidateComparison, ValidateFieldAccess, ValidateLogical,
        },
        stmt::{CheckFunctionReturns, CheckReturnStmt},
    },
};

pub fn type_checking(sess: &hir::Session<'_>, em: &mut ErrorManager, semantic: &Semantic<'_>) {
    let mut tl = TypeLowering::new(semantic);

    let mut tc = TypeChecking {
        semantic,
        em,
        lowerer: &mut tl,
        ctx: TypeCheckingCtx::default(),
    };

    tc.visit_module(sess.get_root());
}

struct TypeChecking<'tc, 'hir, 'sem> {
    em: &'tc mut ErrorManager,
    lowerer: &'tc mut TypeLowering<'tc, 'sem, 'hir>,
    semantic: &'tc Semantic<'sem>,
    ctx: TypeCheckingCtx<'hir>,
}

impl<'hir> TypeChecking<'_, 'hir, '_> {
    fn check_boolean_condition(&mut self, cond: &'hir Expression<'hir>, name: &'static str) {
        if self
            .semantic
            .type_of(&cond.id)
            .is_some_and(|ty| !ty.is_boolean())
        {
            self.em.emit_error(SemanticError {
                kind: SemanticErrorKind::NonBooleanCondition(name),
                span: cond.span,
            });
        }
    }
}

#[derive(Default)]
struct TypeCheckingCtx<'hir> {
    funcs: Vec<&'hir Item<'hir>>,
}

impl<'hir> Visitor<'hir> for TypeChecking<'_, 'hir, '_> {
    type Result = ();
    type Ctx = TypeCheckingCtx<'hir>;

    fn get_ctx(&mut self) -> &mut Self::Ctx { &mut self.ctx }

    fn visit_variable(&mut self, base: &'hir Expression<'hir>, path: &'hir hir::Path) {
        let node = path
            .def()
            .get()
            .unwrap();

        let ty = self
            .semantic
            .type_id_of(&node)
            .unwrap_or_else(|| todo!("Infer types"));
        self.semantic.set_type_of(base.id, ty);
    }

    fn visit_struct_access(
        &mut self,
        expr: &'hir Expression<'hir>,
        access: &'hir StructAccess<'hir>,
    ) -> Self::Result {
        walk_struct_access(self, access);

        ValidateFieldAccess { st: access.st, field: access.field }
            .apply(self.semantic, self.em)
            .inspect(|&id| {
                self.semantic.set_type_of(expr.id, id);
            });
    }

    fn visit_ref(
        &mut self,
        base: &'hir Expression<'hir>,
        r: &'hir Expression<'hir>,
    ) -> Self::Result {
        walk_ref(self, r);

        if let Some(ty) = self.semantic.type_of(&r.id) {
            let kind = semantic::types::TypeKind::Ref(ty);
            let id = self.semantic.get_or_intern_type(kind).id;
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
        f: &'hir ForStmt<'hir>,
    ) -> Self::Result {
        walk_for(self, f);
        if let Some(cond) = f.cond {
            self.check_boolean_condition(cond, "for");
        }
    }

    fn visit_block(
        &mut self,
        expr: &'hir Expression<'hir>,
        block: &BlockExpr<'hir>,
    ) -> Self::Result {
        walk_block(self, block);
        match block.tail {
            Some(tail) => {
                if let Some(id) = self.semantic.type_of(&tail.id) {
                    self.semantic.set_type_of(expr.id, id.id);
                }
            }
            None => {
                self.semantic.set_type_of(expr.id, self.semantic.get_or_intern_type(semantic::TypeKind::empty()).id);
            }
        }
    }

    fn visit_if(
        &mut self,
        base: &'hir hir::Expression,
        cond: &'hir Expression<'hir>,
        if_true: &'hir Expression<'hir>,
        if_false: Option<&'hir Expression<'hir>>,
    ) -> Self::Result {
        walk_if(self, cond, if_true, if_false);
        self.check_boolean_condition(cond, "if");
        ValidateIf {
            if_true,
            if_false,
            span: base.span,
        }
        .apply(self.semantic, self.em)
        .inspect(|ty| {
            self.semantic.set_type_of(base.id, *ty);
        });
    }

    fn visit_use(&mut self, item: &'hir Item<'hir>, u: &'hir hir::UseItem<'hir>) -> Self::Result {
       walk_use(self, item, u);

       let def = u.path.def().expect_resolved();

       if let Some(ty) = self.semantic.type_of(&def) {
           self.semantic.set_type_of(item.id, ty.id);
       }
    }

    fn visit_type_alias(&mut self, item: &'hir Item<'hir>, ty: &'hir Type<'hir>, name: &'hir PathDef) -> Self::Result {
       walk_type_alias(self, item, ty, name);

       let ty = self.lowerer.lower_hir_type(ty);
       self.semantic.set_type_of(item.id, ty.id);
    }

    fn visit_deref(
        &mut self,
        base: &'hir Expression<'hir>,
        r: &'hir Expression<'hir>,
    ) -> Self::Result {
        walk_deref(self, r);

        if let Some(ty) = self.semantic.type_of(&r.id) {
            match ty.kind {
                TypeKind::Ref(ty) => {
                    self.semantic.set_type_of(base.id, ty.id);
                }
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
        args: &'hir [Expression<'hir>],
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

    fn visit_array_expr(&mut self,
        expr: &'hir Expression<'hir>,
        exprs: &'hir [Expression<'hir>]
    ) -> Self::Result {
        walk_array_expr(self, exprs);

        ValidateArrayExpr {
            exprs
        }
        .apply(self.semantic, self.em)
        .inspect(|id| {
            self.semantic.set_type_of(expr.id, *id);
        });
    }

    fn visit_array_access(
        &mut self,
        expr: &'hir Expression<'hir>,
        array: &'hir Expression<'hir>,
        index: &'hir Expression<'hir>,
    ) -> Self::Result {
        walk_array_access(self, array, index);

        ValidateArrayAccess { arr: array, index }
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

    fn visit_literal(
        &mut self,
        expr: &'hir Expression<'hir>,
        lit: &hir::expr::LitValue,
    ) -> Self::Result {
        use hir::expr::LitValue;
        let ty = match lit {
            LitValue::Int(_) => &Type::I32,
            LitValue::Float(_) => &Type::F32,
            LitValue::Bool(_) => &Type::BOOL,
            LitValue::Char(_) => &Type::CHAR,
            LitValue::Str(_) => {
                let ctype = self.semantic.get_or_intern_type(semantic::TypeKind::Primitive(semantic::PrimitiveType::Char));
                let ty = self.semantic.get_or_intern_type(semantic::TypeKind::Ref(ctype));
                self.semantic.set_type_of(expr.id, ty.id);
                return;
            }
        };
        let ty = self.lowerer.lower_hir_type(ty);
        self.semantic.set_type_of(expr.id, ty.id);
    }

    fn visit_arithmetic(
        &mut self,
        base: &'hir Expression<'hir>,
        left: &'hir Expression<'hir>,
        op: &hir::expr::ArithmeticOp,
        right: &'hir Expression<'hir>,
    ) -> Self::Result {
        walk_arithmetic(self, left, op, right);

        ValidateArithmetic {
            left,
            right,
            span: base.span,
        }
        .apply(self.semantic, self.em)
        .inspect(|&id| {
            self.semantic.set_type_of(base.id, id);
        });
    }

    fn visit_logical(
        &mut self,
        base: &'hir Expression<'hir>,
        left: &'hir Expression<'hir>,
        op: &hir::expr::LogicalOp,
        right: &'hir Expression<'hir>,
    ) -> Self::Result {
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
        right: &'hir Expression<'hir>,
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

    fn visit_field(
        &mut self,
        _def: &'hir Item<'hir>,
        field: &'hir hir::item::Field<'hir>,
    ) -> Self::Result {
        walk_field(self, field);
        let ty = self.lowerer.lower_hir_type(field.ty).id;
        self.semantic.set_type_of(field.id, ty);
    }

    fn visit_variable_definition(&mut self,
        base: &'hir Item<'hir>,
        name: &'hir PathDef,
        ty: Option<&'hir Type<'hir>>,
        init: Option<&'hir Expression<'hir>>,
        constness: hir::Constness,
    ) -> Self::Result {
        walk_variable_definition(self, base, name, ty, init, constness);
        let ty = ty.expect("TODO: Infer types");
        let ty = self.lowerer.lower_hir_type(ty);
        self.semantic.set_type_of(base.id, ty.id);
    }

    fn visit_param(&mut self, param: &'hir hir::Param<'hir>) -> Self::Result {
        let ty = self.lowerer.lower_hir_type(param.ty);
        self.semantic.set_type_of(param.id, ty.id);
    }

    fn visit_struct_definition(
        &mut self,
        base: &'hir Item<'hir>,
        name: &'hir PathDef,
        fields: &'hir [hir::item::Field<'hir>],
    ) -> Self::Result {
        walk_struct_definition(self, base, name, fields);

        let name = name.ident.sym;
        let fields = self.lowerer.lower_fields(fields);

        let struct_type = semantic::types::TypeKind::Struct { name, fields };
        let id = self.semantic.get_or_intern_type(struct_type).id;
        self.semantic.set_type_of(base.id, id);
    }

    fn visit_assignment(
        &mut self,
        base: &'hir Expression<'hir>,
        left: &'hir Expression<'hir>,
        right: &'hir Expression<'hir>,
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
        def: &'hir hir::Item<'hir>,
        func: &'hir hir::Function<'hir>,
    ) -> Self::Result {
        {
            let params = func.params.iter().map(|p| p.ty);
            let params = self.lowerer.lower_hir_types_iter(params);
            let ret_ty = self.lowerer.lower_hir_type(func.ret_ty);

            let func_type = semantic::types::TypeKind::Function { is_variadic: func.is_variadic, params, ret_ty };
            let ty = self.semantic.get_or_intern_type(func_type).id;
            self.semantic.set_type_of(def.id, ty);
        }

        walk_function_definition(self, def, func);

        if let Some(body) = func.body {
            CheckFunctionReturns {
                def,
                body,
                span: def.span,
            }
            .apply(self.semantic, self.em);
        }

    }

    fn visit_tuple_access(
            &mut self,
            expr: &'hir Expression<'hir>,
            tuple: &'hir Expression<'hir>,
            index: u16,
    ) -> Self::Result {
        walk_tuple_access(self, tuple);
        ValidateTupleAccess {
            tuple,
            index,
            span: expr.span
        }
        .apply(self.semantic, self.em)
        .inspect(|id| {
            self.semantic.set_type_of(expr.id, *id);
        });
    }

    fn visit_expression_as_stmt(
        &mut self,
        _base: &'hir hir::Statement<'hir>,
        expr: &'hir Expression<'hir>,
    ) -> Self::Result {
        walk_expression(self, expr);

        if !expr.has_side_effect() {
            self.em.emit_warning(SemanticWarning {
                kind: SemanticWarningKind::UselessExpressionAsStmt,
                span: expr.span,
            });
        }
    }

    fn visit_return(
        &mut self,
        base: &'hir hir::Statement<'hir>,
        ret: Option<&'hir Expression<'hir>>,
    ) -> Self::Result {
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
    fn enter_function(&mut self, func: &'hir hir::Item<'hir>) { self.funcs.push(func); }

    fn exit_function(&mut self) { self.funcs.pop(); }
}
