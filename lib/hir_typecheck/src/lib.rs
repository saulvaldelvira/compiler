use error_manager::ErrorManager;
use hir::visitor::{walk_arithmetic, walk_array_access, walk_assignment, walk_call, walk_definition, walk_function_definition, walk_logical, walk_return, walk_struct_access, Visitor, VisitorCtx};
use hir::{Definition, Expression, Type};
use semantic::rules::stmt::{CheckFunctionReturns, CheckReturnStmt};
use semantic::rules::{
    SemanticRule,
    expr::{ValidateArithmetic, ValidateArrayAccess, ValidateAssignment, ValidateCall, ValidateFieldAccess, ValidateLogical}
};
use semantic::Semantic;
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
        lowerer: &mut tl,
        ctx: TypeCheckingCtx::default(),
    };

    tc.visit_program(sess.get_root_program());
}

struct TypeChecking<'tc, 'hir, 'sem> {
    em: &'tc mut ErrorManager,
    lowerer: &'tc mut TypeLowering<'tc, 'sem, 'hir>,
    semantic: &'tc Semantic<'sem>,
    ctx: TypeCheckingCtx<'hir>,
}

#[derive(Default)]
struct TypeCheckingCtx<'hir> {
    funcs: Vec<&'hir Definition<'hir>>,
}

impl<'hir> Visitor<'hir> for TypeChecking<'_,'hir,'_> {
    type Result = ();
    type Ctx = TypeCheckingCtx<'hir>;

    fn get_ctx(&mut self) -> &mut Self::Ctx { &mut self.ctx }

    fn visit_variable(&mut self, base: &'hir Expression<'hir>, path: &'hir hir::Path<'hir>) {
        if let Some(def) = path.def.get() {
            let ty = def.ty.unwrap_or_else(|| {
                todo!("Infer types")
            });
            let ty = self.lowerer.lower_hir_type(ty).id;
            self.semantic.set_type_of(base.id, ty);
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

    fn visit_definition(&mut self, def: &'hir Definition<'hir>) -> Self::Result {
        if let Some(ty) = def.ty {
            let ty = self.lowerer.lower_hir_type(ty);
            self.semantic.set_type_of(def.id, ty.id);
        }
        walk_definition(self, def);
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
            body: &'hir [hir::Statement<'hir>]
    ) -> Self::Result {
        walk_function_definition(self, def, params, body);

        let ty = self.lowerer.lower_hir_type(def.ty.unwrap());
        let (_, ret_type) = ty.as_function_type().unwrap();

        CheckFunctionReturns {
            def,
            body,
            ret_type,
            span: def.span,
        }
        .apply(self.semantic, self.em);
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

    fn exit_function(&mut self) {}
}
