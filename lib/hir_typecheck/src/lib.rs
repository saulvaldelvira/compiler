use error_manager::ErrorManager;
use hir::visitor::{walk_arithmetic, walk_array_access, walk_assignment, walk_call, walk_function_definition, walk_logical, walk_struct_access, Visitor};
use hir::{Expression, Type};
use semantic::rules::stmt::CheckFunctionReturns;
use semantic::rules::{
    SemanticRule,
    expr::{ValidateArithmetic, ValidateArrayAccess, ValidateAssignment, ValidateCall, ValidateFieldAccess, ValidateLogical}
};
use semantic::{PrimitiveType, Semantic};
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
    };

    tc.visit_program(sess.get_root_program());
}

struct TypeChecking<'tc, 'hir, 'sem> {
    em: &'tc mut ErrorManager,
    lowerer: &'tc mut TypeLowering<'tc, 'sem, 'hir>,
    semantic: &'tc Semantic<'sem>,
}

impl<'hir> Visitor<'hir> for TypeChecking<'_,'hir,'_> {
    type Result = ();

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
        walk_function_definition(self, params, body);

        let ty = self.lowerer.lower_hir_type(def.ty.unwrap());
        let (_, ret_ty) = ty.as_function_type().unwrap();

        let ret_type = ret_ty.unwrap_or_else(|| {
            self.semantic.get_primitive_type(PrimitiveType::Empty)
        });

        CheckFunctionReturns {
            def,
            body,
            ret_type,
            span: def.span,
        }
        .apply(self.semantic, self.em);

    }
}
