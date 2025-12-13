use hir::expr::{ExpressionKind, LitValue};
use hir::{BlockExpr, Expression, ItemKind};
use mir::{BasiBlockBuilder, BasicBlock, LocalId, MirEnd, MirExpression, MirExpressionKind, MirId, MirLiteral, RValueId, Terminator};
use semantic::Semantic;


struct HirLowering<'l, 'hir, 'mir> {
    hir: &'l hir::Session<'hir>,
    mir: &'l mir::Mir<'mir>,

    end: Option<&'l mut BasiBlockBuilder<'l, 'mir>>,
   semantic: &'l Semantic<'mir>,
}

pub fn lower_hir<'hir, 'mir>(hir: &hir::Session<'hir>, mir: &mir::Mir<'mir>, sem: &Semantic<'mir>) {
    HirLowering {
        hir, mir, semantic: sem, end: None
    };
}

impl<'hir, 'mir> HirLowering<'_, 'hir, 'mir> {

    fn lower_block(&self, builder: &mut BasiBlockBuilder<'_, 'mir>,  block: &BlockExpr<'hir>) -> Option<RValueId> {
        for stmt in block.stmts {
            self.lower_stmt(stmt);
        }

        block.tail.map(|tail| {
            self.lower_expr(builder, tail).unwrap()
        })
    }

    fn lower_stmt(&self, stmt: &'hir hir::Statement<'hir>) {

    }

    fn lower_expr(&self, b: &mut BasiBlockBuilder<'_, 'mir>, expr: &'hir hir::Expression<'hir>) -> Option<RValueId> {
        match &expr.kind {
            ExpressionKind::If { cond, if_true, if_false } => {
                let cond = self.lower_expr(b, expr).unwrap();

                let mut next = BasiBlockBuilder::new(self.mir);
                let out = if_true.tail.map(|t| {
                    b.add_local(self.semantic.type_of(&t.id).unwrap().id)
                });

                let mut if_block = BasiBlockBuilder::new(self.mir);
                if_block.add_predecesor(b.id);
                let if_val = self.lower_block(&mut if_block, if_true);
                if let Some(if_val) = if_val {
                    if_block.add_assignment(out.unwrap(), if_val);
                }
                let if_id = if_block.build(Terminator::Branch(next.id)).id;

                let mut else_block = BasiBlockBuilder::new(self.mir);
                else_block.add_predecesor(b.id);
                if let Some(if_false) = if_false {
                    let else_value = self.lower_expr(&mut else_block, if_false);
                    if let Some(else_value) = else_value {
                        b.add_assignment(out.unwrap(), else_value);
                    }
                }
                let else_id = else_block.build(Terminator::Branch(next.id)).id;

                b.build(Terminator::CondBranch(cond, if_id, else_id));

                let out = out.map(|out| {
                    b.add_expression(MirExpressionKind::Var(out))
                });

                b.add_predecesor(if_id);
                b.add_predecesor(else_id);


                out
            }
            ExpressionKind::Array(expressions) => todo!(),
            ExpressionKind::Unary { op, expr } => todo!(),
            ExpressionKind::Block(block_expr) => todo!(),
            ExpressionKind::Ref(expression) => todo!(),
            ExpressionKind::Deref(expression) => todo!(),
            ExpressionKind::Logical { left, op, right } => todo!(),
            ExpressionKind::Comparison { left, op, right } => todo!(),
            ExpressionKind::Arithmetic { left, op, right } => todo!(),
            ExpressionKind::Assignment { left, right } => todo!(),
            ExpressionKind::Variable(path) => todo!(),
            ExpressionKind::Literal(lit) => {
                match lit {
                    LitValue::Int(i) => {
                        Some(b.add_expression(MirExpressionKind::Literal(MirLiteral::Int(*i))))
                    }
                    LitValue::Float(_) => todo!(),
                    LitValue::Str(symbol) => todo!(),
                    LitValue::Bool(_) => todo!(),
                    LitValue::Char(_) => todo!(),
                }
            }
            ExpressionKind::Call { callee, args } => todo!(),
            ExpressionKind::Cast { expr, to } => todo!(),
            ExpressionKind::ArrayAccess { arr, index } => todo!(),
            ExpressionKind::TupleAccess { tuple, index } => todo!(),
            ExpressionKind::StructAccess { st, field } => todo!(),
        }
    }

    fn lower_function(&self, func: &'hir hir::Item<'hir>) -> Option<MirId> {
        let ItemKind::Function { is_extern, is_variadic, name, params, ret_ty, body } = func.kind else { unreachable!() };
        if is_extern { return None }


        let mut builder = BasiBlockBuilder::new(self.mir);
        let id = builder.id;

        let (_, params, ret_ty) = self.semantic.type_of(&func.id).unwrap().as_function_type().unwrap();

        let ret_val_id = builder.add_local(ret_ty.id);

        let mut end_block = BasiBlockBuilder::new(self.mir);

        let out = self.lower_block(&mut builder, &body.unwrap());
        if let Some(out) = out {
            builder.add_assignment(ret_val_id, out);
        }

        let block = builder.build(Terminator::Branch(end_block.id));

        end_block.add_predecesor(block.id);

        Some(id)
    }

}


