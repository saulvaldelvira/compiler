use core::mem;
use std::collections::HashMap;
use std::fs::metadata;
use std::io::BufRead;
use std::process::id;

use hir::expr::{ArithmeticOp, CmpOp, ExpressionKind, LitValue, LogicalOp};
use hir::stmt::StatementKind;
use hir::{BlockExpr, HirId, ItemKind};
use mir::{
    BasicBlock, ExprId, LocalId, Mir, MirEnd, MirExpression, MirExpressionKind, MirId, MirLiteral, MirLocal, MirStart, MirStatement, RValue, Terminator
};
use semantic::{Semantic, TypeId};

fn convert_logical(value: LogicalOp) -> mir::Operator {
    match value {
        LogicalOp::And => mir::Operator::And,
        LogicalOp::Or => mir::Operator::Or,
    }
}

fn convert_arithmetic(value: ArithmeticOp) -> mir::Operator {
    match value {
        ArithmeticOp::Add => mir::Operator::Add,
        ArithmeticOp::Sub => mir::Operator::Sub,
        ArithmeticOp::Mul => mir::Operator::Mul,
        ArithmeticOp::Div => mir::Operator::Div,
        ArithmeticOp::Mod => mir::Operator::Mod,
    }
}

fn convert_comparison(value: CmpOp) -> mir::Operator {
    match value {
        CmpOp::Gt => mir::Operator::Gt,
        CmpOp::Ge => mir::Operator::Ge,
        CmpOp::Lt => mir::Operator::Lt,
        CmpOp::Le => mir::Operator::Le,
        CmpOp::Eq => mir::Operator::Eq,
        CmpOp::Neq => mir::Operator::Neq,
    }
}

#[derive(Default)]
pub struct Block {
    pub predecesors: Vec<MirId>,
    pub locals: Vec<MirLocal>,
    pub statements: Vec<MirStatement>,
    pub expressions: Vec<MirExpression>,
    pub id: MirId,
}

impl Block {

}

pub struct BasiBlockBuilder<'b> {
    blocks: HashMap<MirId, Block>,
    current: Option<MirId>,

    mir: &'b mut Mir,
}

impl<'b> BasiBlockBuilder<'b> {
    pub fn new(mir: &'b mut Mir) -> Self {
        Self {
            blocks: HashMap::new(),
            current: Default::default(),
            mir,
        }
    }
    fn current(&mut self) -> &mut Block {
        self.blocks.get_mut(&self.current.unwrap()).unwrap()
    }
    /* pub fn add_predecesor(&mut self, b: Option<MirId>, pred: MirId) { */
    /*     let id = b.unwrap_or(self.current.unwrap()); */
    /*     self.blocks.get_mut(&id).unwrap().predecesors.push(pred); */
    /* } */
    pub fn add_expression(&mut self, expr: MirExpressionKind) -> ExprId {
        let id = self.mir.next_rvalue_id();
        self.current().expressions.push(MirExpression { id, kind: expr });
        id
    }
    pub fn add_assignment(&mut self, lvalue: LocalId, rvalue: RValue) {
        eprintln!("Add asign: {lvalue:?} = {rvalue:?}");
        let assignment = MirStatement::Assignment(lvalue, rvalue);
        self.current().statements.push(assignment);
    }

    pub fn add_block(&mut self) -> MirId {
        let id = self.mir.next_id();
        self.blocks.insert(id, Block {
            id,
            ..Default::default()
        });
        id
    }

    pub fn position_at(&mut self, id: MirId) {
        self.current = Some(id);
    }
    pub fn build(&mut self, terminator: Terminator, next: impl Into<Option<MirId>>) -> Result<(), Terminator> {
        if self.current.is_none() { return Err(terminator) }
        match terminator {
            Terminator::CondBranch(_, mir_id, mir_id1) => {
                let id = self.current().id;
                self.blocks.get_mut(&mir_id).unwrap().predecesors.push(id);
                self.blocks.get_mut(&mir_id1).unwrap().predecesors.push(id);
            }
            Terminator::Branch(mir_id) => {
                let id = self.current().id;
                self.blocks.get_mut(&mir_id).unwrap().predecesors.push(id);
            }
            Terminator::End => {}
        }
        let block = self.blocks.remove(&self.current.unwrap()).unwrap();
        let bb = BasicBlock {
            id: block.id,
            statements: block.statements,
            locals: block.locals,
            terminator,
            exprs: block.expressions,
            predecesors: block.predecesors,
        };
        self.mir.register_bb(bb);
        self.current = next.into();
        Ok(())
    }

    /* pub fn build_as_start(self, next: MirId) -> MirStart { */
    /*     MirStart { */
    /*         id: self.mir.next_id(), */
    /*         succesor: next, */
    /*         locals: self.locals, */
    /*         terminator: Terminator::Branch(next), */
    /*     } */
    /* } */

    /* pub fn build_as_end(self, return_id: Option<LocalId>) -> MirEnd { */
    /*     MirEnd { */
    /*         id: self.mir.next_id(), */
    /*         predecesors: self.predecesors, */
    /*         return_id, */
    /*     } */
    /* } */
}

struct HirLowering<'l, 'hir, 'mir> {
    hir: &'l hir::Session<'hir>,

    locals: HashMap<HirId, LocalId>,
    current_function_end: Vec<(Option<LocalId>, MirId)>,
    semantic: &'l Semantic<'mir>,
}

pub fn lower_hir<'hir, 'mir, 'sem>(hir: &hir::Session<'hir>, mir: &mut mir::Mir, sem: &Semantic<'mir>) {
    let root = hir.get_root();
    let mut builder = BasiBlockBuilder::new(mir);
    HirLowering {
        hir,
        locals: Default::default(),
        semantic: sem,
        current_function_end: vec![],
    }
    .lower_module(root, &mut builder);
}

impl<'l, 'hir, 'mir> HirLowering<'l, 'hir, 'mir> {
    fn add_local(&mut self, b: &mut BasiBlockBuilder<'l>, hir_id: Option<HirId>, ty: TypeId) -> LocalId {
        let id = b.mir.next_local_id();
        b.current().locals.push(MirLocal { id, ty });
        if let Some(hir_id) = hir_id {
            self.locals.insert(hir_id, id);
        }
        id
    }

    fn lower_module(&mut self, m: &hir::Module<'hir>, bb: &mut BasiBlockBuilder<'l>) {
        m.items.iter().for_each(|i| self.lower_item(i, bb));
    }
    fn lower_item(&mut self, i: &hir::Item<'hir>, bb: &mut BasiBlockBuilder<'l>) {
        match &i.kind {
            ItemKind::Mod(module) => self.lower_module(module, bb),
            ItemKind::Function(_) => { self.lower_function(i, bb); },
            ItemKind::Variable { name, ty, init, constness } => {
                let ty = self.semantic.type_of(&i.id).unwrap();
                let local = self.add_local(bb, Some(i.id), ty.id);
                if let Some(init) = init {
                    let res = self.lower_expr(bb, init);
                    bb.add_assignment(local, res.unwrap());
                }
            }
            ItemKind::Struct { name, fields } => todo!(),
            ItemKind::Use(use_item) => todo!(),
            ItemKind::TypeAlias { ty, name } => todo!(),
        }

    }
    fn lower_block(
        &mut self,
        builder: &mut BasiBlockBuilder<'l>,
        block: &BlockExpr<'hir>,
    ) -> Option<RValue> {
        for stmt in block.stmts {
            self.lower_stmt(builder, stmt);
        }

        block
            .tail
            .map(|tail| self.lower_expr(builder, tail).unwrap())
    }

    fn lower_stmt(&mut self, b: &mut BasiBlockBuilder<'l>, stmt: &'hir hir::Statement<'hir>) {
        match &stmt.kind {
            StatementKind::Expr(e) => {
                self.lower_expr(b, e);
            }
            StatementKind::While { cond, body } => todo!(),
            StatementKind::For(for_stmt) => todo!(),
            StatementKind::Empty => {}
            StatementKind::Break => todo!(),
            StatementKind::Continue => todo!(),
            StatementKind::Return(expr) => {
                let expr = expr.map(|e| self.lower_expr(b, e));
                let (ret_id, end_block) = self.current_function_end.last_mut().unwrap();
                if let Some(id) = ret_id {
                    b.add_assignment(*id, expr.unwrap().unwrap());
                }
                b.build(Terminator::Branch(*end_block), None);

            }
            StatementKind::Item(item) => self.lower_item(item, b),
        }
    }

    /* fn lower_lvalue( */
    /*     &self, */
    /*     b: &mut BasiBlockBuilder<'l, 'mir>, */
    /*     expr: &'hir hir::Expression<'hir>, */
    /* ) -> LocalId */
    /* { */
    /*     match &expr.kind { */
    /*         ExpressionKind::Variable(v) => { */
    /*             v.de */
    /*         } */
    /*         _ => todo!() */
    /*     } */
    /* } */

    fn lower_expr(
        &mut self,
        bb: &mut BasiBlockBuilder<'l>,
        expr: &'hir hir::Expression<'hir>,
    ) -> Option<RValue> {
        Some(match &expr.kind {
            ExpressionKind::If {
                cond,
                if_true,
                if_false,
            } => {
                let result_ty = self.semantic.type_of(&expr.id).map(|ty| ty.id);
                let result_id = result_ty.map(|id| self.add_local(bb, None, id));

                let then_block = dbg!(bb.add_block());
                let merge_block = dbg!(bb.add_block());
                let else_block = if_false.is_some().then(|| dbg!(bb.add_block()));

                let cond_val = self.add_local(bb, None, self.semantic.type_of(&cond.id).unwrap().id);
                let cond = self.lower_expr(bb, cond).unwrap();
                bb.add_assignment(cond_val, cond);

                if let Some(else_block) = else_block {
                    bb.build(Terminator::CondBranch(cond_val, then_block, else_block), then_block).unwrap();
                } else {
                    bb.build(Terminator::CondBranch(cond_val, then_block, merge_block), then_block).unwrap();
                }

                let if_result = self.lower_expr(bb, if_true);
                if let Some(if_result) = if_result {
                    bb.add_assignment(result_id.unwrap(), if_result);
                }
                bb.build(Terminator::Branch(merge_block), merge_block).unwrap_or_else(|_| {
                    bb.position_at(merge_block);
                });

                if let Some(else_expr) = if_false {
                    bb.position_at(else_block.unwrap());
                    let else_result = self.lower_expr(bb, else_expr);
                    if let Some(if_result) = else_result {
                        bb.add_assignment(result_id.unwrap(), if_result);
                    }
                    bb.build(Terminator::Branch(merge_block), merge_block).unwrap_or_else(|_| {
                        bb.position_at(merge_block);
                    });
                }

                return result_id.map(RValue::Local)
            }
            ExpressionKind::Array(expressions) => todo!(),
            ExpressionKind::Unary { op, expr } => todo!(),
            ExpressionKind::Block(block_expr) => {
                return self.lower_block(bb, block_expr)
            }
            ExpressionKind::Ref(expression) => todo!(),
            ExpressionKind::Deref(expression) => todo!(),
            ExpressionKind::Logical { left, op, right } => {
                let left = self.lower_expr(bb, left).unwrap();
                let right = self.lower_expr(bb, right).unwrap();
                let ty = self.semantic.type_of(&expr.id).unwrap();
                let result = self.add_local(bb, None, ty.id);
                let e = bb.add_expression(MirExpressionKind::Operation(left, convert_logical(*op) ,right));
                bb.add_assignment(result, RValue::Expr(e));
                RValue::Local(result)
            }
            ExpressionKind::Comparison { left, op, right } => {
                let left = self.lower_expr(bb, left).unwrap();
                let right = self.lower_expr(bb, right).unwrap();
                let ty = self.semantic.type_of(&expr.id).unwrap();
                let result = self.add_local(bb, None, ty.id);
                let e = bb.add_expression(MirExpressionKind::Operation(left, convert_comparison(*op) ,right));
                bb.add_assignment(result, RValue::Expr(e));
                RValue::Local(result)
            }
            ExpressionKind::Arithmetic { left, op, right } => {
                let left = self.lower_expr(bb, left).unwrap();
                let right = self.lower_expr(bb, right).unwrap();
                let ty = self.semantic.type_of(&expr.id).unwrap();
                let result = self.add_local(bb, None, ty.id);
                let e = bb.add_expression(MirExpressionKind::Operation(left, convert_arithmetic(*op) ,right));
                bb.add_assignment(result, RValue::Expr(e));
                RValue::Local(result)
            }
            ExpressionKind::Assignment { left, right } => {
                let left = self.lower_expr(bb, left).unwrap();
                let RValue::Local(id) = left else { unreachable!() };
                let right = self.lower_expr(bb, right).unwrap();
                bb.add_assignment(id, right);
                left
            }
            ExpressionKind::Variable(path) => {
                let def = path.def().expect_resolved();
                let local = self.locals.get(&def).unwrap();
                RValue::Local(*local)
            }
            ExpressionKind::Literal(lit) => match lit {
                LitValue::Int(i) => {
                    RValue::Literal(MirLiteral::Int(*i))
                }
                LitValue::Float(_) => todo!(),
                LitValue::Str(symbol) => todo!(),
                LitValue::Bool(_) => todo!(),
                LitValue::Char(_) => todo!(),
            },
            ExpressionKind::Call { callee, args } => todo!(),
            ExpressionKind::Cast { expr, to } => todo!(),
            ExpressionKind::ArrayAccess { arr, index } => {
                todo!()
                /* let array = self.lower_expr(b, arr).unwrap(); */
                /* let index = self.lower_expr(b, index).unwrap(); */
                /* RValue::Expr(MirExpressionKind::ArrayAccess(array, index)) */
            }
            ExpressionKind::TupleAccess { tuple, index } => todo!(),
            ExpressionKind::StructAccess(_) => todo!(),
        }
        )
    }

    fn lower_function(&mut self, func: &hir::Item<'hir>, bb: &mut BasiBlockBuilder<'l>) -> Option<MirId> {
        let ItemKind::Function(hir::Function {
            is_extern,
            is_variadic,
            name,
            params,
            ret_ty,
            body,
        }) = func.kind
        else {
            unreachable!()
        };
        if *is_extern {
            return None;
        }

        let start_id = dbg!(bb.add_block());
        bb.position_at(start_id);

        let (_, param_tys, ret_ty) = self
            .semantic
            .type_of(&func.id)
            .unwrap()
            .as_function_type()
            .unwrap();

        for (param, ty) in params.iter().zip(param_tys.iter()) {
            self.add_local(bb, Some(param.id), ty.id);
        }

        let ret_val_id = (!ret_ty.is_empty_type()).then(|| self.add_local(bb, None, ret_ty.id));


        let end_block = dbg!(bb.add_block());
        self.current_function_end.push((ret_val_id, end_block));

        let out = self.lower_expr(bb, body.unwrap());

        bb.build(Terminator::Branch(end_block), end_block).unwrap_or_else(|_| {
            bb.position_at(end_block);
        });

        if let Some(out) = out {
            bb.add_assignment(ret_val_id.unwrap(), out);
        }

        let (ret_val_id, end_block) = self.current_function_end.pop().unwrap();

        bb.mir.register_func(start_id);
        bb.build(Terminator::End, None).unwrap();

        self.locals.clear();

        Some(start_id)
    }
}
