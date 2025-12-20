use core::mem;
use std::collections::HashMap;

use hir::expr::{ArithmeticOp, CmpOp, ExpressionKind, LitValue, LogicalOp};
use hir::stmt::StatementKind;
use hir::{BlockExpr, HirId, ItemKind};
use mir::{
    BasicBlock, ExprId, LocalId, Mir, MirEnd, MirExpression, MirExpressionKind, MirId, MirLiteral, MirLocal, MirStatement, RValue, Terminator
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

pub struct BasiBlockBuilder<'b, 'mir> {
    pub predecesors: Vec<MirId>,
    pub locals: HashMap<HirId, MirLocal>,
    pub statements: Vec<MirStatement>,
    pub expressions: Vec<MirExpression>,
    pub id: MirId,

    mir: &'b mut Mir,
}

impl<'b> BasiBlockBuilder<'b> {
    #[must_use]
    pub fn new_from_self(&self) -> Self {
        Self::new(self.mir)
    }
    pub fn new(mir: &'b mut Mir) -> Self {
        Self {
            id: mir.next_id(),
            expressions: Vec::new(),
            locals: HashMap::new(),
            statements: Vec::new(),
            predecesors: Vec::new(),
            mir,
        }
    }
    pub fn add_predecesor(&mut self, pred: MirId) {
        self.predecesors.push(pred);
    }
    pub fn add_expression(&mut self, expr: MirExpressionKind) -> ExprId {
        let id = self.mir.next_rvalue_id();
        self.expressions.push(MirExpression { id, kind: expr });
        id
    }
    pub fn add_assignment(&mut self, lvalue: LocalId, rvalue: RValue) {
        let assignment = MirStatement::Assignment(lvalue, rvalue);
        self.statements.push(assignment);
    }
    pub fn build(&mut self, terminator: Terminator) -> &'mir BasicBlock<'mir> {
        let bb = self.mir.alloc(BasicBlock {
            id: self.id,
            statements: &*self.mir.alloc_iter(self.statements.iter().copied()),
            locals: self.mir.alloc_iter(self.locals.values().copied()),
            terminator,
            exprs: self.mir.alloc_iter(self.expressions.iter().copied()),
            predecesors: self.mir.alloc_iter(self.predecesors.iter().copied()),
        });
        self.mir.register_bb(bb);
        bb
    }

    pub fn build_as_end(self, return_id: Option<LocalId>) -> &'mir MirEnd<'mir> {
        self.mir.alloc(MirEnd {
            id: self.mir.next_id(),
            predecesors: self.mir.alloc_iter(self.predecesors),
            return_id,
        })
    }
}

struct HirLowering<'l, 'hir, 'mir> {
    hir: &'l hir::Session<'hir>,
    mir: &'l mir::Mir<'mir>,

    locals: HashMap<HirId, LocalId>,
    current_function_end: Vec<(Option<LocalId>, BasiBlockBuilder<'l, 'mir>)>,
    semantic: &'l Semantic<'mir>,
}

pub fn lower_hir<'hir, 'mir, 'sem>(hir: &hir::Session<'hir>, mir: &mir::Mir<'mir>, sem: &Semantic<'mir>) {
    let root = hir.get_root();
    let mut bb = BasiBlockBuilder::new(mir);
    HirLowering {
        hir,
        mir,
        locals: Default::default(),
        semantic: sem,
        current_function_end: vec![],
    }
    .lower_module(&mut bb, root);
}

impl<'l, 'hir, 'mir> HirLowering<'l, 'hir, 'mir> {
    fn add_local(&mut self, b: &mut BasiBlockBuilder<'l, 'mir>, hir_id: Option<HirId>, ty: TypeId) -> LocalId {
        let id = self.mir.next_local_id();
        if let Some(hir_id) = hir_id {
            b.locals.insert(hir_id, MirLocal { id, ty });
            self.locals.insert(hir_id, id);
        }
        id
    }

    fn lower_module(&mut self, b: &mut BasiBlockBuilder<'l, 'mir>, m: &hir::Module<'hir>) {
        m.items.iter().for_each(|i| self.lower_item(b, i));
    }
    fn lower_item(&mut self, b: &mut BasiBlockBuilder<'l, 'mir>, i: &hir::Item<'hir>) {
        match &i.kind {
            ItemKind::Mod(module) => self.lower_module(b, module),
            ItemKind::Function(_) => { self.lower_function(i); },
            ItemKind::Variable { name, ty, init, constness } => {
                let ty = self.semantic.type_of(&i.id).unwrap();
                let local = self.add_local(b, Some(i.id), ty.id);
                if let Some(init) = init {
                    let res = self.lower_expr(b, init);
                    b.add_assignment(local, res.unwrap());
                }
            }
            ItemKind::Struct { name, fields } => todo!(),
            ItemKind::Use(use_item) => todo!(),
            ItemKind::TypeAlias { ty, name } => todo!(),
        }

    }
    fn lower_block(
        &mut self,
        builder: &mut BasiBlockBuilder<'l, 'mir>,
        block: &BlockExpr<'hir>,
    ) -> Option<RValue> {
        for stmt in block.stmts {
            self.lower_stmt(builder, stmt);
        }

        block
            .tail
            .map(|tail| self.lower_expr(builder, tail).unwrap())
    }

    fn lower_stmt(&mut self, b: &mut BasiBlockBuilder<'l, 'mir>, stmt: &'hir hir::Statement<'hir>) {
        match &stmt.kind {
            StatementKind::Expr(e) => {
                if let Some(res) = self.lower_expr(b, e) {
                    let ty = self.semantic.type_of(&e.id).unwrap();
                    let lval = self.add_local(b, None, ty.id);
                    b.add_assignment(lval, res);
                }

            }
            StatementKind::While { cond, body } => todo!(),
            StatementKind::For(for_stmt) => todo!(),
            StatementKind::Empty => {}
            StatementKind::Break => todo!(),
            StatementKind::Continue => todo!(),
            StatementKind::Return(expr) => {
                let expr = expr.map(|e| self.lower_expr(b, e));
                let (ret_id, end_block) = self.current_function_end.last_mut().unwrap();
                end_block.add_predecesor(b.id);
                if let Some(id) = ret_id {
                    b.add_assignment(*id, expr.unwrap().unwrap());
                }
                b.build(Terminator::Branch(end_block.id));

            }
            StatementKind::Item(item) => self.lower_item(b, item),
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
        b: &mut BasiBlockBuilder<'l, 'mir>,
        expr: &'hir hir::Expression<'hir>,
    ) -> Option<RValue> {
        Some(
        match &expr.kind {
            ExpressionKind::If {
                cond,
                if_true,
                if_false,
            } => {
                let result_ty = self.semantic.type_of(&expr.id).map(|ty| ty.id);
                let result_id = result_ty.map(|id| self.add_local(b, None, id));

                let mut then_block = BasiBlockBuilder::new(self.mir);
                let mut merge_block = BasiBlockBuilder::new(self.mir);
                let else_block = if_false.is_some().then(|| BasiBlockBuilder::new(self.mir));

                let cond = self.lower_expr(b, expr).unwrap();

                merge_block.add_predecesor(then_block.id);
                if let Some(else_block) = else_block {
                    merge_block.add_predecesor(else_block.id);
                    b.build(Terminator::CondBranch(cond, then_block.id, else_block.id));
                } else {
                    merge_block.add_predecesor(b.id);
                    b.build(Terminator::CondBranch(cond, then_block.id, merge_block.id));
                }

                then_block.add_predecesor(b.id);
                let if_result = self.lower_expr(&mut then_block, if_true);
                if let Some(if_result) = if_result {
                    then_block.add_assignment(result_id.unwrap(), if_result);
                }
                then_block.build(Terminator::Branch(merge_block.id));

                if let Some(else_expr) = if_false {
                    let mut else_block = BasiBlockBuilder::new(self.mir);
                    let else_result = self.lower_expr(&mut else_block, else_expr);
                    if let Some(if_result) = else_result {
                        else_block.add_assignment(result_id.unwrap(), if_result);
                    }
                    else_block.build(Terminator::Branch(merge_block.id));
                }

                mem::swap(b, &mut merge_block);

                return result_id.map(RValue::Local)
            }
            ExpressionKind::Array(expressions) => todo!(),
            ExpressionKind::Unary { op, expr } => todo!(),
            ExpressionKind::Block(block_expr) => {
                return self.lower_block(b, block_expr)
            }
            ExpressionKind::Ref(expression) => todo!(),
            ExpressionKind::Deref(expression) => todo!(),
            ExpressionKind::Logical { left, op, right } => {
                let left = self.lower_expr(b, left).unwrap();
                let right = self.lower_expr(b, right).unwrap();
                RValue::Expr(b.add_expression(MirExpressionKind::Operation(left, convert_logical(*op) ,right)))
            }
            ExpressionKind::Comparison { left, op, right } => todo!(),
            ExpressionKind::Arithmetic { left, op, right } => todo!(),
            ExpressionKind::Assignment { left, right } => {
                let left = self.lower_expr(b, left).unwrap();
                let RValue::Local(id) = left else { unreachable!() };
                let right = self.lower_expr(b, right);
                b.add_assignment(id, right.unwrap());
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

    fn lower_function(&mut self, func: &hir::Item<'hir>) -> Option<MirId> {
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

        let mut builder = BasiBlockBuilder::new(self.mir);
        let id = builder.id;

        let (_, param_tys, ret_ty) = self
            .semantic
            .type_of(&func.id)
            .unwrap()
            .as_function_type()
            .unwrap();

        for (param, ty) in params.iter().zip(param_tys.iter()) {
            self.add_local(&mut builder, Some(param.id), ty.id);
        }

        let ret_val_id = (!ret_ty.is_empty_type()).then(|| self.add_local(&mut builder, None, ret_ty.id));

        let end_block = BasiBlockBuilder::new(self.mir);
        self.current_function_end.push((ret_val_id, end_block));


        let out = self.lower_expr(&mut builder, body.unwrap());
        if let Some(out) = out {
            builder.add_assignment(ret_val_id.unwrap(), out);
        }

        let (ret_val_id, mut end_block) = self.current_function_end.pop().unwrap();

        end_block.add_predecesor(builder.id);
        builder.build(Terminator::Branch(end_block.id));

        let USEE = end_block.build_as_end(ret_val_id);
        self.mir.register_end_block(USEE);

        Some(id)
    }
}
