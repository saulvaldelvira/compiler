use core::mem;
use std::collections::HashMap;

use hir::expr::{ArithmeticOp, CmpOp, ExpressionKind, LitValue, LogicalOp};
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

    mir: &'b Mir<'mir>,
}

impl<'b, 'mir> BasiBlockBuilder<'b, 'mir> {
    #[must_use]
    pub fn new_from_self(&self) -> Self {
        Self::new(self.mir)
    }
    pub fn new(mir: &'b Mir<'mir>) -> Self {
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
    pub fn add_local(&mut self, hir_id: Option<HirId>, ty: TypeId) -> LocalId {
        let id = self.mir.next_local_id();
        if let Some(hir_id) = hir_id {
            self.locals.insert(hir_id, MirLocal { id, ty });
        }
        id
    }
    pub fn add_assignment(&mut self, lvalue: LocalId, rvalue: RValue) {
        let assignment = MirStatement::Assignment(lvalue, rvalue);
        self.statements.push(assignment);
    }
    pub fn build(&mut self, terminator: Terminator) -> &'mir BasicBlock<'mir> {
        self.mir.alloc(BasicBlock {
            id: self.id,
            statements: &*self.mir.alloc_iter(self.statements.iter().copied()),
            locals: self.mir.alloc_iter(self.locals.values().copied()),
            terminator,
            exprs: self.mir.alloc_iter(self.expressions.iter().copied()),
            predecesors: self.mir.alloc_iter(self.predecesors.iter().copied()),
        })
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

    end: Option<&'l mut BasiBlockBuilder<'l, 'mir>>,
    semantic: &'l Semantic<'mir>,
}

pub fn lower_hir<'hir, 'mir>(hir: &hir::Session<'hir>, mir: &mir::Mir<'mir>, sem: &Semantic<'mir>) {
    HirLowering {
        hir,
        mir,
        semantic: sem,
        end: None,
    };
}

impl<'l, 'hir, 'mir> HirLowering<'l, 'hir, 'mir> {
    fn lower_block(
        &self,
        builder: &mut BasiBlockBuilder<'l, 'mir>,
        block: &BlockExpr<'hir>,
    ) -> Option<RValue> {
        for stmt in block.stmts {
            self.lower_stmt(stmt);
        }

        block
            .tail
            .map(|tail| self.lower_expr(builder, tail).unwrap())
    }

    fn lower_stmt(&self, stmt: &'hir hir::Statement<'hir>) {}

    fn lower_expr(
        &self,
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
                let result_id = result_ty.map(|id| b.add_local(None, id));

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
            ExpressionKind::Block(block_expr) => todo!(),
            ExpressionKind::Ref(expression) => todo!(),
            ExpressionKind::Deref(expression) => todo!(),
            ExpressionKind::Logical { left, op, right } => {
                let left = self.lower_expr(b, left).unwrap();
                let right = self.lower_expr(b, right).unwrap();
                RValue::Expr(b.add_expression(MirExpressionKind::Operation(left, convert_logical(*op) ,right)))
            }
            ExpressionKind::Comparison { left, op, right } => todo!(),
            ExpressionKind::Arithmetic { left, op, right } => todo!(),
            ExpressionKind::Assignment { left, right } => todo!(),
            ExpressionKind::Variable(path) => todo!(),
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
            ExpressionKind::ArrayAccess { arr, index } => todo!(),
            ExpressionKind::TupleAccess { tuple, index } => todo!(),
            ExpressionKind::StructAccess(_) => todo!(),
        }
        )
    }

    fn lower_function(&self, func: &'hir hir::Item<'hir>) -> Option<MirId> {
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

        let mut end_block = BasiBlockBuilder::new(self.mir);

        for (param, ty) in params.iter().zip(param_tys.iter()) {
            builder.add_local(Some(param.id), ty.id);
        }

        let ret_val_id = (!ret_ty.is_empty_type()).then(|| builder.add_local(None, ret_ty.id));

        let out = self.lower_expr(&mut builder, body.unwrap());
        if let Some(out) = out {
            builder.add_assignment(ret_val_id.unwrap(), out);
        }

        end_block.add_predecesor(builder.id);
        builder.build(Terminator::Branch(end_block.id));

        let USEE = end_block.build_as_end(ret_val_id);

        Some(id)
    }
}
