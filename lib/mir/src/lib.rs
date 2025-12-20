use core::cell::RefCell;
use core::sync::atomic::{AtomicUsize, Ordering};
use std::collections::HashMap;
use semantic::TypeId;

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    Neq,
    And,
    Or,
    Mod,
    Assign,
}

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub struct MirId(usize);

#[derive(Debug, Clone)]
pub struct LocalId(MirId);
#[derive(Debug, Clone)]
pub struct ExprId(MirId);

#[derive(Debug, Clone)]
pub enum RValue {
    Expr(ExprId),
    Local(LocalId),
    Literal(MirLiteral),
}

#[derive(Debug, Clone)]
pub enum MirStatement {
    Assignment(LocalId, RValue),
}

#[derive(Debug, Clone)]
pub enum Terminator {
    CondBranch(RValue, MirId, MirId),
    Branch(MirId),
}

#[derive(Debug, Clone)]
pub enum MirLiteral {
    Int(i32),
}

#[derive(Debug, Clone)]
pub enum MirExpressionKind {
    Operation(RValue, Operator, RValue),
    ArrayAccess(LocalId, RValue),
    Var(LocalId),
}

#[derive(Debug, Clone)]
pub struct MirExpression {
    pub id: ExprId,
    pub kind: MirExpressionKind,
}

#[derive(Debug, Clone)]
pub struct MirStart {
    pub id: MirId,
    pub return_id: Option<LocalId>,
    pub succesors: Vec<MirId>,
}

#[derive(Debug, Clone)]
pub struct MirEnd {
    pub id: MirId,
    pub return_id: Option<LocalId>,
    pub predecesors: Vec<MirId>,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: MirId,
    pub predecesors: Vec<MirId>,
    pub locals: Vec<MirLocal>,
    pub exprs: Vec<MirExpression>,
    pub statements: Vec<MirStatement>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone)]
pub struct MirLocal {
    pub id: LocalId,
    pub ty: TypeId,
}

#[derive(Debug, Clone)]
pub enum MirNode {
    BB(BasicBlock),
    End(MirEnd),
}

pub struct Mir {
    ids: AtomicUsize,
    bbs: RefCell<HashMap<MirId, MirNode>>,
}

impl core::fmt::Debug for Mir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (id, bb) in self.bbs.borrow().iter() {
            match bb {
                MirNode::BB(bb) => write!(f, "{} = {bb:#?}", id.0),
                MirNode::End(mir_end) => write!(f, "{} = {mir_end:#?}", id.0),
            }?;
        }
        Ok(())
    }
}

impl Mir {
    pub fn new() -> Self {
        Self { ids: AtomicUsize::new(100), bbs: Default::default() }
    }

    pub fn next_id(&self) -> MirId {
        MirId(self.ids.fetch_add(1, Ordering::Relaxed))
    }

    pub fn register_bb(&self, bb: BasicBlock) {
        self.bbs.borrow_mut().insert(bb.id, MirNode::BB(bb));
    }

    pub fn register_end_block(&self, bb: MirEnd) {
        self.bbs.borrow_mut().insert(bb.id, MirNode::End(bb));
    }

    pub fn next_rvalue_id(&self) -> ExprId {
        ExprId(self.next_id())
    }
    pub fn next_local_id(&self) -> LocalId {
        LocalId(self.next_id())
    }
}

