use core::cell::RefCell;
use core::fmt::Debug;
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

#[derive(Default, Clone, Copy, Eq, Hash, PartialEq)]
pub struct MirId(usize);

impl core::fmt::Debug for MirId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl core::fmt::Debug for LocalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.0)
    }
}

impl core::fmt::Debug for ExprId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.0)
    }
}


#[derive(Clone, Copy)]
pub struct LocalId(MirId);
#[derive(Clone, Copy)]
pub struct ExprId(MirId);

#[derive(Clone, Copy)]
pub enum RValue {
    Expr(ExprId),
    Local(LocalId),
    Literal(MirLiteral),
}

impl Debug for RValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expr(arg0) => write!(f, "Expr({})", arg0.0.0),
            Self::Local(arg0) => write!(f, "Local({})", arg0.0.0),
            Self::Literal(arg0) => write!(f, "{arg0:#?}"),
        }
    }
}

#[derive(Clone)]
pub enum MirStatement {
    Assignment(LocalId, RValue),
}

impl Debug for MirStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assignment(arg0, arg1) => write!(f, "{arg0:#?} = {arg1:#?}")
        }
    }
}

#[derive(Debug, Clone)]
pub enum Terminator {
    CondBranch(LocalId, MirId, MirId),
    Branch(MirId),
    End
}

#[derive(Debug, Clone, Copy)]
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
    pub locals: Vec<MirLocal>,
    pub succesor: MirId,
    pub terminator: Terminator,
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

#[derive(Clone)]
pub struct MirLocal {
    pub id: LocalId,
    pub ty: TypeId,
}

impl Debug for MirLocal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {:?}", self.id.0, self.ty)
    }
}

#[derive(Debug, Clone)]
pub enum MirNode {
    BB(BasicBlock),
    End(MirEnd),
}

pub struct Mir {
    ids: AtomicUsize,
    funcs: Vec<MirId>,
    bbs: HashMap<MirId, BasicBlock>,
}

impl core::fmt::Debug for Mir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (id, bb) in self.bbs.iter() {
            writeln!(f, "{} = {bb:#?}", id.0)?;
        }
        Ok(())
    }
}

impl Mir {
    pub fn new() -> Self {
        Self { ids: AtomicUsize::new(100), funcs: Vec::new(), bbs: HashMap::new() }
    }

    pub fn next_id(&self) -> MirId {
        MirId(self.ids.fetch_add(1, Ordering::Relaxed))
    }

    pub fn next_rvalue_id(&self) -> ExprId {
        ExprId(self.next_id())
    }
    pub fn next_local_id(&self) -> LocalId {
        LocalId(self.next_id())
    }

    pub fn register_bb(&mut self, bb: BasicBlock) {
        self.bbs.insert(bb.id, bb);
    }
    pub fn register_func(&mut self, func: MirId) {
        self.funcs.push(func);
    }
}

