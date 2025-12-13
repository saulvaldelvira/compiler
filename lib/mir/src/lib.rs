use core::sync::atomic::{AtomicUsize, Ordering};
use semantic::TypeId;

arena::define_arenas!(
    [visibility = pub]
);

#[derive(Debug,Clone,Copy)]
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

#[derive(Clone, Copy)]
pub struct MirId(usize);

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

#[derive(Clone, Copy)]
pub enum MirStatement {
    Assignment(LocalId, RValue),
}

#[derive(Clone, Copy)]
pub enum Terminator {
    CondBranch(RValue, MirId, MirId),
    Branch(MirId),
}

#[derive(Clone, Copy)]
pub enum MirLiteral {
    Int(i32),
}

#[derive(Clone, Copy)]
pub enum MirExpressionKind {
    Operation(RValue, Operator, RValue),
    Var(LocalId),
}

#[derive(Clone, Copy)]
pub struct MirExpression {
    pub id: ExprId,
    pub kind: MirExpressionKind,
}

pub struct MirStart<'mir> {
    pub id: MirId,
    pub return_id: Option<LocalId>,
    pub succesors: &'mir [MirId],
}

#[derive(Clone, Copy)]
pub struct MirEnd<'mir> {
    pub id: MirId,
    pub return_id: Option<LocalId>,
    pub predecesors: &'mir [MirId],
}

#[derive(Clone, Copy)]
pub struct BasicBlock<'mir> {
    pub id: MirId,
    pub predecesors: &'mir [MirId],
    pub locals: &'mir [MirLocal],
    pub exprs: &'mir [MirExpression],
    pub statements: &'mir [MirStatement],
    pub terminator: Terminator,
}

#[derive(Clone, Copy)]
pub struct MirLocal {
    pub id: LocalId,
    pub ty: TypeId,
}

pub struct Mir<'mir> {
    arena: Arena<'mir>,
    ids: AtomicUsize,
}

impl<'mir> Mir<'mir> {
    pub fn new() -> Self {
        Self { arena: Arena::new(), ids: AtomicUsize::new(100) }
    }

    pub fn next_id(&self) -> MirId {
        MirId(self.ids.fetch_add(1, Ordering::Relaxed))
    }

    pub fn alloc<T, C>(&self, val: T) -> &'mir T
    where
        T: ArenaAllocable<'mir, C>
    {
        self.arena.alloc(val)
    }

    pub fn alloc_iter<T, I, C>(&self, val: I) -> &'mir [T]
    where
        T: ArenaAllocable<'mir, C>,
        I: IntoIterator<Item = T>,
        <I as IntoIterator>::IntoIter: ExactSizeIterator,
    {
        self.arena.alloc_iter(val)
    }

    pub fn next_rvalue_id(&self) -> ExprId {
        ExprId(self.next_id())
    }
    pub fn next_local_id(&self) -> LocalId {
        LocalId(self.next_id())
    }
}

