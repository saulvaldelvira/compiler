use core::{mem, ptr};
use core::sync::atomic::{AtomicUsize, Ordering};

use semantic::TypeId;


arena::define_arenas!();

#[derive(Clone, Copy)]
pub struct MirId(usize);

#[derive(Clone, Copy)]
pub struct LocalId(MirId);
#[derive(Clone, Copy)]
pub struct RValueId(MirId);

#[derive(Clone, Copy)]
pub enum MirStatement {
    Assignment(LocalId, RValueId),
}

#[derive(Clone, Copy)]
pub enum Terminator {
    CondBranch(RValueId, MirId, MirId),
    Branch(MirId),
}

#[derive(Clone, Copy)]
pub enum MirLiteral {
    Int(i32),
}

#[derive(Clone, Copy)]
pub enum MirExpressionKind {
    Literal(MirLiteral),
    Var(LocalId),
}

#[derive(Clone, Copy)]
pub struct MirExpression {
    id: RValueId,
    kind: MirExpressionKind,
}

pub struct MirStart<'mir> {
    id: MirId,
    return_id: Option<LocalId>,
    succesors: &'mir [MirId],
}

pub struct MirEnd<'mir> {
    pub id: MirId,
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

pub struct BasiBlockBuilder<'b, 'mir> {
    pub predecesors: Vec<MirId>,
    pub locals: Vec<MirLocal>,
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
           locals: Vec::new(),
           statements: Vec::new(),
           predecesors: Vec::new(),
           mir,
       }
    }
    pub fn add_predecesor(&mut self, pred: MirId) {
        self.predecesors.push(pred);
    }
    pub fn add_expression(&mut self, expr: MirExpressionKind) -> RValueId {
        let id = RValueId(self.mir.next_id());
        self.expressions.push(MirExpression { id, kind: expr });
        id
    }
    pub fn add_local(&mut self, ty: TypeId) -> LocalId {
        let id = LocalId(self.mir.next_id());
        self.locals.push(MirLocal { id, ty });
        id
    }
    pub fn add_assignment(&mut self, lvalue: LocalId, rvalue: RValueId) {
        let assignment = MirStatement::Assignment(lvalue, rvalue);
        self.statements.push(assignment);
    }
    pub fn build(&mut self, terminator: Terminator) -> &'mir BasicBlock<'mir> {
        let mut new_self = self.new_from_self();
        mem::swap(&mut new_self, self);
        let bb = BasicBlock {
            id: new_self.id,
            statements: &*new_self.mir.arena.alloc_iter(new_self.statements),
            locals: new_self.mir.arena.alloc_iter(new_self.locals),
            terminator,
            exprs: new_self.mir.arena.alloc_iter(new_self.expressions),
            predecesors: new_self.mir.arena.alloc_iter(new_self.predecesors),

        };
        self.mir.arena.alloc(bb)
    }
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

    pub fn alloc_basic_block(&self, bb: BasicBlock<'mir>) -> &'mir BasicBlock<'mir> {
        self.arena.alloc(bb)
    }
}

