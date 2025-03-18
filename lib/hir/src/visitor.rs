use std::ops::ControlFlow;

use crate::def::PathDef;
use crate::{Constness, Definition, Expression, Program};

pub trait Visitor<'hir> {
    type Result: VisitorResult;

    fn visit_program(&mut self, prog: &'hir Program<'hir>) -> Self::Result {
        walk_program(self, prog)
    }

    fn visit_definition(&mut self, def: &'hir Definition<'hir>) -> Self::Result {
        walk_definition(self, def)
    }

    fn visit_variable_definition(
        &mut self,
        constness: &Constness,
        init: &Option<&'hir Expression<'hir>>,
    ) -> Self::Result {
        if let Some(init) = init {
        }
        todo!()
    }

}

macro_rules! walk_iter {
    ($v:expr, $it:expr, $fn:ident) => {
        {
            for elem in $it .iter() {
                $v . $fn (elem);
            }
        }
    };
}

pub fn walk_program<'hir, V>(v: &mut V, prog: &'hir Program<'hir>) -> V::Result
where
    V: Visitor<'hir> + ?Sized
{
    walk_iter!(v, prog.defs, visit_definition);
    V::Result::output()
}

pub fn walk_definition<'hir, V>(v: &mut V, def: &'hir Definition<'hir>) -> V::Result
where
    V: Visitor<'hir> + ?Sized
{
    use crate::def::DefinitionKind;
    match &def.kind {
        DefinitionKind::Variable { constness, init } => todo!(),
        DefinitionKind::Function { body } => todo!(),
        DefinitionKind::Struct { fields } => todo!(),
    }

}

pub trait VisitorResult {
    type T;
    type Residual;

    fn output() -> Self;
    fn from_residual(residual: Self::Residual) -> Self;
    fn from_branch(b: ControlFlow<Self::Residual, Self::T>) -> Self;
    fn branch(self) -> ControlFlow<Self::Residual, Self::T>;
}

impl VisitorResult for () {
    type T = ();
    type Residual = core::convert::Infallible;

    fn output() -> Self {}
    fn from_residual(_residual: Self::Residual) -> Self {}
    fn from_branch(_b: ControlFlow<Self::Residual>) -> Self {}
    fn branch(self) -> ControlFlow<Self::Residual> {
        ControlFlow::Continue(())
    }
}

impl<B,C: Default> VisitorResult for ControlFlow<B,C> {
    type T = C;
    type Residual = B;

    fn output() -> Self {
        ControlFlow::Continue(Self::T::default())
    }

    fn from_residual(residual: Self::Residual) -> Self {
        ControlFlow::Break(residual)
    }

    fn from_branch(b: ControlFlow<Self::Residual, Self::T>) -> Self {
        b
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::T> {
        self
    }
}

impl<T: Default,E> VisitorResult for Result<T,E> {
    type T = T;
    type Residual = E;

    fn output() -> Self {
        Result::Ok(T::default())
    }

    fn from_residual(residual: Self::Residual) -> Self {
        Self::Err(residual)
    }

    fn from_branch(b: ControlFlow<Self::Residual, Self::T>) -> Self {
        match b{
            ControlFlow::Continue(c) => Ok(c),
            ControlFlow::Break(b) => Err(b)
        }
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::T> {
        match self {
            Ok(t) => ControlFlow::Continue(t),
            Err(err) => ControlFlow::Break(err)
        }
    }
}

impl<T> VisitorResult for Option<T> {
    type T = T;
    type Residual = ();

    fn output() -> Self {
        None
    }

    fn from_residual(_residual: Self::Residual) -> Self {
        None
    }

    fn from_branch(b: ControlFlow<Self::Residual, Self::T>) -> Self {
        match b {
            ControlFlow::Continue(c) => Some(c),
            ControlFlow::Break(_) => None,
        }
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::T> {
        match self {
            Some(e) => ControlFlow::Continue(e),
            None => ControlFlow::Break(()),
        }
    }
}
