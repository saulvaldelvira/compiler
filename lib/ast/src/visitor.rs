use std::ops::ControlFlow;

use crate::item::{Item, ItemKind, Module};
use crate::Block;
use crate::{
    expr::ExpressionKind,
    stmt::{Statement, StatementKind},
    types::{Type, TypeKind},
    Expression,
};

pub trait Visitor {
    type Result: VisitorResult;

    fn visit_expression(&mut self, expr: &Expression) -> Self::Result {
        walk_expression(self, expr)
    }

    fn visit_statement(&mut self, stmt: &Statement) -> Self::Result {
        walk_statement(self, stmt)
    }

    fn visit_type(&mut self, ty: &Type) -> Self::Result { walk_type(self, ty) }

    fn visit_module(&mut self, prog: &Module) -> Self::Result { walk_module(self, prog) }

    fn visit_item(&mut self, item: &Item) -> Self::Result {
        walk_item(self, item)
    }
}

pub fn walk_type<V>(v: &mut V, ty: &Type) -> V::Result
where
    V: Visitor + ?Sized,
{
    match &ty.kind {
        TypeKind::Array { ty, .. } => v.visit_type(ty),
        TypeKind::Ref { of, .. } => v.visit_type(of),
        TypeKind::Int(_)
        | TypeKind::Float(_)
        | TypeKind::Bool(_)
        | TypeKind::Char(_)
        | TypeKind::Path(_)
        | TypeKind::Empty(_) => V::Result::output(),
    }
}

pub fn walk_item<V>(v: &mut V, item: &Item) -> V::Result
where
    V: Visitor + ?Sized,
{
    match &item.kind {
        ItemKind::Variable { ty, init, .. } => {
            if let Some(ty) = ty {
                v.visit_type(ty);
            }
            if let Some(init) = init {
                v.visit_expression(init);
            }
            V::Result::output()
        }
        ItemKind::Function {
            params: args,
            return_type,
            body,
            ..
        } => {
            for param in args {
                v.visit_type(&param.ty);
            }
            if let Some(rty) = return_type {
                v.visit_type(rty);
            }
            if let Some(body) = body {
                for stmt in &body.val {
                    v.visit_statement(stmt);
                }
            }
            V::Result::output()
        }
        ItemKind::Struct { fields, .. } => {
            for f in &fields.val {
                v.visit_type(&f.ty);
            }
            V::Result::output()
        },
        ItemKind::Mod(m) => v.visit_module(m),
        ItemKind::Use { .. } => V::Result::output(),
    }
}

pub fn walk_statement<V>(v: &mut V, stmt: &Statement) -> V::Result
where
    V: Visitor + ?Sized,
{
    match &stmt.kind {
        StatementKind::Expression(expression, _) => v.visit_expression(expression),
        StatementKind::Print(_, expressions, _)
        | StatementKind::Read(_, expressions, _) =>
        {
            for expr in expressions {
                v.visit_expression(expr);
            }
            V::Result::output()
        }
        StatementKind::Item(item) => v.visit_item(item),
        StatementKind::Block(block) => {
            for stmt in &block.val {
                v.visit_statement(stmt);
            }
            V::Result::output()
        }
        StatementKind::If {
            cond,
            if_body,
            else_body,
            ..
        } => {
            v.visit_expression(&cond.val);
            v.visit_statement(if_body);
            if let Some(e) = else_body {
                v.visit_statement(e);
            }
            V::Result::output()
        }
        StatementKind::While { cond, body, .. } => {
            v.visit_expression(cond);
            v.visit_statement(body);
            V::Result::output()
        }
        StatementKind::For {
            init,
            cond,
            inc,
            body,
            ..
        } => {
            if let Some(init) = init {
                v.visit_item(init);
            }
            if let Some(cond) = cond {
                v.visit_expression(cond);
            }
            if let Some(inc) = inc {
                v.visit_expression(inc);
            }

            v.visit_statement(body);

            V::Result::output()
        }
        StatementKind::Empty(_) | StatementKind::Break(_) | StatementKind::Continue(_) => {
            V::Result::output()
        }
        StatementKind::Return { expr, .. } => {
            if let Some(expr) = expr {
                v.visit_expression(expr);
            }
            V::Result::output()
        }
    }
}

pub fn walk_expression<V>(v: &mut V, expr: &Expression) -> V::Result
where
    V: Visitor + ?Sized,
{
    match &expr.kind {
        ExpressionKind::Unary { expr, .. } => v.visit_expression(expr),
        ExpressionKind::Paren(pexpr) => v.visit_expression(&pexpr.val),
        ExpressionKind::Cast { expr, ty, .. } => {
            v.visit_expression(expr);
            v.visit_type(ty)
        }
        ExpressionKind::Binary { left, right, .. } => {
            v.visit_expression(left);
            v.visit_expression(right);
            V::Result::output()
        }
        ExpressionKind::Ternary {
            cond,
            if_true,
            if_false,
        } => {
            v.visit_expression(cond);
            v.visit_expression(if_true);
            v.visit_expression(if_false);
            V::Result::output()
        }
        ExpressionKind::Path(_) | ExpressionKind::Literal(_) => V::Result::output(),
        ExpressionKind::Call { callee, args } => {
            v.visit_expression(callee);
            for a in &args.val {
                v.visit_expression(a);
            }
            V::Result::output()
        }
        ExpressionKind::ArrayAccess { arr, index, .. } => {
            v.visit_expression(arr);
            v.visit_expression(index);
            V::Result::output()
        }
        ExpressionKind::StructAccess { st, .. } => v.visit_expression(st),
    }
}

pub fn walk_module<V>(v: &mut V, module: &Module) -> V::Result
where
    V: Visitor + ?Sized,
{
    use crate::ModuleBody;

    match &module.body {
        ModuleBody::Inline(Block { val: items, ..}) |
        ModuleBody::Extern { items, .. } |
        ModuleBody::Slf(items, _) => {
            for item in items {
                v.visit_item(item);
            }
        }
    }
    V::Result::output()
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
    fn branch(self) -> ControlFlow<Self::Residual> { ControlFlow::Continue(()) }
}

impl<B, C: Default> VisitorResult for ControlFlow<B, C> {
    type T = C;
    type Residual = B;

    fn output() -> Self { ControlFlow::Continue(Self::T::default()) }

    fn from_residual(residual: Self::Residual) -> Self { ControlFlow::Break(residual) }

    fn from_branch(b: ControlFlow<Self::Residual, Self::T>) -> Self { b }

    fn branch(self) -> ControlFlow<Self::Residual, Self::T> { self }
}

impl<T: Default, E> VisitorResult for Result<T, E> {
    type T = T;
    type Residual = E;

    fn output() -> Self { Result::Ok(T::default()) }

    fn from_residual(residual: Self::Residual) -> Self { Self::Err(residual) }

    fn from_branch(b: ControlFlow<Self::Residual, Self::T>) -> Self {
        match b {
            ControlFlow::Continue(c) => Ok(c),
            ControlFlow::Break(b) => Err(b),
        }
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::T> {
        match self {
            Ok(t) => ControlFlow::Continue(t),
            Err(err) => ControlFlow::Break(err),
        }
    }
}

impl<T> VisitorResult for Option<T> {
    type T = T;
    type Residual = ();

    fn output() -> Self { None }

    fn from_residual(_residual: Self::Residual) -> Self { None }

    fn from_branch(b: ControlFlow<Self::Residual, Self::T>) -> Self {
        match b {
            ControlFlow::Continue(c) => Some(c),
            ControlFlow::Break(()) => None,
        }
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::T> {
        match self {
            Some(e) => ControlFlow::Continue(e),
            None => ControlFlow::Break(()),
        }
    }
}
