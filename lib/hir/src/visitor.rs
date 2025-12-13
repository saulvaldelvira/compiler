use std::ops::ControlFlow;

use crate::expr::BlockExpr;
use crate::item::Item;
use crate::{Constness, Param, UseItem};
use crate::{
Expression, HirId, Ident, Module, Path, PathDef,
Statement, Type,
item::Field,
expr::{ArithmeticOp, CmpOp, LitValue, LogicalOp, UnaryOp},
hir, stmt,
};

pub trait Visitor<'hir> {
type Result: VisitorResult;
type Ctx: VisitorCtx<'hir>;

fn get_ctx(&mut self) -> &mut Self::Ctx;

fn visit_module(&mut self, m: &'hir Module<'hir>) -> Self::Result { walk_module(self, m) }

fn visit_param(&mut self, param: &'hir Param<'hir>) -> Self::Result {
    walk_param(self, param)
}

fn visit_item(&mut self, item: &'hir Item<'hir>) -> Self::Result {
    walk_item(self, item)
}

fn visit_use(&mut self, item: &'hir Item<'hir>, u: &'hir UseItem<'hir>) -> Self::Result {
    walk_use(self, item, u)
}

fn visit_type_alias(&mut self, item: &'hir Item<'hir>, ty: &'hir Type<'hir>, name: &'hir PathDef) -> Self::Result {
    walk_type_alias(self, item, ty, name)
}

fn visit_variable_definition(&mut self,
    base: &'hir Item<'hir>,
    name: &'hir PathDef,
    ty: Option<&'hir Type<'hir>>,
    init: Option<&'hir Expression<'hir>>,
    constness: Constness,
) -> Self::Result {
    walk_variable_definition(self, base, name, ty, init, constness)
}

fn visit_expression(&mut self, expr: &'hir Expression<'hir>) -> Self::Result {
    walk_expression(self, expr)
}

fn visit_expression_as_stmt(
    &mut self,
    _base: &'hir Statement<'hir>,
    expr: &'hir Expression<'hir>,
) -> Self::Result {
    walk_expression(self, expr)
}

fn visit_ref(
    &mut self,
    _base: &'hir Expression<'hir>,
    r: &'hir Expression<'hir>,
) -> Self::Result {
    walk_ref(self, r)
}

fn visit_cast(
    &mut self,
    _base: &'hir Expression<'hir>,
    expr: &'hir Expression<'hir>,
    to: &'hir Type<'hir>,
) -> Self::Result {
    walk_cast(self, expr, to)
}

fn visit_deref(
    &mut self,
    _base: &'hir Expression<'hir>,
    r: &'hir Expression<'hir>,
) -> Self::Result {
    walk_deref(self, r)
}

fn visit_unary(&mut self, _op: &UnaryOp, expr: &'hir Expression<'hir>) -> Self::Result {
    walk_unary(self, expr)
}

fn visit_logical(
    &mut self,
    _base: &'hir Expression<'hir>,
    left: &'hir Expression<'hir>,
    op: &LogicalOp,
    right: &'hir Expression<'hir>,
) -> Self::Result {
    walk_logical(self, left, op, right)
}

fn visit_comparison(
    &mut self,
    _base: &'hir Expression<'hir>,
    left: &'hir Expression<'hir>,
    op: &CmpOp,
    right: &'hir Expression<'hir>,
) -> Self::Result {
    walk_comparison(self, left, op, right)
}

fn visit_arithmetic(
    &mut self,
    _base: &'hir Expression<'hir>,
    left: &'hir Expression<'hir>,
    op: &ArithmeticOp,
    right: &'hir Expression<'hir>,
) -> Self::Result {
    walk_arithmetic(self, left, op, right)
}

fn visit_assignment(
    &mut self,
    _base: &'hir Expression<'hir>,
    left: &'hir Expression<'hir>,
    right: &'hir Expression<'hir>,
) -> Self::Result {
    walk_assignment(self, left, right)
}

fn visit_variable(
    &mut self,
    _base: &'hir Expression<'hir>,
    path: &'hir hir::Path,
) -> Self::Result {
    walk_variable(self, path)
}

fn visit_function_definition(
    &mut self,
    _is_extern: bool,
    _is_variadic: bool,
    base: &'hir Item<'hir>,
    name: &'hir PathDef,
    params: &'hir [Param<'hir>],
    ret_ty: &'hir Type<'hir>,
    body: Option<&'hir BlockExpr<'hir>>,
) -> Self::Result {
    walk_function_definition(self, base, name, params, ret_ty, body)
}

fn visit_struct_definition(
    &mut self,
    base: &'hir Item<'hir>,
    name: &'hir PathDef,
    fields: &'hir [Field<'hir>],
) -> Self::Result {
    walk_struct_definition(self, base, name, fields)
}

fn visit_field(
    &mut self,
    _def: &'hir Item<'hir>,
    param: &'hir Field<'hir>,
) -> Self::Result {
    walk_field(self, param)
}

fn visit_statement(&mut self, stmt: &'hir Statement<'hir>) -> Self::Result {
    walk_statement(self, stmt)
}

fn visit_block(
    &mut self,
    block: &BlockExpr<'hir>,
) -> Self::Result {
    walk_block(self, block)
    }

    fn visit_break(&mut self, _base: &'hir Statement<'hir>) -> Self::Result {
        Self::Result::output()
    }

    fn visit_empty(&mut self, _base: &'hir Statement<'hir>) -> Self::Result {
        Self::Result::output()
    }

    fn visit_continue(&mut self, _base: &'hir Statement<'hir>) -> Self::Result {
        Self::Result::output()
    }

    fn visit_return(
        &mut self,
        _base: &'hir Statement<'hir>,
        ret: Option<&'hir Expression<'hir>>,
    ) -> Self::Result {
        walk_return(self, ret)
    }

    fn visit_literal(&mut self, _expr: &'hir Expression<'hir>, _lit: &LitValue) -> Self::Result {
        Self::Result::output()
    }

    fn visit_pathdef(&mut self, _owner: HirId, _pdef: &'hir PathDef) -> Self::Result {
        Self::Result::output()
    }

    fn visit_path(&mut self, _path: &'hir Path) -> Self::Result { Self::Result::output() }

    fn visit_for(
        &mut self,
        _base: &'hir Statement,
        init: Option<&'hir Item<'hir>>,
        cond: Option<&'hir Expression<'hir>>,
        inc: Option<&'hir Expression<'hir>>,
        body: &'hir Statement<'hir>,
    ) -> Self::Result {
        walk_for(self, init, cond, inc, body)
    }

    fn visit_while(
        &mut self,
        _base: &'hir Statement,
        cond: &'hir Expression<'hir>,
        body: &'hir Statement<'hir>,
    ) -> Self::Result {
        walk_while(self, cond, body)
    }

    fn visit_if(
        &mut self,
        _base: &'hir Expression,
        cond: &'hir Expression<'hir>,
        if_true: &'hir BlockExpr<'hir>,
        if_false: Option<&'hir BlockExpr<'hir>>,
    ) -> Self::Result {
        walk_if(self, cond, if_true, if_false)
    }

    fn visit_call(
        &mut self,
        _expr: &'hir Expression<'hir>,
        callee: &'hir Expression<'hir>,
        args: &'hir [Expression<'hir>],
    ) -> Self::Result {
        walk_call(self, callee, args)
    }

    fn visit_array_access(
        &mut self,
        _expr: &'hir Expression<'hir>,
        array: &'hir Expression<'hir>,
        index: &'hir Expression<'hir>,
    ) -> Self::Result {
        walk_array_access(self, array, index)
    }

    fn visit_struct_access(
        &mut self,
        _expr: &'hir Expression<'hir>,
        st: &'hir Expression<'hir>,
        field: Ident,
    ) -> Self::Result {
        walk_struct_access(self, st, field)
    }

    fn visit_type(&mut self, ty: &'hir Type<'hir>) -> Self::Result { walk_type(self, ty) }
}

macro_rules! walk_iter {
    ($v:expr, $it:expr, $fn:ident) => {{
        for elem in $it.iter() {
            $v.$fn(elem);
        }
    }};
}

macro_rules! walk_opt {
    ($v:expr, $opt:expr, $fn:ident) => {
        if let Some(elem) = $opt {
            $v.$fn(elem);
        }
    };
}

pub fn walk_module<'hir, V>(v: &mut V, prog: &'hir Module<'hir>) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_pathdef(prog.id, prog.name);
    v.get_ctx().enter_module(prog);
    for item in prog.items {
        v.visit_item(item);
    }
    v.get_ctx().exit_module();
    V::Result::output()
}

pub fn walk_type<'hir, V>(v: &mut V, ty: &'hir Type<'hir>) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    use hir::types::TypeKind;
    match &ty.kind {
        TypeKind::Path(path) => {
            v.visit_path(path);
        }
        TypeKind::Tuple(tys) => {
            for ty in *tys {
                v.visit_type(ty);
            }
        }
        TypeKind::Ref(r) => {
            v.visit_type(r);
        }
        TypeKind::Array(ty, _) => {
            v.visit_type(ty);
        }
        TypeKind::Function { is_variadic: _, params, ret_ty } => {
            walk_iter!(v, params, visit_type);
            v.visit_type(ret_ty);
        }
        TypeKind::Primitive(_) => {}
    }
    V::Result::output()
}

pub fn walk_use<'hir, V>(v: &mut V, item: &'hir Item<'hir>, s: &'hir UseItem<'hir>) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_path(&s.path);
    if let Some(n) = s.new_name {
        v.visit_pathdef(item.id, n);
    }
    V::Result::output()
}

pub fn walk_type_alias<'hir, V>(v: &mut V, item: &'hir Item<'hir>, ty: &'hir Type<'hir>, name: &'hir PathDef) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_type(ty);
    v.visit_pathdef(item.id, name);
    V::Result::output()
}

pub fn walk_param<'hir, V>(v: &mut V, param: &'hir Param<'hir>) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_pathdef(param.id, param.name);
    v.visit_type(param.ty);
    V::Result::output()
}

pub fn walk_item<'hir, V>(v: &mut V, item: &'hir Item<'hir>) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    use crate::item::ItemKind;

    match &item.kind {
        ItemKind::Variable { name, ty, init, constness } => v.visit_variable_definition(item, name, *ty, *init, *constness),
        ItemKind::Use(u) => v.visit_use(item, u),
        ItemKind::Function {
            is_extern,
            is_variadic,
            name,
            params,
            body,
            ret_ty,
        } => v.visit_function_definition(*is_extern, *is_variadic, item, name, params, ret_ty, body.as_ref()),
        ItemKind::Struct { fields, name } => v.visit_struct_definition(item, name, fields),
        ItemKind::Mod(m) => v.visit_module(m),
        ItemKind::TypeAlias { ty, name } => v.visit_type_alias(item, ty, name)
    }
}

pub fn walk_variable_definition<'hir, V>(
    v: &mut V,
    base: &'hir Item<'hir>,
    name: &'hir PathDef,
    ty: Option<&'hir Type<'hir>>,
    init: Option<&'hir Expression<'hir>>,
    _constness: Constness,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_pathdef(base.id, name);
    walk_opt!(v, init, visit_expression);
    walk_opt!(v, ty, visit_type);
    V::Result::output()
}

pub fn walk_function_definition<'hir, V>(
    v: &mut V,
    base: &'hir Item<'hir>,
    name: &'hir PathDef,
    params: &'hir [Param<'hir>],
    ret_ty: &'hir Type<'hir>,
    body: Option<&'hir BlockExpr<'hir>>,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_pathdef(base.id, name);
    v.get_ctx().enter_function(base);
    for p in params {
        v.visit_param(p);
    }
    walk_opt!(v, body, visit_block);
    v.visit_type(ret_ty);
    v.get_ctx().exit_function();
    V::Result::output()
}

pub fn walk_struct_definition<'hir, V>(
    v: &mut V,
    def: &'hir Item<'hir>,
    name: &'hir PathDef,
    fields: &'hir [Field<'hir>],
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_pathdef(def.id, name);
    v.get_ctx().enter_struct(def);
    for f in fields {
        v.visit_field(def, f);
    }
    v.get_ctx().exit_struct();
    V::Result::output()
}

pub fn walk_field<'hir, V>(
    v: &mut V,
    f: &'hir Field<'hir>,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_pathdef(f.id, f.name);
    v.visit_type(f.ty);
    V::Result::output()
}

pub fn walk_ref<'hir, V>(v: &mut V, r: &'hir Expression<'hir>) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_expression(r);
    V::Result::output()
}

pub fn walk_cast<'hir, V>(
    v: &mut V,
    expr: &'hir Expression<'hir>,
    to: &'hir Type<'hir>,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_expression(expr);
    v.visit_type(to);
    V::Result::output()
}

pub fn walk_deref<'hir, V>(v: &mut V, r: &'hir Expression<'hir>) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_expression(r);
    V::Result::output()
}

pub fn walk_unary<'hir, V>(v: &mut V, expr: &'hir Expression<'hir>) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_expression(expr);
    V::Result::output()
}

pub fn walk_logical<'hir, V>(
    v: &mut V,
    left: &'hir Expression<'hir>,
    _op: &LogicalOp,
    right: &'hir Expression<'hir>,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_expression(left);
    v.visit_expression(right);
    V::Result::output()
}

pub fn walk_variable<'hir, V>(v: &mut V, path: &'hir Path) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_path(path);
    V::Result::output()
}

pub fn walk_call<'hir, V>(
    v: &mut V,
    callee: &'hir Expression<'hir>,
    args: &'hir [Expression<'hir>],
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_expression(callee);
    walk_iter!(v, args, visit_expression);
    V::Result::output()
}

pub fn walk_array_access<'hir, V>(
    v: &mut V,
    array: &'hir Expression<'hir>,
    index: &'hir Expression<'hir>,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_expression(array);
    v.visit_expression(index);
    V::Result::output()
}

pub fn walk_struct_access<'hir, V>(
    v: &mut V,
    st: &'hir Expression<'hir>,
    _field: Ident,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_expression(st);
    V::Result::output()
}

pub fn walk_for<'hir, V>(
    v: &mut V,
    init: Option<&'hir Item<'hir>>,
    cond: Option<&'hir Expression<'hir>>,
    inc: Option<&'hir Expression<'hir>>,
    body: &'hir Statement<'hir>,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    walk_opt!(v, init, visit_item);
    walk_opt!(v, cond, visit_expression);
    walk_opt!(v, inc, visit_expression);
    v.visit_statement(body);
    V::Result::output()
}

pub fn walk_while<'hir, V>(
    v: &mut V,
    cond: &'hir Expression<'hir>,
    body: &'hir Statement<'hir>,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_expression(cond);
    v.visit_statement(body);
    V::Result::output()
}

pub fn walk_if<'hir, V>(
    v: &mut V,
    cond: &'hir Expression<'hir>,
    if_true: &BlockExpr<'hir>,
    if_false: Option<&BlockExpr<'hir>>,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_expression(cond);
    v.visit_block(if_true);
    walk_opt!(v, if_false, visit_block);
    V::Result::output()
}

pub fn walk_return<'hir, V>(v: &mut V, expr: Option<&'hir Expression<'hir>>) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    walk_opt!(v, expr, visit_expression);
    V::Result::output()
}

pub fn walk_assignment<'hir, V>(
    v: &mut V,
    left: &'hir Expression<'hir>,
    right: &'hir Expression<'hir>,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_expression(left);
    v.visit_expression(right);
    V::Result::output()
}

pub fn walk_arithmetic<'hir, V>(
    v: &mut V,
    left: &'hir Expression<'hir>,
    _op: &ArithmeticOp,
    right: &'hir Expression<'hir>,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_expression(left);
    v.visit_expression(right);
    V::Result::output()
}

pub fn walk_comparison<'hir, V>(
    v: &mut V,
    left: &'hir Expression<'hir>,
    _op: &CmpOp,
    right: &'hir Expression<'hir>,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_expression(left);
    v.visit_expression(right);
    V::Result::output()
}

pub fn walk_expression<'hir, V>(v: &mut V, expr: &'hir Expression<'hir>) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    use crate::expr::ExpressionKind;
    match &expr.kind {
        ExpressionKind::Array(expressions) => {
            walk_iter!(v, expressions, visit_expression);
        }
        ExpressionKind::Cast {
            expr: casted_expr,
            to,
        } => {
            v.visit_cast(expr, casted_expr, to);
        }
        ExpressionKind::Ref(e) => {
            v.visit_ref(expr, e);
        }
        ExpressionKind::Deref(e) => {
            v.visit_deref(expr, e);
        }
        ExpressionKind::Unary { op, expr } => {
            v.visit_unary(op, expr);
        }
        ExpressionKind::Logical { left, op, right } => {
            v.visit_logical(expr, left, op, right);
        }
        ExpressionKind::Comparison { left, op, right } => {
            v.visit_comparison(expr, left, op, right);
        }
        ExpressionKind::Arithmetic { left, op, right } => {
            v.visit_arithmetic(expr, left, op, right);
        }
        ExpressionKind::Assignment { left, right } => {
            v.visit_assignment(expr, left, right);
        }
        ExpressionKind::Variable(path) => {
            v.visit_variable(expr, path);
        }
        ExpressionKind::Literal(lit_value) => {
            v.visit_literal(expr, lit_value);
        }
        ExpressionKind::Call { callee, args } => {
            v.visit_call(expr, callee, args);
        }
        ExpressionKind::ArrayAccess { arr, index } => {
            v.visit_array_access(expr, arr, index);
        }
        ExpressionKind::StructAccess { st, field } => {
            v.visit_struct_access(expr, st, *field);
        }
        ExpressionKind::Block(block) => {
            v.visit_block(block);
        }
        ExpressionKind::If {
            cond,
            if_true,
            if_false,
        } => {
            v.visit_if(expr, cond, if_true, if_false.as_ref());
        }
    }

    V::Result::output()
}

pub fn walk_statement<'hir, V>(v: &mut V, stmt: &'hir Statement<'hir>) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    use stmt::StatementKind;
    match &stmt.kind {
        StatementKind::Expr(expression) => v.visit_expression_as_stmt(stmt, expression),
        StatementKind::Item(item) => v.visit_item(item),
        StatementKind::While { cond, body } => v.visit_while(stmt, cond, body),
        StatementKind::For {
            init,
            cond,
            inc,
            body,
        } => v.visit_for(stmt, *init, *cond, *inc, body),
        StatementKind::Empty => v.visit_empty(stmt),
        StatementKind::Break => v.visit_break(stmt),
        StatementKind::Continue => v.visit_continue(stmt),
        StatementKind::Return(expression) => v.visit_return(stmt, *expression),
    };

    V::Result::output()
}

pub fn walk_block<'hir, V>(v: &mut V, block: &BlockExpr<'hir>) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    for stmt in block.stmts {
        v.visit_statement(stmt);
    }
    walk_opt!(v, block.tail, visit_expression);
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

pub trait VisitorCtx<'ast> {
    fn enter_function(&mut self, _func: &'ast Item<'ast>) {}
    fn exit_function(&mut self) {}

    fn enter_module(&mut self, _mod: &'ast Module<'ast>) {}
    fn exit_module(&mut self) {}

    fn enter_struct(&mut self, _mod: &'ast Item<'ast>) {}
    fn exit_struct(&mut self) {}
}

impl VisitorCtx<'_> for () {}

pub struct BaseVisitorCtx<'hir> {
    funcs: Vec<&'hir Item<'hir>>,
}

impl<'hir> BaseVisitorCtx<'hir> {
    pub fn current_function(&self) -> Option<&'hir Item<'hir>> { self.funcs.last().copied() }
}

impl<'hir> VisitorCtx<'hir> for BaseVisitorCtx<'hir> {
    fn enter_function(&mut self, func: &'hir Item<'hir>) { self.funcs.push(func); }

    fn exit_function(&mut self) { self.funcs.pop().unwrap(); }
}
