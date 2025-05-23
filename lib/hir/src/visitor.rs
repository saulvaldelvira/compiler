use std::ops::ControlFlow;

use crate::{
    Constness, Definition, Expression, HirId, Ident, ModItem, ModItemKind, Module, Path, PathDef,
    Statement, Type,
    def::Field,
    expr::{ArithmeticOp, CmpOp, LitValue, LogicalOp, UnaryOp},
    hir, stmt,
};

pub trait Visitor<'hir> {
    type Result: VisitorResult;
    type Ctx: VisitorCtx<'hir>;

    fn get_ctx(&mut self) -> &mut Self::Ctx;

    fn visit_module(&mut self, m: &'hir Module<'hir>) -> Self::Result { walk_module(self, m) }

    fn visit_module_item(&mut self, item: &'hir ModItem<'hir>) -> Self::Result {
        walk_module_item(self, item)
    }

    fn visit_definition(&mut self, def: &'hir Definition<'hir>) -> Self::Result {
        walk_definition(self, def)
    }

    fn visit_variable_definition(
        &mut self,
        def: &'hir Definition<'hir>,
        constness: &Constness,
        ty: Option<&'hir Type<'hir>>,
        init: Option<&'hir Expression<'hir>>,
    ) -> Self::Result {
        walk_variable_definition(self, def, constness, ty, init)
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

    fn visit_ternary(
        &mut self,
        _base: &'hir Expression<'hir>,
        cond: &'hir Expression<'hir>,
        if_true: &'hir Expression<'hir>,
        if_false: &'hir Expression<'hir>,
    ) -> Self::Result {
        walk_ternary(self, cond, if_true, if_false)
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
        base: &'hir Definition<'hir>,
        params: &'hir [Definition<'hir>],
        ret_ty: &'hir Type<'hir>,
        body: &'hir [Statement<'hir>],
    ) -> Self::Result {
        walk_function_definition(self, base, params, ret_ty, body)
    }

    fn visit_struct_definition(
        &mut self,
        base: &'hir Definition<'hir>,
        fields: &'hir [Field<'hir>],
    ) -> Self::Result {
        walk_struct_definition(self, base, fields)
    }

    fn visit_field(
        &mut self,
        def: &'hir Definition<'hir>,
        param: &'hir Field<'hir>,
    ) -> Self::Result {
        walk_field(self, def, param)
    }

    fn visit_statement(&mut self, stmt: &'hir Statement<'hir>) -> Self::Result {
        walk_statement(self, stmt)
    }

    fn visit_block(
        &mut self,
        _base: &'hir Statement<'hir>,
        block: &'hir [Statement<'hir>],
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

    fn visit_print(
        &mut self,
        _base: &'hir Statement<'hir>,
        expr: &'hir Expression<'hir>,
    ) -> Self::Result {
        walk_print(self, expr)
    }

    fn visit_read(
        &mut self,
        _base: &'hir Statement<'hir>,
        expr: &'hir Expression<'hir>,
    ) -> Self::Result {
        walk_read(self, expr)
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
        init: Option<&'hir Definition<'hir>>,
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
        _base: &'hir Statement,
        cond: &'hir Expression<'hir>,
        if_true: &'hir Statement<'hir>,
        if_false: Option<&'hir Statement<'hir>>,
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
    for item in prog.items {
        v.visit_module_item(item);
    }
    V::Result::output()
}

pub fn walk_module_item<'hir, V>(v: &mut V, item: &'hir ModItem<'hir>) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    match item.kind {
        ModItemKind::Mod(module) => v.visit_module(module),
        ModItemKind::Def(definition) => v.visit_definition(definition),
    }
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
        TypeKind::Ref(r) => {
            v.visit_type(r);
        }
        TypeKind::Array(ty, _) => {
            v.visit_type(ty);
        }
        TypeKind::Function { params, ret_ty } => {
            walk_iter!(v, params, visit_type);
            v.visit_type(ret_ty);
        }
        TypeKind::Primitive(_) => {}
    }
    V::Result::output()
}

pub fn walk_definition<'hir, V>(v: &mut V, def: &'hir Definition<'hir>) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    use crate::def::DefinitionKind;

    v.visit_pathdef(def.id, def.name);

    match &def.kind {
        DefinitionKind::Variable {
            constness,
            ty,
            init,
        } => v.visit_variable_definition(def, constness, ty.as_deref(), *init),
        DefinitionKind::Function {
            params,
            body,
            ret_ty,
        } => v.visit_function_definition(def, params, ret_ty, body),
        DefinitionKind::Struct { fields } => v.visit_struct_definition(def, fields),
    }
}

pub fn walk_variable_definition<'hir, V>(
    v: &mut V,
    _def: &'hir Definition<'hir>,
    _constness: &Constness,
    ty: Option<&'hir Type<'hir>>,
    init: Option<&'hir Expression<'hir>>,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    walk_opt!(v, init, visit_expression);
    walk_opt!(v, ty, visit_type);
    V::Result::output()
}

pub fn walk_function_definition<'hir, V>(
    v: &mut V,
    base: &'hir Definition<'hir>,
    params: &'hir [Definition<'hir>],
    ret_ty: &'hir Type<'hir>,
    body: &'hir [Statement<'hir>],
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.get_ctx().enter_function(base);
    for p in params {
        v.visit_definition(p);
    }
    for stmt in body {
        v.visit_statement(stmt);
    }
    v.visit_type(ret_ty);
    v.get_ctx().exit_function();
    V::Result::output()
}

pub fn walk_struct_definition<'hir, V>(
    v: &mut V,
    def: &'hir Definition<'hir>,
    fields: &'hir [Field<'hir>],
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    for f in fields {
        v.visit_field(def, f);
    }
    V::Result::output()
}

pub fn walk_field<'hir, V>(
    v: &mut V,
    def: &'hir Definition<'hir>,
    f: &'hir Field<'hir>,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_pathdef(def.id, f.name);
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

pub fn walk_print<'hir, V>(v: &mut V, pr: &'hir Expression<'hir>) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_expression(pr);
    V::Result::output()
}

pub fn walk_read<'hir, V>(v: &mut V, re: &'hir Expression<'hir>) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_expression(re);
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
    init: Option<&'hir Definition<'hir>>,
    cond: Option<&'hir Expression<'hir>>,
    inc: Option<&'hir Expression<'hir>>,
    body: &'hir Statement<'hir>,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    walk_opt!(v, init, visit_definition);
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
    if_true: &'hir Statement<'hir>,
    if_false: Option<&'hir Statement<'hir>>,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_expression(cond);
    v.visit_statement(if_true);
    walk_opt!(v, if_false, visit_statement);
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

pub fn walk_ternary<'hir, V>(
    v: &mut V,
    cond: &'hir Expression<'hir>,
    if_true: &'hir Expression<'hir>,
    if_false: &'hir Expression<'hir>,
) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    v.visit_expression(cond);
    v.visit_expression(if_true);
    v.visit_expression(if_false);
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
        ExpressionKind::Ternary {
            cond,
            if_true,
            if_false,
        } => {
            v.visit_ternary(expr, cond, if_true, if_false);
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
        StatementKind::Def(definition) => v.visit_definition(definition),
        StatementKind::Block(statements) => v.visit_block(stmt, statements),
        StatementKind::If {
            cond,
            if_true,
            if_false,
        } => v.visit_if(stmt, cond, if_true, *if_false),
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
        StatementKind::Print(expression) => v.visit_print(stmt, expression),
        StatementKind::Read(expression) => v.visit_read(stmt, expression),
    };

    V::Result::output()
}

pub fn walk_block<'hir, V>(v: &mut V, block: &'hir [Statement<'hir>]) -> V::Result
where
    V: Visitor<'hir> + ?Sized,
{
    for stmt in block {
        v.visit_statement(stmt);
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

pub trait VisitorCtx<'ast> {
    fn enter_function(&mut self, _func: &'ast hir::Definition<'ast>) {}
    fn exit_function(&mut self) {}
}

impl VisitorCtx<'_> for () {}

pub struct BaseVisitorCtx<'hir> {
    funcs: Vec<&'hir Definition<'hir>>,
}

impl<'hir> BaseVisitorCtx<'hir> {
    pub fn current_function(&self) -> Option<&'hir Definition<'hir>> { self.funcs.last().copied() }
}

impl<'hir> VisitorCtx<'hir> for BaseVisitorCtx<'hir> {
    fn enter_function(&mut self, func: &'hir hir::Definition<'hir>) { self.funcs.push(func); }

    fn exit_function(&mut self) { self.funcs.pop().unwrap(); }
}
