use hir::ItemKind;
use hir::{Statement, stmt::StatementKind};
use semantic::{PrimitiveType, Ty, TypeKind};
use span::Span;

use crate::code_generator::{CodeGenerator, MemoryAddress};

pub trait SizeOf {
    fn size_of(&self) -> u64;
}

impl SizeOf for PrimitiveType {
    fn size_of(&self) -> u64 {
        match self {
            PrimitiveType::I16 |
            PrimitiveType::Bool => 2,
            PrimitiveType::Char => 1,
            PrimitiveType::F32 => 4,
            PrimitiveType::I8 |
            PrimitiveType::I32 |
            PrimitiveType::I64 |
            PrimitiveType::U8 |
            PrimitiveType::U16 |
            PrimitiveType::U32 |
            PrimitiveType::U64 |
            PrimitiveType::F64 => panic!("Unsupported numeric type"),
        }
    }
}

impl SizeOf for Ty<'_> {
    fn size_of(&self) -> u64 {
        match self.kind {
            TypeKind::Primitive(p) => p.size_of(),
            TypeKind::Tuple(tys) => tys.iter().map(|t| t.size_of()).sum(),
            TypeKind::Ref(_) => PrimitiveType::I16.size_of(),
            TypeKind::Array(ty, len) => ty.size_of() * u64::from(len),
            TypeKind::Struct { fields, .. } => fields.iter().map(|f| f.ty.size_of()).sum(),
            TypeKind::Function { .. } => todo!(),
        }
    }
}

pub fn assign_memory_locals(
    cg: &mut CodeGenerator,
    mut acc: i32,
    stmt: &hir::Statement<'_>,
) -> i32 {
    match &stmt.kind {
        StatementKind::Item(d) if matches!(d.kind, ItemKind::Variable { .. }) => {
            let acc = cg.sem.type_of(&d.id).unwrap().size_of() as i32 + acc;
            cg.set_address(d.id, MemoryAddress::Relative(-acc));
            acc
        }
        StatementKind::While { body, .. } => assign_memory_locals(cg, acc, body),
        StatementKind::For { body, init, .. } => {
            if let Some(init) = init {
                let def = Statement::new(StatementKind::Item(init), Span::dummy());
                acc = assign_memory_locals(cg, acc, &def);
            }
            assign_memory_locals(cg, acc, body)
        }
        StatementKind::Expr(expr) => assign_memory_locals_expr(cg, acc, expr),
        _ => acc,
    }
}

fn assign_memory_locals_block(
    cg: &mut CodeGenerator,
    mut acc: i32,
    block: &hir::BlockExpr<'_>,
) -> i32 {
    for stmt in block.stmts {
        acc = assign_memory_locals(cg, acc, stmt);
    }
    if let Some(tail) = block.tail {
        acc = assign_memory_locals_expr(cg, acc, tail);
    }
    acc
}

pub fn assign_memory_locals_expr(
    cg: &mut CodeGenerator,
    mut acc: i32,
    expr: &hir::Expression<'_>,
) -> i32 {
    use hir::expr::ExpressionKind;

    match &expr.kind {
        ExpressionKind::Array(expressions) => {
            for e in *expressions {
                acc = assign_memory_locals_expr(cg, acc, e);
            }
            acc
        }
        ExpressionKind::Unary { expr, .. }
        | ExpressionKind::Cast { expr, .. } => assign_memory_locals_expr(cg, acc, expr),
        ExpressionKind::Block(block) => assign_memory_locals_block(cg, acc, block),
        ExpressionKind::If { cond, if_true, if_false } => {
            acc = assign_memory_locals_expr(cg, acc, cond);
            acc = assign_memory_locals_block(cg, acc, if_true);
            if let Some(if_false) = if_false {
                acc = assign_memory_locals_expr(cg, acc, if_false);
            }
            acc
        }
        ExpressionKind::Ref(expression) |
        ExpressionKind::Deref(expression) => assign_memory_locals_expr(cg, acc, expression),
        ExpressionKind::Logical { left, right, .. } |
        ExpressionKind::Comparison { left, right, .. } |
        ExpressionKind::Arithmetic { left, right,  .. } |
        ExpressionKind::Assignment { left, right,  .. }  => {
            acc = assign_memory_locals_expr(cg, acc, left);
            assign_memory_locals_expr(cg, acc, right)
        }
        ExpressionKind::Call { callee, args } => {
            acc = assign_memory_locals_expr(cg, acc, callee);
            for arg in *args {
                acc = assign_memory_locals_expr(cg, acc, arg);
            }
            acc
        }
        ExpressionKind::ArrayAccess { arr, index } => {
            acc = assign_memory_locals_expr(cg, acc, arr);
            assign_memory_locals_expr(cg, acc, index)
        }
        ExpressionKind::StructAccess { st, .. } => assign_memory_locals_expr(cg, acc, st),
        _ => acc
    }
}
