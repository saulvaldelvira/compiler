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
            PrimitiveType::Int |
            PrimitiveType::Bool => 2,
            PrimitiveType::Char => 1,
            PrimitiveType::Float => 4,
            PrimitiveType::Empty => 0,
        }
    }
}

impl SizeOf for Ty<'_> {
    fn size_of(&self) -> u64 {
        match self.kind {
            TypeKind::Primitive(p) => p.size_of(),
            TypeKind::Ref(_) => PrimitiveType::Int.size_of(),
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
        StatementKind::Block(statements) => {
            statements
                .iter()
                .fold(acc, |a, stmt| assign_memory_locals(cg, a, stmt))
        }
        StatementKind::If {
            if_true, if_false, ..
        } => {
            acc = assign_memory_locals(cg, acc, if_true);
            if let Some(if_false) = if_false {
                acc = assign_memory_locals(cg, acc, if_false);
            }
            acc
        }
        StatementKind::While { body, .. } => assign_memory_locals(cg, acc, body),
        StatementKind::For { body, init, .. } => {
            if let Some(init) = init {
                let def = Statement::new(StatementKind::Item(init), Span::new());
                acc = assign_memory_locals(cg, acc, &def);
            }
            assign_memory_locals(cg, acc, body)
        }
        _ => acc,
    }
}
