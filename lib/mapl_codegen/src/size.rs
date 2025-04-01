use hir::def::DefinitionKind;
use hir::stmt::StatementKind;
use hir::Statement;
use semantic::{PrimitiveType, Semantic, Ty, TypeKind};
use span::Span;

use crate::code_generator::{CodeGenerator, MemoryAddress};

pub trait SizeOf {
    fn size_of(&self) -> usize;
}

impl SizeOf for PrimitiveType {
    fn size_of(&self) -> usize {
        match self {
            PrimitiveType::Int => 2,
            PrimitiveType::Char => 1,
            PrimitiveType::Float => 4,
            PrimitiveType::Bool => 1,
            PrimitiveType::Empty => 0,
        }
    }
}

impl SizeOf for Ty<'_> {
    fn size_of(&self) -> usize {
        match self.kind {
            TypeKind::Primitive(p) => p.size_of(),
            TypeKind::Ref(_) => PrimitiveType::Int.size_of(),
            TypeKind::Array(ty, len) => ty.size_of() * len,
            TypeKind::Struct { fields, .. } => fields.iter().map(|f| f.ty.size_of()).sum(),
            TypeKind::Function { .. } => todo!(),
        }
    }
}

pub fn assign_memory_locals(cg: &mut CodeGenerator, mut acc: i32, stmt: &hir::Statement<'_>, sem: &Semantic<'_>) -> i32 {
    match &stmt.kind {
        StatementKind::Def(d) if matches!(d.kind, DefinitionKind::Variable { .. }) => {
            let acc = sem.type_of(&d.id).unwrap().size_of() as i32 + acc;
            cg.set_address(d.id, MemoryAddress::Relative(-acc));
            acc
        },
        StatementKind::Block(statements) => {
            statements.iter()
                .fold(acc, |a, stmt| {
                    assign_memory_locals(cg, a, stmt, sem)
                })
        },
        StatementKind::If { if_true, if_false, .. } => {
            acc = assign_memory_locals(cg, acc, if_true, sem);
            if let Some(if_false) = if_false {
                acc = assign_memory_locals(cg, acc, if_false, sem);
            }
            acc
        }
        StatementKind::While { body, .. } => assign_memory_locals(cg, acc, body, sem),
        StatementKind::For { body, init, .. } => {
            if let Some(init) = init {
                let def = Statement::new(StatementKind::Def(init), Span::new());
                acc = assign_memory_locals(cg, acc, &def, sem);
            }
            assign_memory_locals(cg, acc, body, sem)
        },
        _ => acc,
    }
}
