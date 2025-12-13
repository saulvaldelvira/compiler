use core::fmt;

use hir::{Ident, Item, ItemKind};
use hir::Statement;
use span::FilePosition;

use super::Metadata;
use crate::{code_generator::CodeGenerator, mir::MaplInstruction};

impl Metadata for Statement<'_> {
    fn metadata(&self, cg: &mut CodeGenerator) -> MaplInstruction {
        let FilePosition { start_line, .. } = cg.source.file_position(&self.span).expect("Expected a valid span");
        MaplInstruction::Literal(format!("#line {start_line}"))
    }
}

fn mapl_ty_metadata(ty: &semantic::Ty<'_>, out: &mut dyn fmt::Write) -> fmt::Result {
    use semantic::{PrimitiveType, TypeKind};
    match ty.kind {
        TypeKind::Primitive(prim) => {
            write!(out, "{}", match prim {
                PrimitiveType::I16 |
                PrimitiveType::Bool => "int",
                PrimitiveType::Char => "char",
                PrimitiveType::F32 => "float",
                PrimitiveType::I8 |
                PrimitiveType::I32 |
                PrimitiveType::I64 |
                PrimitiveType::U8 |
                PrimitiveType::U16 |
                PrimitiveType::U32 |
                PrimitiveType::U64 |
                PrimitiveType::F64 => panic!("Unsupported numeric type"),
            })
        }
        TypeKind::Ref(_) => todo!(),
        TypeKind::Array(ty, len) => {
            mapl_ty_metadata(ty, out)?;
            write!(out, " * {len}")
        }
        TypeKind::Tuple(_) => todo!(),
        TypeKind::Struct { name, .. } => write!(out, "{name}"),
        TypeKind::Function { .. } => todo!(),
    }
}

fn def_var(global: &str, name: &Ident, ty: &semantic::Ty<'_>) -> MaplInstruction {
    let mut s = String::new();
    mapl_ty_metadata(ty, &mut s).unwrap();
    name.sym.borrow(|name| {
        MaplInstruction::Literal(format!("{global} {name} : {s}"))
    })
}

impl Metadata for Item<'_> {
    fn metadata(&self, cg: &mut CodeGenerator) -> MaplInstruction {
        match self.kind {
            ItemKind::Variable { name, .. } => {
                let ty = cg.sem.type_of(&self.id).unwrap();
                def_var("#global", &name.ident, ty)
            }
            /* ItemKind::Struct { fields, name } => { */
            /*     let mut v = Vec::new(); */
            /*     v.push(MaplInstruction::Literal(format!( */
            /*         "#type {} : {{", */
            /*         name.ident.sym */
            /*     ))); */
            /*     for field in fields { */
            /*         let ty = cg.sem.type_of(&field.id).unwrap(); */
            /*         v.push(def_var("", &field.name.ident, ty)); */
            /*     } */
            /*     v.push(MaplInstruction::Literal("}".to_string())); */
            /*     MaplInstruction::Compose(v.into_boxed_slice()) */
            /* } */
            ItemKind::Struct { .. } |
            ItemKind::TypeAlias { .. } |
            ItemKind::Use(_) |
            ItemKind::Mod(_) |
            ItemKind::Function { .. } => MaplInstruction::Empty,
        }
    }
}
