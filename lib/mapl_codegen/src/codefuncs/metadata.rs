use core::fmt;

use hir::def::DefinitionKind;
use hir::{Definition, ModItem, Statement};
use span::FilePosition;

use crate::code_generator::CodeGenerator;
use crate::mir::MaplInstruction;

use super::Metadata;

impl Metadata for Statement<'_> {
    fn metadata(&self, cg: &mut CodeGenerator) -> MaplInstruction {
        let FilePosition { start_line, .. } = self.span.file_position(cg.source());
        MaplInstruction::Literal(format!("#line {start_line}"))
    }
}

fn mapl_ty_metadata(ty: &semantic::Ty<'_>, out: &mut dyn fmt::Write) -> fmt::Result {
    use semantic::{TypeKind,PrimitiveType};
    match ty.kind {
        TypeKind::Primitive(prim) => {
            write!(out, "{}",
                match prim {
                    PrimitiveType::Int => "int",
                    PrimitiveType::Char => "char",
                    PrimitiveType::Float => "float",
                    PrimitiveType::Bool => "int",
                    PrimitiveType::Empty => unreachable!(),
                })
        }
        TypeKind::Ref(_) => todo!(),
        TypeKind::Array(ty, len) => {
            mapl_ty_metadata(ty, out)?;
            write!(out, " * {len}")
        }
        TypeKind::Struct { name, .. } => write!(out, "{name}"),
        TypeKind::Function { .. } => todo!(),
    }
}

fn def_var(global: &str, name: String, ty: &semantic::Ty<'_>) -> MaplInstruction {
    let mut s = String::new();
    mapl_ty_metadata(ty, &mut s).unwrap();
    MaplInstruction::Literal(format!("{global} {name} : {s}"))
}

impl Metadata for Definition<'_> {
    fn metadata(&self, cg: &mut CodeGenerator) -> MaplInstruction {

        match self.kind {
            DefinitionKind::Variable { .. } => {
                let ty = cg.sem.type_of(&self.id).unwrap();
                def_var("#global", self.name.ident.sym.to_string(), ty)
            }
            DefinitionKind::Struct { fields } => {
                let mut v = Vec::new();
                v.push(MaplInstruction::Literal(format!("#type {} : {{", self.name.ident.sym)));
                for field in fields {
                    let ty = cg.sem.type_of(&field.id).unwrap();
                    v.push(
                        def_var("", field.name.ident.sym.to_string(), ty)
                    );
                }
                v.push(MaplInstruction::Literal("}".to_string()));
                MaplInstruction::Compose(v.into_boxed_slice())
            }
            DefinitionKind::Function { .. } => { MaplInstruction::Empty },
        }

    }
}

impl Metadata for ModItem<'_> {
    fn metadata(&self, cg: &mut CodeGenerator) -> MaplInstruction {
        match self.kind {
            hir::ModItemKind::Mod(_) => MaplInstruction::Empty,
            hir::ModItemKind::Def(definition) => definition.metadata(cg),
        }
    }
}

