use core::fmt;

use hir::def::DefinitionKind;
use hir::{Definition, Statement};
use semantic::Semantic;
use span::FilePosition;

use crate::code_generator::CodeGenerator;
use crate::mir::MaplInstruction;

use super::Metadata;

impl Metadata for Statement<'_> {
    fn metadata(&self, cg: &mut CodeGenerator<'_>, _sem: &Semantic<'_>) -> MaplInstruction {
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
    fn metadata(&self, _cg: &mut CodeGenerator<'_>, sem: &Semantic<'_>) -> MaplInstruction {

        match self.kind {
            DefinitionKind::Variable { .. } => {
                let ty = sem.type_of(&self.id).unwrap();
                def_var("#global", self.name.ident.sym.to_string(), ty)
            }
            DefinitionKind::Struct { fields } => {
                let mut v = Vec::new();
                v.push(MaplInstruction::Literal(format!("#type {} : {{", self.name.ident.sym)));
                for field in fields {
                    let ty = sem.type_of(&field.id).unwrap();
                    v.push(
                        def_var("", field.name.ident.sym.to_string(), ty)
                    );
                }
                v.push(MaplInstruction::Literal("}".to_string()));
                MaplInstruction::Compose(v.into_boxed_slice())
            }
            DefinitionKind::Module(_) |
            DefinitionKind::Function { .. } => { MaplInstruction::Empty },
        }

    }
}
