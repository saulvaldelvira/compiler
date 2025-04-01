use code_generator::CodeGenerator;
use codefuncs::{Define, Metadata};
use hir::def::DefinitionKind;
use mir::MaplInstruction;
use semantic::Semantic;

mod mir;
mod size;
mod codefuncs;
mod code_generator;

pub fn gen_code_mapl(hir: &hir::Session<'_>, sem: &Semantic<'_>, source: &str, fname: &str) -> String {
    let mut cg = CodeGenerator::new(source);
    let mut ins = Vec::new();

    ins.push(MaplInstruction::Literal(format!("#SOURCE \"{fname}\"")));

    let prog = hir.get_root_program();
    prog.defs.iter()
        .filter(|def| !matches!(def.kind, DefinitionKind::Function { .. }))
        .for_each(|def| {
            let m = def.metadata(&mut cg, sem);
            ins.push(m);
            let def = def.define(&mut cg, sem);
            ins.push(def);
        });

    ins.push(MaplInstruction::Call("main".to_string()));
    ins.push(MaplInstruction::Halt);

    prog.defs.iter()
        .filter(|def| matches!(def.kind, DefinitionKind::Function { .. }))
        .for_each(|vdef| {
            let def = vdef.define(&mut cg, sem);
            ins.push(def);
        });

    MaplInstruction::Compose(ins.into_boxed_slice()).to_string()
}
