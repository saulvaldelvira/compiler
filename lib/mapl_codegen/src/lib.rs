use code_generator::CodeGenerator;
use codefuncs::{Define, Metadata};
use hir::{ModItemKind, def::DefinitionKind};
use mir::MaplInstruction;
use semantic::Semantic;

mod code_generator;
mod codefuncs;
mod mir;
mod size;

pub fn gen_code_mapl<'sem, 'hir, 'src>(
    hir: &'sem hir::Session<'hir>,
    sem: &'sem Semantic<'sem>,
    source: &'src str,
    fname: &str,
) -> String
where
    'hir: 'sem,
    'src: 'hir,
{
    let mut cg = CodeGenerator::new(source, sem, hir);
    let mut ins = Vec::new();

    let fname = std::path::absolute(fname)
        .map(|pb| pb.to_str().unwrap().to_owned())
        .unwrap_or_else(|_| fname.to_owned());
    ins.push(MaplInstruction::Literal(format!("#SOURCE \"{fname}\"")));

    let prog = hir.get_root();

    prog.items
        .iter()
        .filter(|def| {
            match def.kind {
                ModItemKind::Mod(_) => false,
                ModItemKind::Def(definition) => {
                    !matches!(definition.kind, DefinitionKind::Function { .. })
                }
            }
        })
        .for_each(|def| {
            let m = def.metadata(&mut cg);
            ins.push(m);
            let def = def.define(&mut cg);
            ins.push(def);
        });

    ins.push(MaplInstruction::Call("main".to_string()));
    ins.push(MaplInstruction::Halt);

    prog.items
        .iter()
        .filter(|def| {
            match def.kind {
                ModItemKind::Mod(_) => true,
                ModItemKind::Def(definition) => {
                    matches!(definition.kind, DefinitionKind::Function { .. })
                }
            }
        })
        .for_each(|vdef| {
            let def = vdef.define(&mut cg);
            ins.push(def);
        });

    MaplInstruction::Compose(ins.into_boxed_slice()).to_string()
}
