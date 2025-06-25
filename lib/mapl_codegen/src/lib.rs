#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]

use code_generator::CodeGenerator;
use codefuncs::Define;
use mir::MaplInstruction;
use semantic::Semantic;
use span::Source;

mod code_generator;
mod codefuncs;
mod mir;
mod size;

pub fn gen_code_mapl<'sem, 'hir, 'src>(
    hir: &hir::Session<'hir>,
    sem: &Semantic<'sem>,
    source: &'src Source,
) -> String
where
    'hir: 'sem,
    'src: 'hir,
{
    let mut cg = CodeGenerator::new(source, sem, hir);
    let mut ins = Vec::new();

    let prog = hir.get_root();
    let file = source.get(prog.span.fileid).unwrap();

    let fname = file.filename().map(|fname| {
        std::path::absolute(fname)
            .map_or_else(
                |_| fname.to_owned(),
                |pb| pb.to_str().unwrap().to_owned()
            )
    }).unwrap_or_default();

    ins.push(MaplInstruction::Literal(format!("#SOURCE \"{fname}\"")));


    ins.push(MaplInstruction::Call("self_main".to_string()));
    ins.push(MaplInstruction::Halt);

    ins.push(prog.define(&mut cg));

    MaplInstruction::Compose(ins.into_boxed_slice()).to_string()
}
