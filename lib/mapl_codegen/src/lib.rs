#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]

use code_generator::CodeGenerator;
use codefuncs::Define;
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
        .map_or_else(
            |_| fname.to_owned(),
            |pb| pb.to_str().unwrap().to_owned());
    ins.push(MaplInstruction::Literal(format!("#SOURCE \"{fname}\"")));

    let prog = hir.get_root();

    ins.push(MaplInstruction::Call("ROOT_main".to_string()));
    ins.push(MaplInstruction::Halt);

    ins.push(prog.define(&mut cg));

    MaplInstruction::Compose(ins.into_boxed_slice()).to_string()
}
