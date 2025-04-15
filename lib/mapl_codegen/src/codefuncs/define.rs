use hir::def::DefinitionKind;
use hir::{Definition, ModItem, Module, Statement};

use crate::code_generator::{CodeGenerator, FunctionCtx, MemoryAddress};
use crate::mir::{MaplInstruction, MaplType};
use crate::size::assign_memory_locals;
use crate::size::SizeOf;

use super::{Address, Define, Eval, Execute};

impl Define for Definition<'_> {
    fn define(&self, cg: &mut CodeGenerator) -> MaplInstruction {
        match self.kind {
            DefinitionKind::Variable { init, .. } => {
                let ty = cg.sem.type_of(&self.id).unwrap();
                if cg.is_global() {
                    let addr = cg.next_global_offset(ty.size_of());
                    cg.set_address(self.id, addr);
                }
                if let Some(init) = init {
                    let ty = MaplType::from(ty);
                    let ins = [
                        self.address(cg),
                        init.eval(cg),
                        MaplInstruction::Store(ty)
                    ];
                    return MaplInstruction::Compose(Box::new(ins))
                }
            }
            DefinitionKind::Function { params, body, .. } => {
                return define_func(self, params, body, cg)
            },
            DefinitionKind::Struct { fields } => {
                let mut acc = 0;
                for f in fields {
                    cg.set_address(f.id, MemoryAddress::Relative(acc));
                    let ty = cg.sem.type_of(&f.id).unwrap();
                    acc += ty.size_of() as i32;
                }
            },
        }
        MaplInstruction::Empty
    }
}

impl Define for ModItem<'_> {
    fn define(&self, cg: &mut CodeGenerator) -> MaplInstruction {
        match self.kind {
            hir::ModItemKind::Mod(module) => module.define(cg),
            hir::ModItemKind::Def(definition) => definition.define(cg),
        }
    }
}

impl Define for Module<'_> {
    fn define(&self, cg: &mut CodeGenerator) -> MaplInstruction {
        cg.enter_module(self.name.to_string());
        let defs = self.items.iter().map(|d| d.define(cg)).collect();
        cg.exit_module();
        MaplInstruction::Compose(defs)
    }
}

fn define_func<'hir>(
    def: &Definition<'hir>,
    params: &[Definition<'hir>],
    body: &[Statement<'hir>],
    cg: &mut CodeGenerator,
) -> MaplInstruction
{
    let mut params_size: i32 = 4;
    for param in params.iter().rev() {
        cg.set_address(param.id, MemoryAddress::Relative(params_size));
        let ty = cg.sem.type_of(&param.id).unwrap();
        params_size += ty.size_of() as i32;
    }

    let ty = cg.sem.type_of(&def.id).unwrap();
    let (_, ret) = ty.as_function_type().unwrap();
    let ret_size = ret.size_of();

    let locals = body.iter().fold(0, |acc, stmt| assign_memory_locals(cg, acc, stmt));
    let mut vec = Vec::new();

    cg.mangle_symbol(def.id, &def.name.ident.sym.to_string());
    let name = cg.get_mangled_symbol(&def.id).unwrap();
    vec.push(MaplInstruction::DefineLabel(name));
    vec.push(MaplInstruction::Enter(locals as usize));

    cg.enter_function(FunctionCtx{
        ret_size: ret_size as u16,
        locals_size: (locals) as u16,
        params_size: (params_size - 4) as u16,
    });

    body.iter()
        .map(|stmt| stmt.execute(cg))
        .for_each(|stmt| vec.push(stmt));

    cg.exit_function();

    if ret.is_empty_type() {
        vec.push(MaplInstruction::Return {
            locals: locals as u16,
            params: (params_size - 4) as u16,
            ret_size: 0
        });
    }

    MaplInstruction::Compose(vec.into_boxed_slice())
}
