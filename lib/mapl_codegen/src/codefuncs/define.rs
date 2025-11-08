use hir::{Item, ItemKind, Module, Param, Statement};

use super::{Address, Define, Eval, Execute};
use crate::codefuncs::Metadata;
use crate::{
    code_generator::{CodeGenerator, FunctionCtx, MemoryAddress},
    mir::{MaplInstruction, MaplType},
    size::{SizeOf, assign_memory_locals},
};

impl Define for Item<'_> {
    fn define(&self, cg: &mut CodeGenerator) -> MaplInstruction {
        match self.kind {
            ItemKind::Variable{ init, .. } => {
                let ty = cg.sem.type_of(&self.id).unwrap();
                if cg.is_global() {
                    let addr = cg.next_global_offset(ty.size_of());
                    cg.set_address(self.id, addr);
                }
                if let Some(init) = init {
                    let ty = MaplType::from(ty);
                    let ins = [self.address(cg), init.eval(cg), MaplInstruction::Store(ty)];
                    return MaplInstruction::Compose(Box::new(ins));
                }
            }
            ItemKind::Function { params, body, .. } => {
                return define_func(self, params, body.unwrap(), cg);
            }
            ItemKind::Struct { fields, .. } => {
                let mut acc = 0;
                for f in fields {
                    cg.set_address(f.id, MemoryAddress::Relative(acc));
                    let ty = cg.sem.type_of(&f.id).unwrap();
                    acc += ty.size_of() as i32;
                }
            },
            ItemKind::Mod(m) => return m.define(cg),
            ItemKind::Use(_) => {}
        }
        MaplInstruction::Empty
    }
}

impl Define for Module<'_> {
    fn define(&self, cg: &mut CodeGenerator) -> MaplInstruction {
        cg.enter_module(self.name.ident.sym.to_string());
        let mut defs = vec![];

        self.items
            .iter()
            .filter(|def| {
                matches!(def.kind, ItemKind::Variable { .. })
            })
        .for_each(|def| {
            let m = def.metadata(cg);
            defs.push(m);
            let def = def.define(cg);
            defs.push(def);
        });

        self.items
            .iter()
            .filter(|def| {
                matches!(def.kind, ItemKind::Mod { .. })
            })
        .for_each(|def| {
            let def = def.define(cg);
            defs.push(def);
        });

        self.items
            .iter()
            .filter(|def| {
                !matches!(def.kind, ItemKind::Variable { .. } | ItemKind::Mod(_))
            })
        .for_each(|def| {
            let m = def.metadata(cg);
            defs.push(m);
            let def = def.define(cg);
            defs.push(def);
        });

        cg.exit_module();
        MaplInstruction::Compose(defs.into())
    }
}

fn define_func<'hir>(
    def: &Item<'hir>,
    params: &[Param<'hir>],
    body: &[Statement<'hir>],
    cg: &mut CodeGenerator,
) -> MaplInstruction {
    let mut params_size: i32 = 4;
    for param in params.iter().rev() {
        cg.set_address(param.id, MemoryAddress::Relative(params_size));
        let ty = cg.sem.type_of(&param.id).unwrap();
        params_size += ty.size_of() as i32;
    }

    let ty = cg.sem.type_of(&def.id).unwrap();
    let (_, ret) = ty.as_function_type().unwrap();
    let ret_size = ret.size_of();

    let locals = body
        .iter()
        .fold(0, |acc, stmt| assign_memory_locals(cg, acc, stmt));
    let mut vec = Vec::new();

    cg.mangle_symbol(def.id, &def.get_name().to_string());
    let name = cg.get_mangled_symbol(def.id).unwrap();
    vec.push(MaplInstruction::DefineLabel(name));
    vec.push(MaplInstruction::Enter(locals as usize));

    cg.enter_function(FunctionCtx {
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
            ret_size: 0,
        });
    }

    MaplInstruction::Compose(vec.into_boxed_slice())
}
