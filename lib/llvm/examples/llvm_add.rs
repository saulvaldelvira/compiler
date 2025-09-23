use llvm::analysis::VeryfierFailureAction;
use llvm::core::{Builder, Module, Type};

pub fn main() {
    let mut module = Module::new("test_mod");

    let mut param_tys = [Type::int_32(), Type::int_32()];

    let func_ty = Type::function(Type::int_32(), &mut param_tys, false);

    let mut sum_func = module.add_function("sum", func_ty);

    let mut body = sum_func.append_basic_block("entry");

    {
        let mut builder = Builder::new();
        builder.position_at_end(&mut body);
        let tmp = builder.add(sum_func.param(0), sum_func.param(1), "tmp");
        builder.ret(tmp);
    }

    module.verify(VeryfierFailureAction::Print).unwrap_or_else(|err| {
        eprintln!("ERROR: {err}");
    });

    module.print("/tmp/test_mod.ll").unwrap_or_else(|err| {
        eprintln!("Error writting module ({err})");
    });
}
