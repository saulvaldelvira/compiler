use llvm::analysis::VeryfierFailureAction;
use llvm::core::Type;

pub fn main() {
    let ctx = llvm::Context::new();
    let mut module = ctx.create_module("test_mod");

    let mut param_tys = [Type::int_32(&ctx), Type::int_32(&ctx)];

    let func_ty = Type::function(Type::int_32(&ctx), &mut param_tys, false);

    let mut sum_func = module.add_function("sum", func_ty);

    let mut body = sum_func.append_basic_block("entry");

    {
        let mut builder = ctx.create_builder();
        builder.position_at_end(&mut body);
        let param_1 = sum_func.param(0);
        let param_2 = sum_func.param(1);
        let tmp = builder.add(param_1, param_2, "tmp");
        builder.ret(tmp);
    }

    sum_func.verify(VeryfierFailureAction::Print).unwrap_or_else(|err| {
        eprint!("ERROR: {err}");
    });

    module.verify(VeryfierFailureAction::Print).unwrap_or_else(|err| {
        eprintln!("ERROR: {err}");
    });

    module.print("/tmp/test_mod.ll").unwrap_or_else(|err| {
        eprintln!("Error writting module ({err})");
    });
}
