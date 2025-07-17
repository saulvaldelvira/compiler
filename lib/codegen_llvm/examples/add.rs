use codegen_llvm::{Codegen, CodegenCtx};
use interner::Symbol;
use llvm::analysis::VeryfierFailureAction;
use llvm::core::Module;

pub fn main() {
    let compiler = compiler_driver::Compiler::from_string("fn add(a: int, b: int) -> int {
        return a + b;
    }");

    let (hir_sess, _) = compiler.compile().unwrap();

    let main = hir_sess.get_root().find_item(Symbol::new("add")).unwrap();

    let mut module = Module::new("test_mod");
    let mut ctx = CodegenCtx::new(&hir_sess, &mut module);

    main.codegen(&mut ctx);

    module.verify(VeryfierFailureAction::Print).unwrap_or_else(|err| {
        eprintln!("ERROR: {err}");
    });

    module.print("/tmp/test_mod.ll").unwrap_or_else(|err| {
        eprintln!("Error writting module ({err})");
    });
}
