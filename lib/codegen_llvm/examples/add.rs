use codegen_llvm::codegen;
use llvm::analysis::VeryfierFailureAction;

pub fn main() {
    let compiler = compiler_driver::Compiler::from_string("fn add(a: int, b: int) -> int {
        return a + b;
    }");

    let (hir_sess, sem) = compiler.compile().unwrap();

    let mut module = codegen(&hir_sess, &sem, &compiler.source().borrow()).into_values().next().unwrap();

    module.verify(VeryfierFailureAction::Print).unwrap_or_else(|err| {
        eprintln!("ERROR: {err}");
    });

    module.print("/tmp/test_mod.ll").unwrap_or_else(|err| {
        eprintln!("Error writting module ({err})");
    });
}
