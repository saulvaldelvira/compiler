use ast::Program;

mod identification;
use identification::Identification;

mod type_checking;
use type_checking::TypeChecking;

pub fn perform_identification(program: &Program) -> Result<(),usize> {
    let mut ident = Identification::new();
    ident.process(program);
    match ident.n_errors() {
        0 => Ok(()),
        n => Err(n)
    }
}

pub fn perform_type_checking(program: &Program) -> Result<(),u32> {
    let mut typechecking = TypeChecking::default();
    typechecking.process(program);
    match typechecking.n_errors {
        0 => Ok(()),
        n => Err(n)
    }
}
