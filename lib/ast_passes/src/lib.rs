use ast::Program;
use error_manager::ErrorManager;
use identification::Identification;
use type_checking::TypeCheking;

mod identification;
mod type_checking;

pub fn perform_identification(program: &Program) -> Result<(),usize> {
    let mut ident = Identification::new();
    ident.process(program);
    match ident.n_errors() {
        0 => Ok(()),
        n => Err(n)
    }
}

pub fn perform_typechecking(program: &Program) -> Result<(),ErrorManager> {
    let mut tcheck = TypeCheking::new();
    tcheck.process(program);
    let TypeCheking { error_manager, .. } = tcheck;
    match error_manager.n_errors() {
        0 => Ok(()),
        _ => Err(error_manager)
    }
}
