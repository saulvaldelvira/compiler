use std::io::{stdout, Write};

use ast::Program;
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

pub fn perform_typechecking(program: &Program) -> Result<(),usize> {
    let mut ident = TypeCheking::new();
    ident.process(program);
    match ident.get_error_manager().n_errors() {
        0 => Ok(()),
        n => {
            let mut s = String::new();
            ident.get_error_manager().print_errors(&mut s).unwrap();
            eprint!("{s}");
            stdout().flush().unwrap();
            Err(n)
        }
    }
}
