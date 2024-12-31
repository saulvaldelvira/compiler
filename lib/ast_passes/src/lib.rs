use ast::Program;
use identification::Identification;

mod identification;

pub fn perform_identification(program: &Program) -> Result<(),usize> {
    let mut ident = Identification::new();
    ident.process(program);
    match ident.n_errors() {
        0 => Ok(()),
        n => Err(n)
    }
}

