use core::fmt;
use std::fmt::Write;

use ast::{Program, Visitor, AST};
use identification::Identification;
use print::PrintVisitor;

mod identification;
mod print;

pub fn perform_identification(program: &Program) -> Result<(),usize> {
    let mut ident = Identification::new();
    ident.process(program);
    match ident.n_errors() {
        0 => Ok(()),
        n => Err(n)
    }
}

pub fn print_ast(program: &AST, to: &mut dyn Write) -> fmt::Result {
    let mut print = PrintVisitor(to);
    print.visit_ast(program)
}
