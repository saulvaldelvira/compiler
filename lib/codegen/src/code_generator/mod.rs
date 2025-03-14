use std::fmt::{Arguments, Write};
use ast::Program;

mod mapl;
pub use mapl::MaplCodeGenerator;

pub fn gen_code<CG: CodeGenerator>(program: &Program) -> String {
    CG::build().generate(program)
}

pub trait CodeGenerator {
    fn build() -> Self;
    fn generate(self, program: &Program) -> String;
}

struct BaseCodeGenerator {
    buf: String,
}

impl BaseCodeGenerator {
    fn new() -> Self {
        Self { buf: String::new() }
    }
    fn write(&mut self, src: &str) {
        self.buf.write_fmt(format_args!("{src}\n")).unwrap();
    }
    fn write_fmt(&mut self, src: Arguments<'_>) {
        self.buf.write_fmt(src).unwrap();
        self.buf.write_str("\n").unwrap();
    }
}

