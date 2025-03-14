use crate::code_generator::{BaseCodeGenerator, CodeGenerator};
use ast::types::{Type, TypeKind};
use core::fmt::Write;
use ast::Program;

pub struct MaplCodeGenerator(BaseCodeGenerator);

impl CodeGenerator for MaplCodeGenerator {

    fn build() -> Self {
        MaplCodeGenerator(BaseCodeGenerator::new())
    }

    fn generate(mut self, program: &Program) -> String {
        program.define(&mut self);
        self.0.buf
    }

}

impl MaplCodeGenerator {
    fn pushi(&mut self, n: i32) {
        writeln!(self.0.buf, "PUSHI {n}").unwrap();
    }
    fn out(&mut self, tk: &Type) {
        let t = match tk.kind {
            TypeKind::Int => "I",
            _ => todo!()
        };
        self.0.buf.write_fmt(format_args!("OUT{t}\n")).unwrap();
    }
}

mod execute;
mod eval;
mod define;

trait Execute {
    fn execute(&self, cg: &mut MaplCodeGenerator);
}

trait Eval {
    fn eval(&self, cg: &mut MaplCodeGenerator);
}

trait Define {
    fn define(&self, cg: &mut MaplCodeGenerator);
}


