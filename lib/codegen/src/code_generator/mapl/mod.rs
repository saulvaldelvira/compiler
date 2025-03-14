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

fn get_type_suffix(t: &Type) -> &str {
    match t.kind {
        TypeKind::Int => "I",
        _ => todo!()
    }
}

impl MaplCodeGenerator {
    fn pushi(&mut self, n: i32) {
        writeln!(self.0.buf, "PUSHI {n}").unwrap();
    }

    fn pushf(&mut self, n: f64) {
        writeln!(self.0.buf, "PUSHF {n}").unwrap();
    }

    fn pushb(&mut self, n: u8) {
        writeln!(self.0.buf, "PUSHB {n}").unwrap();
    }

    fn add(&mut self, ty: &Type) {
        self.sufixed_op("ADD", ty);
    }

    fn subs(&mut self, ty: &Type) {
        self.sufixed_op("SUB", ty);
    }

    fn mul(&mut self, ty: &Type) {
        self.sufixed_op("MUL", ty);
    }

    fn div(&mut self, ty: &Type) {
        self.sufixed_op("DIV", ty);
    }

    fn sufixed_op(&mut self, op: &str, ty: &Type) {
        let t = get_type_suffix(ty);
        writeln!(self.0.buf, "{op}{t}").unwrap();
    }

    fn out(&mut self, t: &Type) {
        self.0.buf.write_fmt(format_args!("OUT{}\n", get_type_suffix(t))).unwrap();
    }
    fn discard_type(&mut self, t: &Type) {
        match &t.kind {
            TypeKind::Int
            | TypeKind::Float
            | TypeKind::Bool
            | TypeKind::Char => {
                self.0.buf.write_fmt(format_args!("POP{}\n", get_type_suffix(t))).unwrap();
            }
            _ => { }
        }
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


