use crate::code_generator::{BaseCodeGenerator, CodeGenerator};
use ast::declaration::{DeclarationKind, FunctionDecl, MemoryAddress, VariableDecl};
use ast::stmt::{DeclarationStmt, StatementKind};
use ast::types::{Type, TypeKind};
use core::fmt::Write;
use std::rc::Rc;
use ast::{Declaration, Program, Statement};

struct LoopCtx {
    cond_label: String,
    end_label: String,
}

pub struct MaplCodeGenerator {
    base: BaseCodeGenerator,
    current_function: Option<Rc<FunctionDecl>>,
    loops: Vec<LoopCtx>,
    label_counter: usize,
}

fn get_last_variable_decl(f: &FunctionDecl) -> Option<&VariableDecl> {
    f.body.stmts.iter()
        .filter_map(|stmt| {
            let Statement {
                kind: StatementKind::Decl(
                          DeclarationStmt {
                              inner : Declaration { kind : DeclarationKind::Variable(v), .. },
                              ..
                          }
                      ),
                      ..
            } = stmt else { return None };

            Some(v)
        }).map(|v| &**v).next_back()
}

impl CodeGenerator for MaplCodeGenerator {

    fn build() -> Self {
        MaplCodeGenerator {
            base: BaseCodeGenerator::new(),
            current_function: None,
            label_counter: 0,
            loops: Vec::new(),
        }
    }

    fn generate(mut self, program: &Program) -> String {
        program.define(&mut self);
        self.base.buf
    }

}

fn get_type_suffix(t: &Type) -> &str {
    match t.kind {
        TypeKind::Int => "I",
        TypeKind::Char => "B",
        _ => todo!()
    }
}

impl MaplCodeGenerator {
    fn anon_label(&mut self) -> String {
        self.label_counter += 1;
        format!("label_{}", self.label_counter)
    }

    fn enter_loop(&mut self, cond_label: String, end_label: String) {
        self.loops.push(LoopCtx { cond_label, end_label });
    }

    fn exit_loop(&mut self) -> (String,String) {
        let LoopCtx { cond_label, end_label } = self.loops.pop().unwrap();
        (cond_label,end_label)
    }

    fn continue_current_loop(&mut self)  {
        let MaplCodeGenerator { base, loops, ..} = self;
        let l = &loops.last().as_ref().unwrap().end_label;
        base.write_fmt(format_args!("JMP {l}"));
    }

    fn exit_current_loop(&mut self) {
        let MaplCodeGenerator { base, loops, ..} = self;
        let l = &loops.last().as_ref().unwrap().cond_label;
        base.write_fmt(format_args!("JMP {l}"));
    }

    fn pushi(&mut self, n: i32) {
        writeln!(self.base.buf, "PUSHI {n}").unwrap();
    }

    fn pushf(&mut self, n: f64) {
        writeln!(self.base.buf, "PUSHF {n}").unwrap();
    }

    fn pushb(&mut self, n: u8) {
        writeln!(self.base.buf, "PUSHB {n}").unwrap();
    }

    fn pushaddr(&mut self, addr: &MemoryAddress) {
        match addr {
            MemoryAddress::Absolute(abs) => self.base.write_fmt(format_args!("PUSHA {abs}")),
            MemoryAddress::Relative(rel) => {
                self.base.write("PUSHA bp");
                self.base.write_fmt(format_args!("PUSHI {rel}"));
                self.base.write("ADD");
            }
        }
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
        writeln!(self.base.buf, "{op}{t}\n").unwrap();
    }

    fn out(&mut self, t: &Type) {
        self.base.write_fmt(format_args!("OUT{}", get_type_suffix(t)));
    }
    fn discard_type(&mut self, t: &Type) {
        match &t.kind {
            TypeKind::Int
            | TypeKind::Float
            | TypeKind::Bool
            | TypeKind::Char => {
                self.base.write_fmt(format_args!("POP{}", get_type_suffix(t)));
            }
            _ => { }
        }
    }
}

mod execute;
mod eval;
mod define;
mod address;

trait Execute {
    fn execute(&self, cg: &mut MaplCodeGenerator);
}

trait Eval {
    fn eval(&self, cg: &mut MaplCodeGenerator);
}

trait Define {
    fn define(&self, cg: &mut MaplCodeGenerator);
}

trait Address {
    fn address(&self, cg: &mut MaplCodeGenerator);
}
