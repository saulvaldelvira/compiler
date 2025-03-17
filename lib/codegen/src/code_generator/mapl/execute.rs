use std::ops::Deref;

use crate::memory::{MaplSizeStrategy, SizeStrategy};

use super::{get_last_variable_decl, Address, Define, Eval, Execute, MaplCodeGenerator};
use ast::declaration::MemoryAddress;
use ast::stmt::{BlockStmt, BreakStmt, ContinueStmt, DeclarationStmt, ExprAsStmt, ForStmt, IfStmt, PrintStmt, ReadStmt, ReturnStmt, StatementKind, WhileStmt};
use ast::Statement;

impl Execute for PrintStmt {
    fn execute(&self, cg: &mut MaplCodeGenerator) {
        self.expr.eval(cg);
        cg.out(&self.expr.ty.unwrap());

    }
}

impl Execute for Statement {
    fn execute(&self, cg: &mut MaplCodeGenerator) {
        match &self.kind {
            StatementKind::Expression(expr_as_stmt) => expr_as_stmt.execute(cg),
            StatementKind::Print(print_stmt) => print_stmt.execute(cg),
            StatementKind::Read(read_stmt) => read_stmt.execute(cg),
            StatementKind::Decl(declaration_stmt) => declaration_stmt.execute(cg),
            StatementKind::Block(block_stmt) => block_stmt.execute(cg),
            StatementKind::If(if_stmt) => if_stmt.execute(cg),
            StatementKind::While(while_stmt) => while_stmt.execute(cg),
            StatementKind::For(for_stmt) => for_stmt.execute(cg),
            StatementKind::Empty(_) => {},
            StatementKind::Return(return_stmt) => return_stmt.execute(cg),
            StatementKind::Break(break_stmt) => break_stmt.execute(cg),
            StatementKind::Continue(continue_stmt) => continue_stmt.execute(cg),
        }
    }
}

impl Execute for ReadStmt {
    fn execute(&self, cg: &mut MaplCodeGenerator) {
        self.expr.address(cg);
        cg.sufixed_op("IN", &self.expr.ty.unwrap());
        cg.sufixed_op("STORE", &self.expr.ty.unwrap());
    }
}

impl Execute for BreakStmt {
    fn execute(&self, cg: &mut MaplCodeGenerator) {
        cg.exit_current_loop();
    }
}

impl Execute for ContinueStmt {
    fn execute(&self, cg: &mut MaplCodeGenerator) {
        cg.continue_current_loop();
    }
}

impl Execute for ForStmt {
    fn execute(&self, cg: &mut MaplCodeGenerator) {
        if let Some(init) = &self.init {
            init.define(cg);
        }
        let cond_label = cg.anon_label();
        let end_label = cg.anon_label();
        cg.base.write_fmt(format_args!("{cond_label}:"));
        if let Some(cond) = &self.cond {
            cond.eval(cg);
            cg.base.write_fmt(format_args!("JZ {end_label}"));
        }

        cg.enter_loop(cond_label, end_label);
        self.body.execute(cg);
        let (cond_label,end_label) = cg.exit_loop();

        if let Some(inc) = &self.inc {
            inc.eval(cg);
            cg.base.write_fmt(format_args!("JMP {cond_label}"));
        }

        cg.base.write_fmt(format_args!("{end_label}:"));
    }
}

impl Execute for IfStmt {
    fn execute(&self, cg: &mut MaplCodeGenerator) {
        let else_label = cg.anon_label();
        let end_label = cg.anon_label();

        self.cond.eval(cg);
        cg.base.write_fmt(format_args!("JZ {else_label}"));
        self.if_true.execute(cg);
        cg.base.write_fmt(format_args!("JMP {end_label}"));
        cg.base.write_fmt(format_args!("{else_label}:"));
        if let Some(if_false) = &self.if_false {
            if_false.execute(cg);
        }
        cg.base.write_fmt(format_args!("{end_label}:"));

    }
}

impl Execute for WhileStmt {
    fn execute(&self, cg: &mut MaplCodeGenerator) {
        let cond_label = cg.anon_label();
        let end_label = cg.anon_label();

        cg.base.write_fmt(format_args!("{cond_label}:"));
        self.cond.eval(cg);
        cg.base.write_fmt(format_args!("JZ {end_label}"));

        cg.enter_loop(cond_label, end_label);
        self.stmts.execute(cg);
        let (cond_label,end_label) = cg.exit_loop();

        cg.base.write_fmt(format_args!("JMP {cond_label}"));
        cg.base.write_fmt(format_args!("{end_label}:"));
    }
}

impl Execute for BlockStmt {
    fn execute(&self, cg: &mut MaplCodeGenerator) {
        for stmt in &self.stmts {
            stmt.execute(cg);
        }
    }
}

impl Execute for ExprAsStmt {
    fn execute(&self, cg: &mut MaplCodeGenerator) {
        if self.expr.has_side_effect() {
            self.expr.eval(cg);
            cg.discard_type(&self.expr.ty.unwrap());
        }
    }
}

impl Execute for DeclarationStmt {
    fn execute(&self, cg: &mut MaplCodeGenerator) {
        self.inner.define(cg);
    }
}

impl Execute for ReturnStmt {
    fn execute(&self, cg: &mut MaplCodeGenerator) {
        if let Some(expr) = &self.expr {
            expr.eval(cg);
        }

        let Some(cf) = &cg.current_function else { unreachable!() };

        let ret_size = MaplSizeStrategy::size_of(&cf.return_type);

        let params_size: usize = cf.args.iter()
                                   .map(|vd| vd.ty.as_ref())
                                   .map(|t| MaplSizeStrategy::size_of(&t.unwrap()))
                                   .sum();

        let locals_size = - get_last_variable_decl(cf)
                               .map(|vd| {
                                   let addr = vd.address.unwrap();
                                   let MemoryAddress::Relative(addr) = addr.deref() else { unreachable!() };
                                   *addr
                               })
                               .unwrap_or(0);

        cg.base.write_fmt(format_args!("ret {ret_size}, {locals_size}, {params_size}"));
    }
}
