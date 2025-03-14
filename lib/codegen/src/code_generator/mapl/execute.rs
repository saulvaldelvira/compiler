use std::ops::Deref;

use crate::memory::{MaplSizeStrategy, SizeStrategy};

use super::{get_last_variable_decl, Define, Eval, Execute, MaplCodeGenerator};
use ast::declaration::MemoryAddress;
use ast::stmt::{DeclarationStmt, ExprAsStmt, PrintStmt, ReturnStmt, StatementKind};
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
            StatementKind::Decl(declaration_stmt) => declaration_stmt.execute(cg),
            StatementKind::Block(_block_stmt) => todo!(),
            StatementKind::If(_if_stmt) => todo!(),
            StatementKind::While(_while_stmt) => todo!(),
            StatementKind::For(_for_stmt) => todo!(),
            StatementKind::Empty(_empty_stmt) => todo!(),
            StatementKind::Break(_break_stmt) => todo!(),
            StatementKind::Continue(_continue_stmt) => todo!(),
            StatementKind::Return(return_stmt) => return_stmt.execute(cg),
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
                                   .map(|vd| &vd.ty)
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
