use super::{Define, Eval, Execute, MaplCodeGenerator};
use ast::stmt::{DeclarationStmt, ExprAsStmt, PrintStmt, StatementKind};
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
            StatementKind::Return(_return_stmt) => todo!(),
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
