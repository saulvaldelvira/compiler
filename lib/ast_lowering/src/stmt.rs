use ast::Statement;

use crate::AstLowering;


impl<'low, 'hir: 'low> AstLowering<'low, 'hir> {

    pub(super) fn lower_statement(&mut self, stmt: &ast::Statement) -> &'hir hir::Statement<'hir> {
        self.arena.alloc(self.lower_statement_owned(stmt))
    }

    pub(super) fn lower_statements(&mut self, stmts: &[ast::Statement]) -> &'hir [hir::Statement<'hir>] {
        self.arena.alloc_iter(
            stmts.iter().map(|stmt| self.lower_statement_owned(&stmt))
        )
    }

    fn lower_statement_owned(&mut self, stmt: &ast::Statement) -> hir::Statement<'hir> {
        use ast::stmt::StatementKind as SK;
        use hir::stmt::StatementKind as HSK;
        let kind = match &stmt.kind {
            SK::Expression(expr, _) => {
                HSK::Expr(self.lower_expression(expr))
            },
            SK::Print(span, expressions, span1) => todo!(),
            SK::Read(span, expressions, span1) => todo!(),
            SK::Decl(declaration) => {
                let def = self.lower_definition(declaration);
                HSK::Def(def)
            },
            SK::Block(block) => todo!(),
            SK::If { kw_if, cond, if_body, kw_else, else_body } => todo!(),
            SK::While { kw_while, cond, body } => todo!(),
            SK::For { kw_for, init, cond, inc, body } => todo!(),
            SK::Empty(span) => todo!(),
            SK::Break(span) => todo!(),
            SK::Continue(span) => todo!(),
            SK::Return { kw_ret, expr, semmicollon } => todo!(),
        };

        hir::Statement {
            kind,
            span: stmt.span,
            id: self.next_id(),
        }
    }
}
