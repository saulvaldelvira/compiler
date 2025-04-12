use crate::AstLowering;


impl<'low, 'hir: 'low> AstLowering<'low, 'hir> {

    pub(super) fn lower_statement(&mut self, stmt: &ast::Statement) -> &'hir hir::Statement<'hir> {
        self.sess.alloc(self.lower_statement_owned(stmt))
    }

    pub(super) fn lower_statements(&mut self, stmts: &[ast::Statement]) -> &'hir [hir::Statement<'hir>] {
        self.sess.alloc_iter(
            stmts.iter().map(|stmt| self.lower_statement_owned(stmt))
        )
    }

    fn lower_statement_owned(&mut self, stmt: &ast::Statement) -> hir::Statement<'hir> {
        use ast::stmt::StatementKind as SK;
        use hir::stmt::StatementKind as HSK;
        let kind = match &stmt.kind {
            SK::Expression(expr, _) => {
                HSK::Expr(self.lower_expression(expr))
            },
            SK::Print(_, expressions, _) => {
                if expressions.len() > 1 {
                    let stmts = self.sess.alloc_iter(
                        expressions.iter().map(|expr| {
                            hir::Statement::new(HSK::Print(self.lower_expression(expr)), expr.span)
                        })
                    );
                    HSK::Block(stmts)
                } else {
                    HSK::Print(self.lower_expression(&expressions[0]))
                }
            },
            SK::Read(_, expressions, _) => {
                if expressions.len() > 1 {
                    let stmts = self.sess.alloc_iter(
                        expressions.iter().map(|expr| {
                            hir::Statement::new(HSK::Read(self.lower_expression(expr)), expr.span)
                        })
                    );
                    HSK::Block(stmts)
                } else {
                    HSK::Read(self.lower_expression(&expressions[0]))
                }
            },
            SK::Decl(declaration) => {
                let def = self.lower_definition(declaration);
                HSK::Def(def)
            },
            SK::Block(block) => {
                let block = self.lower_statements(&block.val);
                HSK::Block(block)
            },
            SK::If { cond, if_body, else_body, .. } => {
                let cond = self.lower_expression(&cond.val);
                let if_true = self.lower_statement(if_body);
                let if_false = else_body.as_ref().map(|e| self.lower_statement(e));
                HSK::If { cond, if_true, if_false }
            },
            SK::While { cond, body, .. } => {
                let cond = self.lower_expression(cond);
                let body = self.lower_statement(body);
                HSK::While { cond, body }
            }
            SK::For { init, cond, inc, body, .. } => {
                let init = init.as_ref().map(|i| self.lower_definition(i));
                let cond = cond.as_ref().map(|c| self.lower_expression(c));
                let inc = inc.as_ref().map(|i| self.lower_expression(i));
                let body = self.lower_statement(body);
                HSK::For { init, cond, inc, body }
            }
            SK::Empty(_) => HSK::Empty,
            SK::Break(_) => HSK::Break,
            SK::Continue(_) => HSK::Continue,
            SK::Return { expr, .. } => HSK::Return(expr.as_ref().map(|e| self.lower_expression(e)))
        };

        hir::Statement::new(kind, stmt.span)
    }
}
