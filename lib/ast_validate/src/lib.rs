use ast::item::{Item, ItemKind, Module};
use ast::{
    Expression, Visitor, expr::ExpressionKind,
    stmt::StatementKind, visitor,
};
use error::{Error, ErrorKind};
use error_manager::ErrorManager;
use precedence::Precedence;

mod error;
mod precedence;

struct AstValidator<'v> {
    em: &'v mut ErrorManager,
}

pub fn validate_ast(prog: &Module, em: &mut ErrorManager) {
    let mut v = AstValidator { em };
    v.visit_module(prog);
}

impl AstValidator<'_> {
    fn warn_unnecesary_paren(&mut self, no_paren: &Expression, precedence: u8) {
        if let ExpressionKind::Paren(p) = &no_paren.kind {
            if p.val.precedence() >= precedence {
                self.em.emit_warning(Error {
                    kind: ErrorKind::UnnecesaryParenthesis,
                    span: no_paren.span,
                });
            }
        }
    }
}

impl Visitor for AstValidator<'_> {
    type Result = ();

    fn visit_expression(&mut self, expr: &'_ Expression) {
        visitor::walk_expression(self, expr);
        match &expr.kind {
            ExpressionKind::Call { args, .. } => {
                for arg in &args.val {
                    self.warn_unnecesary_paren(arg, 0);
                }
            }
            ExpressionKind::ArrayAccess { index, .. } => {
                self.warn_unnecesary_paren(index, 0);
            }
            ExpressionKind::Binary { left, right, .. } => {
                self.warn_unnecesary_paren(left, expr.precedence());
                self.warn_unnecesary_paren(right, expr.precedence());
            }
            ExpressionKind::Ternary {
                cond,
                if_true,
                if_false,
            } => {
                self.warn_unnecesary_paren(cond, expr.precedence());
                self.warn_unnecesary_paren(if_true, expr.precedence());
                self.warn_unnecesary_paren(if_false, expr.precedence());
            }
            _ => {}
        }
    }

    fn visit_statement(&mut self, stmt: &'_ ast::Statement) {
        visitor::walk_statement(self, stmt);
        match &stmt.kind {
            StatementKind::If { cond, .. } => {
                self.warn_unnecesary_paren(&cond.val, 0);
            }
            StatementKind::Print(_, args, _)
            | StatementKind::Read(_, args, _) => {
                for arg in args {
                    self.warn_unnecesary_paren(arg, 0);
                }
            }
            _ => {}
        }
    }

    fn visit_item(&mut self, item: &'_ Item) {
        visitor::walk_item(self, item);
        match &item.kind {
            ItemKind::Variable { init: Some(init), .. } => {
                self.warn_unnecesary_paren(init, 1);
            },
            ItemKind::Function { variadic_span, kw_extern, .. } => {
                if let Some(varspan) = variadic_span && !kw_extern.is_some() {
                    self.em.emit_error(Error {
                        kind: ErrorKind::NonExternVariadic,
                        span: *varspan,
                    });
                }
            }
            _ => {}
        }
    }
}
