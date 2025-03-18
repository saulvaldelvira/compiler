use ast::expr::ExpressionKind;
use ast::{visitor, Expression, Program, Visitor};
use error_manager::ErrorManager;

struct AstValidator {
    errors: ErrorManager,
}

pub fn validate_ast(prog: &Program) -> ErrorManager {
    let mut v = AstValidator { errors: ErrorManager::new() };
    v.visit_program(prog);
    let AstValidator { errors } = v;
    errors
}

impl AstValidator {
    fn warn_unnecesary_paren(&mut self, expr: &Expression) {
        let ExpressionKind::Paren(paren) = &expr.kind else { return };
        let can_ignore = matches!(paren.val.kind, ExpressionKind::Path(_) | ExpressionKind::Literal(_));
        if can_ignore {
            self.errors.warning("Unnecesary parenthesis", paren.span());
        }
    }
}

impl Visitor<'_> for AstValidator {
    type Result = ();

    fn visit_expression(&mut self, expr: &'_ Expression) -> Self::Result {
       visitor::walk_expression(self, expr);
       match &expr.kind {
           ExpressionKind::Call { args, .. } => {
               for arg in &args.val {
                   self.warn_unnecesary_paren(arg);
               }
           },
           ExpressionKind::ArrayAccess { index, .. } => {
               self.warn_unnecesary_paren(index);
           },
           _ => {}
       }
    }
}
