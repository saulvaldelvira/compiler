use ast::expr::ExpressionKind;
use ast::{visitor, Expression, Program, Visitor};
use error_manager::ErrorManager;

struct AstValidator<'v> {
    em: &'v mut ErrorManager,
}

pub fn validate_ast(prog: &Program, em: &mut ErrorManager) {
    let mut v = AstValidator { em };
    v.visit_program(prog);
}

impl AstValidator<'_> {
    fn warn_unnecesary_paren(&mut self, expr: &Expression) {
        let ExpressionKind::Paren(paren) = &expr.kind else { return };
        let can_ignore = matches!(paren.val.kind, ExpressionKind::Path(_) | ExpressionKind::Literal(_));
        if can_ignore {
            self.em.emit_warning(error_manager::StringError {
                msg:  "Unnecesary parenthesis".into(),
                span: paren.span(),
            });
        }
    }
}

impl Visitor<'_> for AstValidator<'_> {
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
