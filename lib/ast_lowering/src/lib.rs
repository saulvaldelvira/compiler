
struct AstLowering<'low, 'hir: 'low> {
    sess: &'low hir::Session<'hir>,
}

mod expr;
mod stmt;
mod def;
mod ty;

use ast::Symbol;
use hir::Ident;
use span::Spanned;

pub fn lower(sess: &hir::Session<'_>, prog: &ast::Program) {
    let lowering = AstLowering::new(sess);
    let p = lowering.lower(prog);
    sess.set_root(p);
}

fn ident(spanned: &Spanned<Symbol>) -> Ident {
    Ident { sym: spanned.val, span: spanned.span }
}

impl<'low, 'hir: 'low> AstLowering<'low, 'hir> {

    fn new(sess: &'low hir::Session<'hir>) -> Self {
        Self {
            sess,
        }
    }

    fn lower(mut self, prog: &ast::Program) -> &'hir hir::Program<'hir> {
        let defs = self.lower_definitions(&prog.decls);
        let prog = hir::Program::new(defs);
        self.sess.alloc(prog)
    }

}
