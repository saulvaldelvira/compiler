
struct AstLowering<'low, 'hir: 'low> {
    sess: &'low hir::Session<'hir>,
}

mod expr;
mod stmt;
mod def;
mod ty;

use hir::Ident;

pub fn lower(sess: &hir::Session<'_>, prog: &ast::Program) {
    let lowering = AstLowering::new(sess);
    let p = lowering.lower(prog);
    sess.set_root(p);
}

fn ident(spanned: &ast::declaration::Ident) -> Ident {
    Ident { sym: spanned.sym, span: spanned.span }
}

impl<'low, 'hir: 'low> AstLowering<'low, 'hir> {

    fn new(sess: &'low hir::Session<'hir>) -> Self {
        Self {
            sess,
        }
    }

    fn lower(mut self, prog: &ast::Program) -> &'hir hir::Program<'hir> {
        let defs = self.lower_definitions(&prog.decls);
        let impls = self.lower_impls(&prog.impls);
        let prog = hir::Program::new(defs, impls);
        self.sess.alloc(prog)
    }

}
