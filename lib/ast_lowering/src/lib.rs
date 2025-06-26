struct AstLowering<'low, 'hir: 'low> {
    sess: &'low hir::Session<'hir>,
}

mod item;
mod expr;
mod stmt;
mod ty;

use ast::{Block, Symbol};
use hir::Ident;
use span::Spanned;

pub fn lower(sess: &hir::Session<'_>, prog: &ast::Module) {
    let mut lowering = AstLowering::new(sess);
    let p = lowering.lower_module(prog);
    sess.set_root(p);
}

fn ident(spanned: &Spanned<Symbol>) -> Ident {
    Ident {
        sym: spanned.val,
        span: spanned.span,
    }
}

impl<'low, 'hir: 'low> AstLowering<'low, 'hir> {
    fn new(sess: &'low hir::Session<'hir>) -> Self { Self { sess } }

    fn lower_module(&mut self, m: &ast::Module) -> &'hir hir::Module<'hir> {
        let m = self.lower_module_owned(m);
        self.sess.alloc(m)
    }

    fn lower_module_owned(&mut self, m: &ast::Module) -> hir::Module<'hir> {
        use ast::ModuleBody;

        let items = match &m.body {
            ModuleBody::Inline(Block { val: items, .. }) |
            ModuleBody::Extern { items, .. } |
            ModuleBody::Slf(items) => self.lower_items(items),
        };
        let name = self.lower_pathdef(ident(&m.name));
        hir::Module::new(name, items, m.span)
    }
}
