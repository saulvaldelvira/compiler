
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

pub fn lower(sess: &hir::Session<'_>, prog: &ast::Module) {
    let mut lowering = AstLowering::new(sess);
    let p = lowering.lower_module(prog);
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

    fn lower_module(&mut self, m: &ast::Module) -> &'hir hir::Module<'hir> {
        let m = self.lower_module_owned(m);
        self.sess.alloc(m)
    }

    fn lower_module_owned(&mut self, m: &ast::Module) -> hir::Module<'hir> {
        let items = self.lower_module_items(&m.elems);
        hir::Module::new(items)
    }

    fn lower_module_items(&mut self, mi: &[ast::ModuleItem]) -> &'hir [hir::ModuleItem<'hir>] {
        self.sess.alloc_iter(
            mi.iter().map(|mi| self.lower_module_item(mi))
        )
    }

    fn lower_module_item(&mut self, mi: &ast::ModuleItem) -> hir::ModuleItem<'hir> {
        match mi {
            ast::ModuleItem::Decl(d) => hir::ModuleItem::Def(self.lower_definition_owned(d)),
            ast::ModuleItem::Module(module) => hir::ModuleItem::Module(self.lower_module_owned(module)),
        }
    }

}
