
struct AstLowering<'low, 'hir: 'low> {
    sess: &'low hir::Session<'hir>,
}

mod expr;
mod stmt;
mod def;
mod ty;

use ast::{ModItem, Symbol};
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
        let items = self.lower_mod_items(&m.elems);
        let name = m.name.val;
        hir::Module::new(name, items, m.span)
    }

    fn lower_mod_items(&mut self, mi: &[ast::ModItem]) -> &'hir [hir::ModItem<'hir>] {
        self.sess.alloc_iter(
            mi.iter().map(|mi| self.lower_mod_item_owned(mi))
        )
    }

    fn lower_mod_item_owned(&mut self, mi: &ast::ModItem) -> hir::ModItem<'hir> {
        let kind = match mi {
            ModItem::Decl(declaration) => hir::ModItemKind::Def(self.lower_definition(declaration)),
            ModItem::Mod(module) => hir::ModItemKind::Mod(self.lower_module(module)),
        };

        hir::ModItem::new(kind)
    }

}
