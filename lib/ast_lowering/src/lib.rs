mod item;
mod expr;
mod stmt;
mod ty;

use ast::{Ast, Block, Symbol};
use hir::Ident;
use span::{Span, Spanned};

struct AstLowering<'low, 'hir: 'low> {
    sess: &'low hir::Session<'hir>,
}


#[allow(clippy::implicit_hasher)]
pub fn lower(sess: &hir::Session<'_>, ast: &Ast) {
    let mut lowering = AstLowering { sess };

    let name = Ident {
        span: Span::dummy(),
        sym: Symbol::new("root"),
    };
    let root = hir::Module::new(
        lowering.lower_pathdef(name),
        lowering.lower_items(&ast.items),
        Span::dummy(),
    );

    let root = sess.alloc(root);
    sess.set_root(root);
}

fn ident(spanned: &Spanned<Symbol>) -> Ident {
    Ident {
        sym: spanned.val,
        span: spanned.span,
    }
}

impl<'low, 'hir: 'low> AstLowering<'low, 'hir> {

    fn lower_module(&mut self, m: &ast::Module) -> &'hir hir::Module<'hir> {
        let m = self.lower_module_owned(m);
        self.sess.alloc(m)
    }

    fn lower_module_owned(&mut self, m: &ast::Module) -> hir::Module<'hir> {
        use ast::ModuleBody;

        let items = match &m.body {
            ModuleBody::Inline(Block { val: items, .. }) => self.lower_items(items),
            ModuleBody::Extern { items, .. } => {
                self.lower_items(items)
            }
        };
        let name = self.lower_pathdef(ident(&m.name));
        hir::Module::new(name, items, m.span)
    }
}
