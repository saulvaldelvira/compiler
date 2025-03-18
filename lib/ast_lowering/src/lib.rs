
struct AstLowering<'low, 'hir: 'low> {
    arena: &'low hir::Arena<'hir>,
    table: SymbolTable,
    ids: usize,
}

mod symbol_table;

mod expr;
mod stmt;
mod def;
mod ty;

use ast::Symbol;
use hir::{Arena, HirId, Ident};
use span::Spanned;
use symbol_table::SymbolTable;

pub fn lower<'a, 'hir>(sess: &'a hir::Session<'hir>, prog: &ast::Program) {
    let p = sess.with_arena(|arena| {
        let lowering = AstLowering::new(arena);
        lowering.lower(prog)
    });
    sess.set_root(p);
}

fn ident(spanned: &Spanned<Symbol>) -> Ident {
    Ident { sym: spanned.val, span: spanned.span }
}

impl<'low, 'hir: 'low> AstLowering<'low, 'hir> {

    fn new(arena: &'low Arena<'hir>) -> Self {
        Self {
            arena,
            table: SymbolTable {},
            ids: 1,
        }
    }

    fn lower(mut self, prog: &ast::Program) -> &'hir hir::Program<'hir> {
        let defs = self.lower_definitions(&prog.decls);
        let prog = hir::Program {
            id: self.next_id(),
            defs
        };
        self.arena.alloc(prog)
    }

    fn next_id(&mut self) -> HirId {
        let id = HirId(self.ids);
        self.ids += 1;
        id
    }


}
