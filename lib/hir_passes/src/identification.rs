use std::collections::HashMap;

use error_manager::ErrorManager;
use hir::visitor::{walk_function_definition, Visitor};
use hir::HirId;
use session::Symbol;

#[derive(Default)]
struct SymbolTable {
    scopes: Vec<HashMap<Symbol,HirId>>,
}

impl SymbolTable {

    fn define(&mut self, sym: Symbol, id: HirId) {
        self.scopes.last_mut().unwrap().insert(sym, id);
    }

    fn get(&mut self, sym: &Symbol) -> Option<HirId> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.get(sym) {
                return Some(*id);
            }
        }
        None
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Default::default());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }
}

pub struct Identification<'ident, 'hir> {
    hir_sess: &'ident hir::Session<'hir>,
    em: &'ident mut ErrorManager,
    st: SymbolTable,
}

impl<'ident, 'hir: 'ident> Identification<'ident, 'hir> {
    pub fn new(hir_sess: &'ident hir::Session<'hir>, em: &'ident mut ErrorManager) -> Self {
        let mut ident = Self {
            st: SymbolTable::default(),
            hir_sess,
            em,
        };
        ident.st.enter_scope();
        ident
    }
}

impl<'ident, 'hir: 'ident> Visitor<'hir> for Identification<'ident, 'hir> {
    type Result = ();

    fn visit_pathdef(&mut self, def: &'hir hir::Definition<'hir>, pdef: &'hir hir::def::PathDef) -> Self::Result {
        self.st.define(pdef.ident.sym, def.id);
    }

    fn visit_function_definition(
            &mut self,
            _def: &'hir hir::Definition<'hir>,
            params: &'hir [hir::Definition<'hir>],
            body: &'hir [hir::Statement<'hir>]
    ) -> Self::Result {
        self.st.enter_scope();
        walk_function_definition(self, params, body);
        self.st.exit_scope();
    }

    fn visit_path(&mut self, path: &'hir hir::Path<'hir>) -> Self::Result {
        let found_def = self.st.get(&path.ident.sym).map(|id| {
            self.hir_sess
                .get_node(&id)
                .expect_definition()
        });
        match found_def {
            Some(def) => path.def.resolve(def),
            None => {
                self.em.error(format!("Undefined variable '{:#?}'", path.ident.sym), path.ident.span);
            }
        }
    }
}


