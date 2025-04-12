use std::collections::HashMap;

use error_manager::ErrorManager;
use hir::path::PathSegment;
use hir::visitor::{walk_function_definition, walk_module, Visitor};
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
    ctx: (),
}

impl<'ident, 'hir: 'ident> Identification<'ident, 'hir> {
    pub fn new(hir_sess: &'ident hir::Session<'hir>, em: &'ident mut ErrorManager) -> Self {
        let mut ident = Self {
            st: SymbolTable::default(),
            hir_sess,
            em,
            ctx: (),
        };
        ident.st.enter_scope();
        ident
    }

    fn visit_path_segment(&mut self, path: &'hir PathSegment<'hir>) {
        let found_def = self.st.get(&path.ident.sym).map(|id| {
            self.hir_sess
                .get_node(&id)
                .expect_definition()
        });
        match found_def {
            Some(def) => path.def.resolve(def),
            None => {
                self.em.emit_error(error_manager::StringError {
                    msg: format!("Undefined symbol '{:#?}'", path.ident.sym).into(),
                    span: path.ident.span,
                });
            }
        }
    }

    fn resolve_relative_segment(&mut self, left: &'hir PathSegment<'hir>, right: &'hir PathSegment<'hir>) {
        use hir::def::DefinitionKind;

        let Some(def) = left.def.get() else { return };

        match def.kind {
            DefinitionKind::Variable { .. } => todo!(),
            DefinitionKind::Function { .. } => todo!(),
            DefinitionKind::Struct { .. } => todo!(),
            DefinitionKind::Module(m) => {
               match m.find_definition(&right.ident.sym) {
                   Some(def) => right.def.resolve(def),
                   None => {
                       self.em.emit_error(error_manager::StringError {
                           msg: format!("Undefined symbol '{:#?}'", right.ident.sym).into(),
                           span: right.ident.span,
                       });
                   }
               }
            },
        }
    }
}

impl<'ident, 'hir: 'ident> Visitor<'hir> for Identification<'ident, 'hir> {
    type Result = ();
    type Ctx = ();

    fn visit_pathdef(&mut self, def: &'hir hir::Definition<'hir>, pdef: &'hir hir::PathDef) -> Self::Result {
        self.st.define(pdef.ident.sym, def.id);
    }

    fn visit_function_definition(
            &mut self,
            def: &'hir hir::Definition<'hir>,
            params: &'hir [hir::Definition<'hir>],
            ret_ty: &'hir hir::Type<'hir>,
            body: &'hir [hir::Statement<'hir>]
    ) -> Self::Result {
        self.st.enter_scope();
        walk_function_definition(self, def, params, ret_ty, body);
        self.st.exit_scope();
    }

    fn visit_module(&mut self, prog: &'hir hir::Module<'hir>) -> Self::Result {
        self.st.enter_scope();
        walk_module(self, prog);
        self.st.exit_scope();
    }

    fn visit_path(&mut self, path: &'hir hir::Path<'hir>) -> Self::Result {
        self.visit_path_segment(&path.segments[0]);
        let it = path.segments.iter().skip(1);
        let it = path.segments.iter().zip(it);
        for (l, r) in it {
            self.resolve_relative_segment(l, r);
        }
    }

    fn get_ctx(&mut self) -> &mut Self::Ctx { &mut self.ctx }
}


