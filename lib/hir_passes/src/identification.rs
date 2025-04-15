use std::collections::HashMap;
use error_manager::ErrorManager;
use hir::node_map::HirNodeKind;
use hir::path::PathSegment;
use hir::visitor::{walk_function_definition, walk_module, walk_variable, Visitor};
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

    fn visit_path_segment(&mut self, path: &'hir PathSegment) {
        let found_def = self.st.get(&path.ident.sym);
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

    fn resolve_relative_segment(&mut self, left: &'hir PathSegment, right: &'hir PathSegment) {

        let Some(def) = left.def.get() else { return };
        let node = self.hir_sess.get_node(&def);

        match node {
            HirNodeKind::Module(module) => {
               match module.find_definition(&right.ident.sym) {
                   Some(def) => right.def.resolve(def.id),
                   None => {
                       self.em.emit_error(error_manager::StringError {
                           msg: format!("Undefined symbol '{:#?}::{:#?}'", module.name, right.ident.sym).into(),
                           span: right.ident.span,
                       });
                   }
               }
            }
            HirNodeKind::Def(definition) => {
                self.em.emit_error(error_manager::StringError {
                    msg: format!("Can't access item '{:#?}' of '{:#?}'", right.ident.sym, definition.name.ident.sym).into(),
                    span: right.ident.span,
                });
            }
            _ => unreachable!()
        }
    }
}

impl<'ident, 'hir: 'ident> Visitor<'hir> for Identification<'ident, 'hir> {
    type Result = ();
    type Ctx = ();

    fn visit_pathdef(&mut self, owner: HirId, pdef: &'hir hir::PathDef) -> Self::Result {
        self.st.define(pdef.ident.sym, owner);
    }

    fn visit_variable(&mut self, base: &'hir hir::Expression<'hir>, path: &'hir hir::Path) -> Self::Result {
        walk_variable(self, path);

        if let Some(id) = path.def().get() {
            let node = self.hir_sess.get_node(&id).unwrap_if_mod_item();
            if !matches!(node, HirNodeKind::Def(_)) {
                self.em.emit_error(error_manager::StringError {
                    msg: "Variable path must resolve to a definition".into(),
                    span: base.span
                });
            }
        }
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
        self.st.define(prog.name, prog.id);
        self.st.enter_scope();
        walk_module(self, prog);
        self.st.exit_scope();
    }

    fn visit_path(&mut self, path: &'hir hir::Path) -> Self::Result {
        self.visit_path_segment(&path.segments[0]);
        let it = path.segments.iter().skip(1);
        let it = path.segments.iter().zip(it);
        for (l, r) in it {
            self.resolve_relative_segment(l, r);
        }
    }

    fn get_ctx(&mut self) -> &mut Self::Ctx { &mut self.ctx }
}


