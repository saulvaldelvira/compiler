use error_manager::ErrorManager;
use hir::visitor::Visitor;

use std::collections::HashMap;

use error_manager::{FilePosition, Span};
use hir::def::DefinitionKind;
use hir::visitor::{walk_struct_definition, VisitorCtx};
use hir::{
    HirId,
    node_map::HirNodeKind,
    path::PathSegment,
    visitor::walk_variable,
};
use session::Symbol;

pub fn identify(sess: &hir::Session<'_>, source: &str, em: &mut ErrorManager) {
    let prog = sess.get_root();
    let mut ident = Identification::new(sess, source, em);
    ident.visit_module(prog);
}

#[derive(Default)]
struct SymbolTable {
    scopes: Vec<HashMap<Symbol, HirId>>,
}

impl SymbolTable {

    fn get_top(&self, sym: Symbol) -> Option<HirId> {
        self.scopes.last().unwrap().get(&sym).copied()
    }

    fn define(&mut self, sym: Symbol, id: HirId) {
        self.scopes.last_mut().unwrap().insert(sym, id);
    }

    fn get(&mut self, sym: Symbol) -> Option<HirId> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.get(&sym) {
                return Some(*id);
            }
        }
        None
    }

    fn enter_scope(&mut self) { self.scopes.push(HashMap::default()); }

    fn exit_scope(&mut self) { self.scopes.pop(); }
}

pub struct Ctx {
    st: SymbolTable,
}

impl<'ast> VisitorCtx<'ast> for Ctx {
    fn enter_function(&mut self, _func: &'ast hir::Definition<'ast>) {
        self.st.enter_scope();
    }

    fn exit_function(&mut self) {
        self.st.exit_scope();
    }

    fn enter_module(&mut self, _mod: &'ast hir::Module<'ast>) {
        self.st.enter_scope();
    }

    fn exit_module(&mut self) {
        self.st.exit_scope();
    }
}

pub struct Identification<'ident, 'hir> {
    hir_sess: &'ident hir::Session<'hir>,
    em: &'ident mut ErrorManager,
    source: &'ident str,
    ctx: Ctx,
}

impl<'ident, 'hir: 'ident> Identification<'ident, 'hir> {
    pub fn new(hir_sess: &'ident hir::Session<'hir>, source: &'ident str, em: &'ident mut ErrorManager) -> Self {
        let mut ident = Self {
            ctx: Ctx {
                st: SymbolTable::default(),
            },
            source,
            hir_sess,
            em,
        };
        ident.ctx.st.enter_scope();
        ident
    }

    fn visit_path_segment(&mut self, path: &'hir PathSegment) {
        let found_def = self.ctx.st.get(path.ident.sym);
        match found_def {
            Some(def) => path.def.resolve(def),
            None => {
                self.em.emit_error(IdentificationError {
                    kind: IdentificationErrorKind::Undefined(path.ident.sym.to_string()),
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
                match module.find_item(&right.ident.sym) {
                    Some(def) => right.def.resolve(def.inner_id()),
                    None => {
                        self.em.emit_error(error_manager::StringError {
                            msg: format!(
                                "Undefined symbol '{:#?}::{:#?}'",
                                module.name, right.ident.sym
                            )
                            .into(),
                            span: right.ident.span,
                        });
                    }
                }
            }
            HirNodeKind::Def(definition) => {
                self.em.emit_error(error_manager::StringError {
                    msg: format!(
                        "Can't access item '{:#?}' of '{:#?}'",
                        right.ident.sym, definition.name.ident.sym
                    )
                    .into(),
                    span: right.ident.span,
                });
            }
            _ => unreachable!(),
        }
    }
}

fn try_shadow(node: HirNodeKind<'_>) -> Result<(), &'static str>  {
    match node {
        HirNodeKind::Def(definition) => {
            match definition.kind {
                DefinitionKind::Variable { .. } => Ok(()),
                DefinitionKind::Function { .. } => Err("function"),
                DefinitionKind::Struct { .. } => Err("struct")
            }
        }
        HirNodeKind::Field(_) => Err("field"),
        HirNodeKind::Module(_) => Err("module"),
        _ => unreachable!(),
    }
}

impl<'ident, 'hir: 'ident> Visitor<'hir> for Identification<'ident, 'hir> {
    type Result = ();
    type Ctx = Ctx;

    fn visit_pathdef(&mut self, owner: HirId, pdef: &'hir hir::PathDef) {
        let name = pdef.ident.sym;
        if let Some(prev) = self.ctx.st.get_top(name) {
            let prev = self.hir_sess.get_node(&prev);
            if let Err(shadowed_ty) = try_shadow(prev) {
                let owner = self.hir_sess.get_node(&owner);
                let prev_span = prev.get_span().unwrap();
                let pos = prev_span.file_position(self.source);
                self.em.emit_error(IdentificationError {
                    kind: IdentificationErrorKind::Redefinition {
                        node_type: shadowed_ty,
                        name: name.to_string(),
                        prev: pos,
                    },
                    span: owner.get_span().unwrap(),
                });

                return;
            }
        }

        self.ctx.st.define(name, owner);
    }

    fn visit_struct_definition(
        &mut self,
        base: &'hir hir::Definition<'hir>,
        fields: &'hir [hir::def::Field<'hir>],
    ) -> Self::Result {
        self.ctx.st.enter_scope();
        walk_struct_definition(self, base, fields);
        self.ctx.st.exit_scope();
    }

    fn visit_variable(&mut self, base: &'hir hir::Expression<'hir>, path: &'hir hir::Path) {
        walk_variable(self, path);

        if let Some(id) = path.def().get() {
            let node = self.hir_sess.get_node(&id);
            if !matches!(node, HirNodeKind::Def(_)) {
                self.em.emit_error(error_manager::StringError {
                    msg: "Variable path must resolve to a definition".into(),
                    span: base.span,
                });
            }
        }
    }

    fn visit_path(&mut self, path: &'hir hir::Path) {
        self.visit_path_segment(&path.segments[0]);
        let it = path.segments.iter().skip(1);
        let it = path.segments.iter().zip(it);
        for (l, r) in it {
            self.resolve_relative_segment(l, r);
        }
    }

    fn get_ctx(&mut self) -> &mut Self::Ctx { &mut self.ctx }
}

#[derive(Debug, PartialEq)]
enum IdentificationErrorKind {
    Redefinition {
        name: String,
        node_type: &'static str,
        prev: FilePosition,
    },
    Undefined(String),
}

#[derive(Debug, PartialEq)]
struct IdentificationError {
    kind: IdentificationErrorKind,
    span: Span,
}

impl error_manager::Error for IdentificationError {
    fn get_span(&self) -> Span { self.span }

    fn write_msg(&self, out: &mut dyn core::fmt::Write) -> core::fmt::Result {
        match &self.kind {
            IdentificationErrorKind::Redefinition { name, node_type, prev } => {
                write!(out, "Redefinition of '{name}' (previous definition: {node_type} at {prev})")
            },
            IdentificationErrorKind::Undefined(name) => write!(out, "Undefined symbol '{name}'"),
        }
    }
}

#[cfg(test)]
mod test;
