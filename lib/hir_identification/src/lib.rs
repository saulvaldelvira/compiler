//! Identification phase
//!
//! The main goal of the identification phase is to link
//! all [paths](hir::Path) with their corresponding [definition](hir::PathDef)
//!
//! # Example
//! ```text
//! fn foo() { /* (id: 1) */ }
//! fn main() {
//!     let a: int = 12; // (id: 2)
//!
//!     a = a + 1; // (a -> 2)
//!
//!     let a: char = 'a'; // (id: 3)
//!
//!     print a; // (b -> 3)
//!
//!     foo(); // foo -> 1
//!
//!     let foo: float = 1.2; // (id: 4)
//!     foo = foo * 1.2; // (foo -> 4)
//! }
//! ```

use error_manager::ErrorManager;
use hir::visitor::{walk_function_definition, Visitor};
use hir::{Item, ItemKind, Module, Path};

use std::collections::HashMap;

use error_manager::{FilePosition, Span};
use hir::visitor::VisitorCtx;
use hir::{
    HirId,
    node_map::HirNodeKind,
    path::PathSegment,
    visitor::walk_variable,
};
use session::Symbol;

/// Performs identification for the given hir tree
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

fn path_can_be_variable_ty(sess: &hir::Session<'_>, path: &Path) -> Option<bool> {
    let id = path.def().get()?;
    let node = sess.get_node(&id);
    let val = match node {
        HirNodeKind::Item(item) if item.is_definition() => true,
        HirNodeKind::Item(Item { kind: ItemKind::Use(u), .. }) => {
            path_can_be_variable_ty(sess, &u.path)?
        }
        HirNodeKind::Use(u) => path_can_be_variable_ty(sess, &u.path)?,
        _ => false,
    };
    Some(val)
}

impl<'ast> VisitorCtx<'ast> for Ctx {
    fn enter_function(&mut self, _func: &'ast hir::Item<'ast>) {
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

    fn enter_struct(&mut self, _mod: &'ast hir::Item<'ast>) {
        self.st.enter_scope();
    }

    fn exit_struct(&mut self) {
        self.st.enter_scope();
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

    fn resolve_from_module(&mut self, module: &'hir Module<'hir>, right: &'hir PathSegment) {
        match module.find_item(right.ident.sym) {
            Some(def) => {
                right.def.resolve(def.id);
            },
            None => {
                self.em.emit_error(IdentificationError {
                    kind: IdentificationErrorKind::UndefinedAccess(
                              module.name.ident.sym.to_string(),
                              right.ident.sym.to_string(),
                          ),
                    span: right.ident.span,
                });
            }
        }
    }

    fn item_resolve(&mut self, item: &'hir Item<'hir>, right: &'hir PathSegment) {
        if let Some(module) = item.as_module() {
            self.resolve_from_module(module, right);
        }
        else if let Some(useitem) = item.as_use() {
            let Some(def) = useitem.path.def().get() else { return };
            let node = self.hir_sess.get_node(&def);
            match node {
                HirNodeKind::Item(item) => self.item_resolve(item, right),
                HirNodeKind::Module(module) => self.resolve_from_module(module, right),
                n => unreachable!("Found node {n:?}"),
            }
        }
        else {
            self.em.emit_error(IdentificationError {
                kind: IdentificationErrorKind::CantAccess(
                          item.get_name().to_string(),
                          right.ident.sym.to_string(),
                          item.kind.get_repr(),
                      ),
                 span: right.ident.span,
            });
        }
    }


    fn resolve_relative_segment(&mut self, left: &'hir PathSegment, right: &'hir PathSegment) {
        let Some(def) = left.def.get() else { return };
        let node = self.hir_sess.get_node(&def);


        match node {
            HirNodeKind::Item(item) => self.item_resolve(item, right),
            HirNodeKind::Module(module) => self.resolve_from_module(module, right),
            n => unreachable!("Found node {n:?}"),
        }
    }

    fn define(&mut self, name: Symbol, owner: HirId) {
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
}

fn try_shadow(node: HirNodeKind<'_>) -> Result<(), &'static str>  {
    match node {
        HirNodeKind::Item(item) => {
            match item.kind {
                ItemKind::Variable { .. } |
                ItemKind::Use(_) => Ok(()),
                ItemKind::Function { .. } => Err("function"),
                ItemKind::Struct { .. } => Err("struct"),
                ItemKind::Mod(_) => Err("module"),
            }
        }
        HirNodeKind::Field(_) => Err("field"),
        _ => unreachable!(),
    }
}

impl<'ident, 'hir: 'ident> Visitor<'hir> for Identification<'ident, 'hir> {
    type Result = ();
    type Ctx = Ctx;

    fn visit_pathdef(&mut self, owner: HirId, pdef: &'hir hir::PathDef) -> Self::Result {
        self.define(pdef.ident.sym, owner);
    }

    fn visit_function_definition(
            &mut self,
            base: &'hir hir::Item<'hir>,
            name: &'hir hir::PathDef,
            params: &'hir [hir::Item<'hir>],
            ret_ty: &'hir hir::Type<'hir>,
            body: &'hir [hir::Statement<'hir>],
    ) -> Self::Result {
        self.define(name.ident.sym, base.id);
        self.ctx.st.enter_scope();
        walk_function_definition(self, base, name, params, ret_ty, body);
        self.ctx.st.exit_scope();
    }

    fn visit_variable(&mut self, base: &'hir hir::Expression<'hir>, path: &'hir hir::Path) {
        walk_variable(self, path);

        if path_can_be_variable_ty(self.hir_sess, path).is_some_and(|c| !c) {
            self.em.emit_error(IdentificationError {
                kind: IdentificationErrorKind::VariablePathInvalidTy,
                span: base.span,
            });
        }
    }

    fn visit_path(&mut self, path: &'hir hir::Path) {
        self.visit_path_segment(&path.segments()[0]);
        let it = path.segments().iter().skip(1);
        let it = path.segments().iter().zip(it);
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
    UndefinedAccess(String, String),
    CantAccess(String, String, &'static str),
    VariablePathInvalidTy,
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
            IdentificationErrorKind::UndefinedAccess(base, name) => write!(out, "Undefined symbol '{base}::{name}'"),
            IdentificationErrorKind::CantAccess(base, name, ty) => write!(out, "Can't access item '{name}' on '{base}' ({ty})"),
            IdentificationErrorKind::VariablePathInvalidTy => write!(out, "Variable path must resolve to a definition"),
        }
    }
}

#[cfg(test)]
mod test;
