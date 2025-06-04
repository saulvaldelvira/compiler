use error_manager::ErrorManager;
use hir::visitor::Visitor;
use identification::Identification;

mod identification;

pub fn identify(sess: &hir::Session<'_>, source: &str, em: &mut ErrorManager) {
    let prog = sess.get_root();
    let mut ident = Identification::new(sess, source, em);
    ident.visit_module(prog);
}
