use error_manager::ErrorManager;
use hir::visitor::Visitor;
use identification::Identification;

mod identification;

pub fn identify(sess: &hir::Session<'_>, em: &mut ErrorManager) {
    let prog = sess.get_root_program();
    let mut ident = Identification::new(sess, em);
    ident.visit_program(prog);
}
