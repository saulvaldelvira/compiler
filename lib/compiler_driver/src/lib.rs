use std::io::{self, stderr, stdin, Read};
use std::path::Path;

use error_manager::ErrorManager;
use lexer::Lexer;
use semantic::Semantic;

pub struct Compiler {
    source: String,
}

fn step_emit(text: &str, em: &ErrorManager) -> Option<()> {
    em.print_warnings(text,  &mut stderr().lock()).unwrap();

    if em.has_errors() {
        em.print_errors(text,  &mut stderr().lock()).unwrap();
        None
    } else {
        Some(())
    }
}

impl Compiler {
    pub fn from_filename<P: AsRef<Path>>(fname: P) -> io::Result<Self> {
        let source = std::fs::read_to_string(fname)?;
        Ok(Self {
            source
        })
    }

    pub fn from_stdin() -> io::Result<Self> {
        let mut source = String::new();
        stdin().read_to_string(&mut source)?;
        Ok(Self {
            source
        })
    }

    pub fn process(&self) -> Option<hir::Session<'_>> {
        let mut lexer_errs = ErrorManager::new();
        let mut parse_errs = ErrorManager::new();

        let stream = Lexer::new(&self.source, &mut lexer_errs).into_token_stream();
        let program = parser::parse(stream, &self.source, &mut parse_errs);

        step_emit(&self.source, &lexer_errs)?;
        step_emit(&self.source, &parse_errs)?;

        let program = program.unwrap();

        let hir_sess = hir::Session::default();
        ast_lowering::lower(&hir_sess, &program);

        let mut em = ErrorManager::new();
        hir_passes::identify(&hir_sess, &mut em);
        step_emit(&self.source, &em)?;

        let semantic = Semantic::default();
        hir_typecheck::type_checking(&hir_sess, &mut em, &semantic);

        step_emit(&self.source, &em)?;

        Some(hir_sess)
    }

}
