use std::io::{self, stderr, stdin, Read};
use std::path::Path;

use error_manager::ErrorManager;
use lexer::Lexer;
use semantic::Semantic;
use span::{FilePosition, Span};

#[derive(Clone,Copy,Debug)]
pub enum Emit {
    Hir,
    Mapl
}

pub struct Compiler {
    source: CompilerSource,
}

pub struct CompilerSource {
    filename: String,
    text: String,
}

impl CompilerSource {
    pub fn new(filename: String, text: String) -> Self {
        Self { filename, text }
    }

    pub fn filename(&self) -> &str { &self.filename }

    pub fn text(&self) -> &str { &self.text }

    pub fn get_file_position(&self, span: Span) -> FilePosition {
        span.file_position(&self.text)
    }
}

fn step_emit(text: &str, em: &mut ErrorManager) -> Option<()> {
    em.print_warnings(text,  &mut stderr().lock()).unwrap();
    em.clear_warnings();

    if em.has_errors() {
        em.print_errors(text,  &mut stderr().lock()).unwrap();
        None
    } else {
        Some(())
    }
}

impl Compiler {
    pub fn from_filename<P: AsRef<Path>>(fname: P) -> io::Result<Self> {
        let source = std::fs::read_to_string(&fname)?;
        Ok(Self {
            source: CompilerSource::new(fname.as_ref().to_str().unwrap().to_string(), source)
        })
    }

    pub fn from_stdin() -> io::Result<Self> {
        let mut source = String::new();
        stdin().read_to_string(&mut source)?;
        Ok(Self {
            source: CompilerSource::new("/dev/stdin".to_string(), source),
        })
    }

    pub fn process(&self, emit: Emit) -> Option<String> {
        let mut lexer_errs = ErrorManager::new();
        let mut parse_errs = ErrorManager::new();

        let stream = Lexer::new(&self.source.text, &mut lexer_errs).into_token_stream();
        let program = parser::parse(stream, &self.source.text, &mut parse_errs);

        step_emit(&self.source.text, &mut lexer_errs)?;
        step_emit(&self.source.text, &mut parse_errs)?;

        let program = program.unwrap();

        let mut em = ErrorManager::new();

        ast_validate::validate_ast(&program, &mut em);
        step_emit(&self.source.text, &mut em)?;

        let hir_sess = hir::Session::default();
        ast_lowering::lower(&hir_sess, &program);

        hir_passes::identify(&hir_sess, &mut em);
        step_emit(&self.source.text, &mut em)?;

        #[cfg(debug_assertions)]
        eprintln!("\
================================================================================
{:#?}
================================================================================",
        hir_sess.get_root_program());

        let semantic = Semantic::default();
        hir_typecheck::type_checking(&hir_sess, &mut em, &semantic);
        step_emit(&self.source.text, &mut em)?;

        Some(match emit {
            Emit::Hir => {
                hir_serialize::hir_serialize(&hir_sess, &semantic, &self.source.text)
            }
            Emit::Mapl => {
                mapl_codegen::gen_code_mapl(&hir_sess, &semantic, &self.source.text, &self.source.filename)
            },
        })

    }

}
