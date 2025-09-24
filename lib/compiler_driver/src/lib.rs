use core::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::{
    io::{self, Read, stderr, stdin},
    path::Path,
};

use codegen_llvm::Codegen;
use error_manager::ErrorManager;
use semantic::Semantic;
use span::source::{FileId, FileName, SourceMap};

#[derive(Clone, Copy, Debug)]
pub enum Emit {
    Hir,
    LlvmIr,
    Mapl,
    Asm,
    Bin,
}

pub struct Compiler {
    source: RefCell<SourceMap>,
}

fn step_emit(text: &SourceMap, em: &mut ErrorManager) -> Option<()> {
    em.print_warnings(text, &mut stderr().lock()).unwrap();
    em.clear_warnings();

    if em.has_errors() {
        em.print_errors(text, &mut stderr().lock()).unwrap();
        None
    } else {
        Some(())
    }
}

pub enum Output {
    Hir(String),
    Mapl(String),
    LlvmIr(HashMap<FileId, String>),
    ForBin(HashMap<FileId, String>),
    Asm(HashMap<FileId, String>),
}

impl Compiler {

    pub fn new(source: SourceMap) -> Self {
        Self { source: source.into() }
    }

    pub fn source(&self) -> &RefCell<SourceMap> {
        &self.source
    }

    /// Creates a new compiler instance for the given filename
    ///
    /// # Errors
    /// If it fails to read the given path
    pub fn from_filename<P: AsRef<Path>>(fname: P) -> io::Result<Self> {
        let contents = std::fs::read_to_string(&fname)?;
        let mut source = SourceMap::default();
        source.add_file(FileName::Path(fname.as_ref().into()), contents.into());
        Ok(Self::new(source))
    }

    pub fn from_string(src: impl Into<Rc<str>>) -> Self {
        let mut source = SourceMap::default();
        source.add_file_annon(src.into());
        Self::new(source)
    }

    /// Creates a new compiler instance for the contents of the standard input
    ///
    /// # Errors
    /// If it fails to read stdin
    pub fn from_stdin() -> io::Result<Self> {
        let mut contents = String::new();
        stdin().read_to_string(&mut contents)?;
        let mut source = SourceMap::default();
        source.add_file(FileName::Stdin, contents.into());
        Ok(Self::new(source))
    }

    pub fn generate_ast(&self) -> Option<ast::Module> {
        let mut em = ErrorManager::new();

        // FIXME: This is BAD
        let (src, id) = self.source.borrow().get_file_for_offset(0).unwrap().into_parts();
        let program = parser::parse(&src, id, &self.source, &mut em);

        step_emit(&self.source.borrow(), &mut em)?;

        let program = program.unwrap();

        let mut em = ErrorManager::new();

        ast_validate::validate_ast(&program, &mut em);
        step_emit(&self.source.borrow(), &mut em)?;

        Some(program)
    }

    pub fn generate_hir<'hir>(&self, root: &ast::Module) -> hir::Session<'hir> {
        let hir_sess = hir::Session::default();
        ast_lowering::lower(&hir_sess, root);
        hir_sess
    }

    pub fn compile(&self) -> Option<(hir::Session<'_>, semantic::Semantic<'_>)> {
        let program = self.generate_ast()?;

        let hir_sess = self.generate_hir(&program);

        let mut em = ErrorManager::new();

        hir_identification::identify(&hir_sess, &self.source.borrow(), &mut em);
        step_emit(&self.source.borrow(), &mut em)?;

        let semantic = Semantic::default();
        hir_typecheck::type_checking(&hir_sess, &mut em, &semantic);
        step_emit(&self.source.borrow(), &mut em)?;

        /* #[cfg(debug_assertions)] */
        /* eprintln!( */
        /*     "\ */
/* ================================================================================ */
/* {:#?} */
/* ================================================================================", */
        /*     hir_sess.get_root() */
        /* ); */

        Some((hir_sess, semantic))
    }

    pub fn check(&self) -> Option<()> {
        self.compile().map(|_| ())
    }

    pub fn process(&self, emit: Emit) -> Option<Output> {
        let (hir_sess, semantic) = self.compile()?;
        Some(match emit {
            Emit::Hir => Output::Hir(hir_print::hir_print_html(&hir_sess, &semantic, &self.source.borrow())),
            Emit::Asm | Emit::Bin | Emit::LlvmIr => {
                let codegen = Codegen::new();
                let modules = codegen.codegen(&hir_sess, &semantic, &self.source.borrow());
                let mut out = HashMap::new();
                for (k, v) in modules {
                    out.insert(k, v.to_string());
                }

                match emit {
                    Emit::Bin => Output::ForBin(out),
                    Emit::LlvmIr => Output::LlvmIr(out),
                    Emit::Asm => Output::Asm(out),
                    _ => unreachable!()
                }
            }
            Emit::Mapl => {
                Output::Mapl(
                    mapl_codegen::gen_code_mapl(
                        &hir_sess,
                        &semantic,
                        &self.source.borrow()
                    )
                )
            }
        })
    }
}
