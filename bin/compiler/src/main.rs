use std::io::stderr;
use std::process::exit;
use std::{env, fs, io::{stdin, Read}, process};

pub mod config;
use config::{Config, Target};
use error_manager::ErrorManager;
use lexer::Lexer;
use parser::parse;
use semantic::Semantic;

fn step_emit(text: &str, em: &ErrorManager) {
    em.print_warnings(text,  &mut stderr().lock()).unwrap();

    if em.n_errors() > 0 {
        em.print_errors(text,  &mut stderr().lock()).unwrap();
        process::exit(1);
    }
}

fn process(text: &str, _target: Target) -> String {
    let mut lexer_errs = ErrorManager::new();
    let mut parse_errs = ErrorManager::new();

    let stream = Lexer::new(text, &mut lexer_errs).into_token_stream();
    let program = parse(stream, text, &mut parse_errs);

    step_emit(text, &lexer_errs);
    step_emit(text, &parse_errs);

    let program = program.unwrap();

    let mut em = ErrorManager::new();

    ast_validate::validate_ast(&program, &mut em);
    step_emit(text, &em);

    let hir_sess = hir::Session::default();
    ast_lowering::lower(&hir_sess, &program);

    let mut em = ErrorManager::new();
    hir_passes::identify(&hir_sess, &mut em);
    step_emit(text, &em);

    let semantic = Semantic::default();
    hir_typecheck::type_checking(&hir_sess, &mut em, &semantic);

    let program = hir_sess.get_root_program();

    #[cfg(debug_assertions)]
    eprintln!("\
        ================================================================================
        {program:#?}
        ================================================================================");

    step_emit(text, &em);

    "".to_string()
}

fn main() {
    let conf = Config::parse(env::args());
    for file in &conf.files {
        let text = fs::read_to_string(file).unwrap_or_else(|err| {
            eprintln!("Error reading \"{file}\": {err}");
            process::exit(1);
        });
        let out = process(&text, conf.target);

        let fname = conf.out_file.clone().unwrap_or_else(|| {
            let ext = file.char_indices().rev().find(|&(_,c)| c == '.').map(|(i,_)| i).unwrap_or(0);
            let start = &file[..ext];
            format!("{start}.out")
        });

        fs::write(&fname, out).unwrap();
        println!("Program written to {fname}");
    }
    if conf.files.is_empty() {
        let mut text = String::new();
        stdin().read_to_string(&mut text).unwrap_or_else(|err| {
            eprintln!("Error reading stdin: {err}");
            exit(1)
        });
        let out = process(&text, conf.target);
        match conf.out_file {
            Some(f) => {
                fs::write(&f, out).unwrap();
                println!("Program written to {f}");
            },
            None => println!("{out}")
        }
    }
}

