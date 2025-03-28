use std::{env, process};

pub mod config;
use compiler_driver::Compiler;
use config::Config;

fn run_compiler(comp: &Compiler) {
    comp.process().inspect(|_sess| {
        #[cfg(debug_assertions)]
        eprintln!("\
            ================================================================================
            {:#?}
            ================================================================================",
            _sess.get_root_program()
        );
    });
}

fn main() {
    let conf = Config::parse(env::args());
    for file in &conf.files {
        let comp = Compiler::from_filename(file).unwrap_or_else(|err| {
            eprintln!("Error reading \"{file}\": {err}");
            process::exit(1);
        });
        run_compiler(&comp);
    }
    if conf.files.is_empty() {
        let comp = Compiler::from_stdin().unwrap_or_else(|err| {
            eprintln!("Error reading stdin: {err}");
            process::exit(1);
        });
        run_compiler(&comp);
    }
}

