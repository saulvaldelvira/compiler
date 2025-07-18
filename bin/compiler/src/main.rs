use std::path::{Path, PathBuf};
use std::{env, fs, process};

pub mod config;
use compiler_driver::{Compiler, Output};
use config::Config;
use span::source::FileName;

fn main() {
    let conf = Config::parse(env::args());
    for file in &conf.files {
        let comp = Compiler::from_filename(file).unwrap_or_else(|err| {
            eprintln!("Error reading \"{file}\": {err}");
            process::exit(1);
        });
        if conf.check {
            comp.check().unwrap_or_else(|| {
                eprintln!("Check failed");
                process::exit(1);
            });
            process::exit(0);
        }
        let Some(prog) = comp.process(conf.emit) else {
            continue;
        };


        match prog {
            Output::Hir(prog) |
            Output::Mapl(prog) => {
                let fname = conf.out_file.clone().map(PathBuf::from).unwrap_or_else(|| {
                    let mut path = PathBuf::from(file);
                    path.set_extension(conf.get_extension());
                    path
                });
                fs::write(&fname, prog).unwrap();
                println!("Program written to {}", fname.display());
            }
            Output::LlvmIr(modules) => {
                let src = comp.source().borrow();
                for (id, code) in modules {
                    let file = src.get_file_for_id(&id).unwrap();
                    match &file.fname {
                        FileName::Path(path) => {
                            let mut path = path.clone();
                            path.set_extension("ll");
                            fs::write(path, code).unwrap();
                        },
                        FileName::Stdin => todo!(),
                        FileName::Annon => todo!(),
                    }

                }
            }
        }

    }
    if conf.files.is_empty() {
        /* let comp = Compiler::from_stdin().unwrap_or_else(|err| { */
        /*     eprintln!("Error reading stdin: {err}"); */
        /*     process::exit(1); */
        /* }); */
        /* if conf.check { */
        /*     comp.check().unwrap_or_else(|| { */
        /*         eprintln!("Check failed"); */
        /*         process::exit(1); */
        /*     }); */
        /*     process::exit(0); */
        /* } */
        /* let Some(out) = comp.process(conf.emit) else { */
        /*     return; */
        /* }; */
        /* match conf.out_file { */
        /*     Some(f) => { */
        /*         fs::write(&f, out).unwrap(); */
        /*         println!("Program written to {f}"); */
        /*     } */
        /*     None => println!("{out}"), */
        /* } */
    }
}
