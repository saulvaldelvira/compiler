use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Command;
use std::{env, fs, process};

pub mod config;
use compiler_driver::{Compiler, Output};
use config::Config;
use span::source::{FileId, FileName};

fn gen_llvm(comp: &Compiler, mods: &HashMap<FileId, String>) {
    let src = comp.source().borrow();
    for (id, code) in mods {
        let file = src.get_file_for_id(id).unwrap();
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

fn gen_asm(comp: &Compiler, mods: &HashMap<FileId, String>) {
    gen_llvm(comp, mods);

    for f in comp.source().borrow().files() {
        let mut path = PathBuf::from(f.path().unwrap());
        path.set_extension("ll");
        Command::new("llc")
            .arg(path)
            .spawn()
            .unwrap()
            .wait()
            .unwrap();
    }
}

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
                gen_llvm(&comp, &modules);
            }
            Output::Asm(modules) => {
                gen_asm(&comp, &modules);
            }
            Output::ForBin(modules) => {
                gen_llvm(&comp, &modules);

                let out = conf.out_file.as_deref().unwrap_or("a.out");

                let mut gcc = Command::new("clang");
                gcc.args(["-Wno-override-module", "-o", out]);

                for file in comp.source().borrow().files() {
                    let mut path = PathBuf::from(file.path().unwrap());
                    path.set_extension("ll");
                    gcc.arg(path);
                }

                gcc.spawn().unwrap().wait().unwrap();
                println!("Program written to {out}");

                for file in comp.source().borrow().files() {
                    let mut path = PathBuf::from(file.path().unwrap());

                    path.set_extension("ll");
                    fs::remove_file(path).unwrap();
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
