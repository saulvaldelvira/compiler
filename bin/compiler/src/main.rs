use std::path::{Path, PathBuf};
use std::process::Command;
use std::{env, fs, process};

pub mod config;
use compiler_driver::{Compiler, Output};
use config::Config;
use span::source::FileName;

fn main() {
    let mut tmp_dir = {
        let tmp_path = ["/dev/shm", "/tmp"]
            .into_iter()
            .find(|f| fs::exists(f).is_ok_and(|v| v))
            .unwrap_or(".compiler_cache");
        let mut path = PathBuf::from(tmp_path);
        while path.exists() {
            path.push(process::id().to_string());
        }
        let _ = fs::create_dir(&path);
        path
    };


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
                for (id, code) in &modules {
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
            Output::Asm(modules) => {
                let src = comp.source().borrow();
                for (id, code) in &modules {
                    let file = src.get_file_for_id(id).unwrap();
                    match &file.fname {
                        FileName::Path(path) => {
                            let mut path = path.clone();
                            path.set_extension("S");

                            tmp_dir.push("tmp.ll");
                            fs::write(&tmp_dir, code).unwrap();
                            Command::new("llc")
                                .arg(&tmp_dir)
                                .arg("-o")
                                .arg(&path)
                                .spawn()
                                .unwrap()
                                .wait()
                                .unwrap();
                            tmp_dir.pop();
                        },
                        FileName::Stdin => todo!(),
                        FileName::Annon => todo!(),
                    }
                }
            }
            Output::ForBin(modules) => {
                let out = conf.out_file.clone()
                                      .unwrap_or_else(|| format!("a.{}", conf.get_extension()));

                let mut gcc = Command::new("clang");
                gcc.args(["-Wno-override-module", "-o", &out]);

                for (id, code) in &modules {
                    tmp_dir.push(id.as_usize().to_string());
                    tmp_dir.set_extension("ll");
                    fs::write(&tmp_dir, code).unwrap();
                    gcc.arg(&tmp_dir);
                    tmp_dir.pop();
                }

                gcc.spawn().unwrap().wait().unwrap();
                println!("Program written to {out}");
            }
        }

    }
    fs::remove_dir_all(tmp_dir).unwrap();
}
