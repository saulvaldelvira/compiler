use std::{env, fs, process};

pub mod config;
use compiler_driver::Compiler;
use config::Config;

fn main() {
    let conf = Config::parse(env::args());
    for file in &conf.files {
        let comp = Compiler::from_filename(file).unwrap_or_else(|err| {
            eprintln!("Error reading \"{file}\": {err}");
            process::exit(1);
        });
        let Some(prog) = comp.process() else { continue };
        let fname = conf.out_file.clone().unwrap_or_else(|| {
            let ext = file.char_indices().rev().find(|&(_,c)| c == '.').map(|(i,_)| i).unwrap_or(0);
            let start = &file[..ext];
            format!("{start}.out")
        });
        fs::write(&fname, prog).unwrap();
        println!("Program written to {fname}");
    }
    if conf.files.is_empty() {
        let comp = Compiler::from_stdin().unwrap_or_else(|err| {
            eprintln!("Error reading stdin: {err}");
            process::exit(1);
        });
        let Some(out) = comp.process() else { return };
        match conf.out_file {
            Some(f) => {
                fs::write(&f, out).unwrap();
                println!("Program written to {f}");
            },
            None => println!("{out}")
        }
    }
}

