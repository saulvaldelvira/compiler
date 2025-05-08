use std::{env::Args, process};

use compiler_driver::Emit;

pub struct Config {
    pub files: Vec<String>,
    pub out_file: Option<String>,
    pub emit: Emit,
}

impl Config {
    pub fn parse(args: Args) -> Self {
        let mut conf = Self {
            files: Vec::new(),
            out_file: None,
            emit: Emit::Mapl,
        };
        let mut args = args.skip(1);
        while let Some(arg) = args.next() {
            match arg.as_str() {
                /* Parse args */
                "-o" => {
                    conf.out_file = Some(args.next().unwrap_or_else(|| {
                        eprintln!("Missing argument for '-o'");
                        process::exit(1);
                    }))
                }
                "--emit" => {
                    let em = args.next().unwrap_or_else(|| {
                        eprintln!("Missing argument for '--emit'");
                        process::exit(1);
                    });
                    match em.as_str() {
                        "mapl" => conf.emit = Emit::Mapl,
                        "hir" => conf.emit = Emit::Hir,
                        a => {
                            eprintln!("Unknown argument for '--emit': {a}");
                            process::exit(1);
                        }
                    }
                }
                _ => conf.files.push(arg),
            }
        }
        conf
    }
}
