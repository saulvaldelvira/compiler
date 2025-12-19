use std::env;
use std::{env::Args, process};

use compiler_driver::Emit;

pub struct Config {
    pub files: Vec<String>,
    pub out_file: Option<String>,
    pub emit: Emit,
    pub check: bool,
}

impl Config {

    #[must_use]
    pub fn parse(args: Args) -> Self {
        let mut conf = Self {
            files: Vec::new(),
            out_file: None,
            emit: Emit::Bin,
            check: false,
        };
        let mut args = args.skip(1);
        while let Some(arg) = args.next() {
            match arg.as_str() {
                /* Parse args */
                "-o" => {
                    conf.out_file = Some(args.next().unwrap_or_else(|| {
                        eprintln!("Missing argument for '-o'\n");
                        help();
                    }));
                }
                "-h" | "--help" => help(),
                "--check" => conf.check = true,
                "--emit" => {
                    let em = args.next().unwrap_or_else(|| {
                        eprintln!("Missing argument for '--emit'\n");
                        help();
                    });
                    match em.as_str() {
                        "mapl" => conf.emit = Emit::Mapl,
                        "llvm-ir" => conf.emit = Emit::LlvmIr,
                        "hir" => conf.emit = Emit::Hir,
                        "hir-json" => conf.emit = Emit::HirJson,
                        "asm" => conf.emit = Emit::Asm,
                        "bin" => conf.emit = Emit::Bin,
                        a => {
                            eprintln!("Unknown argument for '--emit': {a}\n");
                            help();
                        }
                    }
                }
                _ => conf.files.push(arg),
            }
        }
        conf
    }

    pub fn get_extension(&self) -> &'static str {
        match self.emit {
            Emit::Hir => "html",
            Emit::HirJson => "json",
            Emit::Mapl => "mapl",
            Emit::LlvmIr => "ll",
            Emit::Asm => "s",
            Emit::Bin => "out",
        }
    }
}

fn help() -> ! {
    let name = env::args().next();
    println!("\
USAGE: {name} <file1>..<fileN> [-o <output>] [--emit <output_type>] [--check]
OPTIONS:
    -o      Specify output file
    --emit  Set type of output
        hir: An html representation of the program
        llvm-ir: LLVM Intermediate representation
        asm: Assembly
        bin: Binary executable (default)
    --check  Only check, don't generate anything
    -h, --help  Print this help message and exit",
name = name.as_deref().unwrap_or("compiler")
);
    process::exit(1);
}
