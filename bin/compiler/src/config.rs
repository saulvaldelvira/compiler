use std::env::Args;
use std::process;

#[derive(Clone, Copy)]
pub enum Target {
    Mapl,
}

pub struct Config {
    pub files: Vec<String>,
    pub target: Target,
    pub out_file: Option<String>,
}

impl Config {
    pub fn parse(args: Args) -> Self {
        let mut conf = Self{
            files: Vec::new(),
            target: Target::Mapl,
            out_file: None,
        };
        let mut args = args.skip(1);
        while let Some(arg) = args.next() {
            match arg.as_str() {
                /* Parse args */
                "-o" => conf.out_file = Some(args.next().unwrap_or_else(|| {
                    eprintln!("Missing argument for '-o'");
                    process::exit(1);
                })),
                _ => conf.files.push(arg),
            }
        }
        conf
    }
}
