use std::env::Args;

#[derive(Clone, Copy)]
pub enum Op {
    Compile,
    Print,
}

pub struct Config {
    files: Vec<String>,
    op: Op,
}

impl Config {
    pub fn parse(args: Args) -> Self {
        let mut conf = Self{
            files: Vec::new(),
            op: Op::Compile,
        };
        for arg in args.skip(1) {
            match arg.as_str() {
                /* Parse args */
                "-print" => conf.op = Op::Print,
                _ => conf.files.push(arg),
            }
        }
        conf
    }
    pub fn op(&self) -> Op { self.op }
    pub fn files(&self) -> &[String] {
        &self.files
    }
}
