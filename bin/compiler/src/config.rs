use std::env::Args;

#[derive(Clone, Copy)]
pub enum Target {
    Mapl,
}

pub struct Config {
    files: Vec<String>,
    target: Target,
}

impl Config {
    pub fn parse(args: Args) -> Self {
        let mut conf = Self{
            files: Vec::new(),
            target: Target::Mapl,
        };
        for arg in args.skip(1) {
            match arg.as_str() {
                /* Parse args */
                "" => {},
                _ => conf.files.push(arg),
            }
        }
        conf
    }
    pub fn files(&self) -> &[String] {
        &self.files
    }
    pub fn target(&self) -> Target { self.target }
}
