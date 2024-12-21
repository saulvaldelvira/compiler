use std::env::Args;

pub struct Config {
    files: Vec<String>,
}

impl Config {
    pub fn parse(args: Args) -> Self {
        let mut conf = Self{
            files: Vec::new(),
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
}
