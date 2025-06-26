#![feature(test)]
extern crate test;
use core::cell::RefCell;

use error_manager::ErrorManager;
use parser::parse;
use span::SourceMap;
use test::Bencher;

#[bench]
fn bench(b: &mut Bencher) {
    const INPUT: &str = "
fn main() {
    let a = 12;
    let b = 23;

    if (a > 5) {
        b = a + b;
    } else {
        while (a <= 5) {
            b = a * b;
            if (12) {
                let a = 12;
                let b = 23;

                if (a > 5) {
                    b = a + b;
                } else {
                    while (a <= 5) {
                        b = a * b;
                    }
                }
            }
        }
    }
}
";

    b.iter(move || {
        let mut source = SourceMap::default();
        let (text, id) = source.add_file_anon(INPUT.into()).into_parts();

        parse(&text, id, &RefCell::new(source), &mut ErrorManager::new());
    });
}
