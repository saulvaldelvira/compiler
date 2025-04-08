#![feature(test)]
extern crate test;
use error_manager::ErrorManager;
use test::Bencher;

use lexer::Lexer;
use parser::parse;

#[bench]
fn bench(b: &mut Bencher) {
    const INPUT: &str = r#"
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
"#;

    b.iter(move || {
        let mut em = ErrorManager::new();
        let stream = Lexer::new(INPUT, &mut em)
                           .into_token_stream();
        parse(stream, INPUT, &mut ErrorManager::new());
    })
}

