#![feature(test)]
extern crate test;
use test::Bencher;

use lexer::Lexer;
use parser::Parser;

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

    let tokens = Lexer::new(INPUT).tokenize().unwrap();
    b.iter(move || {
        Parser::new(&tokens, INPUT).parse().unwrap();
    })
}

