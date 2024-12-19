#![feature(test)]
extern crate test;
use test::Bencher;

use lexer::Lexer;
use parser::Parser;

#[bench]
fn bench(b: &mut Bencher) {
    const INPUT: &str = r#"
var a = 12;
var b = 23;

if (a > 5) {
    b = a + b;
} else {
    while (a <= 5) {
        b = a * b;
        if (12) {
            var a = 12;
            var b = 23;

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
"#;

    let mut lexer = Lexer::new(INPUT);
    let tokens = lexer.tokenize();
    b.iter(move || {
        let tok = tokens.clone();
        let mut parser = Parser::new(tok);
        parser.parse();
    })
}

