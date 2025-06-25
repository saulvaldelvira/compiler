#![feature(test)]
extern crate test;
use error_manager::ErrorManager;
use lexer::Lexer;
use parser::parse;
use span::Source;
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
        let mut em = ErrorManager::new();

        let mut source = Source::default();
        let file = source.add_file_anon(INPUT.into());

        let stream = Lexer::new(file, &mut em).into_token_stream();
        parse(stream, file, &mut ErrorManager::new());
    });
}
