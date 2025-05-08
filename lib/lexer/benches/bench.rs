#![feature(test)]
extern crate test;
use std::hint;

use error_manager::ErrorManager;
use lexer::Lexer;
use test::Bencher;

#[bench]
fn lexer(b: &mut Bencher) {
    const INPUT: &str = "\
/* Hello world program to
   test my compiler! :) */

int main(){
    print(\"Hello world!\\n\");
    float f = 1.1;
    let non_ascii = \"ɞɝセソɮɸツ\";
    return 0; // Inline comment
}
// UTF8 characters: ɞ
// Unc@mm3nt this line to trigger an error
";

    b.iter(|| {
        Lexer::new(INPUT, &mut ErrorManager::new())
            .into_token_stream()
            .for_each(|token| {
                let _ = hint::black_box(|| token);
            });
    });
}
