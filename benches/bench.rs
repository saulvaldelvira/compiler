#![feature(test)]
extern crate test;
use compiler::lexer::Lexer;
use test::Bencher;

#[bench]
fn lexer(b: &mut Bencher) {
    let input = "\
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
    let mut text = input.to_string();
    for _ in 0..10000 {
        text.push_str(input);
    }
    let mut lexer = Lexer::new();
    b.iter(|| {
        let _tokens = lexer.tokenize(&text);
    });
}
