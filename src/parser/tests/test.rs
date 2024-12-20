use lexer::Lexer;
use parser::Parser;

fn find_errors(src: &str) -> u32 {
    let lexer = Lexer::new(src);
    let tokens = lexer.tokenize().expect("");
    /* We shouldn't test lexer here
       Only test the parser phase */
    /* assert!(!lexer.has_errors()); */

    let parser = Parser::new(&tokens, src);
    match parser.parse() {
        Ok(_) => 0,
        Err(n) => n
    }
}

#[test]
fn valid() {
    const INPUT: &str = r#"
var a = 12;
var b = 23;

if (a > 5) {
    b = a + b;
} else {
    while (a <= 5) {
        b = a * b;
    }
}
"#;

    assert_eq!(find_errors(INPUT), 0);
}

#[test]
fn invalid() {
    const INPUT: &str = r#"
var a = 12;
var b = 23

var c = 1;

if (a > 5) {
    b = a + b;
} else
    while (a <= 5) {
        b = a * b;
    }
}
"#;

    assert_eq!(find_errors(INPUT), 2);
}
