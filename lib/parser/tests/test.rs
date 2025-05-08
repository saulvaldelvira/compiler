use error_manager::ErrorManager;
use lexer::Lexer;
use parser::parse;

fn find_errors(src: &str) -> usize {
    let mut em = ErrorManager::new();
    let stream = Lexer::new(src, &mut em).into_token_stream();
    /* We shouldn't test lexer here
    Only test the parser phase */
    /* assert!(!lexer.has_errors()); */

    let mut perr = ErrorManager::new();
    parse(stream, src, &mut perr);
    perr.n_errors()
}

#[test]
fn valid() {
    const INPUT: &str = r#"
fn main() {
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
"#;

    assert_eq!(find_errors(INPUT), 0);
}

#[test]
fn invalid() {
    const INPUT: &str = r#"
fn main() {
    let a = 12;
    let b = 23

    let c = 1;
    if (a > 5) {
        b = a + b;
    } else
        while (a <= 5) {
            b = a * b;
    }
}

fn foo() {
    let b = 12;

    if (;) {

    }
}

"#;

    assert_eq!(find_errors(INPUT), 2);
}
