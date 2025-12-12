use core::cell::RefCell;

use error_manager::ErrorManager;
use parser::parse;
use span::source::SourceMap;

fn find_errors(src: &str) -> usize {
    let mut source = SourceMap::default();
    let (src, id) = source.add_file_annon(src.into()).into_parts();
    let source = RefCell::new(source);

    let mut em = ErrorManager::new();
    parse(&src, id, &source, &mut em);
    em.n_errors()
}

#[test]
fn valid() {
    const INPUT: &str = r"
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
";

    assert_eq!(find_errors(INPUT), 0);
}

#[test]
fn invalid() {
    const INPUT: &str = r"
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

";

    assert_eq!(find_errors(INPUT), 3);
}
