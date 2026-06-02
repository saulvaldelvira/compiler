use core::cell::RefCell;

use error_manager::ErrorManager;
use parser::error::{ParseError, ParseErrorKind};
use parser::parse;
use span::Span;
use span::source::SourceMap;

fn find_errors(src: &str, it: &[ParseError]) {
    let mut source = SourceMap::default();
    let (src, id) = source.add_file_annon(src.into()).into_parts();
    let source = RefCell::new(source);

    let mut em = ErrorManager::new();
    parse(&src, id, &source, &mut em);
    let mut it = it.iter();
    for err in em.errors_iterator_cast::<ParseError>() {
        let exp = it.next().unwrap_or_else(|| {
            panic!("Expected error: {err:#?}");
        });
        let is_ok = match (&err.kind, &exp.kind) {
            (ParseErrorKind::ExpectedToken { tokens: l_tokens, found: l_found },
                ParseErrorKind::ExpectedToken { tokens: r_tokens, found: r_found }) => l_tokens == r_tokens && l_found == r_found,
            (ParseErrorKind::ExpectedNode(l0), ParseErrorKind::ExpectedNode(r0)) => l0 == r0,
            (ParseErrorKind::ExpectedConstruct { expected: l_expected, found: l_found },
                ParseErrorKind::ExpectedConstruct { expected: r_expected, found: r_found }) => l_expected == r_expected && l_found == r_found,
            (ParseErrorKind::InvalidBinaryOp(l0), ParseErrorKind::InvalidBinaryOp(r0))
            | (ParseErrorKind::InvalidUnaryOp(l0), ParseErrorKind::InvalidUnaryOp(r0)) => l0 == r0,
            (ParseErrorKind::InvalidEscape(l0), ParseErrorKind::InvalidEscape(r0)) => l0 == r0,
            (ParseErrorKind::ReadFile(la, lb), ParseErrorKind::ReadFile(ra, rb)) => la == ra && lb.to_string() == rb.to_string(),
            _ => core::mem::discriminant(&err.kind) == core::mem::discriminant(&exp.kind),
        };
        assert!(is_ok, "Expected {exp:#?} got {err:#?}");
        assert!(err.span == exp.span, "Expected {:?} got {:?}", exp.span, err.span);
    }
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

    find_errors(INPUT, &[]);
}

#[test]
fn invalid() {
    const INPUT: &str = r"
a + 1
fn main() {
    1 return 1;

    foo(


}
    ";

    find_errors(INPUT, &[
        ParseError {
            kind: ParseErrorKind::ExpectedNode("item"),
            span: Span { offset: 1, len: 1 }
        },
        ParseError {
            kind: ParseErrorKind::MissingSemmicolon,
            span: Span { offset: 23, len: 1 },
        },
        ParseError {
            kind: ParseErrorKind::ExpectedNode(
                "Expression",
            ),
            span: Span { offset: 47, len: 1 },
        },
        ParseError {
            kind: ParseErrorKind::CantPeek,
            span: Span { offset: 47, len: 1 },
        }
    ]);
}
