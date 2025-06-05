use compiler_driver::Compiler;
use error_manager::ErrorManager;

use crate::{IdentificationError, IdentificationErrorKind};

#[test]
fn simple_main_undefined() {
    let input: &str = "fn main() {
        a = 12;
        lol();
    }";

    let expected = vec![
        IdentificationErrorKind::Undefined("a".to_string()),
        IdentificationErrorKind::Undefined("lol".to_string()),
    ];

    Tester {
        input,
        expected,
    }.test();
}

#[test]
fn simple_main_ok() {
    Tester::all_ok("fn main() {
        let a: int = 12;

        a = 12;

        let a: char;
        a = 'a';

        print a;
    }");
}

pub struct Tester<'a> {
    input: &'a str,
    expected: Vec<IdentificationErrorKind>,
}

impl Tester<'_> {
    fn test(self) {
        let compiler = Compiler::from_string(self.input);
        let ast = compiler.generate_ast().unwrap();
        let hir = hir::Session::default();
        compiler.generate_hir(&ast, &hir);

        let mut em = ErrorManager::new();
        crate::identify(&hir, self.input, &mut em);

        let errors = em.errors_iterator_cast::<IdentificationError>();

        for (err, exp) in errors.zip(self.expected.into_iter()) {
            assert_eq!(err.kind, exp);
        }
    }

    fn all_ok(input: &str) {
        Tester { input, expected: vec![] }.test();
    }
}
