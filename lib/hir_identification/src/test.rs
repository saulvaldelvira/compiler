use compiler_driver::Compiler;
use error_manager::{ErrorManager, FilePosition};
use hir::expr::ExpressionKind;
use hir::stmt::StatementKind;
use hir::{Expression, Item, ItemKind};
use interner::Symbol;

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
fn simple_redefinition() {
    let input: &str = "
        let a: int;
        struct S {
            a: int, // OK
            b: char,
            a: char, // BAD
        }
        fn foo() {
            let S: int = 12; // OK
            print S;
        }
        struct foo { }
        struct GOOD {
            foo: int, // OK
        }
        ";

    let expected = vec![
        IdentificationErrorKind::Redefinition {
            name: "a".to_string(),
            node_type: "field",
            prev: FilePosition { start_line: 4, start_col: 13, end_line: 4, end_col: 19 }
        },
        IdentificationErrorKind::Redefinition {
            name: "foo".to_string(),
            node_type: "function",
            prev: FilePosition { start_line: 8, start_col: 9, end_line: 11, end_col: 10 }
        },
    ];

    Tester {
        input,
        expected,
    }.test();
}

#[test]
fn simple_ok() {
    let hir = Tester::all_ok("
 fn foo() { }
 fn main() {
     let a: int = 12;

     a = a + 1;

     let a: char = 'a';

     print a;

     foo();

     let foo: float = 1.2;
     foo = foo * 1.2;
 },");

    let root = hir.get_root();

    let main = root.find_item(Symbol::new("main")).unwrap();
    let ItemKind::Function { body, .. } = main.kind else {
        panic!();
    };

    let StatementKind::Item(Item { id: def1_id, .. }) = &body[0].kind else { panic!() };
    let assignment = body[1];
    let StatementKind::Expr(Expression { kind: ExpressionKind::Assignment { left, .. }, ..}) = assignment.kind else { panic!() };
    let ExpressionKind::Variable(path) = &left.kind else { panic!() };
    let def = path.def();
    assert_eq!(*def1_id, def.get().unwrap());

    let StatementKind::Item(Item { id: def2_id, .. }) = &body[2].kind else { panic!() };
    let StatementKind::Print(Expression { kind: ExpressionKind::Variable(path), .. }) = &body[3].kind else { panic!() };
    let def = path.def();
    assert_eq!(*def2_id, def.get().unwrap());

    assert_ne!(def1_id, def2_id);
}

pub struct Tester<'a> {
    input: &'a str,
    expected: Vec<IdentificationErrorKind>,
}

impl Tester<'_> {
    fn test<'hir>(&self) -> hir::Session<'hir> {
        let compiler = Compiler::from_string(self.input);
        let ast = compiler.generate_ast().expect("AST should generate correctly");
        let hir = compiler.generate_hir(&ast);

        let mut em = ErrorManager::new();

        crate::identify(&hir, compiler.source(), &mut em);

        let errors = em.errors_iterator_cast::<IdentificationError>();

        for (err, exp) in errors.zip(self.expected.iter()) {
            assert_eq!(&err.kind, exp);
        }

        hir
    }

    fn all_ok<'hir>(input: &str) -> hir::Session<'hir> {
        Tester { input, expected: vec![] }.test()
    }
}
