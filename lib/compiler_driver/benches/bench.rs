#![feature(test)]
use std::fs;

use compiler_driver::{Compiler, Emit};
use test::Bencher;
extern crate test;

const INPUT: &str = r#"
    mod abc {
        fn lol() -> int {
            return 1;
        }

        mod def {
            fn jej(i: int, c: char) -> bool {
                return false;
            }
        }

        mod abc2 {
            fn lol() -> int {
                return 1;
            }

            mod def {
                fn jej(i: int, c: char) -> bool {
                    return false;
                }
            }

            fn pium() -> bool {
                return def::jej(12, 'a');
            }
        }

        fn pium() -> bool {
            return abc2::def::jej(12, 'a');
        }

    }

    fn main() {
        let a: int = abc::lol();

        if (abc::pium()) {
            while (abc::def::jej(a, '2')) {
                print 'a';
            }
        } else {

        }
    }
"#;

#[bench]
fn bench_compilation(b: &mut Bencher) {
    b.iter(|| {
        let comp = Compiler::from_string(INPUT).unwrap();
        comp.process(Emit::Mapl);
    });
}

#[bench]
fn bench_huge(b: &mut Bencher) {
    let mut src = String::from(INPUT);

    for i in 0..100 {
        src = format!("\n mod _nested_module_A{i}_ {{ \n {src} \n }}  {INPUT} ");
    }

    fs::write("/tmp/s", &src).unwrap();


    b.iter(|| {
        let comp = Compiler::from_string(&src).unwrap();
        comp.process(Emit::Mapl);
    });
}
