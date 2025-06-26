#![feature(test)]
use std::fs;
use std::rc::Rc;

use compiler_driver::{Compiler, Emit};
use span::{FileName, SourceMap};
use test::Bencher;
extern crate test;

const INPUT: &str = "
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
";

#[bench]
fn bench_compilation(b: &mut Bencher) {
    b.iter(|| {
        let mut source = SourceMap::default();
        source.add_file(FileName::Anon, Rc::from(INPUT));
        let comp = Compiler::new(source);
        comp.process(Emit::Mapl);
    });
}

#[bench]
fn bench_huge(b: &mut Bencher) {
    let mut src = Rc::<str>::from(INPUT);

    for i in 0..100 {
        src = format!("\n mod _nested_module_A{i}_ {{ \n {src} \n }}  {INPUT} ").into();
    }

    fs::write("/tmp/s", &*src).unwrap();

    b.iter(|| {
        let mut source = SourceMap::default();
        source.add_file(FileName::Anon, Rc::clone(&src));
        let comp = Compiler::new(source);
        comp.process(Emit::Mapl);
    });
}
