use core::marker::PhantomData;
use core::mem;

use crate::TypedArena;

#[test]
fn zst() {
    #[derive(Clone,Copy)]
    struct Zero {
        _m: PhantomData<i32>,
    }
    #[allow(clippy::pedantic)]
    impl Zero {
        fn lol(&self) {
            println!("0");
        }
    }
    assert_eq!(mem::size_of::<Zero>(), 0);

    let arena = TypedArena::<Zero>::default();

    let mut refs = vec![];
    for _ in 0..999 {
        let single = arena.alloc(Zero { _m: PhantomData });
        refs.push(single);
    }

    let array = arena.alloc_iter((0..100).map(|_| Zero { _m: PhantomData}));
    assert_eq!(array.len(), 100);

    array[50].lol();
}
