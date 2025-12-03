use crate::Symbol;

#[test]
fn predefined_symbols() {
    let kwself = Symbol::new("self");
    assert_eq!(kwself, Symbol::KWSELF);

    let kwsuper = Symbol::new("super");
    assert_eq!(kwsuper, Symbol::KWSUPER);
}
