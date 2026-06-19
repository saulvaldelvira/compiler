use hir::{
    Type,
    types::{PrimitiveType, TypeKind},
};
use semantic::{Semantic, TypeLowering};
use span::Span;

#[test]
fn unique_types() {
    let hir_sess = hir::Session::default();

    let int = hir_sess.alloc(Type::new(TypeKind::Primitive(PrimitiveType::I32), Span::dummy()));

    let sem = Semantic::default();
    let mut tl = TypeLowering::new(&sem);

    let int = tl.lower_hir_type(int);

    let manual_int =
        sem.get_or_intern_type(semantic::TypeKind::Primitive(semantic::PrimitiveType::I32));
    assert_eq!(int.id, manual_int.id);
}
