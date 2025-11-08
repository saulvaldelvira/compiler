use hir::{
    Type,
    types::{PrimitiveType, TypeKind},
};
use semantic::{Semantic, TypeLowering};

#[test]
fn unique_types() {
    let hir_sess = hir::Session::default();

    let int = hir_sess.alloc(Type::new(TypeKind::Primitive(PrimitiveType::I32)));

    let params = [
        TypeKind::Primitive(PrimitiveType::I32),
        TypeKind::Primitive(PrimitiveType::Char),
    ];

    let params = hir_sess.alloc_iter(params.into_iter().map(Type::new));

    let ret_ty = hir_sess.alloc(Type::new(TypeKind::Primitive(PrimitiveType::Empty)));

    let func = TypeKind::Function { is_variadic: false, params, ret_ty };
    let func = hir_sess.alloc(Type::new(func));

    let sem = Semantic::default();
    let mut tl = TypeLowering::new(&sem);

    let int = tl.lower_hir_type(int);
    let func = tl.lower_hir_type(func);

    let semantic::TypeKind::Function { params, .. } = func.kind else {
        panic!()
    };

    let func_int = params[0];

    assert_eq!(int.id, func_int.id);

    let manual_int =
        sem.get_or_intern_type(semantic::TypeKind::Primitive(semantic::PrimitiveType::I32));
    assert_eq!(int.id, manual_int.id);
}
