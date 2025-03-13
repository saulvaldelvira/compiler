use ast::types::{Type, TypeKind};

pub trait SizeStrategy {
    const CALL_FRAME: usize;
    fn size_of(t: &Type) -> usize;
}

pub struct MaplSizeStrategy;

impl SizeStrategy for MaplSizeStrategy {
    const CALL_FRAME: usize = 4;

    fn size_of(t: &Type) -> usize {
        match t.kind {
            TypeKind::Int => 2,
            TypeKind::Float => 4,
            TypeKind::Bool => 1,
            TypeKind::Char => 1,
            TypeKind::Empty => 0,
            TypeKind::String => todo!(),
            TypeKind::Custom(_) => todo!(),
            _ => unreachable!()
        }
    }
}
