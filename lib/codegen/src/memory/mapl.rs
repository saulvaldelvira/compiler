use ast::types::{Type, TypeKind};

use super::SizeStrategy;

pub struct MaplSizeStrategy;

impl SizeStrategy for MaplSizeStrategy {
    const CALL_FRAME: usize = 4;

    fn size_of(t: &Type) -> usize {
        match &t.kind {
            TypeKind::Int => 2,
            TypeKind::Ref(_) => 2,
            TypeKind::Float => 4,
            TypeKind::Bool => 1,
            TypeKind::Char => 1,
            TypeKind::Empty => 0,
            TypeKind::Array(array) => Self::size_of(&array.of) * array.length,
            TypeKind::String => todo!(),
            TypeKind::Struct(st) => {
                st.decl.unwrap()
                  .fields.iter()
                  .map(|field| Self::size_of(&field.ty))
                  .sum()
            }
            TypeKind::Error(_) => unreachable!(),
        }
    }
}

