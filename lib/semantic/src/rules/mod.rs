use error_manager::ErrorManager;

pub mod expr;
pub mod stmt;

pub trait SemanticRule<'sem> {
    type Result;

    fn apply(&self, sem: &crate::Semantic<'sem>, em: &mut ErrorManager) -> Self::Result;
}

