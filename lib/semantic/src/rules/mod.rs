use error_manager::ErrorManager;

pub mod expr;
pub mod stmt;

pub trait SemanticRule {
    type Result;

    fn apply(&self, sem: &crate::Semantic<'_>, em: &mut ErrorManager) -> Self::Result;
}

