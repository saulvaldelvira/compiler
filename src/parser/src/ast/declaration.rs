use builders::AsBox;
use super::Expr;

#[derive(AsBox)]
pub enum Declaration {
    VariableDecl { name: String, init: Option<Expr> },
}

