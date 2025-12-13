use ast::{
    Expression,
    expr::{BinaryExprOp, ExpressionKind},
};

pub trait Precedence {
    fn precedence(&self) -> u8;
}

impl Precedence for Expression {
    fn precedence(&self) -> u8 {
        match &self.kind {
            ExpressionKind::Binary { op, .. } => {
                match op.val {
                    BinaryExprOp::Assign => 1,
                    BinaryExprOp::Add | BinaryExprOp::Sub => 3,
                    BinaryExprOp::Mul | BinaryExprOp::Div | BinaryExprOp::Mod => 4,
                    BinaryExprOp::Gt
                    | BinaryExprOp::Ge
                    | BinaryExprOp::Lt
                    | BinaryExprOp::Le
                    | BinaryExprOp::Eq
                    | BinaryExprOp::Neq => 5,
                    BinaryExprOp::And | BinaryExprOp::Or => 6,
                }
            }
            ExpressionKind::If { .. } | ExpressionKind::Block(_) => 2,
            ExpressionKind::Unary { .. }
            | ExpressionKind::Paren(_)
            | ExpressionKind::Path(_)
            | ExpressionKind::Literal(_) => 9,
            ExpressionKind::Cast { .. } => 7,
            ExpressionKind::Call { .. } |
            ExpressionKind::ArrayAccess { .. } |
            ExpressionKind::TupleAccess { .. } |
            ExpressionKind::StructAccess { .. } => 8,
        }
    }
}
