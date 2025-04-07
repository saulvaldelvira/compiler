use ast::expr::{BinaryExprOp, ExpressionKind, LitValue};
use error_manager::ErrorManager;
use lexer::TokenStream;

use crate::Parser;

fn get_tokens<'l, 's>(src: &'s str, em: &'l mut ErrorManager) -> TokenStream<'l, 's> {
    let l = lexer::Lexer::new(src, em);
    l.into_token_stream()
}

fn with_parser<R>(src: &str, f: impl FnOnce(Parser) -> R) -> R {
    f(
        Parser {
            em: &mut ErrorManager::new(),
            src,
            stream: get_tokens(src, &mut ErrorManager::new())
        }
    )
}

#[test]
fn parse_expr() {
    let src = "1 + 2 * 5";
    let expr = with_parser(src, |mut p| p.expression()).unwrap();

    let ExpressionKind::Binary { op, left, right } = expr.kind else {
        panic!("Expected binary");
    };

    assert!(matches!(op.val, BinaryExprOp::Add));
    let ExpressionKind::Literal(lit) = left.kind else {
        panic!("Expected literal");
    };

    assert_eq!(lit.val, LitValue::Int(1));

    let ExpressionKind::Binary { op, left, right } = right.kind else {
        panic!("Expected binary");
    };

    assert!(matches!(op.val, BinaryExprOp::Mul));

    let ExpressionKind::Literal(lit) = left.kind else {
        panic!("Expected literal");
    };
    assert_eq!(lit.val, LitValue::Int(2));

    let ExpressionKind::Literal(lit) = right.kind else {
        panic!("Expected literal");
    };
    assert_eq!(lit.val, LitValue::Int(5));
}
