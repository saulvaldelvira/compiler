use error_manager::ErrorManager;
use lexer::{token::TokenKind, Lexer};
use span::source::SourceMap;

#[test]
fn tokenize_test() {
    const INPUT: &str = "let a = 12 \n \t \r { \t   -> 1.2  \n 'a' '\\n' \"hiiii\" ";

    let mut em = ErrorManager::new();
    let mut source = SourceMap::default();
    let (file, id) = source.add_file_annon(INPUT.into()).into_parts();
    let stream = Lexer::new(&file, id, &mut em).into_token_stream();

    let expected = [
        TokenKind::Let,
        TokenKind::Identifier,
        TokenKind::Equal,
        TokenKind::IntLiteral,
        TokenKind::LeftBrace,
        TokenKind::Arrow,
        TokenKind::FloatLiteral,
    ];

    let idents = ["a", "12", "1.2", "'a'", "'\\n'", "\"hiiii\""];
    let mut ident_iter = idents.iter();

    for (act, &exp) in stream.zip(expected.iter()) {
        assert_eq!(act.kind, exp);
        if matches!(
            act.kind,
            TokenKind::Identifier
                | TokenKind::IntLiteral
                | TokenKind::FloatLiteral
                | TokenKind::CharLiteral
                | TokenKind::String
        ) {
            let span = act.span.slice(0, INPUT);
            let exp_ident = ident_iter.next().unwrap();
            assert_eq!(span, *exp_ident);
        }
    }
}
