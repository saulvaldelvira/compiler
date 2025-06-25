use error_manager::ErrorManager;
use lexer::Lexer;
use span::Source;

#[test]
fn string() {
    const INPUT: &str = r#"
    " And I said, \"Hello world!\" "
"#;
    let mut source = Source::default();
    let file = source.add_file_anon(INPUT.into());
    let tokens = Lexer::new(file, &mut ErrorManager::new())
        .into_token_stream()
        .collect::<Vec<_>>();
    assert_eq!(tokens.len(), 1);
    let slice = tokens[0].span.slice(INPUT);
    assert_eq!(slice, r#"" And I said, \"Hello world!\" ""#);
}
