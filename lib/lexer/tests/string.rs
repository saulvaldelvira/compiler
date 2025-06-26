use error_manager::ErrorManager;
use lexer::Lexer;
use span::SourceMap;

#[test]
fn string() {
    const INPUT: &str = r#"
    " And I said, \"Hello world!\" "
"#;
    let mut source = SourceMap::default();
    let (file, id) = source.add_file_anon(INPUT.into()).into_parts();
    let tokens = Lexer::new(&file, id, &mut ErrorManager::new())
        .into_token_stream()
        .collect::<Vec<_>>();
    assert_eq!(tokens.len(), 1);
    let slice = tokens[0].span.slice(0, INPUT);
    assert_eq!(slice, r#"" And I said, \"Hello world!\" ""#);
}
