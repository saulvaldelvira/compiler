use lexer::Lexer;

#[test]
fn string() {
    const INPUT: &str = r#"
    " And I said, \"Hello world!\" "
"#;
    let tokens = Lexer::new(INPUT).tokenize().unwrap();
    assert_eq!(tokens.len(),1);
    let slice = tokens[0].span.slice(INPUT);
    assert_eq!(slice, r#"" And I said, \"Hello world!\" ""#)

}
