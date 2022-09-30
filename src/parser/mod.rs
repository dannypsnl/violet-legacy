pub mod violet;

#[test]
fn module_simple_case() {
    assert!(violet::ModParser::new().parse("(module hello)").is_ok());
}
