mod parser;
#[macro_use]
extern crate lalrpop_util;

fn main() {
    lalrpop_mod!(pub violet);

    #[test]
    fn calculator1() {
        assert!(violet::TermParser::new().parse("22").is_ok());
    }
}
