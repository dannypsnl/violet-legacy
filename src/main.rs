use std::fs;

mod parser;
#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub violet);

fn main() -> std::io::Result<()> {
    let s = fs::read_to_string("example/hello.ss")?;
    match violet::TermParser::new().parse(s.as_str()) {
        Ok(_) => {
            println!("parse ok");
        }
        Err(e) => {
            println!("parse failed {:?}", e);
        }
    }
    return Ok(());
}

#[test]
fn calculator1() {
    assert!(violet::TermParser::new().parse("22").is_ok());
}
