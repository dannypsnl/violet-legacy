use std::fs;

pub mod error;
pub mod violet;

pub fn parse_file(path: &str) -> std::io::Result<()> {
    let s = fs::read_to_string(path)?;
    match violet::FileParser::new().parse(s.as_str()) {
        Ok(_) => {
            println!("parse ok");
        }
        Err(e) => {
            println!("parse failed {:?}", e);
        }
    }
    Ok(())
}

#[test]
fn module_simple_case() {
    assert!(violet::ModParser::new().parse("(module hello)").is_ok());
}

#[test]
fn test_output() {
    println!("{:?}", violet::ModParser::new().parse("(modul hello)"));
}
