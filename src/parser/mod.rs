use lalrpop_util::ParseError::*;
use miette::{NamedSource, Result};
use std::fs;

pub mod error;
pub mod violet;
use error::{Err, ParseError};

pub fn parse_file(path: &str) -> Result<(), Err> {
    let s = fs::read_to_string(path)?;
    match violet::FileParser::new().parse(s.as_str()) {
        Ok(_) => {
            println!("parse ok");
            Ok(())
        }
        Err(e) => match e {
            InvalidToken { location } => Err(ParseError {
                src: NamedSource::new(path, s[location..location + 1].to_string()),
                bad_bit: (location, location + 1).into(),
                reason: "InvalidToken".to_string(),
            })?,
            UnrecognizedEOF { location, expected } => Err(ParseError {
                src: NamedSource::new(path, s[location - 1..location].to_string()),
                bad_bit: (location - 1, location).into(),
                reason: "UnrecognizedEOF".to_string(),
            })?,
            UnrecognizedToken { token, expected } => Err(ParseError {
                src: NamedSource::new(path, s[token.0..token.2].to_string()),
                bad_bit: (token.0, token.2 - token.0).into(),
                reason: "UnrecognizedToken".to_string(),
            })?,
            ExtraToken { token } => Err(ParseError {
                src: NamedSource::new(path, s[token.0..token.2].to_string()),
                bad_bit: (token.0, token.2 - token.0).into(),
                reason: "extra token".to_string(),
            })?,
            User { error } => Err(ParseError {
                src: NamedSource::new(path, ""),
                bad_bit: (0, 1).into(),
                reason: error.to_string(),
            })?,
        },
    }
}

#[test]
fn module_simple_case() {
    assert!(violet::ModParser::new().parse("(module hello)").is_ok());
}
