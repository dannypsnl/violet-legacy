use lalrpop_util::ParseError::*;
use miette::{Context, IntoDiagnostic, NamedSource, Result};
use std::fs;

pub mod error;
pub mod violet;
use error::{PError, ParseError};

pub fn parse_file(path: &str) -> Result<(), PError> {
    let s = fs::read_to_string(path)?;
    match violet::FileParser::new().parse(s.as_str()) {
        Ok(_) => {
            println!("parse ok");
            Ok(())
        }
        Err(e) => match e {
            InvalidToken { location } => Err(ParseError {
                src: NamedSource::new(path, s[location..location + 1].to_string()),
                bad_token: (0, 1).into(),
            })?,
            UnrecognizedEOF { location, expected } => Err(ParseError {
                src: NamedSource::new(path, s[location - 1..location].to_string()),
                bad_token: (0, 1).into(),
            })?,
            UnrecognizedToken { token, expected } => Err(ParseError {
                src: NamedSource::new(path, s[token.0..token.2].to_string()),
                bad_token: (0, token.2 - token.0).into(),
            })?,
            ExtraToken { token } => Err(ParseError {
                src: NamedSource::new(path, s[token.0..token.2].to_string()),
                bad_token: (0, token.2 - token.0).into(),
            })?,
            User { error } => Err(ParseError {
                src: NamedSource::new(path, s[0..1].to_string()),
                bad_token: (0, 1).into(),
            })?,
        },
    }
}

#[test]
fn module_simple_case() {
    assert!(violet::ModParser::new().parse("(module hello)").is_ok());
}

#[test]
fn define_variable() {
    assert!(violet::TopParser::new().parse("(define x 1)").is_ok());
}

#[test]
fn define_procedure() {
    assert!(violet::TopParser::new().parse("(define (id x) x)").is_ok());
}
