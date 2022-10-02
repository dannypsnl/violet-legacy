pub mod error;
pub mod violet;
use error::{PError, ParseError};
use lalrpop_util::ParseError::*;
use miette::{NamedSource, Result};
use std::fs;

pub fn parse_file(path: &str) -> Result<crate::ast::File, PError> {
    let s = fs::read_to_string(path)?;
    match violet::FileParser::new().parse(s.as_str()) {
        Ok(result) => Ok(result),
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
