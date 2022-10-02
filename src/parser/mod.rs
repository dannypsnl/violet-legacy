pub mod error;
pub mod violet;
use error::{PError, ParseError};
use lalrpop_util::ParseError::*;
use miette::{NamedSource, Result};
use std::fs;

pub fn parse_module_path(path: &str) -> Result<crate::ast::File, PError> {
    let input = fs::read_to_string(path)?;
    let mut r = parse_module(path, input.as_str())?;
    r.source = input;
    Ok(r)
}

pub fn parse_module(path: &str, input: &str) -> Result<crate::ast::File, PError> {
    match violet::FileParser::new().parse(input) {
        Ok(result) => Ok(result),
        Err(e) => match e {
            InvalidToken { location } => Err(ParseError {
                src: NamedSource::new(path, input[location..location + 1].to_string()),
                bad_token: (0, 1).into(),
            })?,
            UnrecognizedEOF { location, expected } => Err(PError::UnrecognizedEOF {
                src: NamedSource::new(path, input[location - 1..location].to_string()),
                expected: normalize(expected),
                span: (0, 1).into(),
            })?,
            UnrecognizedToken { token, expected } => Err(PError::UnrecognizedToken {
                src: NamedSource::new(path, input[token.0..token.2].to_string()),
                expected: normalize(expected),
                actual: token.1.to_string(),
                span: (0, token.2 - token.0).into(),
            })?,
            ExtraToken { token } => Err(ParseError {
                src: NamedSource::new(path, input[token.0..token.2].to_string()),
                bad_token: (0, token.2 - token.0).into(),
            })?,
            User { error } => Err(ParseError {
                src: NamedSource::new(path, input[0..1].to_string()),
                bad_token: (0, 1).into(),
            })?,
        },
    }
}

fn normalize(expected: Vec<String>) -> Vec<String> {
    expected
        .into_iter()
        .map(|s| {
            s.strip_prefix("\"")
                .unwrap()
                .strip_suffix("\"")
                .unwrap()
                .to_string()
        })
        .collect::<Vec<String>>()
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
