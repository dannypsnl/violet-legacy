pub mod error;
pub mod violet;
use error::{PError, ParseError};
use lalrpop_util::ParseError::*;
use miette::{NamedSource, Result};
use std::fs;

pub fn parse_module_path(path: &str) -> Result<crate::ast::ModFile, PError> {
    let input: &'static str = Box::leak(fs::read_to_string(path)?.into_boxed_str());
    let mut mod_file = parse_module(path, input)?;
    mod_file.path = path.to_string();
    mod_file.source = input;
    Ok(mod_file)
}

pub fn parse_module(path: &str, input: &'static str) -> Result<crate::ast::ModFile, PError> {
    match violet::FileParser::new().parse(input) {
        Ok(result) => Ok(result),
        Err(e) => match e {
            InvalidToken { location } => Err(ParseError {
                src: NamedSource::new(path, input),
                bad_token: (location, 1).into(),
            })?,
            UnrecognizedEOF { location, expected } => Err(PError::UnrecognizedEOF {
                src: NamedSource::new(path, input),
                expected: normalize(expected),
                span: (location - 1, 1).into(),
            })?,
            UnrecognizedToken { token, expected } => Err(PError::UnrecognizedToken {
                src: NamedSource::new(path, input),
                expected: normalize(expected),
                actual: token.1.to_string(),
                span: (token.0, token.2 - token.0).into(),
            })?,
            ExtraToken { token } => Err(ParseError {
                src: NamedSource::new(path, input),
                bad_token: (token.0, token.2 - token.0).into(),
            })?,
            User { error } => Err(ParseError {
                src: NamedSource::new(path, input),
                bad_token: (0, input.len() - 1).into(),
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
