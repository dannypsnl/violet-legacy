use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Diagnostic, Error, Debug)]

pub enum PError {
    #[error(transparent)]
    IO(#[from] std::io::Error),

    #[error(transparent)]
    #[diagnostic(transparent)]
    P(#[from] ParseError),
}

#[derive(Diagnostic, Error, Debug)]
#[error("parse failed!")]
#[diagnostic(code(violet::parse))]
pub struct ParseError {
    #[source_code]
    pub src: NamedSource,
    #[label("location")]
    pub bad_bit: SourceSpan,
}
