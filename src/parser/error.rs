use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Diagnostic, Error, Debug)]
#[error(transparent)]

pub enum PError {
    IO(#[from] std::io::Error),

    #[diagnostic(transparent)]
    P(#[from] ParseError),
}

#[derive(Diagnostic, Error, Debug)]
#[error("parse failed!")]
#[diagnostic(code(violet::parse))]
pub struct ParseError {
    #[source_code]
    pub src: NamedSource,
    #[label("bad token")]
    pub bad_token: SourceSpan,
}
