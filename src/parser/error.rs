use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Diagnostic, Error, Debug)]

pub enum Err {
    #[error(transparent)]
    IO(#[from] std::io::Error),

    #[error(transparent)]
    P(#[from] ParseError),
}

#[derive(Diagnostic, Error, Debug)]
#[error("parse failed!")]
#[diagnostic(code(violet::parse), url(docsrs))]
pub struct ParseError {
    #[source_code]
    pub src: NamedSource,

    #[label("location")]
    pub bad_bit: SourceSpan,

    #[help("reason")]
    pub reason: String,
}
