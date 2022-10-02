use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Diagnostic, Error, Debug)]

pub enum PError {
    #[error(transparent)]
    IO(#[from] std::io::Error),

    #[error(transparent)]
    #[diagnostic(transparent)]
    P(#[from] ParseError),

    #[diagnostic(code(ERROR_UNRECOGNIZED_EOF))]
    #[error("unexpected EOF, expected {expected:?}")]
    UnrecognizedEOF {
        #[source_code]
        src: NamedSource,
        expected: Vec<String>,
        #[label("after that")]
        span: SourceSpan,
    },

    #[diagnostic(code(ERROR_UNRECOGNIZED_TOKEN))]
    #[error("unexpected token, expected {expected:?}, but got {actual}")]
    UnrecognizedToken {
        #[source_code]
        src: NamedSource,
        expected: Vec<String>,
        actual: String,
        #[label("in this place")]
        span: SourceSpan,
    },
}

#[derive(Diagnostic, Error, Debug)]
#[diagnostic(code(violet::parse))]
#[error("parse failed!")]
pub struct ParseError {
    #[source_code]
    pub src: NamedSource,
    #[label("bad token")]
    pub bad_token: SourceSpan,
}
