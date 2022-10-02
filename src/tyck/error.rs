use crate::ast::*;
use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Diagnostic, Error, Debug)]
pub enum TyckError {
    #[diagnostic(code(violet::type_check))]
    #[error("type mismatched expected `{expected}`, got `{actual}`")]
    TyMismatch {
        #[source_code]
        src: NamedSource,
        expected: Type,
        actual: Type,
        #[label("at here")]
        span: SourceSpan,
    },

    #[diagnostic(code(violet::identifier_missing))]
    #[error("no identifier `{name}` can be found in the current context")]
    IdMissing {
        #[source_code]
        src: NamedSource,
        name: String,
        #[label("at here")]
        span: SourceSpan,
    },
}
