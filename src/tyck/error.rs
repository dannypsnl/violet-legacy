use crate::ast::*;
use miette::Diagnostic;
use thiserror::Error;

#[derive(Diagnostic, Error, Debug)]
#[error(transparent)]
#[diagnostic(transparent)]
pub enum TyckError {
    M(#[from] TyMismatch),
    I(#[from] IdMissing),
}

#[derive(Diagnostic, Error, Debug)]
#[error("type mismatched expected `{expected}`, got `{actual}`")]
#[diagnostic(code(violet::type_check))]
pub struct TyMismatch {
    pub expected: Type,
    pub actual: Type,
}

#[derive(Diagnostic, Error, Debug)]
#[error("no identifier `{name}` can be found in the current context")]
#[diagnostic(code(violet::identifier_missing))]
pub struct IdMissing {
    pub name: String,
}
