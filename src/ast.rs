use miette::{NamedSource, SourceSpan};

pub type Identifier = String;

#[derive(Debug, Clone)]
pub enum Range {
    R(usize, usize),
}
impl Range {
    pub fn to_span(self: &Self) -> SourceSpan {
        let Range::R(l, r) = self;
        (0, r - l).into()
    }

    pub fn src(self: &Self, path: &str, source: &str) -> NamedSource {
        let Range::R(l, r) = self;
        NamedSource::new(path, source[l.clone()..r.clone()].to_string())
    }
}

#[derive(Debug, Clone)]
pub struct File {
    pub source: String,
    pub module: Mod,
    pub top_list: Vec<Top>,
}

#[derive(Debug, Clone)]
pub struct Mod {
    pub name: Identifier,
    pub export_list: Vec<Identifier>,
}

#[derive(Debug, Clone)]
pub enum Top {
    TypeDecl(Range, Identifier, Type),
    DefineProc(Range, Identifier, Vec<Identifier>, Expr),
    DefineVar(Range, Identifier, Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Base(Identifier),
    Arrow(Vec<Type>, Box<Type>),
    Free(),
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Base(id) => write!(f, "{}", id),
            Self::Arrow(a_list, b) => {
                for a in a_list {
                    write!(f, "{} ", a)?;
                }
                write!(f, "-> {}", b)
            }
            Self::Free() => write!(f, "?"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(Range, i64),
    Id(Range, Identifier),
    Lambda(Range, Vec<String>, Box<Expr>),
}
