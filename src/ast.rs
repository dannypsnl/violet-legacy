use miette::{NamedSource, SourceSpan};

pub type Identifier = String;

#[derive(Debug, Clone)]
pub enum Range {
    R(usize, usize),
}
impl Range {
    pub fn to_span(self: &Self) -> SourceSpan {
        let Range::R(l, r) = self;
        (l.clone(), r - l).into()
    }
}

#[derive(Debug, Clone)]
pub struct ModFile {
    pub path: String,
    pub source: &'static str,
    pub module: Mod,
    pub top_list: Vec<Top>,
}

impl ModFile {
    pub fn new() -> ModFile {
        ModFile {
            path: "".to_string(),
            source: "",
            module: Mod::new(),
            top_list: vec![],
        }
    }

    pub fn as_src(self: &Self) -> NamedSource {
        NamedSource::new(self.path.as_str(), self.source)
    }
}

#[derive(Debug, Clone)]
pub struct Mod {
    pub name: Identifier,
    pub export_list: Vec<Identifier>,
}

impl Mod {
    pub fn new() -> Mod {
        Mod {
            name: "".to_string(),
            export_list: vec![],
        }
    }
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
    Free(i64),
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
            Self::Free(i) => write!(f, "?{}", i.to_owned()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(Range, i64),
    Id(Range, Identifier),
    Lambda(Range, Vec<String>, Box<Expr>),
}
