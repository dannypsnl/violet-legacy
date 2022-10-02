type Identifier = String;

#[derive(Debug, Clone)]
pub struct File {
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
    TypeDecl(Identifier, Type),
    DefineProc(Identifier, Vec<Identifier>, Expr),
    DefineVar(Identifier, Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Base(Identifier),
    Arrow(Box<Type>, Box<Type>),
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Base(id) => write!(f, "{}", id),
            Self::Arrow(a, b) => write!(f, "{} -> {}", a, b),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Id(Identifier),
}
