type Identifier = String;

#[derive(Debug)]
pub struct File {
    pub module: Mod,
    pub top_list: Vec<Top>,
}

#[derive(Debug)]
pub struct Mod {
    pub name: Identifier,
    pub export_list: Vec<Identifier>,
}

#[derive(Debug)]
pub enum Top {
    TypeDecl(Type),
    DefineVar(Identifier, Expr),
}

#[derive(Debug)]
pub enum Type {
    Base(Identifier),
    Arrow(Box<Type>, Box<Type>),
}

#[derive(Debug)]
pub enum Expr {
    Int(i64),
}
