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
    TypeDecl(),
    DefineVar(Identifier),
}
