type Identifier = String;

#[derive(Debug)]
pub struct Mod { 
   pub name: Identifier,
     pub export_list : Vec<Identifier>,
}