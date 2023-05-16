namespace Violet.Ast.Surface

inductive Mode
  | implicit
  | explicit
deriving Repr

inductive Tm
  | type
  | var (name : String)
deriving Repr, Inhabited
instance : Coe String Tm where
  coe s := Tm.var s
abbrev Typ := Tm

abbrev Telescope := Array $ String × Mode × Typ
abbrev Constructor := String × Array Typ

inductive Definition
  | «def» (name : String) (tele : Telescope) (ret_ty : Typ) (body : Tm)
  | data (name : String) (constructors : Array Constructor)
deriving Repr

structure Program where
  definitions : Array Definition
deriving Repr

end Violet.Ast.Surface
