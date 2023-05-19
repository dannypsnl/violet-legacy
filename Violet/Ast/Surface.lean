namespace Violet.Ast.Surface

inductive Mode
  | implicit
  | explicit
deriving Repr

structure Pattern where
  -- name of constructor
  ctor : String
  vars : Array String
deriving Repr

inductive Tm
  | type
  | var (name : String)
  | «let» (name : String) (ty : Tm) (val : Tm) (body : Tm)
  | «match» (target : Tm) (cases : Array (Pattern × Tm))
  | app (fn : Tm) (arg : Tm)
  | pi (mode : Mode) (name : String) (ty : Tm) (body : Tm)
  | lam (name : String) (body : Tm)
deriving Repr, Inhabited
instance : Coe String Tm where
  coe s := Tm.var s
abbrev Typ := Tm

abbrev Telescope := Array $ String × Mode × Typ
abbrev Ctor := String × Array Typ

inductive Definition
  | «def» (name : String) (tele : Telescope) (ret_ty : Typ) (body : Tm)
  | data (name : String) (constructors : Array Ctor)
deriving Repr

structure Program where
  name : String
  definitions : Array Definition
deriving Repr

end Violet.Ast.Surface
