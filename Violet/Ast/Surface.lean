namespace Violet.Ast.Surface

inductive Mode
  | implicit
  | explicit
deriving Repr, BEq

structure Pattern where
  -- name of constructor
  ctor : String
  vars : Array String
deriving Repr, BEq

inductive Tm
  | type
  | var (name : String)
  | «let» (name : String) (ty : Tm) (val : Tm) (body : Tm)
  | «match» (target : Tm) (cases : Array (Pattern × Tm))
  | app (fn : Tm) (arg : Tm)
  | pi (mode : Mode) (name : String) (ty : Tm) (body : Tm)
  | lam (name : String) (body : Tm)
deriving Repr, Inhabited, BEq
instance : Coe String Tm where
  coe s := Tm.var s
abbrev Typ := Tm

abbrev Telescope := Array <| String × Mode × Typ
abbrev Ctor := String × Array Typ

inductive Definition
  | «def» (name : String) (tele : Telescope) (ret_ty : Typ) (body : Tm)
  | data (name : String) (constructors : Array Ctor)
deriving Repr, BEq

structure Program where
  name : String
  definitions : Array Definition
deriving Repr

def Tm.toString : Tm → String
  | .lam p body => s!"λ {p} => {body.toString}"
  | .pi .implicit p ty body =>
    "{" ++ p ++ " : " ++ ty.toString ++ "} → " ++ body.toString
  | .pi .explicit p ty body =>
    "(" ++ p ++ " : " ++ ty.toString ++ ") → " ++ body.toString
  | .app t u => s!"{t.toString} {u.toString}"
  | .var x => x
  | .let p ty val body =>
    s!"let {p} : {ty.toString} := {val.toString} in {body.toString}"
  | .match p cs => sorry
  | .type => "Type"
instance : ToString Tm where
  toString := Tm.toString

instance : ToString Telescope where
  toString ts := Id.run do
    let mut r := ""
    for (name, mode, ty) in ts do
      let bind := s!"{name} : {ty}"
      match mode with
      | .implicit =>
        r := r ++ "{" ++ bind ++ "}"
      | .explicit =>
        r := r ++ "(" ++ bind ++ ")"
    return r

instance : ToString Definition where
  toString
  | .def x tele ret body =>
    s!"def {x} {tele}: {ret} => {body}"
  | .data n cs => s!"data {n} {cs}"

end Violet.Ast.Surface
