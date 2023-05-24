import Violet.Ast.Surface

namespace Violet.Ast.Core

@[reducible]
abbrev MetaVar := Nat

inductive Ix
  | ix (x : Nat)
  deriving Repr, Inhabited, BEq

inductive Tm
  | type
  | meta (mvar : MetaVar)
  | var (name : Ix)
  | app (fn : Tm) (arg : Tm)
  | pi (name : String) (mode : Surface.Mode) (ty : Tm) (body : Tm)
  | lam (name : String) (body : Tm)
  | «let» (name : String) (ty : Tm) (val : Tm) (body : Tm)
  deriving Repr, Inhabited, BEq
abbrev Typ := Tm

def Tm.toString : Tm → String
  | .lam p body => s!"λ {p} => {body.toString}"
  | .pi p .implicit ty body =>
    "{" ++ p ++ " : " ++ ty.toString ++ "} → " ++ body.toString
  | .pi p .explicit ty body =>
    "(" ++ p ++ " : " ++ ty.toString ++ ") → " ++ body.toString
  | .app t u => s!"{t.toString} {u.toString}"
  | .var (.ix x) => s!"{x}"
  | .meta m => s!"?{m}"
  | .let p ty val body => s!"let {p} : {ty.toString} := {val.toString} in {body.toString}"
  | .type => "Type"
instance : ToString Tm where
  toString := Tm.toString
  

end Violet.Ast.Core
