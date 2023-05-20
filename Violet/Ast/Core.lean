namespace Violet.Ast.Core

inductive Tm
  | type
  | var (idx : Nat)
  | app (fn : Tm) (arg : Tm)
  | pi (name : String) (ty : Tm) (body : Tm)
  | lam (name : String) (body : Tm)
deriving Repr, Inhabited, BEq
abbrev Typ := Tm

end Violet.Ast.Core
