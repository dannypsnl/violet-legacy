namespace Violet.Ast.Core

@[reducible]
abbrev MetaVar := Nat
@[reducible]
abbrev Lvl := Nat

inductive Tm
  | type
  | meta (mvar : MetaVar)
  | var (idx : Nat)
  | app (fn : Tm) (arg : Tm)
  | pi (name : String) (ty : Tm) (body : Tm)
  | lam (name : String) (body : Tm)
deriving Repr, Inhabited, BEq
abbrev Typ := Tm

end Violet.Ast.Core
