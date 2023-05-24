import Violet.Ast.Surface

namespace Violet.Ast.Core

@[reducible]
abbrev MetaVar := Nat
instance : Coe Nat MetaVar := ⟨id⟩

inductive Ix
  | ix (x : Nat)
  deriving Repr, Inhabited, BEq

inductive Tm
  | type
  | meta (mvar : MetaVar)
  | var (name : Ix)
  | app (fn : Tm) (arg : Tm)
  | pi (name : String) (mode : Surface.Mode) (ty : Tm) (body : Tm)
  | lam (name : String) (mode : Surface.Mode) (body : Tm)
  | «let» (name : String) (ty : Tm) (val : Tm) (body : Tm)
  deriving Repr, Inhabited, BEq
abbrev Typ := Tm

end Violet.Ast.Core
