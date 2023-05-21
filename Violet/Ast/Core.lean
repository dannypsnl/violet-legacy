import Violet.Ast.Surface

namespace Violet.Ast.Core

@[reducible]
abbrev MetaVar := Nat

inductive Tm
  | type
  | meta (mvar : MetaVar)
  | var (name : String)
  | app (fn : Tm) (arg : Tm)
  | pi (name : String) (mode : Surface.Mode) (ty : Tm) (body : Tm)
  | lam (name : String) (body : Tm)
  | «let» (name : String) (ty : Tm) (val : Tm) (body : Tm)
deriving Repr, Inhabited, BEq
abbrev Typ := Tm

end Violet.Ast.Core
