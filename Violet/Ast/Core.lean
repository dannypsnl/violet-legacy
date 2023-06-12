import Violet.Ast.Common

namespace Violet.Ast.Core

@[reducible]
abbrev MetaVar := Nat
instance : Coe Nat MetaVar := ⟨id⟩

inductive Ix
  | ix (x : Nat)
  deriving Repr, Inhabited, BEq
instance : Coe Ix Nat where
  coe | .ix x => x

inductive Lvl
  | lvl (v : Nat)
  deriving Repr, Inhabited, BEq, Hashable
def Lvl.toNat : Lvl → Nat
  | .lvl v => v
instance : ToString Lvl where
  toString | (.lvl x) => toString x

structure Pattern where
  -- name of constructor
  ctor : Lvl
  vars : Array String
deriving Repr, BEq

inductive Tm
  | type
  | meta (mvar : MetaVar)
  | var (name : String) (ix : Ix)
  | app (fn arg : Tm)
  | pi (name : String) (mode : Mode) (ty body : Tm)
  | lam (name : String) (mode : Mode) (body : Tm)
  | sigma (name : String) (ty body : Tm)
  | pair (fst snd : Tm)
  | «let» (name : String) (ty val body : Tm)
  | «match» (target : Tm) (cases : Array (Pattern × Tm))
  deriving Repr, Inhabited, BEq
abbrev Typ := Tm

end Violet.Ast.Core
